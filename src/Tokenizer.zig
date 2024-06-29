const std = @import("std");
const testing = std.testing;

const util = @import("util.zig");

/// https://docs.fauna.com/fauna/current/reference/fql_reference/lexical#whitespace
fn isWhitespace(char: u8) bool {
    // character tab            : u+0009
    // line tab                 : u+000B
    // form feed                : u+000C
    // space                    : u+0020
    // no-break space           : u+00A0
    // zero-width no-break space: u+feff (unhandled)
    return char == '\x09' or char == '\x0B' or char == '\x0C' or char == '\x20' or char == '\xA0';
}

/// https://docs.fauna.com/fauna/current/reference/fql_reference/lexical#line-terminators
fn isLineTerminator(char: u8) bool {
    // line-feed          : u+000A
    // carriage return    : u+000D
    // line separator     : u+2028 (unhandled)
    // paragraph separator: u+2029 (unhandled)
    return char == '\x0A' or char == '\x0D';
}

fn isIdentifierChar(char: u8, index: usize) bool {
    return char == '_' or ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or (index > 0 and '0' <= char and char <= '9');
}

fn isQuote(char: u8) bool {
    return char == '"' or char == '\'';
}

fn isNumberChar(char: u8, index: usize) bool {
    return (('0' <= char and char <= '9') or char == '-') or (index == 1 and (char == 'b' or char == 'o' or char == 'x')) or (index > 0 and (char == '.' or char == '_' or char == 'e')) or (index > 2 and (('a' <= char and char <= 'f') or ('A' <= char and char <= 'F')));
}

pub const Token = union(enum) {
    eof,
    eol,

    ampersand,
    ampersand2,
    asterisk,
    asterisk2,
    at,
    bang,
    bang_equal,
    caret,
    colon,
    comma,
    dot,
    dot3,
    equal,
    equal2,
    equal_rarrow,
    larrow,
    larrow_equal,
    lbrace,
    lbracket,
    lparen,
    minus,
    minus_rarrow,
    percent,
    pipe,
    pipe2,
    plus,
    question,
    question2,
    question_dot,
    rarrow,
    rarrow_equal,
    rbrace,
    rbracket,
    rparen,
    semi,
    slash,
    tilde,

    annotation: []const u8,
    word: []const u8,
    number: []const u8,
    string: []const u8,
    comment_line: []const u8,
    comment_block: []const u8,

    pub fn deinit(self: Token, allocator: std.mem.Allocator) void {
        switch (self) {
            inline .annotation,
            .string,
            .word,
            .number,
            .comment_line,
            .comment_block,
            => |s| allocator.free(s),
            else => {},
        }
    }

    pub fn dupe(self: Token, allocator: std.mem.Allocator) !Token {
        return switch (self) {
            inline .annotation,
            .string,
            .word,
            .number,
            => |s, tag| @unionInit(Token, @tagName(tag), try allocator.dupe(u8, s)),
            else => self,
        };
    }
};

const WordPrefix = union(enum) {
    none,
    at,

    fn fromChar(c: u8) WordPrefix {
        return switch (c) {
            '@' => .at,
            else => .none,
        };
    }

    fn len(self: @This()) usize {
        if (self == .none) {
            return 0;
        }

        return 1;
    }

    fn toString(self: @This()) []const u8 {
        return switch (self) {
            .none => "",
            .at => "@",
        };
    }
};

buf_start: usize = 0,
buf_end: usize = 0,
buf: [2048]u8 = undefined,
saved_token: ?Token = null,
is_eof: bool = false,

const Tokenizer = @This();

pub fn write(self: *Tokenizer, bytes: []const u8) !usize {
    if (self.buf_start > 0 and self.buf.len - self.buf_end < bytes.len) {
        const new_end = self.buf_end - self.buf_start;
        std.mem.copyForwards(u8, self.buf[0..new_end], self.buf[self.buf_end..]);
        self.buf_end = new_end;
        self.buf_start = 0;
    }

    const dst = self.buf[self.buf_end..];
    if (dst.len == 0) {
        return error.NoSpaceLeft;
    }

    if (dst.len < bytes.len) {
        @memcpy(dst, bytes[0..dst.len]);
    } else {
        @memcpy(dst[0..bytes.len], bytes);
    }

    const written = @min(dst.len, bytes.len);
    self.buf_end += written;

    return written;
}

pub fn next(self: *Tokenizer, allocator: std.mem.Allocator) !?Token {
    var chars: []const u8 = self.buf[self.buf_start..self.buf_end];

    while (chars.len > 0 and isWhitespace(chars[0])) {
        chars.ptr += 1;
        chars.len -= 1;
        self.buf_start += 1;
    }

    if (chars.len == 0) {
        if (self.is_eof) {
            return null;
        } else {
            return error.NeedMoreData;
        }
    }

    if (isLineTerminator(chars[0])) {
        if (util.slice.indexOfNonePosFn(chars, 1, isLineTerminator)) |pos| {
            self.buf_start += pos;
            return .eol;
        } else {
            self.buf_start += 1;
            return .eol;
        }
    }

    if (@as(?std.meta.Tuple(&.{ Token, usize }), switch (chars[0]) {
        '*' => blk: {
            if (chars.len > 1 and chars[1] == '*') {
                break :blk .{ .asterisk2, 2 };
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .asterisk, 1 };
        },
        '&' => blk: {
            if (chars.len > 1 and chars[1] == '&') {
                break :blk .{ .ampersand2, 2 };
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .ampersand, 1 };
        },
        '!' => blk: {
            if (chars.len > 1 and chars[1] == '=') {
                break :blk .{ .bang_equal, 2 };
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .bang, 1 };
        },
        '^' => .{ .caret, 1 },
        ':' => .{ .colon, 1 },
        ';' => .{ .semi, 1 },
        ',' => .{ .comma, 1 },
        '.' => blk: {
            if (chars.len > 2 and chars[1] == '.' and chars[2] == '.') {
                break :blk .{ .dot3, 3 };
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .dot, 1 };
        },
        '<' => blk: {
            if (chars.len > 1 and chars[1] == '=') {
                break :blk .{ .larrow_equal, 2 };
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .larrow, 1 };
        },
        '{' => .{ .lbrace, 1 },
        '[' => .{ .lbracket, 1 },
        '(' => .{ .lparen, 1 },
        '%' => .{ .percent, 1 },
        '|' => blk: {
            if (chars.len > 1 and chars[1] == '|') {
                break :blk .{ .pipe2, 2 };
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .pipe, 1 };
        },
        '+' => .{ .plus, 1 },
        '?' => blk: {
            if (chars.len > 1) {
                switch (chars[1]) {
                    '.' => break :blk .{ .question_dot, 2 },
                    '?' => break :blk .{ .question2, 2 },
                    else => {},
                }
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .question, 1 };
        },
        '>' => blk: {
            if (chars.len > 1 and chars[1] == '=') {
                break :blk .{ .rarrow_equal, 2 };
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .rarrow, 1 };
        },
        '}' => .{ .rbrace, 1 },
        ']' => .{ .rbracket, 1 },
        ')' => .{ .rparen, 1 },
        '/' => blk: {
            if (chars.len > 1 and (chars[1] == '/' or chars[1] == '*')) {
                break :blk null;
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .slash, 1 };
        },
        '=' => blk: {
            if (chars.len >= 2) {
                switch (chars[1]) {
                    '>' => break :blk .{ .equal_rarrow, 2 },
                    '=' => break :blk .{ .equal2, 2 },
                    else => {},
                }
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .equal, 1 };
        },
        '-' => blk: {
            if (chars.len >= 2) {
                switch (chars[1]) {
                    '>' => break :blk .{ .minus_rarrow, 2 },
                    '0'...'9' => break :blk null,
                    else => {},
                }
            }

            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .minus, 1 };
        },
        '~' => .{ .tilde, 1 },
        else => null,
    })) |match| {
        const token, const len = match;
        self.buf_start += len;
        return token;
    }

    if (chars.len >= 2 and isQuote(chars[0])) {
        const eol_pos = util.slice.indexOfPosFn(chars, 1, isLineTerminator);

        var start: usize = 1;
        while (std.mem.indexOfScalarPos(u8, chars, start, chars[0])) |pos| {
            if (pos > 1 and chars[pos - 1] == '\\') {
                // this quote is escaped
                start = pos + 1;
                continue;
            }

            // found an matching unescaped quote
            if (eol_pos == null or eol_pos.? > pos) {
                self.buf_start += pos + 1;
                return .{ .string = try allocator.dupe(u8, chars[0 .. pos + 1]) };
            }
        } else if (eol_pos == null) {
            // reach end of buf without finding a matching quote or an eol
            return error.NeedMoreData;
        }
    }

    if (chars.len >= 3 and chars[0] == '/' and chars[1] == '/') {
        const pos = util.slice.indexOfPosFn(chars, 2, isLineTerminator);
        if (pos == null) {
            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            self.buf_start = self.buf_end;
            return .{ .comment_line = try allocator.dupe(u8, chars) };
        }

        self.buf_start += pos.? + 1;
        return .{ .comment_line = try allocator.dupe(u8, chars[0..pos.?]) };
    }

    if (chars.len >= 4 and chars[0] == '/' and chars[1] == '*') {
        if (std.mem.indexOfPos(u8, chars, 2, "*/")) |pos| {
            self.buf_start += pos + 2;
            return .{ .comment_block = try allocator.dupe(u8, chars[0 .. pos + 2]) };
        } else {
            return error.NeedMoreData;
        }
    }

    if (isNumberChar(chars[0], 0)) {
        // very loosely tokenize the number, it's okay if it's not valid
        if (util.slice.indexOfNonePosFn(chars, 1, isIdentifierChar)) |dot_pos| {
            if (chars[dot_pos] == '.') {
                if (util.slice.indexOfNonePosFn(chars, dot_pos + 1, isIdentifierChar)) |index| {
                    chars.len = index;
                }
            } else {
                chars.len = dot_pos;
            }
        }

        self.buf_start += chars.len;
        return .{ .number = try allocator.dupe(u8, chars) };
    }

    const prefix = WordPrefix.fromChar(chars[0]);
    if (prefix != .none) {
        chars.ptr += prefix.len();
        chars.len -= prefix.len();
        self.buf_start += prefix.len();
    }

    if (util.slice.indexOfNoneFn(chars, isIdentifierChar)) |index| {
        if (index == 0) {
            std.debug.panic("unexpected non-identifier character: '{d}' \"{s}\"", .{ chars[0], chars });
        }

        chars.len = index;
    }

    if (chars.len > 0) {
        if (prefix == .at) {
            self.buf_start += chars.len;
            return .{ .annotation = try allocator.dupe(u8, chars) };
        }

        self.buf_start += chars.len;
        return .{ .word = try allocator.dupe(u8, chars) };
    }

    std.log.err("found unexpected token: {d} \"{s}{s}\"", .{ chars.len + prefix.len(), prefix.toString(), chars });

    return error.UnexpectedToken;
}

pub const TokenIterator = struct {
    reader: std.io.AnyReader,
    tokenizer: Tokenizer = .{},
    saved_token: ?Token = null,

    pub fn init(reader: std.io.AnyReader) TokenIterator {
        return .{ .reader = reader };
    }

    pub fn deinit(self: TokenIterator, allocator: std.mem.Allocator) void {
        if (self.saved_token) |token| {
            token.deinit(allocator);
        }
    }

    pub fn saveToken(self: *TokenIterator, token: ?Token) void {
        std.debug.assert(self.saved_token == null);
        self.saved_token = token;
    }

    pub fn nextToken(self: *TokenIterator, allocator: std.mem.Allocator) !Token {
        if (try self.next(allocator)) |token| {
            return token;
        }

        return .eof;
    }

    fn dirtyNext(self: *TokenIterator, allocator: std.mem.Allocator) !?Token {
        if (self.saved_token) |token| {
            self.saved_token = null;
            if (token == .eof) {
                return null;
            }
            return token;
        }

        return self.tokenizer.next(allocator) catch |err| {
            if (err == error.NeedMoreData) {
                var buf: [@typeInfo(std.meta.FieldType(Tokenizer, .buf)).Array.len]u8 = undefined;

                const n = try self.reader.read(buf[0 .. self.tokenizer.buf.len - self.tokenizer.buf_end + self.tokenizer.buf_start]);
                if (n == 0) {
                    self.tokenizer.is_eof = true;
                } else {
                    _ = self.tokenizer.write(buf[0..n]) catch unreachable;
                }

                return self.tokenizer.next(allocator);
            }

            return err;
        };
    }

    pub fn next(self: *TokenIterator, allocator: std.mem.Allocator) !?Token {
        if (try self.dirtyNext(allocator)) |token| {
            return token;
        }

        return null;
    }
};

fn expectNextTokenEqualDeep(it: *TokenIterator, expected: ?Token) !void {
    if (try it.next(testing.allocator)) |token| {
        defer token.deinit(testing.allocator);

        try testing.expectEqualDeep(expected, token);
    } else {
        try testing.expectEqualDeep(expected, null);
    }
}

test "read token" {
    {
        var stream = std.io.fixedBufferStream("");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream("2s3a4_y5.6e");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .number = "2s3a4_y5.6e" });
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream("-12_3as_dv.2e3.foo");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .number = "-12_3as_dv.2e3" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "foo" });
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream("collection Product {}");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .word = "collection" });
        try expectNextTokenEqualDeep(&it, .{ .word = "Product" });
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream("move.a->.b");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .word = "move" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "a" });
        try expectNextTokenEqualDeep(&it, .minus_rarrow);
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "b" });
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream("access provider apname { issuer 'url' }");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .word = "access" });
        try expectNextTokenEqualDeep(&it, .{ .word = "provider" });
        try expectNextTokenEqualDeep(&it, .{ .word = "apname" });
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .{ .word = "issuer" });
        try expectNextTokenEqualDeep(&it, .{ .string = "'url'" });
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream("some words /* wrapping around a */ comment block");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .word = "some" });
        try expectNextTokenEqualDeep(&it, .{ .word = "words" });
        try expectNextTokenEqualDeep(&it, .{ .comment_block = "/* wrapping around a */" });
        try expectNextTokenEqualDeep(&it, .{ .word = "comment" });
        try expectNextTokenEqualDeep(&it, .{ .word = "block" });
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream(
            \\// Single-line comments start with double-slash.
            \\/*
            \\  Block comments start with slash-asterisk
            \\  and end with asterisk-slash.
            \\*/
            \\
            \\// Statements don't have to be terminated by ; ...
            \\(1 + 3) * 2
            \\
            \\// ... but can be.
            \\(1 + 3) * 2;
            \\
            \\/////////////////////////////////////////////
            \\// Numbers, Strings, and Operators
            \\
            \\// FQL has integer, decimal, and exponential values stored as
            \\// Int, Long, or Double types, which are all Number types.
            \\// An Int is a signed 32-bit integer type.
            \\// A Long is a signed 64-bit integer type.
            \\// Doubles are double-precision, 64-bit binary type, IEEE 754-2019.
            \\3 // 3
            \\1.5 // 1.5
            \\3.14e5 // 314000
        );
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Single-line comments start with double-slash." });
        try expectNextTokenEqualDeep(&it, .{ .comment_block = "/*\n  Block comments start with slash-asterisk\n  and end with asterisk-slash.\n*/" });
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Statements don't have to be terminated by ; ..." });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .number = "1" });
        try expectNextTokenEqualDeep(&it, .plus);
        try expectNextTokenEqualDeep(&it, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .asterisk);
        try expectNextTokenEqualDeep(&it, .{ .number = "2" });
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// ... but can be." });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .number = "1" });
        try expectNextTokenEqualDeep(&it, .plus);
        try expectNextTokenEqualDeep(&it, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .asterisk);
        try expectNextTokenEqualDeep(&it, .{ .number = "2" });
        try expectNextTokenEqualDeep(&it, .semi);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "/////////////////////////////////////////////" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Numbers, Strings, and Operators" });
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// FQL has integer, decimal, and exponential values stored as" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Int, Long, or Double types, which are all Number types." });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// An Int is a signed 32-bit integer type." });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// A Long is a signed 64-bit integer type." });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Doubles are double-precision, 64-bit binary type, IEEE 754-2019." });
        try expectNextTokenEqualDeep(&it, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// 3" });
        try expectNextTokenEqualDeep(&it, .{ .number = "1.5" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// 1.5" });
        try expectNextTokenEqualDeep(&it, .{ .number = "3.14e5" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// 314000" });
        try expectNextTokenEqualDeep(&it, null);
    }

    {
        var stream = std.io.fixedBufferStream(
            \\@role(server)
            \\function submitOrder(customer, productsRequested) {
            \\  if (productsRequested.length == 0) {
            \\    abort("Cart can't be empty")
            \\  }
            \\
            \\  Order.create({
            \\    customer: customer,
            \\    cart: shoppingCart,
            \\    status: "processing",
            \\    creationDate: Time.now(),
            \\    shipDate: null,
            \\    deliveryAddress: customer.address,
            \\    creditCard: customer.creditCard
            \\  })
            \\}
        );
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, .{ .annotation = "role" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .word = "server" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "function" });
        try expectNextTokenEqualDeep(&it, .{ .word = "submitOrder" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "productsRequested" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "if" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .word = "productsRequested" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "length" });
        try expectNextTokenEqualDeep(&it, .equal2);
        try expectNextTokenEqualDeep(&it, .{ .number = "0" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "abort" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .string = "\"Cart can't be empty\"" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "Order" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "create" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "cart" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "shoppingCart" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "status" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .string = "\"processing\"" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "creationDate" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "Time" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "now" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "shipDate" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "null" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "deliveryAddress" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "address" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .{ .word = "creditCard" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "creditCard" });
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .eol);
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, null);
    }
}
