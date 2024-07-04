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

current_line: usize = 0,
current_col: usize = 0,

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

fn consumeBytes(self: *Tokenizer, n: usize) void {
    const consumed_bytes = self.buf[self.buf_start .. self.buf_start + n];
    self.buf_start += n;

    var line_it = std.mem.splitScalar(u8, consumed_bytes, '\n');
    var last_line = line_it.next().?;
    var new_lines: usize = 0;
    while (line_it.next()) |line| {
        new_lines += 1;
        last_line = line;
    }

    if (new_lines > 0) {
        self.current_col = 0;
        self.current_line += new_lines;
    }

    for (last_line) |c| {
        if (c == '\r') {
            // ignore
            continue;
        }

        self.current_col += 1;
    }
}

pub fn next(self: *Tokenizer, allocator: std.mem.Allocator) !?Token {
    var chars: []const u8 = self.buf[self.buf_start..self.buf_end];

    while (chars.len > 0 and isWhitespace(chars[0])) {
        chars.ptr += 1;
        chars.len -= 1;
        self.consumeBytes(1);
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
            self.consumeBytes(pos);
            return .eol;
        } else {
            self.consumeBytes(1);
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
        self.consumeBytes(len);
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
                self.consumeBytes(pos + 1);
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

            self.consumeBytes(self.buf_end - self.buf_start);
            return .{ .comment_line = try allocator.dupe(u8, chars) };
        }

        self.consumeBytes(pos.? + 1);
        return .{ .comment_line = try allocator.dupe(u8, chars[0..pos.?]) };
    }

    if (chars.len >= 4 and chars[0] == '/' and chars[1] == '*') {
        if (std.mem.indexOfPos(u8, chars, 2, "*/")) |pos| {
            self.consumeBytes(pos + 2);
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

        self.consumeBytes(chars.len);
        return .{ .number = try allocator.dupe(u8, chars) };
    }

    const prefix = WordPrefix.fromChar(chars[0]);
    if (prefix != .none) {
        chars.ptr += prefix.len();
        chars.len -= prefix.len();
        self.consumeBytes(prefix.len());
    }

    if (util.slice.indexOfNoneFn(chars, isIdentifierChar)) |index| {
        if (index == 0) {
            std.debug.panic("unexpected non-identifier character: '{d}' \"{s}\"", .{ chars[0], chars });
        }

        chars.len = index;
    }

    if (chars.len > 0) {
        if (prefix == .at) {
            self.consumeBytes(chars.len);
            return .{ .annotation = try allocator.dupe(u8, chars) };
        }

        self.consumeBytes(chars.len);
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

fn expectNextTokenNull(it: *TokenIterator) !void {
    try testing.expectEqualDeep(null, try it.next(testing.allocator));
}

fn expectNextTokenEqualDeep(it: *TokenIterator, line: usize, col: usize, expected: Token) !void {
    const token = (try it.next(testing.allocator)).?;
    defer token.deinit(testing.allocator);

    try testing.expectEqualDeep(expected, token);
    try testing.expectEqual(line, it.tokenizer.current_line);
    try testing.expectEqual(col, it.tokenizer.current_col);
}

test "read token" {
    {
        var stream = std.io.fixedBufferStream("");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("2s3a4_y5.6e");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, 0, 11, .{ .number = "2s3a4_y5.6e" });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("-12_3as_dv.2e3.foo");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, 0, 14, .{ .number = "-12_3as_dv.2e3" });
        try expectNextTokenEqualDeep(&it, 0, 15, .dot);
        try expectNextTokenEqualDeep(&it, 0, 18, .{ .word = "foo" });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("collection Product {}");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, 0, 10, .{ .word = "collection" });
        try expectNextTokenEqualDeep(&it, 0, 18, .{ .word = "Product" });
        try expectNextTokenEqualDeep(&it, 0, 20, .lbrace);
        try expectNextTokenEqualDeep(&it, 0, 21, .rbrace);
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("move.a->.b");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, 0, 4, .{ .word = "move" });
        try expectNextTokenEqualDeep(&it, 0, 5, .dot);
        try expectNextTokenEqualDeep(&it, 0, 6, .{ .word = "a" });
        try expectNextTokenEqualDeep(&it, 0, 8, .minus_rarrow);
        try expectNextTokenEqualDeep(&it, 0, 9, .dot);
        try expectNextTokenEqualDeep(&it, 0, 10, .{ .word = "b" });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("access provider apname { issuer 'url' }");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, 0, 6, .{ .word = "access" });
        try expectNextTokenEqualDeep(&it, 0, 15, .{ .word = "provider" });
        try expectNextTokenEqualDeep(&it, 0, 22, .{ .word = "apname" });
        try expectNextTokenEqualDeep(&it, 0, 24, .lbrace);
        try expectNextTokenEqualDeep(&it, 0, 31, .{ .word = "issuer" });
        try expectNextTokenEqualDeep(&it, 0, 37, .{ .string = "'url'" });
        try expectNextTokenEqualDeep(&it, 0, 39, .rbrace);
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("some words /* wrapping around a */ comment block");
        var it = TokenIterator.init(stream.reader().any());
        try expectNextTokenEqualDeep(&it, 0, 4, .{ .word = "some" });
        try expectNextTokenEqualDeep(&it, 0, 10, .{ .word = "words" });
        try expectNextTokenEqualDeep(&it, 0, 34, .{ .comment_block = "/* wrapping around a */" });
        try expectNextTokenEqualDeep(&it, 0, 42, .{ .word = "comment" });
        try expectNextTokenEqualDeep(&it, 0, 48, .{ .word = "block" });
        try expectNextTokenNull(&it);
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
        try expectNextTokenEqualDeep(&it, 1, 0, .{ .comment_line = "// Single-line comments start with double-slash." });
        try expectNextTokenEqualDeep(&it, 4, 2, .{ .comment_block = "/*\n  Block comments start with slash-asterisk\n  and end with asterisk-slash.\n*/" });
        try expectNextTokenEqualDeep(&it, 6, 0, .eol);
        try expectNextTokenEqualDeep(&it, 7, 0, .{ .comment_line = "// Statements don't have to be terminated by ; ..." });
        try expectNextTokenEqualDeep(&it, 7, 1, .lparen);
        try expectNextTokenEqualDeep(&it, 7, 2, .{ .number = "1" });
        try expectNextTokenEqualDeep(&it, 7, 4, .plus);
        try expectNextTokenEqualDeep(&it, 7, 6, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, 7, 7, .rparen);
        try expectNextTokenEqualDeep(&it, 7, 9, .asterisk);
        try expectNextTokenEqualDeep(&it, 7, 11, .{ .number = "2" });
        try expectNextTokenEqualDeep(&it, 9, 0, .eol);
        try expectNextTokenEqualDeep(&it, 10, 0, .{ .comment_line = "// ... but can be." });
        try expectNextTokenEqualDeep(&it, 10, 1, .lparen);
        try expectNextTokenEqualDeep(&it, 10, 2, .{ .number = "1" });
        try expectNextTokenEqualDeep(&it, 10, 4, .plus);
        try expectNextTokenEqualDeep(&it, 10, 6, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, 10, 7, .rparen);
        try expectNextTokenEqualDeep(&it, 10, 9, .asterisk);
        try expectNextTokenEqualDeep(&it, 10, 11, .{ .number = "2" });
        try expectNextTokenEqualDeep(&it, 10, 12, .semi);
        try expectNextTokenEqualDeep(&it, 12, 0, .eol);
        try expectNextTokenEqualDeep(&it, 13, 0, .{ .comment_line = "/////////////////////////////////////////////" });
        try expectNextTokenEqualDeep(&it, 14, 0, .{ .comment_line = "// Numbers, Strings, and Operators" });
        try expectNextTokenEqualDeep(&it, 15, 0, .eol);
        try expectNextTokenEqualDeep(&it, 16, 0, .{ .comment_line = "// FQL has integer, decimal, and exponential values stored as" });
        try expectNextTokenEqualDeep(&it, 17, 0, .{ .comment_line = "// Int, Long, or Double types, which are all Number types." });
        try expectNextTokenEqualDeep(&it, 18, 0, .{ .comment_line = "// An Int is a signed 32-bit integer type." });
        try expectNextTokenEqualDeep(&it, 19, 0, .{ .comment_line = "// A Long is a signed 64-bit integer type." });
        try expectNextTokenEqualDeep(&it, 20, 0, .{ .comment_line = "// Doubles are double-precision, 64-bit binary type, IEEE 754-2019." });
        try expectNextTokenEqualDeep(&it, 20, 1, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, 21, 0, .{ .comment_line = "// 3" });
        try expectNextTokenEqualDeep(&it, 21, 3, .{ .number = "1.5" });
        try expectNextTokenEqualDeep(&it, 22, 0, .{ .comment_line = "// 1.5" });
        try expectNextTokenEqualDeep(&it, 22, 6, .{ .number = "3.14e5" });
        try expectNextTokenEqualDeep(&it, 22, 16, .{ .comment_line = "// 314000" });
        try expectNextTokenNull(&it);
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
        try expectNextTokenEqualDeep(&it, 0, 5, .{ .annotation = "role" });
        try expectNextTokenEqualDeep(&it, 0, 6, .lparen);
        try expectNextTokenEqualDeep(&it, 0, 12, .{ .word = "server" });
        try expectNextTokenEqualDeep(&it, 0, 13, .rparen);
        try expectNextTokenEqualDeep(&it, 1, 0, .eol);
        try expectNextTokenEqualDeep(&it, 1, 8, .{ .word = "function" });
        try expectNextTokenEqualDeep(&it, 1, 20, .{ .word = "submitOrder" });
        try expectNextTokenEqualDeep(&it, 1, 21, .lparen);
        try expectNextTokenEqualDeep(&it, 1, 29, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, 1, 30, .comma);
        try expectNextTokenEqualDeep(&it, 1, 48, .{ .word = "productsRequested" });
        try expectNextTokenEqualDeep(&it, 1, 49, .rparen);
        try expectNextTokenEqualDeep(&it, 1, 51, .lbrace);
        try expectNextTokenEqualDeep(&it, 2, 0, .eol);
        try expectNextTokenEqualDeep(&it, 2, 4, .{ .word = "if" });
        try expectNextTokenEqualDeep(&it, 2, 6, .lparen);
        try expectNextTokenEqualDeep(&it, 2, 23, .{ .word = "productsRequested" });
        try expectNextTokenEqualDeep(&it, 2, 24, .dot);
        try expectNextTokenEqualDeep(&it, 2, 30, .{ .word = "length" });
        try expectNextTokenEqualDeep(&it, 2, 33, .equal2);
        try expectNextTokenEqualDeep(&it, 2, 35, .{ .number = "0" });
        try expectNextTokenEqualDeep(&it, 2, 36, .rparen);
        try expectNextTokenEqualDeep(&it, 2, 38, .lbrace);
        try expectNextTokenEqualDeep(&it, 3, 0, .eol);
        try expectNextTokenEqualDeep(&it, 3, 9, .{ .word = "abort" });
        try expectNextTokenEqualDeep(&it, 3, 10, .lparen);
        try expectNextTokenEqualDeep(&it, 3, 31, .{ .string = "\"Cart can't be empty\"" });
        try expectNextTokenEqualDeep(&it, 3, 32, .rparen);
        try expectNextTokenEqualDeep(&it, 4, 0, .eol);
        try expectNextTokenEqualDeep(&it, 4, 3, .rbrace);
        try expectNextTokenEqualDeep(&it, 6, 0, .eol);
        try expectNextTokenEqualDeep(&it, 6, 7, .{ .word = "Order" });
        try expectNextTokenEqualDeep(&it, 6, 8, .dot);
        try expectNextTokenEqualDeep(&it, 6, 14, .{ .word = "create" });
        try expectNextTokenEqualDeep(&it, 6, 15, .lparen);
        try expectNextTokenEqualDeep(&it, 6, 16, .lbrace);
        try expectNextTokenEqualDeep(&it, 7, 0, .eol);
        try expectNextTokenEqualDeep(&it, 7, 12, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, 7, 13, .colon);
        try expectNextTokenEqualDeep(&it, 7, 22, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, 7, 23, .comma);
        try expectNextTokenEqualDeep(&it, 8, 0, .eol);
        try expectNextTokenEqualDeep(&it, 8, 8, .{ .word = "cart" });
        try expectNextTokenEqualDeep(&it, 8, 9, .colon);
        try expectNextTokenEqualDeep(&it, 8, 22, .{ .word = "shoppingCart" });
        try expectNextTokenEqualDeep(&it, 8, 23, .comma);
        try expectNextTokenEqualDeep(&it, 9, 0, .eol);
        try expectNextTokenEqualDeep(&it, 9, 10, .{ .word = "status" });
        try expectNextTokenEqualDeep(&it, 9, 11, .colon);
        try expectNextTokenEqualDeep(&it, 9, 24, .{ .string = "\"processing\"" });
        try expectNextTokenEqualDeep(&it, 9, 25, .comma);
        try expectNextTokenEqualDeep(&it, 10, 0, .eol);
        try expectNextTokenEqualDeep(&it, 10, 16, .{ .word = "creationDate" });
        try expectNextTokenEqualDeep(&it, 10, 17, .colon);
        try expectNextTokenEqualDeep(&it, 10, 22, .{ .word = "Time" });
        try expectNextTokenEqualDeep(&it, 10, 23, .dot);
        try expectNextTokenEqualDeep(&it, 10, 26, .{ .word = "now" });
        try expectNextTokenEqualDeep(&it, 10, 27, .lparen);
        try expectNextTokenEqualDeep(&it, 10, 28, .rparen);
        try expectNextTokenEqualDeep(&it, 10, 29, .comma);
        try expectNextTokenEqualDeep(&it, 11, 0, .eol);
        try expectNextTokenEqualDeep(&it, 11, 12, .{ .word = "shipDate" });
        try expectNextTokenEqualDeep(&it, 11, 13, .colon);
        try expectNextTokenEqualDeep(&it, 11, 18, .{ .word = "null" });
        try expectNextTokenEqualDeep(&it, 11, 19, .comma);
        try expectNextTokenEqualDeep(&it, 12, 0, .eol);
        try expectNextTokenEqualDeep(&it, 12, 19, .{ .word = "deliveryAddress" });
        try expectNextTokenEqualDeep(&it, 12, 20, .colon);
        try expectNextTokenEqualDeep(&it, 12, 29, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, 12, 30, .dot);
        try expectNextTokenEqualDeep(&it, 12, 37, .{ .word = "address" });
        try expectNextTokenEqualDeep(&it, 12, 38, .comma);
        try expectNextTokenEqualDeep(&it, 13, 0, .eol);
        try expectNextTokenEqualDeep(&it, 13, 14, .{ .word = "creditCard" });
        try expectNextTokenEqualDeep(&it, 13, 15, .colon);
        try expectNextTokenEqualDeep(&it, 13, 24, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, 13, 25, .dot);
        try expectNextTokenEqualDeep(&it, 13, 35, .{ .word = "creditCard" });
        try expectNextTokenEqualDeep(&it, 14, 0, .eol);
        try expectNextTokenEqualDeep(&it, 14, 3, .rbrace);
        try expectNextTokenEqualDeep(&it, 14, 4, .rparen);
        try expectNextTokenEqualDeep(&it, 15, 0, .eol);
        try expectNextTokenEqualDeep(&it, 15, 1, .rbrace);
        try expectNextTokenNull(&it);
    }
}
