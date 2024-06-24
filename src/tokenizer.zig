const std = @import("std");
const testing = std.testing;

const util = @import("util.zig");

fn isWordChar(char: u8) bool {
    return ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or ('0' <= char and char <= '9');
}

fn isIdentifierChar(char: u8, index: usize) bool {
    return char == '_' or ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or (index > 0 and '0' <= char and char <= '9');
}

const isNotIdentifierChar = util.predicateNegation(isIdentifierChar);

fn isQuote(char: u8) bool {
    return char == '"' or char == '\'';
}

fn isNumberChar(char: u8, index: usize) bool {
    return (('0' <= char and char <= '9') or ('a' <= char and char <= 'f') or ('A' <= char and char <= 'F') or char == '-') or (index == 1 and (char == 'b' or char == 'o' or char == 'x')) or (index > 0 and (char == '.' or char == '_' or char == 'e'));
}

fn isString(s: []const u8) bool {
    if (s.len < 1) {
        return false;
    }

    return isQuote(s[0]);
}

fn isCommentLine(s: []const u8) bool {
    if (s.len < 2) {
        return false;
    }

    return s[0] == '/' and s[1] == '/';
}

fn isCommentBlock(s: []const u8) bool {
    if (s.len < 2) {
        return false;
    }

    return s[0] == '/' and s[1] == '*';
}

fn isComment(s: []const u8) bool {
    return isCommentLine(s) or isCommentBlock(s);
}

pub const Token = union(enum) {
    eof,

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
    number: u8,

    fn fromU8(c: u8) WordPrefix {
        return switch (c) {
            '@' => .at,
            '0'...'9', '-' => .{ .number = c },
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
            .number => |c| &.{c},
        };
    }
};

pub const TokenIterator = struct {
    reader: std.io.AnyReader,
    buf_start: usize = 0,
    buf_end: usize = 0,
    buf: [64]u8 = undefined,
    saved_token: ?Token = null,

    pub fn init(reader: std.io.AnyReader) TokenIterator {
        return .{ .reader = reader };
    }

    pub fn deinit(self: TokenIterator, allocator: std.mem.Allocator) void {
        if (self.saved_token) |token| {
            token.deinit(allocator);
        }
    }

    fn nextByte(self: *TokenIterator) !u8 {
        if (self.buf_start < self.buf_end) {
            defer self.buf_start += 1;

            return self.buf[self.buf_start];
        }

        return self.reader.readByte();
    }

    fn shiftBuf(self: *TokenIterator) void {
        if (self.buf_start) {
            return;
        }

        std.mem.copyForwards(u8, self.buf, self.buf[self.buf_start..self.buf_end]);
        self.buf_end -= self.buf_start;
        self.buf_start = 0;
    }

    fn saveBytes(self: *TokenIterator, bytes: []const u8) void {
        if (bytes.len == 0) {
            return;
        }

        if (self.buf_start < self.buf_end) {
            if (bytes.len > self.buf_start) {
                std.mem.copyForwards(u8, self.buf[bytes.len..], self.buf[self.buf_start..self.buf_end]);
            } else if (bytes.len < self.buf_start) {
                std.mem.copyBackwards(u8, self.buf[bytes.len..], self.buf[self.buf_start..self.buf_end]);
            } else {
                // the existing bytes are already in the right place
            }
        }

        std.mem.copyForwards(u8, self.buf[0..], bytes);

        self.buf_end = self.buf_end - self.buf_start + bytes.len;
        self.buf_start = 0;
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

    pub fn next(self: *TokenIterator, allocator: std.mem.Allocator) !?Token {
        if (self.saved_token) |token| {
            defer self.saved_token = null;

            if (token == .eof) {
                return null;
            }

            return token;
        }

        var arr = std.ArrayList(u8).init(allocator);
        defer arr.deinit();

        while (true) {
            const char = self.nextByte() catch |err| {
                if (err == error.EndOfStream) {
                    // std.debug.print("breaking 1\n", .{});
                    break;
                }

                return err;
            };

            // std.debug.print("current: {d} \"{s}\" '{c}'\n", .{ arr.items.len, arr.items, char });
            if (isCommentBlock(arr.items)) {
                try arr.append(char);

                if (arr.items.len >= 4 and arr.items[arr.items.len - 1] == '/' and arr.items[arr.items.len - 2] == '*') {
                    break;
                }
            } else if (isComment(arr.items)) {
                if (char == '\n') {
                    break;
                }

                try arr.append(char);
            } else {
                switch (char) {
                    ' ' => if (arr.items.len > 0) {
                        if (isString(arr.items)) {
                            try arr.append(char);
                        } else {
                            self.saveBytes(" ");
                            // std.debug.print("breaking 2\n", .{});
                            break;
                        }
                    },
                    ';' => if (isString(arr.items)) {
                        try arr.append(char);
                    } else if (arr.items.len > 0) {
                        // std.debug.print("breaking 3\n", .{});
                        break;
                    },
                    '\n', '\r' => if (arr.items.len > 0) {
                        // std.debug.print("breaking 4\n", .{});
                        break;
                    },
                    '"', '\'' => {
                        try arr.append(char);
                        if (arr.items.len >= 2 and arr.items[0] == char) {
                            if (arr.items[arr.items.len - 1] != '\\') {
                                // std.debug.print("breaking 5\n", .{});
                                break;
                            }
                        }
                    },
                    else => {
                        try arr.append(char);
                    },
                }
            }
        }

        // std.debug.print("final: {d} \"{s}\"\n", .{ arr.items.len, arr.items });

        if (arr.items.len == 0) {
            return null;
        }

        if (@as(?std.meta.Tuple(&.{ Token, usize }), switch (arr.items[0]) {
            '*' => blk: {
                if (arr.items.len > 1 and arr.items[1] == '*') {
                    break :blk .{ .asterisk2, 2 };
                }

                break :blk .{ .asterisk, 1 };
            },
            '&' => blk: {
                if (arr.items.len > 1 and arr.items[1] == '&') {
                    break :blk .{ .ampersand2, 2 };
                }

                break :blk .{ .ampersand, 1 };
            },
            '!' => blk: {
                if (arr.items.len > 1 and arr.items[1] == '=') {
                    break :blk .{ .bang_equal, 2 };
                }

                break :blk .{ .bang, 1 };
            },
            '^' => .{ .caret, 1 },
            ':' => .{ .colon, 1 },
            ',' => .{ .comma, 1 },
            '.' => blk: {
                if (arr.items.len > 2 and arr.items[1] == '.' and arr.items[2] == '.') {
                    break :blk .{ .dot3, 3 };
                }

                break :blk .{ .dot, 1 };
            },
            '<' => blk: {
                if (arr.items.len > 1 and arr.items[1] == '=') {
                    break :blk .{ .larrow_equal, 2 };
                }

                break :blk .{ .larrow, 1 };
            },
            '{' => .{ .lbrace, 1 },
            '[' => .{ .lbracket, 1 },
            '(' => .{ .lparen, 1 },
            '%' => .{ .percent, 1 },
            '|' => blk: {
                if (arr.items.len > 1 and arr.items[1] == '|') {
                    break :blk .{ .pipe2, 2 };
                }

                break :blk .{ .pipe, 1 };
            },
            '+' => .{ .plus, 1 },
            '?' => blk: {
                if (arr.items.len > 1) {
                    switch (arr.items[1]) {
                        '.' => break :blk .{ .question_dot, 2 },
                        '?' => break :blk .{ .question2, 2 },
                        else => {},
                    }
                }

                break :blk .{ .question, 1 };
            },
            '>' => blk: {
                if (arr.items.len > 1 and arr.items[1] == '=') {
                    break :blk .{ .rarrow_equal, 2 };
                }

                break :blk .{ .rarrow, 1 };
            },
            '}' => .{ .rbrace, 1 },
            ']' => .{ .rbracket, 1 },
            ')' => .{ .rparen, 1 },
            '/' => blk: {
                if (isComment(arr.items)) {
                    break :blk null;
                }

                break :blk .{ .slash, 1 };
            },
            '=' => blk: {
                if (arr.items.len >= 2) {
                    switch (arr.items[1]) {
                        '>' => break :blk .{ .equal_rarrow, 2 },
                        '=' => break :blk .{ .equal2, 2 },
                        else => {},
                    }
                }

                break :blk .{ .equal, 1 };
            },
            '-' => blk: {
                if (arr.items.len >= 2) {
                    switch (arr.items[1]) {
                        '>' => break :blk .{ .minus_rarrow, 2 },
                        '0'...'9' => break :blk null,
                        else => {},
                    }
                }

                break :blk .{ .minus, 1 };
            },
            '~' => .{ .tilde, 1 },
            else => null,
        })) |match| {
            const token, const len = match;
            self.saveBytes(arr.items[len..]);
            return token;
        }

        if (arr.items.len >= 2 and isQuote(arr.items[0]) and arr.items[0] == arr.items[arr.items.len - 1]) {
            return .{ .string = try allocator.dupe(u8, arr.items) };
        }

        if (isCommentLine(arr.items)) {
            return .{ .comment_line = try allocator.dupe(u8, arr.items) };
        }

        if (isCommentBlock(arr.items)) {
            return .{ .comment_block = try allocator.dupe(u8, arr.items) };
        }

        const prefix = WordPrefix.fromU8(arr.items[0]);
        if (prefix == .number) {
            // very loosely tokenize the number, it's okay if it's not valid
            if (util.slice.indexOfPosFn(arr.items, 1, isNotIdentifierChar)) |dot_pos| {
                if (arr.items[dot_pos] == '.') {
                    if (util.slice.indexOfPosFn(arr.items, dot_pos + 1, isNotIdentifierChar)) |index| {
                        self.saveBytes(arr.items[index..]);
                        arr.items.len = index;
                    }
                } else {
                    self.saveBytes(arr.items[dot_pos..]);
                    arr.items.len = dot_pos;
                }
            }

            return .{ .number = try arr.toOwnedSlice() };
        }

        if (prefix != .none) {
            _ = arr.orderedRemove(0);
        }

        const word = blk: {
            if (util.slice.indexOfFn(arr.items, isNotIdentifierChar)) |index| {
                if (index == 0) {
                    break :blk "";
                }

                self.saveBytes(arr.items[index..]);
                arr.items.len = index;
            }

            break :blk arr.items;
        };

        if (word.len > 0) {
            if (prefix == .at) {
                return .{ .annotation = try arr.toOwnedSlice() };
            }

            // std.debug.print("emitting word: {d} \"{s}\"\n", .{ arr.items.len, arr.items });

            return .{ .word = try arr.toOwnedSlice() };
        }

        std.log.err("found unexpected token: {d} \"{s}{s}\"", .{ arr.items.len + prefix.len(), prefix.toString(), arr.items });

        self.saveBytes(arr.items);
        if (prefix != .none) {
            self.saveBytes(prefix.toString());
        }

        return error.UnexpectedToken;
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
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Statements don't have to be terminated by ; ..." });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .number = "1" });
        try expectNextTokenEqualDeep(&it, .plus);
        try expectNextTokenEqualDeep(&it, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .asterisk);
        try expectNextTokenEqualDeep(&it, .{ .number = "2" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// ... but can be." });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .number = "1" });
        try expectNextTokenEqualDeep(&it, .plus);
        try expectNextTokenEqualDeep(&it, .{ .number = "3" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .asterisk);
        try expectNextTokenEqualDeep(&it, .{ .number = "2" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "/////////////////////////////////////////////" });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Numbers, Strings, and Operators" });
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
        try expectNextTokenEqualDeep(&it, .{ .word = "function" });
        try expectNextTokenEqualDeep(&it, .{ .word = "submitOrder" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "productsRequested" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .{ .word = "if" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .word = "productsRequested" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "length" });
        try expectNextTokenEqualDeep(&it, .equal2);
        try expectNextTokenEqualDeep(&it, .{ .number = "0" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .{ .word = "abort" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .{ .string = "\"Cart can't be empty\"" });
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, .{ .word = "Order" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "create" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .lbrace);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "cart" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "shoppingCart" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "status" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .string = "\"processing\"" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "creationDate" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "Time" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "now" });
        try expectNextTokenEqualDeep(&it, .lparen);
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "shipDate" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "null" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "deliveryAddress" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "address" });
        try expectNextTokenEqualDeep(&it, .comma);
        try expectNextTokenEqualDeep(&it, .{ .word = "creditCard" });
        try expectNextTokenEqualDeep(&it, .colon);
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" });
        try expectNextTokenEqualDeep(&it, .dot);
        try expectNextTokenEqualDeep(&it, .{ .word = "creditCard" });
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, .rparen);
        try expectNextTokenEqualDeep(&it, .rbrace);
        try expectNextTokenEqualDeep(&it, null);
    }
}
