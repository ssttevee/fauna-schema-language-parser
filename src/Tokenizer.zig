const std = @import("std");
const testing = std.testing;

const util = @import("util.zig");
const common = @import("common.zig");

fn isIdentifierChar(char: u8, index: usize) bool {
    return char == '_' or ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or (index > 0 and '0' <= char and char <= '9');
}

fn isQuote(char: u8) bool {
    return char == '"' or char == '\'';
}

fn isNumberChar(char: u8, index: usize) bool {
    return (('0' <= char and char <= '9') or char == '-') or (index == 1 and (char == 'b' or char == 'o' or char == 'x')) or (index > 0 and (char == '.' or char == '_' or char == 'e')) or (index > 2 and (('a' <= char and char <= 'f') or ('A' <= char and char <= 'F')));
}

pub const TokenWithLocation = struct {
    token: Token,
    location: ?common.SourceLocation,

    pub fn deinit(self: TokenWithLocation, allocator: std.mem.Allocator) void {
        self.token.deinit(allocator);
    }

    pub fn dupe(self: TokenWithLocation, allocator: std.mem.Allocator) !TokenWithLocation {
        return .{
            .token = try self.token.dupe(allocator),
            .location = self.location,
        };
    }
};

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
is_eof: bool = false,

current_offset: usize = 0,
current_line: usize = 0,
current_col: usize = 0,

const Tokenizer = @This();

pub fn write(self: *Tokenizer, bytes: []const u8) !usize {
    if (self.buf_start > 0 and self.buf.len - self.buf_end < bytes.len) {
        const new_end = self.buf_end - self.buf_start;
        std.mem.copyForwards(u8, self.buf[0..new_end], self.buf[self.buf_start..self.buf_end]);
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

pub fn currentPosition(self: *Tokenizer) common.Position {
    return .{
        .offset = @intCast(self.current_offset),
        .line = @intCast(self.current_line),
        .column = @intCast(self.current_col),
    };
}

fn consumeBytes(self: *Tokenizer, n: usize) void {
    const consumed_bytes = self.buf[self.buf_start .. self.buf_start + n];
    self.buf_start += n;

    self.current_offset += n;

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

    const leading_whitespace_len = util.slice.indexOfNoneFn(chars, util.isWhitespace) orelse chars.len;
    if (leading_whitespace_len > 0) {
        chars.ptr += leading_whitespace_len;
        chars.len -= leading_whitespace_len;
        self.consumeBytes(leading_whitespace_len);
        return error.BumpStartPosition;
    }

    if (chars.len == 0) {
        if (self.is_eof) {
            return null;
        } else {
            return error.NeedMoreData;
        }
    }

    if (util.isLineTerminator(chars[0])) {
        if (util.slice.indexOfNonePosFn(chars, 1, util.isLineTerminator)) |pos| {
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

            if (chars.len == 1 and !self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .asterisk, 1 };
        },
        '&' => blk: {
            if (chars.len > 1 and chars[1] == '&') {
                break :blk .{ .ampersand2, 2 };
            }

            if (chars.len == 1 and !self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .ampersand, 1 };
        },
        '!' => blk: {
            if (chars.len > 1 and chars[1] == '=') {
                break :blk .{ .bang_equal, 2 };
            }

            if (chars.len == 1 and !self.is_eof) {
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

            if (chars.len == 1 and !self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .dot, 1 };
        },
        '<' => blk: {
            if (chars.len > 1 and chars[1] == '=') {
                break :blk .{ .larrow_equal, 2 };
            }

            if (chars.len == 1 and !self.is_eof) {
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

            if (chars.len == 1 and !self.is_eof) {
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

            if (chars.len == 1 and !self.is_eof) {
                return error.NeedMoreData;
            }

            break :blk .{ .question, 1 };
        },
        '>' => blk: {
            if (chars.len > 1 and chars[1] == '=') {
                break :blk .{ .rarrow_equal, 2 };
            }

            if (chars.len == 1 and !self.is_eof) {
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

            if (chars.len == 1 and !self.is_eof) {
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

            if (chars.len == 1 and !self.is_eof) {
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

            if (chars.len == 1 and !self.is_eof) {
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
        const eol_pos = util.slice.indexOfPosFn(chars, 1, util.isLineTerminator);

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

    if (chars.len >= 2 and chars[0] == '/' and chars[1] == '/') {
        const pos = util.slice.indexOfPosFn(chars, 2, util.isLineTerminator);
        if (pos == null) {
            if (!self.is_eof) {
                return error.NeedMoreData;
            }

            self.consumeBytes(self.buf_end - self.buf_start);
            return .{ .comment_line = try allocator.dupe(u8, chars) };
        }

        self.consumeBytes(pos.?);
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

        if (!self.is_eof and chars.len == self.buf_end - self.buf_start) {
            return error.NeedMoreData;
        }

        self.consumeBytes(chars.len);
        return .{ .word = try allocator.dupe(u8, chars) };
    }

    std.log.err("found unexpected token: {d} \"{s}{s}\"", .{ chars.len + prefix.len(), prefix.toString(), chars });

    return error.UnexpectedToken;
}

pub const TokenIterator = struct {
    source_file: ?[]const u8,
    reader: std.io.AnyReader,
    tokenizer: Tokenizer = .{},
    saved_token: ?TokenWithLocation = null,

    pub fn init(reader: std.io.AnyReader, source_file: ?[]const u8) TokenIterator {
        return .{
            .reader = reader,
            .source_file = source_file,
        };
    }

    pub fn deinit(self: TokenIterator, allocator: std.mem.Allocator) void {
        if (self.saved_token) |token| {
            token.deinit(allocator);
        }
    }

    pub fn saveToken(self: *TokenIterator, token: ?TokenWithLocation) void {
        std.debug.assert(self.saved_token == null);
        self.saved_token = token;
    }

    pub fn nextToken(self: *TokenIterator, allocator: std.mem.Allocator) !TokenWithLocation {
        if (try self.next(allocator)) |token| {
            return token;
        }

        return .{
            .token = .eof,
            .location = .{
                .source = self.source_file,
                .start = self.tokenizer.currentPosition(),
                .end = self.tokenizer.currentPosition(),
            },
        };
    }

    pub fn next(self: *TokenIterator, allocator: std.mem.Allocator) !?TokenWithLocation {
        if (self.saved_token) |token| {
            self.saved_token = null;
            if (token.token == .eof) {
                return null;
            }

            return token;
        }

        var start = self.tokenizer.currentPosition();

        while (true) {
            if (self.tokenizer.next(allocator) catch |err| {
                if (err == error.BumpStartPosition) {
                    start = self.tokenizer.currentPosition();
                    continue;
                }

                if (err == error.NeedMoreData and !self.tokenizer.is_eof) {
                    var buf: [@typeInfo(std.meta.FieldType(Tokenizer, .buf)).Array.len]u8 = undefined;

                    const n = try self.reader.read(buf[0 .. self.tokenizer.buf.len - self.tokenizer.buf_end + self.tokenizer.buf_start]);
                    if (n == 0) {
                        self.tokenizer.is_eof = true;
                    } else {
                        _ = self.tokenizer.write(buf[0..n]) catch unreachable;
                    }

                    continue;
                }

                return err;
            }) |token| {
                return .{
                    .token = token,
                    .location = .{
                        .source = self.source_file,
                        .start = start,
                        .end = self.tokenizer.currentPosition(),
                    },
                };
            }

            return null;
        }
    }
};

fn expectNextTokenNull(it: *TokenIterator) !void {
    try testing.expectEqualDeep(null, try it.next(testing.allocator));
}

fn expectNextTokenEqualDeep(it: *TokenIterator, expected: Token, start: [3]u64, end: [3]u64) !void {
    const token = (try it.next(testing.allocator)).?;
    defer token.deinit(testing.allocator);

    try testing.expectEqualDeep(expected, token.token);
    try testing.expectEqual(start[0], token.location.?.start.offset);
    try testing.expectEqual(start[1], token.location.?.start.line);
    try testing.expectEqual(start[2], token.location.?.start.column);
    try testing.expectEqual(end[0], token.location.?.end.offset);
    try testing.expectEqual(end[1], token.location.?.end.line);
    try testing.expectEqual(end[2], token.location.?.end.column);
}

test "read token" {
    {
        var stream = std.io.fixedBufferStream("");
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("2s3a4_y5.6e");
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .number = "2s3a4_y5.6e" }, .{ 0, 0, 0 }, .{ 11, 0, 11 });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("-12_3as_dv.2e3.foo");
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .number = "-12_3as_dv.2e3" }, .{ 0, 0, 0 }, .{ 14, 0, 14 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 14, 0, 14 }, .{ 15, 0, 15 });
        try expectNextTokenEqualDeep(&it, .{ .word = "foo" }, .{ 15, 0, 15 }, .{ 18, 0, 18 });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("collection Product {}");
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .word = "collection" }, .{ 0, 0, 0 }, .{ 10, 0, 10 });
        try expectNextTokenEqualDeep(&it, .{ .word = "Product" }, .{ 11, 0, 11 }, .{ 18, 0, 18 });
        try expectNextTokenEqualDeep(&it, .lbrace, .{ 19, 0, 19 }, .{ 20, 0, 20 });
        try expectNextTokenEqualDeep(&it, .rbrace, .{ 20, 0, 20 }, .{ 21, 0, 21 });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("move.a->.b");
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .word = "move" }, .{ 0, 0, 0 }, .{ 4, 0, 4 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 4, 0, 4 }, .{ 5, 0, 5 });
        try expectNextTokenEqualDeep(&it, .{ .word = "a" }, .{ 5, 0, 5 }, .{ 6, 0, 6 });
        try expectNextTokenEqualDeep(&it, .minus_rarrow, .{ 6, 0, 6 }, .{ 8, 0, 8 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 8, 0, 8 }, .{ 9, 0, 9 });
        try expectNextTokenEqualDeep(&it, .{ .word = "b" }, .{ 9, 0, 9 }, .{ 10, 0, 10 });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("access provider apname { issuer 'url' }");
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .word = "access" }, .{ 0, 0, 0 }, .{ 6, 0, 6 });
        try expectNextTokenEqualDeep(&it, .{ .word = "provider" }, .{ 7, 0, 7 }, .{ 15, 0, 15 });
        try expectNextTokenEqualDeep(&it, .{ .word = "apname" }, .{ 16, 0, 16 }, .{ 22, 0, 22 });
        try expectNextTokenEqualDeep(&it, .lbrace, .{ 23, 0, 23 }, .{ 24, 0, 24 });
        try expectNextTokenEqualDeep(&it, .{ .word = "issuer" }, .{ 25, 0, 25 }, .{ 31, 0, 31 });
        try expectNextTokenEqualDeep(&it, .{ .string = "'url'" }, .{ 32, 0, 32 }, .{ 37, 0, 37 });
        try expectNextTokenEqualDeep(&it, .rbrace, .{ 38, 0, 38 }, .{ 39, 0, 39 });
        try expectNextTokenNull(&it);
    }

    {
        var stream = std.io.fixedBufferStream("some words /* wrapping around a */ comment block");
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .word = "some" }, .{ 0, 0, 0 }, .{ 4, 0, 4 });
        try expectNextTokenEqualDeep(&it, .{ .word = "words" }, .{ 5, 0, 5 }, .{ 10, 0, 10 });
        try expectNextTokenEqualDeep(&it, .{ .comment_block = "/* wrapping around a */" }, .{ 11, 0, 11 }, .{ 34, 0, 34 });
        try expectNextTokenEqualDeep(&it, .{ .word = "comment" }, .{ 35, 0, 35 }, .{ 42, 0, 42 });
        try expectNextTokenEqualDeep(&it, .{ .word = "block" }, .{ 43, 0, 43 }, .{ 48, 0, 48 });
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
            \\
        );
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Single-line comments start with double-slash." }, .{ 0, 0, 0 }, .{ 48, 0, 48 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 48, 0, 48 }, .{ 49, 1, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_block = "/*\n  Block comments start with slash-asterisk\n  and end with asterisk-slash.\n*/" }, .{ 49, 1, 0 }, .{ 128, 4, 2 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 128, 4, 2 }, .{ 130, 6, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Statements don't have to be terminated by ; ..." }, .{ 130, 6, 0 }, .{ 180, 6, 50 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 180, 6, 50 }, .{ 181, 7, 0 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 181, 7, 0 }, .{ 182, 7, 1 });
        try expectNextTokenEqualDeep(&it, .{ .number = "1" }, .{ 182, 7, 1 }, .{ 183, 7, 2 });
        try expectNextTokenEqualDeep(&it, .plus, .{ 184, 7, 3 }, .{ 185, 7, 4 });
        try expectNextTokenEqualDeep(&it, .{ .number = "3" }, .{ 186, 7, 5 }, .{ 187, 7, 6 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 187, 7, 6 }, .{ 188, 7, 7 });
        try expectNextTokenEqualDeep(&it, .asterisk, .{ 189, 7, 8 }, .{ 190, 7, 9 });
        try expectNextTokenEqualDeep(&it, .{ .number = "2" }, .{ 191, 7, 10 }, .{ 192, 7, 11 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 192, 7, 11 }, .{ 194, 9, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// ... but can be." }, .{ 194, 9, 0 }, .{ 212, 9, 18 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 212, 9, 18 }, .{ 213, 10, 0 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 213, 10, 0 }, .{ 214, 10, 1 });
        try expectNextTokenEqualDeep(&it, .{ .number = "1" }, .{ 214, 10, 1 }, .{ 215, 10, 2 });
        try expectNextTokenEqualDeep(&it, .plus, .{ 216, 10, 3 }, .{ 217, 10, 4 });
        try expectNextTokenEqualDeep(&it, .{ .number = "3" }, .{ 218, 10, 5 }, .{ 219, 10, 6 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 219, 10, 6 }, .{ 220, 10, 7 });
        try expectNextTokenEqualDeep(&it, .asterisk, .{ 221, 10, 8 }, .{ 222, 10, 9 });
        try expectNextTokenEqualDeep(&it, .{ .number = "2" }, .{ 223, 10, 10 }, .{ 224, 10, 11 });
        try expectNextTokenEqualDeep(&it, .semi, .{ 224, 10, 11 }, .{ 225, 10, 12 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 225, 10, 12 }, .{ 227, 12, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "/////////////////////////////////////////////" }, .{ 227, 12, 0 }, .{ 272, 12, 45 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 272, 12, 45 }, .{ 273, 13, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Numbers, Strings, and Operators" }, .{ 273, 13, 0 }, .{ 307, 13, 34 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 307, 13, 34 }, .{ 309, 15, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// FQL has integer, decimal, and exponential values stored as" }, .{ 309, 15, 0 }, .{ 370, 15, 61 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 370, 15, 61 }, .{ 371, 16, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Int, Long, or Double types, which are all Number types." }, .{ 371, 16, 0 }, .{ 429, 16, 58 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 429, 16, 58 }, .{ 430, 17, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// An Int is a signed 32-bit integer type." }, .{ 430, 17, 0 }, .{ 472, 17, 42 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 472, 17, 42 }, .{ 473, 18, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// A Long is a signed 64-bit integer type." }, .{ 473, 18, 0 }, .{ 515, 18, 42 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 515, 18, 42 }, .{ 516, 19, 0 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// Doubles are double-precision, 64-bit binary type, IEEE 754-2019." }, .{ 516, 19, 0 }, .{ 583, 19, 67 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 583, 19, 67 }, .{ 584, 20, 0 });
        try expectNextTokenEqualDeep(&it, .{ .number = "3" }, .{ 584, 20, 0 }, .{ 585, 20, 1 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// 3" }, .{ 586, 20, 2 }, .{ 590, 20, 6 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 590, 20, 6 }, .{ 591, 21, 0 });
        try expectNextTokenEqualDeep(&it, .{ .number = "1.5" }, .{ 591, 21, 0 }, .{ 594, 21, 3 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// 1.5" }, .{ 595, 21, 4 }, .{ 601, 21, 10 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 601, 21, 10 }, .{ 602, 22, 0 });
        try expectNextTokenEqualDeep(&it, .{ .number = "3.14e5" }, .{ 602, 22, 0 }, .{ 608, 22, 6 });
        try expectNextTokenEqualDeep(&it, .{ .comment_line = "// 314000" }, .{ 609, 22, 7 }, .{ 618, 22, 16 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 618, 22, 16 }, .{ 619, 23, 0 });
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
        var it = TokenIterator.init(stream.reader().any(), null);
        try expectNextTokenEqualDeep(&it, .{ .annotation = "role" }, .{ 0, 0, 0 }, .{ 5, 0, 5 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 5, 0, 5 }, .{ 6, 0, 6 });
        try expectNextTokenEqualDeep(&it, .{ .word = "server" }, .{ 6, 0, 6 }, .{ 12, 0, 12 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 12, 0, 12 }, .{ 13, 0, 13 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 13, 0, 13 }, .{ 14, 1, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "function" }, .{ 14, 1, 0 }, .{ 22, 1, 8 });
        try expectNextTokenEqualDeep(&it, .{ .word = "submitOrder" }, .{ 23, 1, 9 }, .{ 34, 1, 20 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 34, 1, 20 }, .{ 35, 1, 21 });
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" }, .{ 35, 1, 21 }, .{ 43, 1, 29 });
        try expectNextTokenEqualDeep(&it, .comma, .{ 43, 1, 29 }, .{ 44, 1, 30 });
        try expectNextTokenEqualDeep(&it, .{ .word = "productsRequested" }, .{ 45, 1, 31 }, .{ 62, 1, 48 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 62, 1, 48 }, .{ 63, 1, 49 });
        try expectNextTokenEqualDeep(&it, .lbrace, .{ 64, 1, 50 }, .{ 65, 1, 51 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 65, 1, 51 }, .{ 66, 2, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "if" }, .{ 68, 2, 2 }, .{ 70, 2, 4 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 71, 2, 5 }, .{ 72, 2, 6 });
        try expectNextTokenEqualDeep(&it, .{ .word = "productsRequested" }, .{ 72, 2, 6 }, .{ 89, 2, 23 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 89, 2, 23 }, .{ 90, 2, 24 });
        try expectNextTokenEqualDeep(&it, .{ .word = "length" }, .{ 90, 2, 24 }, .{ 96, 2, 30 });
        try expectNextTokenEqualDeep(&it, .equal2, .{ 97, 2, 31 }, .{ 99, 2, 33 });
        try expectNextTokenEqualDeep(&it, .{ .number = "0" }, .{ 100, 2, 34 }, .{ 101, 2, 35 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 101, 2, 35 }, .{ 102, 2, 36 });
        try expectNextTokenEqualDeep(&it, .lbrace, .{ 103, 2, 37 }, .{ 104, 2, 38 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 104, 2, 38 }, .{ 105, 3, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "abort" }, .{ 109, 3, 4 }, .{ 114, 3, 9 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 114, 3, 9 }, .{ 115, 3, 10 });
        try expectNextTokenEqualDeep(&it, .{ .string = "\"Cart can't be empty\"" }, .{ 115, 3, 10 }, .{ 136, 3, 31 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 136, 3, 31 }, .{ 137, 3, 32 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 137, 3, 32 }, .{ 138, 4, 0 });
        try expectNextTokenEqualDeep(&it, .rbrace, .{ 140, 4, 2 }, .{ 141, 4, 3 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 141, 4, 3 }, .{ 143, 6, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "Order" }, .{ 145, 6, 2 }, .{ 150, 6, 7 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 150, 6, 7 }, .{ 151, 6, 8 });
        try expectNextTokenEqualDeep(&it, .{ .word = "create" }, .{ 151, 6, 8 }, .{ 157, 6, 14 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 157, 6, 14 }, .{ 158, 6, 15 });
        try expectNextTokenEqualDeep(&it, .lbrace, .{ 158, 6, 15 }, .{ 159, 6, 16 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 159, 6, 16 }, .{ 160, 7, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" }, .{ 164, 7, 4 }, .{ 172, 7, 12 });
        try expectNextTokenEqualDeep(&it, .colon, .{ 172, 7, 12 }, .{ 173, 7, 13 });
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" }, .{ 174, 7, 14 }, .{ 182, 7, 22 });
        try expectNextTokenEqualDeep(&it, .comma, .{ 182, 7, 22 }, .{ 183, 7, 23 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 183, 7, 23 }, .{ 184, 8, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "cart" }, .{ 188, 8, 4 }, .{ 192, 8, 8 });
        try expectNextTokenEqualDeep(&it, .colon, .{ 192, 8, 8 }, .{ 193, 8, 9 });
        try expectNextTokenEqualDeep(&it, .{ .word = "shoppingCart" }, .{ 194, 8, 10 }, .{ 206, 8, 22 });
        try expectNextTokenEqualDeep(&it, .comma, .{ 206, 8, 22 }, .{ 207, 8, 23 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 207, 8, 23 }, .{ 208, 9, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "status" }, .{ 212, 9, 4 }, .{ 218, 9, 10 });
        try expectNextTokenEqualDeep(&it, .colon, .{ 218, 9, 10 }, .{ 219, 9, 11 });
        try expectNextTokenEqualDeep(&it, .{ .string = "\"processing\"" }, .{ 220, 9, 12 }, .{ 232, 9, 24 });
        try expectNextTokenEqualDeep(&it, .comma, .{ 232, 9, 24 }, .{ 233, 9, 25 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 233, 9, 25 }, .{ 234, 10, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "creationDate" }, .{ 238, 10, 4 }, .{ 250, 10, 16 });
        try expectNextTokenEqualDeep(&it, .colon, .{ 250, 10, 16 }, .{ 251, 10, 17 });
        try expectNextTokenEqualDeep(&it, .{ .word = "Time" }, .{ 252, 10, 18 }, .{ 256, 10, 22 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 256, 10, 22 }, .{ 257, 10, 23 });
        try expectNextTokenEqualDeep(&it, .{ .word = "now" }, .{ 257, 10, 23 }, .{ 260, 10, 26 });
        try expectNextTokenEqualDeep(&it, .lparen, .{ 260, 10, 26 }, .{ 261, 10, 27 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 261, 10, 27 }, .{ 262, 10, 28 });
        try expectNextTokenEqualDeep(&it, .comma, .{ 262, 10, 28 }, .{ 263, 10, 29 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 263, 10, 29 }, .{ 264, 11, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "shipDate" }, .{ 268, 11, 4 }, .{ 276, 11, 12 });
        try expectNextTokenEqualDeep(&it, .colon, .{ 276, 11, 12 }, .{ 277, 11, 13 });
        try expectNextTokenEqualDeep(&it, .{ .word = "null" }, .{ 278, 11, 14 }, .{ 282, 11, 18 });
        try expectNextTokenEqualDeep(&it, .comma, .{ 282, 11, 18 }, .{ 283, 11, 19 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 283, 11, 19 }, .{ 284, 12, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "deliveryAddress" }, .{ 288, 12, 4 }, .{ 303, 12, 19 });
        try expectNextTokenEqualDeep(&it, .colon, .{ 303, 12, 19 }, .{ 304, 12, 20 });
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" }, .{ 305, 12, 21 }, .{ 313, 12, 29 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 313, 12, 29 }, .{ 314, 12, 30 });
        try expectNextTokenEqualDeep(&it, .{ .word = "address" }, .{ 314, 12, 30 }, .{ 321, 12, 37 });
        try expectNextTokenEqualDeep(&it, .comma, .{ 321, 12, 37 }, .{ 322, 12, 38 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 322, 12, 38 }, .{ 323, 13, 0 });
        try expectNextTokenEqualDeep(&it, .{ .word = "creditCard" }, .{ 327, 13, 4 }, .{ 337, 13, 14 });
        try expectNextTokenEqualDeep(&it, .colon, .{ 337, 13, 14 }, .{ 338, 13, 15 });
        try expectNextTokenEqualDeep(&it, .{ .word = "customer" }, .{ 339, 13, 16 }, .{ 347, 13, 24 });
        try expectNextTokenEqualDeep(&it, .dot, .{ 347, 13, 24 }, .{ 348, 13, 25 });
        try expectNextTokenEqualDeep(&it, .{ .word = "creditCard" }, .{ 348, 13, 25 }, .{ 358, 13, 35 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 358, 13, 35 }, .{ 359, 14, 0 });
        try expectNextTokenEqualDeep(&it, .rbrace, .{ 361, 14, 2 }, .{ 362, 14, 3 });
        try expectNextTokenEqualDeep(&it, .rparen, .{ 362, 14, 3 }, .{ 363, 14, 4 });
        try expectNextTokenEqualDeep(&it, .eol, .{ 363, 14, 4 }, .{ 364, 15, 0 });
        try expectNextTokenEqualDeep(&it, .rbrace, .{ 364, 15, 0 }, .{ 365, 15, 1 });
        try expectNextTokenNull(&it);
    }
}
