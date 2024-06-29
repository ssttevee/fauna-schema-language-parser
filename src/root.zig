const std = @import("std");
const testing = std.testing;

const tokenizer = @import("tokenizer.zig");
const util = @import("util.zig");

pub const Token = tokenizer.Token;
pub const TokenIterator = tokenizer.TokenIterator;

test {
    _ = tokenizer;
}

pub const FQLType = union(enum) {
    pub const Object = struct {
        pub const Field = struct {
            const Key = union(enum) {
                identifier: []const u8,
                string: []const u8,
                wildcard,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self) {
                        inline .identifier, .string => |s| allocator.free(s),
                        else => {},
                    }
                }
            };

            key: Key,
            type: FQLType,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                self.key.deinit(allocator);
                self.type.deinit(allocator);
            }
        };

        fields: ?[]const Field = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.fields) |fields| {
                for (fields) |field| {
                    field.deinit(allocator);
                }

                allocator.free(fields);
            }
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) !void {
            try writer.writeByte('{');

            if (self.fields) |fields| {
                try writer.writeByte(' ');

                for (fields, 0..) |field, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }

                    switch (field.key) {
                        inline .identifier, .string => |s| try writer.writeAll(s),
                        .wildcard => try writer.writeByte('*'),
                    }

                    try writer.writeAll(": ");

                    try field.type.printCanonical(writer);
                }

                try writer.writeByte(' ');
            }

            try writer.writeByte('}');
        }
    };

    pub const Union = struct {
        lhs: *const FQLType,
        rhs: *const FQLType,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            allocator.destroy(self.lhs);
            self.rhs.deinit(allocator);
            allocator.destroy(self.rhs);
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) !void {
            try self.lhs.printCanonical(writer);
            try writer.writeAll(" | ");
            try self.rhs.printCanonical(writer);
        }
    };

    pub const Template = struct {
        name: []const u8,
        parameters: ?[]const FQLType = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.parameters) |parameters| {
                for (parameters) |parameter| {
                    parameter.deinit(allocator);
                }

                allocator.free(parameters);
            }

            allocator.free(self.name);
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) !void {
            try writer.writeAll(self.name);

            if (self.parameters) |parameters| {
                try writer.writeByte('<');

                for (parameters, 0..) |parameter, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }

                    try parameter.printCanonical(writer);
                }

                try writer.writeByte('>');
            }
        }
    };

    pub const Tuple = struct {
        types: ?[]const FQLType = null,
        parens: bool = false,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.types) |types| {
                for (types) |t| {
                    t.deinit(allocator);
                }

                allocator.free(types);
            }
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) !void {
            try writer.writeByte('[');

            if (self.types) |types| {
                for (types, 0..) |fql_type, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }

                    try fql_type.printCanonical(writer);
                }
            }

            try writer.writeByte(']');
        }
    };

    pub const Function = struct {
        pub const Parameters = union(enum) {
            pub const Long = struct {
                types: ?[]const FQLType = null,
                variadic: bool = false,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.types) |types| {
                        for (types) |t| {
                            t.deinit(allocator);
                        }

                        allocator.free(types);
                    }
                }
            };

            long: Long,
            short: *const FQLType,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    .long => |long| long.deinit(allocator),
                    .short => |short| {
                        short.deinit(allocator);
                        allocator.destroy(short);
                    },
                }
            }
        };

        parameters: Parameters,
        return_type: *const FQLType,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            self.parameters.deinit(allocator);
            self.return_type.deinit(allocator);
            allocator.destroy(self.return_type);
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) !void {
            switch (self.parameters) {
                .long => |long| {
                    try writer.writeByte('(');

                    if (long.types) |types| {
                        for (types, 0..) |fql_type, i| {
                            if (i > 0) {
                                try writer.writeAll(", ");
                            }

                            if (i == types.len - 1 and long.variadic) {
                                try writer.writeAll("...");
                            }

                            try fql_type.printCanonical(writer);
                        }
                    }

                    try writer.writeByte(')');
                },
                .short => |s| try s.printCanonical(writer),
            }

            try writer.writeAll(" => ");

            try self.return_type.printCanonical(writer);
        }
    };

    named: []const u8,
    object: Object,
    @"union": Union,
    optional: *const FQLType,
    template: Template,
    tuple: Tuple,
    string_literal: []const u8,
    number_literal: []const u8,
    function: Function,
    isolated: *const FQLType,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            inline .named, .string_literal, .number_literal => |name| allocator.free(name),
            inline .object, .@"union", .template, .tuple, .function => |obj| obj.deinit(allocator),
            inline .optional, .isolated => |optional| {
                optional.deinit(allocator);
                allocator.destroy(optional);
            },
        }
    }

    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) std.io.AnyWriter.Error!void {
        switch (self) {
            inline .named, .string_literal, .number_literal => |str| try writer.writeAll(str),
            .optional => |child| {
                try child.printCanonical(writer);
                try writer.writeByte('?');
            },
            .isolated => |child| {
                try writer.writeByte('(');
                try child.printCanonical(writer);
                try writer.writeByte(')');
            },
            inline else => |t| try t.printCanonical(writer),
        }
    }

    pub fn toCanonicalString(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
        var str = std.ArrayList(u8).init(allocator);
        defer str.deinit();

        try self.printCanonical(str.writer().any());

        return try str.toOwnedSlice();
    }

    pub const Parser = struct {
        const Unmanaged = struct {
            const State = union(enum) {
                const Tuple = struct {
                    types: std.ArrayListUnmanaged(FQLType) = .{},
                    parens: bool = false,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.types.deinit(allocator);
                    }
                };

                const Object = struct {
                    fields: std.ArrayListUnmanaged(FQLType.Object.Field) = .{},
                    state: union(enum) {
                        before_key,
                        after_key: FQLType.Object.Field.Key,
                        after_type: FQLType.Object.Field,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            switch (self) {
                                .before_key => {},
                                inline else => |s| s.deinit(allocator),
                            }
                        }
                    } = .before_key,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.fields.items) |field| {
                            field.deinit(allocator);
                        }

                        self.fields.deinit(allocator);
                        self.state.deinit(allocator);
                    }
                };

                const Template = struct {
                    name: []const u8,
                    parameters: std.ArrayListUnmanaged(FQLType) = .{},

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.parameters.items) |parameter| {
                            parameter.deinit(allocator);
                        }

                        self.parameters.deinit(allocator);
                        allocator.free(self.name);
                    }
                };

                const LongFunction = struct {
                    parameters: []const FQLType,
                    variadic_state: ?union(enum) {
                        start,
                        after_type: FQLType,
                        after_rparen: FQLType,
                    } = null,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self.variadic_state) {
                            .start => {},
                            inline .after_type, .after_rparen => |t| t.deinit(allocator),
                        }

                        for (self.parameters.items) |parameter| {
                            parameter.deinit(allocator);
                        }

                        self.parameters.deinit(allocator);
                    }
                };

                start,
                identifier: []const u8,
                union_lhs: FQLType,
                tuple: State.Tuple,
                object: State.Object,
                template: State.Template,
                short_function: FQLType,
                long_function: LongFunction,
                end: FQLType,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self) {
                        .identifier => |s| allocator.free(s),
                        inline .tuple, .object, .template, .union_lhs, .short_function, .long_function, .end => self.deinit(allocator),
                        .start => {},
                    }
                }
            };

            parent: ?*Unmanaged = null,
            state: State = .start,

            pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                if (self.parent) |parent| {
                    parent.deinit(allocator);
                    allocator.destroy(parent);
                }

                self.state.deinit(allocator);
            }

            inline fn finalizeType(self: *@This(), fql_type: FQLType) void {
                self.state = .{ .end = fql_type };
            }

            fn startChildState(self: *@This(), allocator: std.mem.Allocator) !void {
                self.* = .{ .parent = try util.mem.createCopy(@This(), allocator, self) };
            }

            pub fn pushToken(self: *@This(), allocator: std.mem.Allocator, token: tokenizer.Token) !PushResult {
                // std.debug.print("type parser state: {s}\n", .{@tagName(self.state)});
                if (token == .comment_block or token == .comment_line or (token == .eol and self.state != .end and self.state != .identifier)) {
                    return .{};
                }

                switch (self.state) {
                    .start => switch (token) {
                        .string => |str| {
                            self.state = .{
                                .end = .{
                                    .string_literal = try allocator.dupe(u8, str),
                                },
                            };
                        },
                        .number => |num| {
                            self.state = .{
                                .end = .{
                                    .number_literal = try allocator.dupe(u8, num),
                                },
                            };
                        },
                        .word => |word| {
                            self.state = .{
                                .identifier = try allocator.dupe(u8, word),
                            };
                        },
                        .lbrace => {
                            self.state = .{ .object = .{} };
                        },
                        .lbracket => {
                            self.state = .{ .tuple = .{} };
                            try self.startChildState(allocator);
                        },
                        .lparen => {
                            self.state = .{ .tuple = .{ .parens = true } };
                            try self.startChildState(allocator);
                        },
                        else => {
                            if (self.parent) |parent| {
                                if (token == .dot3 and parent.state == .tuple and parent.state.tuple.parens) {
                                    parent.state = .{
                                        .long_function = .{
                                            .parameters = try parent.state.tuple.types.toOwnedSlice(allocator),
                                            .variadic_state = .start,
                                        },
                                    };

                                    return .{};
                                }
                            }

                            std.log.err("unexpected token: expected string, number, word, lbrace, lbracket or lparens but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .identifier => |identifier| switch (token) {
                        .larrow => {
                            self.state = .{ .template = .{ .name = identifier } };
                            try self.startChildState(allocator);
                        },
                        else => {
                            self.state = .{ .end = .{ .named = identifier } };
                            return .{ .save = token };
                        },
                    },
                    .tuple => |*tuple| switch (token) {
                        .comma => {
                            try self.startChildState(allocator);
                        },
                        else => {
                            if ((tuple.parens and token == .rparen) or (!tuple.parens and token == .rbracket)) {
                                if (tuple.types.items.len == 1) {
                                    self.finalizeType(.{
                                        .isolated = blk: {
                                            defer tuple.types.deinit(allocator);

                                            break :blk try util.mem.createCopy(FQLType, allocator, &tuple.types.items[0]);
                                        },
                                    });
                                } else {
                                    self.finalizeType(.{
                                        .tuple = .{
                                            .types = try tuple.types.toOwnedSlice(allocator),
                                            .parens = tuple.parens,
                                        },
                                    });
                                }

                                return .{};
                            }

                            std.log.err("unexpected token: expected comma or {s} but got {s}", .{ if (tuple.parens) "rparen" else "rbracket", @tagName(token) });
                            return error.UnexpectedToken;
                        },
                    },
                    .object => |*object_state| switch (object_state.state) {
                        .before_key => {
                            object_state.state = .{
                                .after_key = switch (token) {
                                    .word => |word| .{
                                        .identifier = try allocator.dupe(u8, word),
                                    },
                                    .string => |str| .{
                                        .string = try allocator.dupe(u8, str),
                                    },
                                    .asterisk => .wildcard,
                                    else => {
                                        std.log.err("unexpected token: expected word, string or asterisk but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                },
                            };
                        },
                        .after_key => switch (token) {
                            .colon => {
                                try self.startChildState(allocator);
                            },
                            else => {
                                std.log.err("unexpected token: expected colon but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                        .after_type => |field| switch (token) {
                            .comma => {
                                try object_state.fields.append(allocator, field);
                                object_state.state = .before_key;
                            },
                            .rbrace => {
                                var fields = object_state.fields;
                                defer fields.deinit(allocator);

                                try fields.append(allocator, field);

                                self.state = .{
                                    .end = .{
                                        .object = .{
                                            .fields = try fields.toOwnedSlice(allocator),
                                        },
                                    },
                                };
                            },
                            else => {
                                std.log.err("unexpected token: expected comma or rbrace but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                    },
                    .template => |template| switch (token) {
                        .comma => {
                            try self.startChildState(allocator);
                        },
                        .rarrow => {
                            var parameters = template.parameters;
                            defer parameters.deinit(allocator);

                            self.state = .{
                                .end = .{
                                    .template = .{
                                        .name = template.name,
                                        .parameters = try parameters.toOwnedSlice(allocator),
                                    },
                                },
                            };
                        },
                        else => {
                            std.log.err("unexpected token: expected comma or rarrow but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .long_function => |*long_function| {
                        if (long_function.variadic_state) |variadic_state| {
                            switch (variadic_state) {
                                .after_type => |fql_type| {
                                    switch (token) {
                                        .rparen => {
                                            long_function.variadic_state = .{ .after_rparen = fql_type };
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected rparen but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                },
                                .after_rparen => {
                                    switch (token) {
                                        .equal_rarrow => {
                                            try self.startChildState(allocator);
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected equal_rarrow but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                },
                                else => {
                                    std.debug.panic("invalid parser state: long function: variadic_state is {s}", .{@tagName(variadic_state)});
                                },
                            }
                        } else {
                            std.debug.panic("invalid parser state: long function: variadic_state is null", .{});
                        }
                    },
                    .end => |fql_type| {
                        switch (token) {
                            .question => {
                                self.finalizeType(.{ .optional = try util.mem.createCopy(FQLType, allocator, &fql_type) });
                                return .{};
                            },
                            .pipe => {
                                self.state = .{ .union_lhs = fql_type };
                                try self.startChildState(allocator);
                                return .{};
                            },
                            .equal_rarrow => {
                                self.state = .{ .short_function = fql_type };
                                try self.startChildState(allocator);
                                return .{};
                            },
                            else => {},
                        }

                        if (token == .equal_rarrow) {
                            if (fql_type == .tuple) {
                                self.state = .{
                                    .long_function = .{
                                        .parameters = fql_type.tuple.types.?,
                                    },
                                };

                                try self.startChildState(allocator);
                                return .{};
                            }
                        }

                        if (self.parent) |parent| {
                            defer allocator.destroy(parent);
                            defer self.* = parent.*;

                            switch (parent.state) {
                                .tuple => |*tuple| {
                                    try tuple.types.append(allocator, fql_type);
                                },
                                .object => |*object_state| {
                                    std.debug.assert(object_state.state == .after_key);

                                    object_state.state = .{
                                        .after_type = .{
                                            .key = object_state.state.after_key,
                                            .type = fql_type,
                                        },
                                    };
                                },
                                .template => |*template_state| {
                                    try template_state.parameters.append(allocator, fql_type);
                                },
                                .union_lhs => |lhs| {
                                    parent.finalizeType(.{
                                        .@"union" = .{
                                            .lhs = try util.mem.createCopy(FQLType, allocator, &lhs),
                                            .rhs = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                        },
                                    });
                                },
                                .short_function => |param_type| {
                                    parent.finalizeType(.{
                                        .function = .{
                                            .parameters = .{ .short = try util.mem.createCopy(FQLType, allocator, &param_type) },
                                            .return_type = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                        },
                                    });
                                },
                                .long_function => |*long_function| {
                                    if (long_function.variadic_state) |variadic_state| {
                                        switch (variadic_state) {
                                            .start => {
                                                long_function.variadic_state = .{ .after_type = fql_type };
                                            },
                                            .after_rparen => |after_rparen| {
                                                parent.finalizeType(.{
                                                    .function = .{
                                                        .parameters = .{
                                                            .long = .{
                                                                .types = blk: {
                                                                    const types = try allocator.realloc(@constCast(long_function.parameters), long_function.parameters.len + 1);
                                                                    types[types.len - 1] = after_rparen;
                                                                    break :blk types;
                                                                },
                                                                .variadic = true,
                                                            },
                                                        },
                                                        .return_type = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                                    },
                                                });
                                            },
                                            else => {
                                                std.debug.panic("invalid parser parent state: long function: {s}", .{@tagName(variadic_state)});
                                            },
                                        }
                                    } else {
                                        parent.finalizeType(.{
                                            .function = .{
                                                .parameters = .{
                                                    .long = .{
                                                        .types = long_function.parameters,
                                                    },
                                                },
                                                .return_type = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                            },
                                        });
                                    }
                                },
                                else => std.debug.panic("invalid parser parent state: {s}", .{@tagName(parent.state)}),
                            }

                            return .{ .save = token };
                        }

                        defer self.* = .{};

                        return .{ .save = token, .type = fql_type };
                    },
                    else => {
                        std.debug.panic("invalid parser state: {s}", .{@tagName(self.state)});
                    },
                }

                return .{};
            }
        };

        allocator: std.mem.Allocator,
        inner: Unmanaged = .{},

        pub fn init(allocator: std.mem.Allocator) Parser {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: Parser) void {
            self.inner.deinit(self.allocator);
        }

        pub fn reset(self: *Parser) void {
            self.deinit();
            self.inner = .{};
        }

        pub const PushResult = struct {
            save: ?tokenizer.Token = null,
            type: ?FQLType = null,
        };

        pub fn push(self: *Parser, token: tokenizer.Token) !PushResult {
            return try self.inner.pushToken(self.allocator, token);
        }
    };

    pub fn parse(allocator: std.mem.Allocator, it: *tokenizer.TokenIterator) !FQLType {
        var parser = Parser.init(allocator);
        defer parser.deinit();

        while (true) {
            const token = try it.nextToken(allocator);
            defer token.deinit(allocator);

            // std.debug.print("pushing token: {any}\n", .{token});
            const result = try parser.push(token);
            // std.debug.print("parser state: {s} {s}\n", .{ @tagName(parser.inner.state), if (parser.inner.parent) |p| @tagName(p.state) else "" });
            if (result.save) |save| {
                // std.debug.print("saving token: {any}\n", .{token});
                it.saveToken(try save.dupe(allocator));
            }

            if (result.type) |fql_type| {
                return fql_type;
            }
        }
    }
};

pub fn parseType(allocator: std.mem.Allocator, reader: std.io.AnyReader) !FQLType {
    var it = tokenizer.TokenIterator.init(reader);
    defer it.deinit(allocator);

    return FQLType.parse(allocator, &it);
}

fn expectParsedTypeEqual(str: []const u8, expected: FQLType) !void {
    var stream = std.io.fixedBufferStream(str);
    var actual = try parseType(testing.allocator, stream.reader().any());
    defer actual.deinit(testing.allocator);

    try testing.expectEqualDeep(expected, actual);

    const canonical_string = try actual.toCanonicalString(testing.allocator);
    defer testing.allocator.free(canonical_string);

    try testing.expectEqualStrings(str, canonical_string);
}

test parseType {
    try expectParsedTypeEqual("String", .{ .named = "String" });
    try expectParsedTypeEqual("Array<Date>", .{
        .template = .{
            .name = "Array",
            .parameters = &[_]FQLType{.{ .named = "Date" }},
        },
    });
    try expectParsedTypeEqual("{ foo: Uuid, \"bar\": Boolean, *: Any }", .{
        .object = .{
            .fields = &[_]FQLType.Object.Field{
                .{
                    .key = .{ .identifier = "foo" },
                    .type = .{ .named = "Uuid" },
                },
                .{
                    .key = .{ .string = "\"bar\"" },
                    .type = .{ .named = "Boolean" },
                },
                .{
                    .key = .wildcard,
                    .type = .{ .named = "Any" },
                },
            },
        },
    });
    try expectParsedTypeEqual("[Bytes, Number]", .{
        .tuple = .{
            .types = &[_]FQLType{
                .{ .named = "Bytes" },
                .{ .named = "Number" },
            },
        },
    });
    try expectParsedTypeEqual("Time | Null", .{
        .@"union" = .{
            .lhs = &FQLType{ .named = "Time" },
            .rhs = &FQLType{ .named = "Null" },
        },
    });
    try expectParsedTypeEqual("{ x: number } | string", .{
        .@"union" = .{
            .lhs = &FQLType{
                .object = .{
                    .fields = &[_]FQLType.Object.Field{
                        .{
                            .key = .{ .identifier = "x" },
                            .type = .{ .named = "number" },
                        },
                    },
                },
            },
            .rhs = &FQLType{ .named = "string" },
        },
    });
    try expectParsedTypeEqual("'0' | 0", .{
        .@"union" = .{
            .lhs = &FQLType{ .string_literal = "'0'" },
            .rhs = &FQLType{ .number_literal = "0" },
        },
    });
    try expectParsedTypeEqual("ID?", .{
        .optional = &FQLType{ .named = "ID" },
    });

    try expectParsedTypeEqual("(String, ...Number) => String", .{
        .function = .{
            .parameters = .{
                .long = .{
                    .types = &[_]FQLType{
                        .{ .named = "String" },
                        .{ .named = "Number" },
                    },
                    .variadic = true,
                },
            },
            .return_type = &FQLType{ .named = "String" },
        },
    });
}

pub const FQLExpression = union(enum) {
    pub const ObjectLiteral = struct {
        pub const Field = struct {
            pub const Key = union(enum) {
                identifier: []const u8,
                string: []const u8,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self) {
                        inline else => |s| allocator.free(s),
                    }
                }
            };

            key: Key,
            value: *const FQLExpression,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                self.key.deinit(allocator);
                self.value.deinit(allocator);
                allocator.destroy(self.value);
            }

            fn multilineCanonical(self: @This()) bool {
                return self.value.multilineCanonical();
            }
        };

        fields: ?[]const Field = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.fields) |fields| {
                for (fields) |field| {
                    field.deinit(allocator);
                }

                allocator.free(fields);
            }
        }

        fn multilineCanonical(self: @This()) bool {
            return self.fields != null and util.slice.some(self.fields.?, Field.multilineCanonical);
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try writer.writeByte('{');

            if (self.fields) |fields| {
                if (fields.len > 0) {
                    const is_multiline = fields.len > 1 or util.slice.some(fields, Field.multilineCanonical);
                    if (is_multiline) {
                        try writer.writeByte('\n');
                    } else {
                        try writer.writeByte(' ');
                    }

                    for (fields, 0..) |field, i| {
                        if (is_multiline) {
                            try writer.writeBytesNTimes(indent_str, level + 1);
                        } else if (i > 0) {
                            try writer.writeAll(", ");
                        }

                        switch (field.key) {
                            inline else => |k| try writer.writeAll(k),
                        }

                        try writer.writeAll(": ");

                        try field.value.printCanonical(writer, indent_str, level + @intFromBool(is_multiline));

                        if (is_multiline) {
                            try writer.writeAll(",\n");
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    } else {
                        try writer.writeByte(' ');
                    }
                }
            }

            try writer.writeByte('}');
        }
    };

    pub const ArrayLiteral = struct {
        elements: ?[]const FQLExpression = null,
        parens: bool = false,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.elements) |elems| {
                for (elems) |elem| {
                    elem.deinit(allocator);
                }

                allocator.free(elems);
            }
        }

        fn multilineCanonical(self: @This()) bool {
            return self.elements != null and util.slice.some(self.elements.?, FQLExpression.multilineCanonical);
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try writer.writeByte(if (self.parens) '(' else '[');
            if (self.elements) |elems| {
                if (elems.len > 0) {
                    const is_multiline = elems.len > 1 or util.slice.some(elems, FQLExpression.multilineCanonical);
                    if (is_multiline) {
                        try writer.writeByte('\n');
                    }

                    for (elems, 0..) |elem, i| {
                        if (is_multiline) {
                            try writer.writeBytesNTimes(indent_str, level + 1);
                        } else if (i > 0) {
                            try writer.writeAll(", ");
                        }

                        try elem.printCanonical(writer, indent_str, level + @intFromBool(is_multiline));

                        if (is_multiline) {
                            try writer.writeAll(",\n");
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    }
                }
            }
            try writer.writeByte(if (self.parens) ')' else ']');
        }
    };

    pub const BinaryOperation = struct {
        pub const Operator = enum {
            add,
            subtract,
            multiply,
            divide,
            power,
            modulos,
            equality,
            inequality,
            less_than,
            less_than_or_equal,
            greater_than,
            greater_than_or_equal,
            logical_and,
            logical_or,
            bitwise_and,
            bitwise_or,
            bitwise_xor,
            null_coalescence,
            isa,

            fn precedence(self: Operator) usize {
                return switch (self) {
                    .null_coalescence => 0,
                    .logical_or => 1,
                    .logical_and => 2,
                    .equality, .inequality => 3,
                    .less_than, .less_than_or_equal, .greater_than, .greater_than_or_equal => 4,
                    .isa => 5,
                    .bitwise_or => 6,
                    .bitwise_xor => 7,
                    .bitwise_and => 8,
                    .add, .subtract => 9,
                    .multiply, .divide, .modulos => 10,
                    .power => 11,
                };
            }

            fn fromToken(token: tokenizer.Token) ?Operator {
                return switch (token) {
                    .plus => .add,
                    .minus => .subtract,
                    .asterisk => .multiply,
                    .slash => .divide,
                    .asterisk2 => .power,
                    .percent => .modulos,
                    .equal2 => .equality,
                    .bang_equal => .inequality,
                    .larrow => .less_than,
                    .larrow_equal => .less_than_or_equal,
                    .rarrow => .greater_than,
                    .rarrow_equal => .greater_than_or_equal,
                    .ampersand2 => .logical_and,
                    .pipe2 => .logical_or,
                    .ampersand => .bitwise_and,
                    .pipe => .bitwise_or,
                    .caret => .bitwise_xor,
                    .question2 => .null_coalescence,
                    .word => |word| if (std.mem.eql(u8, word, "isa")) .isa else null,
                    else => null,
                };
            }

            pub fn toString(self: Operator) []const u8 {
                return switch (self) {
                    .add => "+",
                    .subtract => "-",
                    .multiply => "*",
                    .divide => "/",
                    .power => "**",
                    .modulos => "%",
                    .equality => "==",
                    .inequality => "!=",
                    .less_than => "<",
                    .less_than_or_equal => "<=",
                    .greater_than => ">",
                    .greater_than_or_equal => ">=",
                    .logical_and => "&&",
                    .logical_or => "||",
                    .bitwise_and => "&",
                    .bitwise_or => "|",
                    .bitwise_xor => "^",
                    .null_coalescence => "??",
                    .isa => "isa",
                };
            }
        };

        operator: Operator,
        lhs: *const FQLExpression,
        rhs: *const FQLExpression,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            self.rhs.deinit(allocator);
            allocator.destroy(self.lhs);
            allocator.destroy(self.rhs);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.lhs.multilineCanonical() or self.rhs.multilineCanonical();
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.lhs.printCanonical(writer, indent_str, level);
            try writer.writeByte(' ');
            try writer.writeAll(self.operator.toString());
            try writer.writeByte(' ');
            try self.rhs.printCanonical(writer, indent_str, level);
        }
    };

    pub const UnaryOperation = struct {
        pub const Operator = enum {
            logical_not,
            bitwise_not,
            arithmetic_not,

            pub fn toString(self: Operator) []const u8 {
                return switch (self) {
                    .logical_not => "!",
                    .bitwise_not => "~",
                    .arithmetic_not => "-",
                };
            }
        };

        operator: Operator,
        operand: *const FQLExpression,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            self.operand.deinit(allocator);
            allocator.destroy(self.operand);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.operand.multilineCanonical();
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try writer.writeAll(self.operator.toString());
            try self.operand.printCanonical(writer, indent_str, level);
        }
    };

    pub const FieldAccessKey = union(enum) {
        identifier: []const u8,
        expression: *const FQLExpression,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            switch (self) {
                .identifier => |identifier| allocator.free(identifier),
                .expression => |expression| {
                    expression.deinit(allocator);
                    allocator.destroy(expression);
                },
            }
        }
    };

    pub const FieldAccess = struct {
        value: *const FQLExpression,
        field: FieldAccessKey,
        optional: bool = false,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            self.value.deinit(allocator);
            self.field.deinit(allocator);
            allocator.destroy(self.value);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.value.multilineCanonical() or (self.field == .expression and self.field.expression.multilineCanonical());
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.value.printCanonical(writer, indent_str, level);
            if (self.optional) {
                try writer.writeByte('?');
            }

            switch (self.field) {
                .identifier => |ident| {
                    try writer.writeByte('.');
                    try writer.writeAll(ident);
                },
                .expression => |expr| {
                    if (self.optional) {
                        try writer.writeByte('.');
                    }

                    try writer.writeByte('[');
                    if (expr.multilineCanonical()) {
                        try writer.writeByte('\n');
                        try writer.writeBytesNTimes(indent_str, level + 1);
                        try expr.printCanonical(writer, indent_str, level + 1);
                        try writer.writeByte('\n');
                        try writer.writeBytesNTimes(indent_str, level);
                    } else {
                        try expr.printCanonical(writer, indent_str, level);
                    }
                    try writer.writeByte(']');
                },
            }
        }
    };

    pub const Invocation = struct {
        function: *const FQLExpression,
        arguments: ?[]const FQLExpression = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.arguments) |arguments| {
                for (arguments) |argument| {
                    argument.deinit(allocator);
                }

                allocator.free(arguments);
            }

            self.function.deinit(allocator);
            allocator.destroy(self.function);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.function.multilineCanonical() or (self.arguments != null and util.slice.some(self.arguments.?, FQLExpression.multilineCanonical));
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.function.printCanonical(writer, indent_str, level);
            try writer.writeByte('(');
            if (self.arguments) |args| {
                if (args.len > 0) {
                    const is_multiline = util.slice.some(args, FQLExpression.multilineCanonical);
                    if (is_multiline) {
                        try writer.writeByte('\n');
                    }

                    for (args, 0..) |arg, i| {
                        if (is_multiline) {
                            try writer.writeBytesNTimes(indent_str, level + 1);
                        } else if (i > 0) {
                            try writer.writeAll(", ");
                        }

                        try arg.printCanonical(writer, indent_str, level + @intFromBool(is_multiline));
                        if (is_multiline) {
                            try writer.writeAll(",\n");
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    }
                }
            }
            try writer.writeByte(')');
        }
    };

    pub const VariableDeclaration = struct {
        name: []const u8,
        type: ?FQLType = null,
        value: *const FQLExpression,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.type) |fql_type| {
                fql_type.deinit(allocator);
            }

            allocator.free(self.name);
            self.value.deinit(allocator);
            allocator.destroy(self.value);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.value.multilineCanonical();
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try writer.writeAll("let ");
            try writer.writeAll(self.name);
            if (self.type) |fql_type| {
                try writer.writeAll(": ");
                try fql_type.printCanonical(writer);
            }
            try writer.writeAll(" = ");
            try self.value.printCanonical(writer, indent_str, level);
        }
    };

    pub const Conditional = struct {
        condition: *const FQLExpression,
        body: *const FQLExpression,
        @"else": ?*const FQLExpression = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.@"else") |@"else"| {
                @"else".deinit(allocator);
                allocator.destroy(@"else");
            }

            self.body.deinit(allocator);
            allocator.destroy(self.body);
            self.condition.deinit(allocator);
            allocator.destroy(self.condition);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.condition.multilineCanonical() or self.body.multilineCanonical() or (self.@"else" != null and self.@"else".?.multilineCanonical());
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try writer.writeAll("if (");
            if (self.condition.multilineCanonical()) {
                try writer.writeByte('\n');
                try writer.writeBytesNTimes(indent_str, level + 1);
                try self.condition.printCanonical(writer, indent_str, level + 1);
                try writer.writeByte('\n');
                try writer.writeBytesNTimes(indent_str, level);
            } else {
                try self.condition.printCanonical(writer, indent_str, level);
            }

            try writer.writeAll(") ");
            try self.body.printCanonical(writer, indent_str, level);
            if (self.@"else") |expr| {
                try writer.writeAll(" else ");
                try expr.printCanonical(writer, indent_str, level);
            }
        }
    };

    pub const Function = struct {
        pub const Parameters = union(enum) {
            long: struct {
                parameters: ?[]const []const u8 = null,
                variadic: bool = false,
            },
            short: []const u8,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    .long => |long| {
                        if (long.parameters) |params| {
                            for (params) |param| {
                                allocator.free(param);
                            }

                            allocator.free(params);
                        }
                    },
                    .short => |s| allocator.free(s),
                }
            }
        };

        parameters: Parameters,
        body: *const FQLExpression,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            self.parameters.deinit(allocator);
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.body.multilineCanonical();
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            switch (self.parameters) {
                .long => |l| {
                    try writer.writeByte('(');

                    if (l.parameters) |params| {
                        for (params, 0..) |param, i| {
                            if (i > 0) {
                                try writer.writeAll(", ");
                            }

                            if (l.variadic and i == params.len - 1) {
                                try writer.writeAll("...");
                            }

                            try writer.writeAll(param);
                        }
                    }

                    try writer.writeByte(')');
                },
                .short => |s| try writer.writeAll(s),
            }

            try writer.writeAll(" => ");

            try self.body.printCanonical(writer, indent_str, level);
        }
    };

    pub const Projection = struct {
        pub const Field = union(enum) {
            short: []const u8,
            long: struct {
                key: []const u8, // apparently this can be an asterisk? not sure what it does though...
                value: *const FQLExpression,
            },

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    .short => |s| allocator.free(s),
                    .long => |l| {
                        allocator.free(l.key);
                        l.value.deinit(allocator);
                        allocator.destroy(l.value);
                    },
                }
            }

            fn multilineCanonical(self: @This()) bool {
                return self == .long and self.long.value.multilineCanonical();
            }
        };

        expression: *const FQLExpression,
        fields: ?[]const Field = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.fields) |fields| {
                for (fields) |field| {
                    field.deinit(allocator);
                }

                allocator.free(fields);
            }

            self.expression.deinit(allocator);
            allocator.destroy(self.expression);
        }

        fn multilineCanonical(self: @This()) bool {
            return self.expression.multilineCanonical() or (self.fields != null and util.slice.some(self.fields.?, Field.multilineCanonical));
        }

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.expression.printCanonical(writer, indent_str, level);

            try writer.writeAll(" {");

            if (self.fields) |fields| {
                if (fields.len > 0) {
                    const is_multiline = fields.len > 1 or util.slice.some(fields, Field.multilineCanonical);
                    if (is_multiline) {
                        try writer.writeByte('\n');
                    } else {
                        try writer.writeByte(' ');
                    }

                    for (fields, 0..) |field, i| {
                        if (is_multiline) {
                            try writer.writeBytesNTimes(indent_str, level + 1);
                        } else if (i > 0) {
                            try writer.writeAll(", ");
                        }

                        switch (field) {
                            .short => |s| try writer.writeAll(s),
                            .long => |l| {
                                try writer.writeAll(l.key);
                                try writer.writeAll(": ");
                                try l.value.printCanonical(writer, indent_str, level + 1);
                            },
                        }

                        if (is_multiline) {
                            try writer.writeAll(",\n");
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    } else {
                        try writer.writeByte(' ');
                    }
                }
            }

            try writer.writeAll("}");
        }
    };

    null,
    identifier: []const u8,
    number_literal: []const u8,
    string_literal: []const u8,
    boolean_literal: bool,
    array_literal: ArrayLiteral,
    object_literal: ObjectLiteral,
    unary_operation: UnaryOperation,
    binary_operation: BinaryOperation,
    field_access: FieldAccess,
    invocation: Invocation,
    variable_declaration: VariableDeclaration,
    block_scope: []const FQLExpression,
    conditional: Conditional,
    isolated: *const FQLExpression,
    function: Function,
    anonymous_field_access: FieldAccessKey,
    projection: Projection,
    non_null_assertion: *const FQLExpression,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        // std.debug.print("deinit FQLExpression.{s}\n", .{@tagName(self)});
        switch (self) {
            inline .isolated, .non_null_assertion => |s| {
                s.deinit(allocator);
                allocator.destroy(s);
            },
            inline .identifier,
            .number_literal,
            .string_literal,
            => |s| allocator.free(s),
            inline .object_literal,
            .array_literal,
            .unary_operation,
            .binary_operation,
            .function,
            .invocation,
            .field_access,
            .variable_declaration,
            .conditional,
            .anonymous_field_access,
            .projection,
            => |expr| expr.deinit(allocator),
            inline .block_scope => |exprs| {
                for (exprs) |expr| {
                    expr.deinit(allocator);
                }

                allocator.free(exprs);
            },
            .null, .boolean_literal => {},
        }
    }

    fn multilineCanonical(self: FQLExpression) bool {
        return switch (self) {
            .identifier, .number_literal, .string_literal, .null, .boolean_literal => false,
            inline .isolated, .non_null_assertion => |child| child.multilineCanonical(),
            .anonymous_field_access => |key| switch (key) {
                .identifier => false,
                .expression => |expr| expr.multilineCanonical(),
            },
            .block_scope => |exprs| exprs.len > 0,
            inline else => |e| e.multilineCanonical(),
        };
    }

    pub fn printCanonical(self: FQLExpression, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) std.io.AnyWriter.Error!void {
        switch (self) {
            inline .identifier, .number_literal, .string_literal => |s| try writer.writeAll(s),
            .null => try writer.writeAll("null"),
            .boolean_literal => |b| try writer.writeAll(if (b) "true" else "false"),
            .isolated => |child| {
                try writer.writeByte('(');
                try child.printCanonical(writer, indent_str, level);
                try writer.writeByte(')');
            },
            .non_null_assertion => |child| {
                try child.printCanonical(writer, indent_str, level);
                try writer.writeByte('!');
            },
            .anonymous_field_access => |key| {
                try writer.writeByte('.');
                switch (key) {
                    .identifier => |ident| try writer.writeAll(ident),
                    .expression => |expr| {
                        try writer.writeByte('[');
                        try expr.printCanonical(writer, indent_str, level);
                        try writer.writeByte(']');
                    },
                }
            },
            .block_scope => |exprs| {
                try writer.writeByte('{');
                if (exprs.len > 0) {
                    try writer.writeByte('\n');

                    for (exprs) |expr| {
                        try writer.writeBytesNTimes(indent_str, level + 1);

                        try expr.printCanonical(writer, indent_str, level + 1);
                        try writer.writeByte('\n');
                    }

                    try writer.writeBytesNTimes(indent_str, level);
                }

                try writer.writeAll("}");
            },
            inline else => |e| try e.printCanonical(writer, indent_str, level),
        }
    }

    pub fn toCanonicalString(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
        var str = std.ArrayList(u8).init(allocator);
        defer str.deinit();

        try self.printCanonical(str.writer().any(), "    ", 0);

        return try str.toOwnedSlice();
    }

    fn isIdentifier(self: FQLExpression) bool {
        return self == .identifier;
    }

    pub const Parser = struct {
        pub const Unmanaged = struct {
            const State = union(enum) {
                const VariableDeclaration = struct {
                    name: ?[]const u8 = null,
                    type: ?union(enum) { parser: FQLType.Parser.Unmanaged, type: FQLType } = null,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.name) |name| {
                            allocator.free(name);
                        }

                        if (self.type) |type_state| {
                            switch (type_state) {
                                inline else => |v| v.deinit(allocator),
                            }
                        }
                    }
                };

                const Conditional = union(enum) {
                    const AfterBody = struct {
                        condition: *const FQLExpression,
                        body: *const FQLExpression,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            self.condition.deinit(allocator);
                            allocator.destroy(self.condition);
                            self.body.deinit(allocator);
                            allocator.destroy(self.body);
                        }
                    };

                    start,
                    after_lparen,
                    after_condition: FQLExpression,
                    after_rparen: FQLExpression,
                    after_body: AfterBody,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self) {
                            inline .after_condition,
                            .after_rparen,
                            .after_body,
                            => |v| v.deinit(allocator),
                            else => {},
                        }
                    }
                };

                const BinaryOperation = struct {
                    lhs: FQLExpression,
                    operator: FQLExpression.BinaryOperation.Operator,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.lhs.deinit(allocator);
                    }
                };

                const ArrayLiteral = struct {
                    elements: std.ArrayListUnmanaged(FQLExpression) = .{},
                    parens: bool = false,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.elements.items) |expr| {
                            expr.deinit(allocator);
                        }

                        var elements_mutable_copy = self.elements;
                        elements_mutable_copy.deinit(allocator);
                    }
                };

                const ObjectLiteral = struct {
                    fields: std.ArrayListUnmanaged(FQLExpression.ObjectLiteral.Field) = .{},
                    state: union(enum) {
                        start,
                        after_key: FQLExpression.ObjectLiteral.Field.Key,
                        end,
                    } = .start,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.fields.items) |item| {
                            item.deinit(allocator);
                        }

                        var fields_mutable_copy = self.fields;
                        fields_mutable_copy.deinit(allocator);
                        switch (self.state) {
                            .after_key => |key| key.deinit(allocator),
                            else => {},
                        }
                    }
                };

                const FieldAccess = struct {
                    value: FQLExpression,
                    optional: bool = false,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.value.deinit(allocator);
                    }
                };

                const Invocation = struct {
                    function: FQLExpression,
                    arguments: std.ArrayListUnmanaged(FQLExpression) = .{},

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.arguments.items) |expr| {
                            expr.deinit(allocator);
                        }

                        var arguments_mutable_copy = self.arguments;
                        arguments_mutable_copy.deinit(allocator);
                        self.function.deinit(allocator);
                    }
                };

                const Projection = struct {
                    expression: FQLExpression,
                    fields: std.ArrayListUnmanaged(FQLExpression.Projection.Field) = .{},
                    state: union(enum) {
                        start,
                        after_identifier: []const u8,
                        end,
                    } = .start,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.state == .after_identifier) {
                            allocator.free(self.state.after_identifier);
                        }

                        for (self.fields.items) |field| {
                            field.deinit(allocator);
                        }

                        var fields_mutable_copy = self.fields;
                        fields_mutable_copy.deinit(allocator);
                        self.expression.deinit(allocator);
                    }
                };

                const LongFunction = struct {
                    parameters: []const []const u8,
                    variadic_state: ?union(enum) {
                        start,
                        after_param: []const u8,
                        after_rparen: []const u8,
                    } = null,

                    fn fromExprs(allocator: std.mem.Allocator, exprs: []const FQLExpression, variadic: bool) !LongFunction {
                        const params = try allocator.alloc([]const u8, exprs.len);
                        for (exprs, 0..) |elem, i| {
                            params[i] = elem.identifier;
                        }

                        return .{ .parameters = params, .variadic_state = if (variadic) .start else null };
                    }
                };

                empty,
                after_lbrace: ?FQLExpression.ObjectLiteral.Field.Key,
                after_identifier: []const u8,
                variable_declaration: State.VariableDeclaration,
                conditional: State.Conditional,
                unary_operation: UnaryOperation.Operator,
                binary_operation: State.BinaryOperation,
                array_literal: State.ArrayLiteral,
                object_literal: State.ObjectLiteral,
                field_access: State.FieldAccess,
                anonymous_field_access: ?FQLExpression,
                invocation: State.Invocation,
                projection: State.Projection,
                block_scope: std.ArrayListUnmanaged(FQLExpression),
                long_function: LongFunction,
                short_function: []const u8,
                end: FQLExpression,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    // std.debug.print("deinit FQLExpression.Parser.Unmanaged.State.{s}\n", .{@tagName(self)});
                    switch (self) {
                        .empty, .unary_operation => {},
                        .long_function => |long_function| {
                            for (long_function.parameters) |str| {
                                allocator.free(str);
                            }

                            allocator.free(long_function.parameters);
                        },
                        .block_scope => |exprs| {
                            for (exprs.items) |expr| {
                                expr.deinit(allocator);
                            }

                            var exprs_mutable_copy = exprs;
                            exprs_mutable_copy.deinit(allocator);
                        },
                        inline .after_lbrace, .anonymous_field_access => |key| if (key) |k| k.deinit(allocator),
                        inline .after_identifier, .short_function => |str| allocator.free(str),
                        inline else => |v| v.deinit(allocator),
                    }
                }
            };

            parent: ?*@This() = null,
            state: State = .empty,

            pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                if (self.parent) |parent| {
                    parent.deinit(allocator);
                    allocator.destroy(parent);
                }

                self.state.deinit(allocator);
            }

            fn startChildState(self: *@This(), allocator: std.mem.Allocator) !void {
                self.* = .{ .parent = try util.mem.createCopy(@This(), allocator, self) };
            }

            inline fn finalizeExpr(self: *@This(), expr: FQLExpression) void {
                self.state = .{ .end = expr };
            }

            pub fn pushToken(self: *@This(), allocator: std.mem.Allocator, token: tokenizer.Token) !PushResult {
                // std.debug.print("expression parser state: {s} {s}\n", .{ @tagName(self.state), if (self.parent) |p| @tagName(p.state) else "" });
                // std.debug.print("got token: {any}\n", .{token});

                if (token == .comment_block or token == .comment_line) {
                    return .{};
                }

                switch (self.state) {
                    .empty => switch (token) {
                        .eol, .semi => {},

                        // number literal
                        .number => |num| self.finalizeExpr(.{ .number_literal = try allocator.dupe(u8, num) }),

                        // string literal
                        .string => |str| self.finalizeExpr(.{ .string_literal = try allocator.dupe(u8, str) }),

                        // word indicates an identifier, null/boolean literal or a let/if statement
                        .word => |word| {
                            if (std.meta.stringToEnum(enum { let, @"if", null, true, false }, word)) |keyword| switch (keyword) {
                                .@"if" => self.state = .{ .conditional = .start },
                                .let => self.state = .{ .variable_declaration = .{} },
                                .null => self.finalizeExpr(.null),
                                .true => self.finalizeExpr(.{ .boolean_literal = true }),
                                .false => self.finalizeExpr(.{ .boolean_literal = false }),
                            } else {
                                self.state = .{ .after_identifier = try allocator.dupe(u8, word) };
                            }
                        },

                        // bang indicates a logical-not unary operation
                        .bang => {
                            self.state = .{ .unary_operation = .logical_not };
                            try self.startChildState(allocator);
                        },

                        // tilde indicates a bitwise-not unary operation
                        .tilde => {
                            self.state = .{ .unary_operation = .bitwise_not };
                            try self.startChildState(allocator);
                        },

                        // minus indicates an arithmetic-not unary operation
                        .minus => {
                            self.state = .{ .unary_operation = .arithmetic_not };
                            try self.startChildState(allocator);
                        },

                        // lbracket indicates an array literal (aka a tuple)
                        .lbracket => {
                            self.state = .{ .array_literal = .{} };
                            try self.startChildState(allocator);
                        },

                        // dot indicates an anonymous field access (i.e. `.foo` or `.["foo"]`)
                        .dot => self.state = .{ .anonymous_field_access = null },

                        // lparen indicates either an isolated (parenthesized) expression, parenthesized tuple or a long form anonymous function (i.e. `() => {}`)
                        .lparen => {
                            // pretend it's a parenthesized tuple until proved otherwise
                            self.state = .{ .array_literal = .{ .parens = true } };
                            try self.startChildState(allocator);
                        },

                        // lbrace indicates either a block scope or an object literal
                        .lbrace => self.state = .{ .after_lbrace = null },
                        else => {
                            if (self.parent) |parent| {
                                switch (parent.state) {
                                    .array_literal => {
                                        if (token == .rbracket) {
                                            defer allocator.destroy(parent);
                                            self.* = parent.*;

                                            return .{ .save = token };
                                        }

                                        if (parent.state.array_literal.parens and token == .dot3) {
                                            if (util.slice.every(parent.state.array_literal.elements, isIdentifier)) {
                                                parent.state = .{
                                                    .long_function = blk: {
                                                        defer parent.state.array_literal.elements.deinit(allocator);

                                                        break :blk try State.LongFunction.fromExprs(
                                                            allocator,
                                                            parent.state.array_literal.elements.items,
                                                            true,
                                                        );
                                                    },
                                                };

                                                self.* = parent.*;
                                                allocator.destroy(parent);

                                                return .{};
                                            }
                                        }
                                    },
                                    .block_scope => {
                                        if (token == .rbrace) {
                                            defer allocator.destroy(parent);
                                            self.* = parent.*;

                                            return .{ .save = token };
                                        }
                                    },
                                    .invocation => {
                                        if (token == .rparen) {
                                            defer allocator.destroy(parent);
                                            self.* = parent.*;

                                            return .{ .save = token };
                                        }
                                    },
                                    else => {},
                                }
                            }

                            std.log.err("unexpected token: expected number, string, word, bang, tilde, lbracket, dot, lparen or lbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .after_identifier => |identifier| {
                        // could be identifier or function
                        switch (token) {
                            .equal_rarrow => {
                                self.state = .{ .short_function = identifier };
                                try self.startChildState(allocator);
                            },
                            else => {
                                self.finalizeExpr(.{ .identifier = identifier });
                                return .{ .save = token };
                            },
                        }
                    },
                    .block_scope => |*block_scope| {
                        self.finalizeExpr(.{ .block_scope = try block_scope.toOwnedSlice(allocator) });
                    },
                    .variable_declaration => |*variable_declaration| {
                        if (variable_declaration.name == null) {
                            switch (token) {
                                .word => |word| {
                                    variable_declaration.name = try allocator.dupe(u8, word);
                                },
                                else => {
                                    std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        } else if (variable_declaration.type == null) {
                            switch (token) {
                                .equal => {
                                    try self.startChildState(allocator);
                                },
                                .colon => {
                                    variable_declaration.type = .{ .parser = .{} };
                                },
                                else => {
                                    std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        } else if (variable_declaration.type.? == .parser) {
                            const result = try variable_declaration.type.?.parser.pushToken(allocator, token);
                            if (result.type) |fql_type| {
                                variable_declaration.type = .{ .type = fql_type };
                            }

                            return .{ .save = result.save };
                        } else {
                            switch (token) {
                                .equal => {
                                    try self.startChildState(allocator);
                                },
                                else => {
                                    std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        }
                    },
                    .conditional => |state| switch (state) {
                        .start => switch (token) {
                            .lparen => {
                                self.state.conditional = .after_lparen;
                                try self.startChildState(allocator);
                            },
                            else => {
                                std.log.err("unexpected token: expected lparen but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                        .after_condition => |condition| switch (token) {
                            .rparen => {
                                self.state.conditional = .{ .after_rparen = condition };
                                try self.startChildState(allocator);
                            },
                            else => {
                                std.log.err("unexpected token: expected rparen but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                        .after_body => |after_body| {
                            if (token == .word and std.mem.eql(u8, token.word, "else")) {
                                try self.startChildState(allocator);
                            } else {
                                self.finalizeExpr(.{
                                    .conditional = .{
                                        .condition = after_body.condition,
                                        .body = after_body.body,
                                    },
                                });

                                return .{ .save = token };
                            }
                        },
                        else => unreachable,
                    },
                    .array_literal => |array_literal| switch (token) {
                        .eol => {},
                        .comma => try self.startChildState(allocator),
                        else => {
                            if (array_literal.parens and token == .rparen and array_literal.elements.items.len == 1) {
                                var elems = array_literal.elements;
                                defer elems.deinit(allocator);

                                // parenthesized tuples with exactly 1 element are actually not tuples at all! :o
                                self.finalizeExpr(.{
                                    .isolated = try util.mem.createCopy(FQLExpression, allocator, &elems.items[0]),
                                });

                                return .{};
                            }

                            if ((array_literal.parens and token == .rparen) or (!array_literal.parens and token == .rbracket)) {
                                var elems = array_literal.elements;
                                defer elems.deinit(allocator);

                                self.finalizeExpr(.{
                                    .array_literal = .{
                                        .elements = try elems.toOwnedSlice(allocator),
                                    },
                                });

                                return .{};
                            }

                            std.log.err("unexpected token: expected comma or {s} but got {s}", .{ if (array_literal.parens) "rparen" else "rbracket", @tagName(token) });
                            return error.UnexpectedToken;
                        },
                    },
                    .field_access => |field_access| switch (token) {
                        .word => |word| {
                            self.finalizeExpr(.{
                                .field_access = .{
                                    .value = try util.mem.createCopy(FQLExpression, allocator, &field_access.value),
                                    .field = .{ .identifier = try allocator.dupe(u8, word) },
                                    .optional = field_access.optional,
                                },
                            });
                        },
                        .lbracket => {
                            try self.startChildState(allocator);
                        },
                        else => {
                            std.log.err("unexpected token: expected word or lbracket but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .anonymous_field_access => |anonymous_field_access| {
                        if (anonymous_field_access) |expr| {
                            switch (token) {
                                .rbracket => {
                                    self.finalizeExpr(.{
                                        .anonymous_field_access = .{
                                            .expression = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        },
                                    });
                                },
                                else => {
                                    std.log.err("unexpected token: expected rbracket but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        } else {
                            switch (token) {
                                .word => |word| {
                                    self.finalizeExpr(.{
                                        .anonymous_field_access = .{
                                            .identifier = try allocator.dupe(u8, word),
                                        },
                                    });
                                },
                                .lbracket => {
                                    try self.startChildState(allocator);
                                },
                                else => {
                                    std.log.err("unexpected token: expected word or lbracket but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        }
                    },
                    .projection => |*projection| switch (projection.state) {
                        .start => switch (token) {
                            .eol => {},
                            .word => |word| projection.state = .{ .after_identifier = try allocator.dupe(u8, word) },
                            .rbrace => {
                                projection.state = .end;
                                return .{ .save = token };
                            },
                            else => {
                                std.log.err("unexpected token: expected word or rbrace but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                        .after_identifier => |identifier| switch (token) {
                            .eol => {},
                            .colon => {
                                try self.startChildState(allocator);
                            },
                            .comma => {
                                try projection.fields.append(allocator, .{
                                    .short = identifier,
                                });

                                projection.state = .start;
                            },
                            .rbrace => {
                                try projection.fields.append(allocator, .{
                                    .short = identifier,
                                });

                                projection.state = .end;
                                return .{ .save = token };
                            },
                            else => {
                                std.log.err("unexpected token: expected colon, comma or rbrace but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                        .end => switch (token) {
                            .comma => {
                                projection.state = .start;
                            },
                            .rbrace => {
                                self.finalizeExpr(.{
                                    .projection = .{
                                        .expression = try util.mem.createCopy(FQLExpression, allocator, &projection.expression),
                                        .fields = try projection.fields.toOwnedSlice(allocator),
                                    },
                                });
                            },
                            else => {
                                std.log.err("unexpected token: expected colon, comma or rbrace but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                    },
                    .after_lbrace => |after_lbrace| {
                        // determine if we're dealing with a block scope or an object literal
                        if (after_lbrace) |key| {
                            switch (token) {
                                .eol => {},
                                .colon => {
                                    self.state = .{
                                        .object_literal = .{
                                            .state = .{
                                                .after_key = key,
                                            },
                                        },
                                    };
                                    try self.startChildState(allocator);
                                },
                                else => {
                                    defer key.deinit(allocator);

                                    // it's not an object literal...
                                    self.state = .{ .block_scope = .{} };
                                    try self.startChildState(allocator);
                                    const result = try self.pushToken(allocator, switch (key) {
                                        .identifier => |word| .{ .word = word },
                                        .string => |str| .{ .string = str },
                                    });
                                    std.debug.assert(result.save == null);

                                    return .{ .save = token, .expr = result.expr };
                                },
                            }
                        } else {
                            switch (token) {
                                .eol => {},
                                .word => |word| {
                                    // it still could be either...
                                    self.state.after_lbrace = .{ .identifier = try allocator.dupe(u8, word) };
                                },
                                .string => |word| {
                                    // it still could be either...
                                    self.state.after_lbrace = .{ .string = try allocator.dupe(u8, word) };
                                },
                                else => {
                                    // it's probably a block scope
                                    self.state = .{ .block_scope = .{} };
                                    return .{ .save = token };
                                },
                            }
                        }
                    },
                    .object_literal => |*object_literal| switch (object_literal.state) {
                        .start => switch (token) {
                            .eol => {},
                            .word => |word| {
                                object_literal.state = .{ .after_key = .{ .identifier = try allocator.dupe(u8, word) } };
                            },
                            .string => |str| {
                                object_literal.state = .{ .after_key = .{ .string = try allocator.dupe(u8, str) } };
                            },
                            .rbrace => {
                                object_literal.state = .end;
                                return .{ .save = token };
                            },
                            else => {
                                std.log.err("unexpected token: expected word, string or rbrace but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                        .after_key => {
                            switch (token) {
                                .colon => {
                                    try self.startChildState(allocator);
                                },
                                else => {
                                    std.log.err("unexpected token: expected colon but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .end => switch (token) {
                            .comma => object_literal.state = .start,
                            .rbrace => {
                                var fields_mutable_copy = object_literal.fields;
                                defer fields_mutable_copy.deinit(allocator);

                                self.finalizeExpr(.{
                                    .object_literal = .{
                                        .fields = try fields_mutable_copy.toOwnedSlice(allocator),
                                    },
                                });
                            },
                            else => {
                                std.log.err("unexpected token: expected comma or rbrace but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        },
                    },
                    .invocation => |invocation| {
                        if (token != .rparen) {
                            try self.startChildState(allocator);
                            return .{ .save = token };
                        }

                        var args = invocation.arguments;
                        defer args.deinit(allocator);

                        self.finalizeExpr(.{
                            .invocation = .{
                                .function = try util.mem.createCopy(FQLExpression, allocator, &invocation.function),
                                .arguments = try args.toOwnedSlice(allocator),
                            },
                        });
                    },
                    .end => |expr| {
                        switch (token) {
                            .dot, .question_dot => {
                                self.state = .{ .field_access = .{ .value = expr, .optional = token == .question_dot } };
                                return .{};
                            },
                            .lbracket => {
                                self.state = .{ .field_access = .{ .value = expr } };
                                try self.startChildState(allocator);
                                return .{};
                            },
                            .lparen => {
                                self.state = .{ .invocation = .{ .function = expr } };
                                return .{};
                            },
                            .lbrace => {
                                self.state = .{ .projection = .{ .expression = expr } };
                                return .{};
                            },
                            .bang => {
                                self.state = .{
                                    .end = .{
                                        .non_null_assertion = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                    },
                                };
                                return .{};
                            },
                            else => {
                                if (BinaryOperation.Operator.fromToken(token)) |operator| {
                                    self.state = .{ .binary_operation = .{ .lhs = expr, .operator = operator } };
                                    try self.startChildState(allocator);
                                    return .{};
                                }

                                if (token == .equal_rarrow) {
                                    // could this actually be a function? :suprised_pikachu:

                                    switch (expr) {
                                        .isolated => |isolated| {
                                            if (isolated.* == .identifier) {
                                                defer allocator.destroy(isolated);

                                                self.state = .{ .long_function = try State.LongFunction.fromExprs(allocator, &.{isolated.*}, false) };
                                                try self.startChildState(allocator);
                                                return .{};
                                            }
                                        },
                                        .array_literal => |array_literal| {
                                            if (array_literal.elements == null or util.slice.every(array_literal.elements.?, isIdentifier)) {
                                                const elems = array_literal.elements orelse try allocator.alloc(FQLExpression, 0);
                                                defer allocator.free(elems);

                                                self.state = .{ .long_function = try State.LongFunction.fromExprs(allocator, elems, false) };
                                                try self.startChildState(allocator);
                                                return .{};
                                            }
                                        },
                                        else => {},
                                    }

                                    std.log.err("unexpected token: equal_rarrow may only come after a list of identifiers", .{});
                                    return error.UnexpectedToken;
                                }
                            },
                        }

                        if (self.parent) |parent| {
                            defer allocator.destroy(parent);
                            defer self.* = parent.*;

                            switch (parent.state) {
                                .unary_operation => |operator| {
                                    parent.finalizeExpr(.{
                                        .unary_operation = .{
                                            .operator = operator,
                                            .operand = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        },
                                    });
                                },
                                .binary_operation => |binary_operation| {
                                    if (expr == .binary_operation and expr.binary_operation.operator.precedence() <= binary_operation.operator.precedence()) {
                                        parent.finalizeExpr(.{
                                            .binary_operation = .{
                                                .lhs = try util.mem.createCopy(FQLExpression, allocator, &FQLExpression{
                                                    .binary_operation = .{
                                                        .lhs = try util.mem.createCopy(FQLExpression, allocator, &binary_operation.lhs),
                                                        .operator = binary_operation.operator,
                                                        .rhs = expr.binary_operation.lhs,
                                                    },
                                                }),
                                                .operator = expr.binary_operation.operator,
                                                .rhs = expr.binary_operation.rhs,
                                            },
                                        });
                                    } else {
                                        parent.finalizeExpr(.{
                                            .binary_operation = .{
                                                .operator = binary_operation.operator,
                                                .lhs = try util.mem.createCopy(FQLExpression, allocator, &binary_operation.lhs),
                                                .rhs = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            },
                                        });
                                    }
                                },
                                .array_literal => |*array_literal| {
                                    try array_literal.elements.append(allocator, expr);
                                },
                                .invocation => |*invocation| switch (token) {
                                    .eol => {},
                                    .comma => {
                                        try invocation.arguments.append(allocator, expr);
                                        return .{};
                                    },
                                    .rparen => {
                                        var args = invocation.arguments;
                                        defer args.deinit(allocator);

                                        try args.append(allocator, expr);

                                        parent.finalizeExpr(.{
                                            .invocation = .{
                                                .function = try util.mem.createCopy(FQLExpression, allocator, &invocation.function),
                                                .arguments = try args.toOwnedSlice(allocator),
                                            },
                                        });

                                        return .{};
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected rparen but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                },
                                .variable_declaration => |variable_declaration| {
                                    if (variable_declaration.name == null) {
                                        std.debug.panic("invalid parser parent state: variable_declaration: name is null", .{});
                                    }

                                    if (variable_declaration.type) |type_state| {
                                        if (type_state != .type) {
                                            std.debug.panic("invalid parser parent state: variable_declaration: type is {s}", .{@tagName(type_state)});
                                        }

                                        parent.finalizeExpr(.{
                                            .variable_declaration = .{
                                                .name = variable_declaration.name.?,
                                                .type = type_state.type,
                                                .value = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            },
                                        });
                                    } else {
                                        parent.finalizeExpr(.{
                                            .variable_declaration = .{
                                                .name = variable_declaration.name.?,
                                                .value = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            },
                                        });
                                    }
                                },
                                .field_access => |field_access| {
                                    if (token != .rbracket) {
                                        std.log.err("unexpected token: expected rbracket but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    }

                                    parent.finalizeExpr(.{
                                        .field_access = .{
                                            .value = try util.mem.createCopy(FQLExpression, allocator, &field_access.value),
                                            .field = .{ .expression = try util.mem.createCopy(FQLExpression, allocator, &expr) },
                                            .optional = field_access.optional,
                                        },
                                    });

                                    return .{};
                                },
                                .object_literal => |*object_literal| {
                                    switch (object_literal.state) {
                                        .after_key => |key| {
                                            try object_literal.fields.append(allocator, .{
                                                .key = key,
                                                .value = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            });
                                            object_literal.state = .end;
                                        },
                                        else => std.debug.panic("invalid parser parent state: object_literal: {s}", .{@tagName(object_literal.state)}),
                                    }
                                },
                                .conditional => |conditional| switch (conditional) {
                                    .after_lparen => {
                                        parent.state.conditional = .{ .after_condition = expr };
                                    },
                                    .after_rparen => |condition| {
                                        parent.state.conditional = .{
                                            .after_body = .{
                                                .condition = try util.mem.createCopy(FQLExpression, allocator, &condition),
                                                .body = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            },
                                        };
                                    },
                                    .after_body => |after_body| {
                                        parent.finalizeExpr(.{
                                            .conditional = .{
                                                .condition = after_body.condition,
                                                .body = after_body.body,
                                                .@"else" = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            },
                                        });
                                    },
                                    else => std.debug.panic("invalid parser parent state: conditional statement: {s}", .{@tagName(conditional)}),
                                },
                                .block_scope => |*block_scope| {
                                    try block_scope.append(allocator, expr);

                                    switch (token) {
                                        .rbrace => {},
                                        else => {
                                            try parent.startChildState(allocator);
                                        },
                                    }
                                },
                                .long_function => |long_function| {
                                    if (long_function.variadic_state != null and long_function.variadic_state.? != .after_rparen) {
                                        std.debug.panic("invalid parser parent state: long function: {s}", .{@tagName(long_function.variadic_state.?)});
                                    }

                                    parent.finalizeExpr(.{
                                        .function = .{
                                            .parameters = .{
                                                .long = blk: {
                                                    if (long_function.variadic_state == null) {
                                                        break :blk .{
                                                            .parameters = long_function.parameters,
                                                            .variadic = false,
                                                        };
                                                    }

                                                    break :blk .{
                                                        .parameters = blk2: {
                                                            const params = try allocator.realloc(@constCast(long_function.parameters), long_function.parameters.len + 1);
                                                            params[params.len - 1] = long_function.variadic_state.?.after_rparen;
                                                            break :blk2 params;
                                                        },
                                                        .variadic = true,
                                                    };
                                                },
                                            },
                                            .body = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        },
                                    });
                                },
                                .short_function => |param| {
                                    parent.finalizeExpr(.{
                                        .function = .{
                                            .parameters = .{
                                                .short = param,
                                            },
                                            .body = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        },
                                    });
                                },
                                .projection => |*projection| {
                                    if (projection.state != .after_identifier) {
                                        std.debug.panic("invalid parser parent state: projection: {s}", .{@tagName(projection.state)});
                                    }

                                    try projection.fields.append(allocator, .{
                                        .long = .{
                                            .key = projection.state.after_identifier,
                                            .value = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        },
                                    });

                                    projection.state = .end;
                                },
                                else => std.debug.panic("invalid parser parent state: {s}", .{@tagName(parent.state)}),
                            }

                            // ensure token is not consumed at this point
                            return .{ .save = token };
                        }

                        self.* = .{};

                        return .{ .save = token, .expr = expr };
                    },
                    else => {
                        if (self.state == .long_function and self.state.long_function.variadic_state != null) {
                            switch (self.state.long_function.variadic_state.?) {
                                .start => {
                                    switch (token) {
                                        .word => |word| {
                                            self.state.long_function.variadic_state = .{ .after_param = try allocator.dupe(u8, word) };
                                            return .{};
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                },
                                .after_param => |param| {
                                    switch (token) {
                                        .rparen => {
                                            self.state.long_function.variadic_state = .{ .after_rparen = param };
                                            return .{};
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                },
                                .after_rparen => {
                                    switch (token) {
                                        .equal_rarrow => {
                                            try self.startChildState(allocator);
                                            return .{};
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                },
                            }
                        }

                        std.debug.panic("invalid parser state: {s}", .{@tagName(self.state)});
                    },
                }

                return .{};
            }
        };

        allocator: std.mem.Allocator,
        inner: Unmanaged = .{},

        pub fn init(allocator: std.mem.Allocator) Parser {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: Parser) void {
            self.inner.deinit(self.allocator);
        }

        pub fn reset(self: *Parser) void {
            self.deinit();
            self.inner = .{};
        }

        pub const PushResult = struct {
            save: ?tokenizer.Token = null,
            expr: ?FQLExpression = null,
        };

        pub fn push(self: *Parser, token: tokenizer.Token) !PushResult {
            return try self.inner.pushToken(self.allocator, token);
        }
    };

    pub fn parse(allocator: std.mem.Allocator, it: *tokenizer.TokenIterator) !FQLExpression {
        var parser = Parser.init(allocator);
        defer parser.deinit();

        while (true) {
            const token = try it.nextToken(allocator);
            defer token.deinit(allocator);

            const result = try parser.push(token);
            if (result.save) |save| {
                // std.debug.print("saving token: {any}\n", .{token});
                it.saveToken(try save.dupe(allocator));
            }

            if (result.expr) |expr| {
                return expr;
            }
        }
    }
};

pub fn parseExpression(allocator: std.mem.Allocator, reader: std.io.AnyReader) !FQLExpression {
    var it = tokenizer.TokenIterator.init(reader);
    defer it.deinit(allocator);

    return FQLExpression.parse(allocator, &it);
}

fn expectParsedExprEqual(str: []const u8, expected: FQLExpression) !void {
    var stream = std.io.fixedBufferStream(str);
    var actual = try parseExpression(testing.allocator, stream.reader().any());
    defer actual.deinit(testing.allocator);

    // std.debug.print("actual: {any}\n", .{actual});

    try testing.expectEqualDeep(expected, actual);

    const canonical_string = try actual.toCanonicalString(testing.allocator);
    defer testing.allocator.free(canonical_string);

    try testing.expectEqualStrings(str, canonical_string);
}

test parseExpression {
    try expectParsedExprEqual(
        \\let myArray: Array<Any> = [
        \\    "Hello",
        \\    45,
        \\    true,
        \\]
    , .{
        .variable_declaration = .{
            .name = "myArray",
            .type = .{
                .template = .{
                    .name = "Array",
                    .parameters = &[_]FQLType{
                        .{ .named = "Any" },
                    },
                },
            },
            .value = &FQLExpression{
                .array_literal = .{
                    .elements = &[_]FQLExpression{
                        .{ .string_literal = "\"Hello\"" },
                        .{ .number_literal = "45" },
                        .{ .boolean_literal = true },
                    },
                },
            },
        },
    });

    try expectParsedExprEqual("(1 + 3) * 2", .{
        .binary_operation = .{
            .lhs = &FQLExpression{
                .isolated = &FQLExpression{
                    .binary_operation = .{
                        .lhs = &FQLExpression{ .number_literal = "1" },
                        .operator = .add,
                        .rhs = &FQLExpression{ .number_literal = "3" },
                    },
                },
            },
            .operator = .multiply,
            .rhs = &FQLExpression{ .number_literal = "2" },
        },
    });

    try expectParsedExprEqual(
        \\"Hello " + [
        \\    "world",
        \\    "!",
        \\]
    , .{
        .binary_operation = .{
            .lhs = &FQLExpression{ .string_literal = "\"Hello \"" },
            .operator = .add,
            .rhs = &FQLExpression{
                .array_literal = .{
                    .elements = &[_]FQLExpression{
                        .{ .string_literal = "\"world\"" },
                        .{ .string_literal = "\"!\"" },
                    },
                },
            },
        },
    });

    try expectParsedExprEqual("hello[\"world\"]", .{
        .field_access = .{
            .value = &FQLExpression{ .identifier = "hello" },
            .field = .{
                .expression = &FQLExpression{ .string_literal = "\"world\"" },
            },
        },
    });

    try expectParsedExprEqual("\"This is a string\"?.at(0)", .{
        .invocation = .{
            .function = &FQLExpression{
                .field_access = .{
                    .value = &FQLExpression{ .string_literal = "\"This is a string\"" },
                    .optional = true,
                    .field = .{ .identifier = "at" },
                },
            },
            .arguments = &[_]FQLExpression{
                .{ .number_literal = "0" },
            },
        },
    });

    try expectParsedExprEqual(
        \\{
        \\    myKey: "myValue",
        \\    "myOtherKey": 4,
        \\}
    , .{
        .object_literal = .{
            .fields = &[_]FQLExpression.ObjectLiteral.Field{
                .{
                    .key = .{ .identifier = "myKey" },
                    .value = &FQLExpression{ .string_literal = "\"myValue\"" },
                },
                .{
                    .key = .{ .string = "\"myOtherKey\"" },
                    .value = &FQLExpression{ .number_literal = "4" },
                },
            },
        },
    });

    try expectParsedExprEqual("house.size == \"big\" && house.color == \"blue\"", .{
        .binary_operation = .{
            .lhs = &FQLExpression{
                .binary_operation = .{
                    .lhs = &FQLExpression{
                        .field_access = .{
                            .value = &FQLExpression{ .identifier = "house" },
                            .field = .{ .identifier = "size" },
                        },
                    },
                    .operator = .equality,
                    .rhs = &FQLExpression{ .string_literal = "\"big\"" },
                },
            },
            .operator = .logical_and,
            .rhs = &FQLExpression{
                .binary_operation = .{
                    .lhs = &FQLExpression{
                        .field_access = .{
                            .value = &FQLExpression{ .identifier = "house" },
                            .field = .{ .identifier = "color" },
                        },
                    },
                    .operator = .equality,
                    .rhs = &FQLExpression{ .string_literal = "\"blue\"" },
                },
            },
        },
    });

    try expectParsedExprEqual(
        \\(x) => {
        \\    let greeting = "hello"
        \\    greeting
        \\}
    , .{
        .function = .{
            .parameters = .{
                .long = .{
                    .parameters = &.{"x"},
                },
            },
            .body = &FQLExpression{
                .block_scope = &[_]FQLExpression{
                    .{
                        .variable_declaration = .{
                            .name = "greeting",
                            .value = &FQLExpression{ .string_literal = "\"hello\"" },
                        },
                    },
                    .{
                        .identifier = "greeting",
                    },
                },
            },
        },
    });

    try expectParsedExprEqual("x => y => z => x + y + z", .{
        .function = .{
            .parameters = .{ .short = "x" },
            .body = &FQLExpression{
                .function = .{
                    .parameters = .{ .short = "y" },
                    .body = &FQLExpression{
                        .function = .{
                            .parameters = .{ .short = "z" },
                            .body = &FQLExpression{
                                .binary_operation = .{
                                    .lhs = &FQLExpression{
                                        .binary_operation = .{
                                            .lhs = &FQLExpression{ .identifier = "x" },
                                            .operator = .add,
                                            .rhs = &FQLExpression{ .identifier = "y" },
                                        },
                                    },
                                    .operator = .add,
                                    .rhs = &FQLExpression{ .identifier = "z" },
                                },
                            },
                        },
                    },
                },
            },
        },
    });

    try expectParsedExprEqual("(symbol, ...amounts) => symbol + amounts.reduce((prev, cur) => prev + cur).toString()", .{
        .function = .{
            .parameters = .{
                .long = .{
                    .parameters = &.{ "symbol", "amounts" },
                    .variadic = true,
                },
            },
            .body = &FQLExpression{
                .binary_operation = .{
                    .lhs = &FQLExpression{ .identifier = "symbol" },
                    .operator = .add,
                    .rhs = &FQLExpression{
                        .invocation = .{
                            .function = &FQLExpression{
                                .field_access = .{
                                    .value = &FQLExpression{
                                        .invocation = .{
                                            .arguments = &[_]FQLExpression{
                                                .{
                                                    .function = .{
                                                        .parameters = .{
                                                            .long = .{
                                                                .parameters = &.{ "prev", "cur" },
                                                            },
                                                        },
                                                        .body = &FQLExpression{
                                                            .binary_operation = .{
                                                                .lhs = &FQLExpression{ .identifier = "prev" },
                                                                .operator = .add,
                                                                .rhs = &FQLExpression{ .identifier = "cur" },
                                                            },
                                                        },
                                                    },
                                                },
                                            },
                                            .function = &FQLExpression{
                                                .field_access = .{
                                                    .value = &FQLExpression{ .identifier = "amounts" },
                                                    .field = .{ .identifier = "reduce" },
                                                },
                                            },
                                        },
                                    },
                                    .field = .{ .identifier = "toString" },
                                },
                            },
                            .arguments = &.{},
                        },
                    },
                },
            },
        },
    });

    try expectParsedExprEqual(
        \\Store.all()! {
        \\    name,
        \\    myCity: .address.city,
        \\}
    , .{
        .projection = .{
            .expression = &FQLExpression{
                .non_null_assertion = &FQLExpression{
                    .invocation = .{
                        .function = &FQLExpression{
                            .field_access = .{
                                .value = &FQLExpression{ .identifier = "Store" },
                                .field = .{ .identifier = "all" },
                            },
                        },
                        .arguments = &.{},
                    },
                },
            },
            .fields = &[_]FQLExpression.Projection.Field{
                .{ .short = "name" },
                .{
                    .long = .{
                        .key = "myCity",
                        .value = &FQLExpression{
                            .field_access = .{
                                .value = &FQLExpression{
                                    .anonymous_field_access = .{ .identifier = "address" },
                                },
                                .field = .{ .identifier = "city" },
                            },
                        },
                    },
                },
            },
        },
    });

    try expectParsedExprEqual(
        \\((x) => x)(
        \\    {
        \\        "hi"
        \\    },
        \\)
    ,
        .{
            .invocation = .{
                .function = &FQLExpression{
                    .isolated = &FQLExpression{
                        .function = .{
                            .parameters = .{
                                .long = .{
                                    .parameters = &.{"x"},
                                },
                            },
                            .body = &FQLExpression{ .identifier = "x" },
                        },
                    },
                },
                .arguments = &[_]FQLExpression{
                    .{
                        .block_scope = &[_]FQLExpression{
                            .{ .string_literal = "\"hi\"" },
                        },
                    },
                },
            },
        },
    );
}

pub const QueryTree = struct {
    allocator: std.mem.Allocator,

    expressions: ?[]FQLExpression = null,

    pub fn deinit(self: @This()) void {
        if (self.expressions) |expressions| {
            for (expressions) |expr| {
                expr.deinit(self.allocator);
            }

            self.allocator.free(expressions);
        }
    }

    pub fn parse(allocator: std.mem.Allocator, reader: std.io.AnyReader) !QueryTree {
        var it = tokenizer.TokenIterator.init(reader);
        defer it.deinit(allocator);

        var exprs = std.ArrayList(FQLExpression).init(allocator);
        while (try it.next(allocator)) |token| {
            it.saveToken(token);

            try exprs.append(try FQLExpression.parse(allocator, &it));
        }

        return .{
            .allocator = allocator,
            .expressions = try exprs.toOwnedSlice(),
        };
    }

    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) !void {
        if (self.expressions) |exprs| {
            for (exprs) |expr| {
                try expr.printCanonical(writer, "    ", 0);
                try writer.writeByte('\n');
            }
        }
    }
};

pub const SchemaDefinition = union(enum) {
    pub const AccessProvider = struct {
        pub const Member = union(enum) {
            pub const Role = struct {
                name: []const u8,
                predicate: ?FQLExpression = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    allocator.free(self.name);
                    if (self.predicate) |predicate| {
                        predicate.deinit(allocator);
                    }
                }
            };

            issuer: []const u8,
            jwks_uri: []const u8,
            role: Member.Role,
            ttl: []const u8,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    inline .issuer,
                    .jwks_uri,
                    => |s| allocator.free(s),
                    .role => |r| r.deinit(allocator),
                    .ttl => {},
                }
            }
        };

        name: []const u8,
        members: ?[]const Member = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.members) |members| {
                for (members) |member| {
                    member.deinit(allocator);
                }

                allocator.free(members);
            }

            allocator.free(self.name);
        }
    };

    pub const Collection = struct {
        pub const Member = union(enum) {
            pub const Field = struct {
                name: []const u8,
                type: FQLType,
                default: ?FQLExpression = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.default) |default| {
                        default.deinit(allocator);
                    }

                    self.type.deinit(allocator);
                    allocator.free(self.name);
                }
            };

            pub const Index = struct {
                pub const Member = union(enum) {
                    terms: []const FQLExpression,
                    values: []const FQLExpression,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self) {
                            inline .terms,
                            .values,
                            => |fields| {
                                for (fields) |field| {
                                    field.deinit(allocator);
                                }

                                allocator.free(fields);
                            },
                        }
                    }
                };

                name: []const u8,
                members: ?[]const @This().Member = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.members) |members| {
                        for (members) |member| {
                            member.deinit(allocator);
                        }

                        allocator.free(members);
                    }

                    allocator.free(self.name);
                }
            };

            pub const Migration = union(enum) {
                pub const Backfill = struct {
                    name: FQLExpression,
                    value: FQLExpression,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.name.deinit(allocator);
                        self.value.deinit(allocator);
                    }
                };

                pub const Move = struct {
                    old_name: FQLExpression,
                    new_name: FQLExpression,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.old_name.deinit(allocator);
                        self.new_name.deinit(allocator);
                    }
                };

                pub const Split = struct {
                    old_name: FQLExpression,
                    new_names: []const FQLExpression,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.new_names) |new_name| {
                            new_name.deinit(allocator);
                        }

                        allocator.free(self.new_names);
                        self.old_name.deinit(allocator);
                    }
                };

                add: FQLExpression,
                backfill: Backfill,
                drop: FQLExpression,
                move: Move,
                move_conflicts: FQLExpression,
                move_wildcard: FQLExpression,
                split: Split,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self) {
                        inline else => |e| e.deinit(allocator),
                    }
                }
            };

            pub const UniqueConstraint = struct {
                terms: ?[]const FQLExpression = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.terms) |terms| {
                        for (terms) |term| {
                            term.deinit(allocator);
                        }

                        allocator.free(terms);
                    }
                }
            };

            pub const CheckConstraint = struct {
                name: []const u8,
                predicate: FQLExpression,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    self.predicate.deinit(allocator);
                    allocator.free(self.name);
                }
            };

            pub const ComputedField = struct {
                name: []const u8,
                type: ?FQLType,
                function: FQLExpression,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.type) |t| {
                        t.deinit(allocator);
                    }

                    self.function.deinit(allocator);
                    allocator.free(self.name);
                }
            };

            field: Field,
            migrations: []const Migration,
            history_days: []const u8,
            document_ttls: ?bool,
            ttl_days: []const u8,
            index: Index,
            unique_constraint: UniqueConstraint,
            check_constraint: CheckConstraint,
            computed_field: ComputedField,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    inline .field,
                    .index,
                    .unique_constraint,
                    .check_constraint,
                    .computed_field,
                    => |v| v.deinit(allocator),
                    .migrations => |migrations| {
                        for (migrations) |migration| {
                            migration.deinit(allocator);
                        }

                        allocator.free(migrations);
                    },
                    inline .history_days, .ttl_days => |s| allocator.free(s),
                    else => {},
                }
            }
        };

        alias: ?FQLExpression = null,

        name: []const u8,
        members: ?[]const Member = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.alias) |alias| {
                alias.deinit(allocator);
            }

            if (self.members) |members| {
                for (members) |member| {
                    member.deinit(allocator);
                }

                allocator.free(members);
            }

            allocator.free(self.name);
        }
    };

    pub const Role = struct {
        pub const Member = union(enum) {
            pub const Membership = struct {
                collection: []const u8,
                predicate: ?FQLExpression = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.predicate) |predicate| {
                        predicate.deinit(allocator);
                    }

                    allocator.free(self.collection);
                }
            };

            pub const Privileges = struct {
                pub const Action = struct {
                    pub const Action = enum {
                        call,
                        create,
                        create_with_id,
                        delete,
                        read,
                        write,
                        history_read,
                    };

                    action: @This().Action,
                    predicate: ?FQLExpression = null,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.predicate) |predicate| {
                            predicate.deinit(allocator);
                        }
                    }
                };

                resource: []const u8,
                actions: ?[]const Action = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.actions) |actions| {
                        for (actions) |action| {
                            action.deinit(allocator);
                        }

                        allocator.free(actions);
                    }

                    allocator.free(self.resource);
                }
            };

            membership: Membership,
            privileges: Privileges,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    inline else => |d| d.deinit(allocator),
                }
            }
        };

        name: []const u8,
        members: ?[]const Member = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.members) |members| {
                for (members) |member| {
                    member.deinit(allocator);
                }

                allocator.free(members);
            }

            allocator.free(self.name);
        }
    };

    pub const Function = struct {
        pub const Parameter = struct {
            name: []const u8,
            type: ?FQLType = null,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                if (self.type) |t| {
                    t.deinit(allocator);
                }

                allocator.free(self.name);
            }
        };

        role: ?FQLExpression = null,
        alias: ?FQLExpression = null,

        name: []const u8,
        parameters: ?[]const Parameter = null,
        return_type: ?FQLType = null,
        body: ?[]const FQLExpression = null,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.role) |expr| {
                expr.deinit(allocator);
            }

            if (self.alias) |expr| {
                expr.deinit(allocator);
            }

            if (self.return_type) |t| {
                t.deinit(allocator);
            }

            if (self.parameters) |parameters| {
                for (parameters) |parameter| {
                    parameter.deinit(allocator);
                }

                allocator.free(parameters);
            }

            if (self.body) |exprs| {
                for (exprs) |expr| {
                    expr.deinit(allocator);
                }

                allocator.free(exprs);
            }

            allocator.free(self.name);
        }
    };

    access_provider: AccessProvider,
    collection: Collection,
    role: Role,
    function: Function,

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |d| d.deinit(allocator),
        }
    }

    fn AnnotationEnum(comptime tag: std.meta.Tag(SchemaDefinition)) type {
        // TODO: uncomment this after https://github.com/ziglang/zig/issues/19985 is fixed
        // const fields = std.meta.TagPayload(SchemaDefinition, tag).fields;
        // var enum_fields: [fields.len]std.builtin.Type.EnumField = undefined;
        // var enum_field_count = 0;
        // for (fields) |field| {
        //     if (field.type == ?FQLExpression) {
        //         enum_fields[enum_field_count] = .{ .name = field.name, .value = enum_field_count };
        //         enum_field_count += 1;
        //     }
        // }
        //
        // if (enum_field_count == 0) {
        //     return @Type(.{
        //         .Enum = .{
        //             .tag_type = u0,
        //             .fields = &.{},
        //             .decls = &.{},
        //             .is_exhaustive = true,
        //         },
        //     });
        // }
        //
        // return @Type(.{
        //     .Enum = .{
        //         .tag_type = std.math.IntFittingRange(0, enum_field_count - 1),
        //         .fields = enum_fields[0..enum_field_count],
        //         .decls = &.{},
        //         .is_exhaustive = true,
        //     },
        // });

        return switch (tag) {
            .access_provider => enum {},
            .collection => enum { alias },
            .role => enum {},
            .function => enum { role, alias },
        };
    }

    pub const Parser = struct {
        pub const Unmanaged = struct {
            pub const State = union(enum) {
                pub const Annotation = struct {
                    name: []const u8,
                    expr_state: union(enum) {
                        start,
                        parsing: FQLExpression.Parser.Unmanaged,
                        end: FQLExpression,
                    } = .start,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self.expr_state) {
                            .start => {},
                            inline else => |v| v.deinit(allocator),
                        }

                        allocator.free(self.name);
                    }
                };

                pub const AccessProvider = union(enum) {
                    start,
                    before_name,
                    after_name: []const u8,
                    body: struct {
                        name: []const u8,
                        members: std.ArrayListUnmanaged(SchemaDefinition.AccessProvider.Member) = .{},
                        state: union(enum) {
                            empty,
                            issuer,
                            jwks_uri,
                            ttl,
                            role: union(enum) {
                                start,
                                name: []const u8,
                                block: []const u8,
                                predicate: struct {
                                    name: []const u8,
                                    expr: FQLExpression.Parser.Unmanaged = .{},
                                },
                                end: struct {
                                    name: []const u8,
                                    expr: FQLExpression,
                                },
                            },
                            end,
                        } = .empty,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            switch (self.state) {
                                .role => |role| {
                                    switch (role) {
                                        .start => {},
                                        inline .name, .block => |s| allocator.free(s),
                                        inline .predicate, .end => |v| {
                                            allocator.free(v.name);
                                            v.expr.deinit(allocator);
                                        },
                                    }
                                },
                                else => {},
                            }

                            var members_mutable_copy = self.members;
                            members_mutable_copy.deinit(allocator);
                            allocator.free(self.name);
                        }
                    },

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self) {
                            .before_name, .start => {},
                            .after_name => |name| allocator.free(name),
                            inline else => |v| v.deinit(allocator),
                        }
                    }
                };

                pub const Collection = struct {
                    const Member = union(enum) {
                        empty,
                        field: struct {
                            name: []const u8,
                            type: ?FQLType = null,
                            parser: ?union(enum) {
                                type: FQLType.Parser.Unmanaged,
                                expr: FQLExpression.Parser.Unmanaged,
                            } = null,
                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                if (self.type) |fql_type| {
                                    fql_type.deinit(allocator);
                                }

                                if (self.parser) |parser| {
                                    switch (parser) {
                                        inline else => |p| p.deinit(allocator),
                                    }
                                }

                                allocator.free(self.name);
                            }
                        },
                        migrations: struct {
                            statements: std.ArrayListUnmanaged(SchemaDefinition.Collection.Member.Migration) = .{},
                            state: union(enum) {
                                before_lbrace,
                                start,

                                first_expr: struct {
                                    tag: std.meta.Tag(SchemaDefinition.Collection.Member.Migration),
                                    parser: FQLExpression.Parser.Unmanaged = .{},
                                },

                                before_arrow: struct {
                                    tag: enum { move, backfill, split },
                                    first_expr: FQLExpression,
                                },

                                move: struct {
                                    first_expr: FQLExpression,
                                    parser: FQLExpression.Parser.Unmanaged = .{},
                                },

                                backfill: struct {
                                    first_expr: FQLExpression,
                                    parser: FQLExpression.Parser.Unmanaged = .{},
                                },

                                split: struct {
                                    first_expr: FQLExpression,
                                    after: std.ArrayListUnmanaged(FQLExpression) = .{},
                                    parser: ?FQLExpression.Parser.Unmanaged = .{},
                                },

                                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                    switch (self) {
                                        .before_lbrace, .start => {},
                                        .first_expr => |first_expr| first_expr.parser.deinit(allocator),
                                        .before_arrow => |before_arrow| before_arrow.first_expr.deinit(allocator),
                                        inline .move, .backfill => |state| {
                                            state.first_expr.deinit(allocator);
                                            state.parser.deinit(allocator);
                                        },
                                        .split => |split| {
                                            if (split.parser) |parser| {
                                                parser.deinit(allocator);
                                            }

                                            for (split.after.items) |expr| {
                                                expr.deinit(allocator);
                                            }

                                            var after_mutable_copy = split.after;
                                            after_mutable_copy.deinit(allocator);
                                            split.first_expr.deinit(allocator);
                                        },
                                    }
                                }
                            } = .before_lbrace,

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                for (self.statements.items) |statement| {
                                    statement.deinit(allocator);
                                }

                                var statements_mutable_copy = self.statements;
                                statements_mutable_copy.deinit(allocator);
                                self.state.deinit(allocator);
                            }
                        },
                        history_days,
                        document_ttls,
                        ttl_days,
                        index: struct {
                            name: ?[]const u8 = null,
                            members: std.ArrayListUnmanaged(SchemaDefinition.Collection.Member.Index.Member) = .{},
                            state: union(enum) {
                                before_lbrace,
                                start,
                                property: struct {
                                    type: enum { terms, values },
                                    fields: std.ArrayListUnmanaged(FQLExpression) = .{},
                                    state: union(enum) {
                                        before_lbracket,
                                        start,
                                        parsing: FQLExpression.Parser.Unmanaged,
                                        end,
                                    } = .before_lbracket,

                                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                        switch (self.state) {
                                            .parsing => |parser| parser.deinit(allocator),
                                            else => {},
                                        }

                                        for (self.fields.items) |expr| {
                                            expr.deinit(allocator);
                                        }

                                        var fields_mutable_copy = self.fields;
                                        fields_mutable_copy.deinit(allocator);
                                    }
                                },

                                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                    switch (self) {
                                        .property => |property| property.deinit(allocator),
                                        else => {},
                                    }
                                }
                            } = .before_lbrace,

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                if (self.name) |name| {
                                    allocator.free(name);
                                }

                                for (self.members.items) |member| {
                                    member.deinit(allocator);
                                }

                                var members_mutable_copy = self.members;
                                members_mutable_copy.deinit(allocator);
                                self.state.deinit(allocator);
                            }
                        },
                        unique: struct {
                            terms: std.ArrayListUnmanaged(FQLExpression) = .{},
                            state: union(enum) {
                                before_lbracket,
                                start,
                                parsing: FQLExpression.Parser.Unmanaged,
                                end,
                            } = .before_lbracket,

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                switch (self.state) {
                                    .parsing => |parser| parser.deinit(allocator),
                                    else => {},
                                }

                                for (self.terms.items) |expr| {
                                    expr.deinit(allocator);
                                }

                                var terms_mutable_copy = self.terms;
                                terms_mutable_copy.deinit(allocator);
                            }
                        },
                        check: struct {
                            name: ?[]const u8 = null,
                            parser: FQLExpression.Parser.Unmanaged = .{},

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                if (self.name) |name| {
                                    allocator.free(name);
                                }

                                self.parser.deinit(allocator);
                            }
                        },
                        compute: struct {
                            name: ?[]const u8 = null,
                            type: ?FQLType = null,
                            parser: ?union(enum) {
                                type: FQLType.Parser.Unmanaged,
                                expr: FQLExpression.Parser.Unmanaged,
                            } = null,

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                if (self.name) |name| {
                                    allocator.free(name);
                                }

                                if (self.type) |fql_type| {
                                    fql_type.deinit(allocator);
                                }

                                if (self.parser) |parser| {
                                    switch (parser) {
                                        inline else => |p| p.deinit(allocator),
                                    }
                                }
                            }
                        },

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            switch (self) {
                                .empty, .history_days, .document_ttls, .ttl_days => {},
                                inline else => |state| state.deinit(allocator),
                            }
                        }
                    };

                    name: ?[]const u8 = null,
                    members: std.ArrayListUnmanaged(SchemaDefinition.Collection.Member) = .{},
                    member_state: ?Member = null,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.name) |name| {
                            allocator.free(name);
                        }

                        if (self.member_state) |state| {
                            state.deinit(allocator);
                        }

                        for (self.members.items) |member| {
                            member.deinit(allocator);
                        }

                        var members_mutable_copy = self.members;
                        members_mutable_copy.deinit(allocator);
                    }
                };

                pub const Role = struct {
                    name: ?[]const u8 = null,
                    members: std.ArrayListUnmanaged(SchemaDefinition.Role.Member) = .{},
                    member_state: ?union(enum) {
                        empty,
                        membership,
                        membership_collection: []const u8,
                        membership_block: struct {
                            collection: []const u8,
                            state: union(enum) {
                                start,
                                predicate: FQLExpression.Parser.Unmanaged,
                                end: FQLExpression,
                            } = .start,

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                switch (self.state) {
                                    .start => {},
                                    inline .predicate, .end => |s| s.deinit(allocator),
                                }

                                allocator.free(self.collection);
                            }
                        },
                        privileges,
                        privileges_resource: []const u8,
                        privileges_block: struct {
                            resource: []const u8,
                            actions: std.ArrayListUnmanaged(SchemaDefinition.Role.Member.Privileges.Action) = .{},
                            state: union(enum) {
                                start,
                                action: SchemaDefinition.Role.Member.Privileges.Action.Action,
                                action_block: SchemaDefinition.Role.Member.Privileges.Action.Action,
                                action_predicate: struct {
                                    action: SchemaDefinition.Role.Member.Privileges.Action.Action,
                                    expr_parser: FQLExpression.Parser.Unmanaged = .{},
                                },
                                action_end: SchemaDefinition.Role.Member.Privileges.Action,

                                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                    switch (self) {
                                        .start, .action, .action_block => {},
                                        .action_predicate => |action_predicate| action_predicate.expr_parser.deinit(allocator),
                                        .action_end => |action| action.deinit(allocator),
                                    }
                                }
                            } = .start,

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                for (self.actions.items) |action| {
                                    action.deinit(allocator);
                                }

                                var actions_mutable_copy = self.actions;
                                actions_mutable_copy.deinit(allocator);
                                self.state.deinit(allocator);
                                allocator.free(self.resource);
                            }
                        },

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            switch (self) {
                                .empty, .membership, .privileges => {},
                                inline .membership_collection, .privileges_resource => |s| allocator.free(s),
                                inline .membership_block, .privileges_block => |s| s.deinit(allocator),
                            }
                        }
                    } = null,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.name) |name| {
                            allocator.free(name);
                        }

                        if (self.member_state) |state| {
                            state.deinit(allocator);
                        }

                        for (self.members.items) |member| {
                            member.deinit(allocator);
                        }

                        var members_mutable_copy = self.members;
                        members_mutable_copy.deinit(allocator);
                    }
                };

                pub const Function = union(enum) {
                    start,
                    after_name: []const u8,
                    params: struct {
                        name: []const u8,
                        params: std.ArrayListUnmanaged(SchemaDefinition.Function.Parameter) = .{},
                        param_state: union(enum) {
                            start,
                            after_name: []const u8,
                            before_type: struct { name: []const u8, type_parser: FQLType.Parser.Unmanaged = .{} },
                            end,
                        } = .start,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            switch (self.param_state) {
                                .start, .end => {},
                                .after_name => |name| allocator.free(name),
                                .before_type => |before_type| {
                                    allocator.free(before_type.name);
                                    before_type.type_parser.deinit(allocator);
                                },
                            }

                            for (self.params.items) |param| {
                                param.deinit(allocator);
                            }

                            var params_mutable_copy = self.params;
                            params_mutable_copy.deinit(allocator);
                            allocator.free(self.name);
                        }
                    },
                    return_type: struct {
                        name: []const u8,
                        params: []const SchemaDefinition.Function.Parameter,
                        type_parser: FQLType.Parser.Unmanaged = .{},

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            for (self.params) |param| {
                                param.deinit(allocator);
                            }

                            allocator.free(self.name);
                            allocator.free(self.params);
                            self.type_parser.deinit(allocator);
                        }
                    },
                    before_body: struct {
                        name: []const u8,
                        params: []const SchemaDefinition.Function.Parameter,
                        return_type: ?FQLType = null,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            if (self.return_type) |return_type| {
                                return_type.deinit(allocator);
                            }

                            for (self.params) |param| {
                                param.deinit(allocator);
                            }

                            allocator.free(self.name);
                            allocator.free(self.params);
                        }
                    },
                    body: struct {
                        name: []const u8,
                        params: []const SchemaDefinition.Function.Parameter,
                        return_type: ?FQLType,
                        exprs: std.ArrayListUnmanaged(FQLExpression) = .{},
                        expr_parser: FQLExpression.Parser.Unmanaged = .{},

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            if (self.return_type) |return_type| {
                                return_type.deinit(allocator);
                            }

                            for (self.params) |param| {
                                param.deinit(allocator);
                            }

                            for (self.exprs.items) |expr| {
                                expr.deinit(allocator);
                            }

                            var exprs_mutable_copy = self.exprs;
                            exprs_mutable_copy.deinit(allocator);
                            self.expr_parser.deinit(allocator);
                            allocator.free(self.name);
                            allocator.free(self.params);
                        }
                    },

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self) {
                            .start => {},
                            .after_name => |name| allocator.free(name),
                            inline else => |state| state.deinit(allocator),
                        }
                    }
                };

                empty,
                annotation: State.Annotation,
                access_provider: State.AccessProvider,
                collection: State.Collection,
                role: State.Role,
                function: State.Function,
                end: SchemaDefinition,
            };

            annotations: std.ArrayListUnmanaged(struct { name: []const u8, value: FQLExpression }) = .{},
            state: State = .empty,

            pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                for (self.annotations.items) |annotation| {
                    annotation.value.deinit(allocator);
                    allocator.free(annotation.name);
                }

                var annotations_mutable_copy = self.annotations;
                annotations_mutable_copy.deinit(allocator);

                switch (self.state) {
                    .empty => {},
                    inline else => |s| s.deinit(allocator),
                }
            }

            pub fn pushToken(self: *Unmanaged, allocator: std.mem.Allocator, token: tokenizer.Token) !PushResult {
                // std.debug.print("schema parser state: {s}\n", .{@tagName(self.state)});
                // std.debug.print("got token: {any}\n", .{token});

                if (token == .comment_line or token == .comment_block) {
                    return .{};
                }

                switch (self.state) {
                    .empty => {
                        switch (token) {
                            .eol => {},
                            .annotation => |annotation| {
                                self.state = .{ .annotation = .{ .name = try allocator.dupe(u8, annotation) } };
                            },
                            .word => |word| {
                                if (std.meta.stringToEnum(enum { access, collection, role, function }, word)) |keyword| {
                                    switch (keyword) {
                                        .access => {
                                            self.state = .{ .access_provider = .start };
                                        },
                                        .collection => {
                                            self.state = .{ .collection = .{} };
                                        },
                                        .role => {
                                            self.state = .{ .role = .{} };
                                        },
                                        .function => {
                                            self.state = .{ .function = .start };
                                        },
                                    }
                                } else {
                                    std.log.err("unexpected token: expected word to equal \"access\", \"collection\", \"role\" or \"function\" but got \"{s}\"", .{word});
                                    return error.UnexpectedToken;
                                }
                            },
                            else => {
                                std.log.err("unexpected token: expected annotation or word but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
                    },
                    .annotation => |*annotation| {
                        switch (annotation.expr_state) {
                            .start => {
                                switch (token) {
                                    .lparen => {
                                        annotation.expr_state = .{ .parsing = .{} };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected lparen but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .parsing => |*parser| {
                                const res = try parser.pushToken(allocator, token);
                                if (res.expr) |expr| {
                                    annotation.expr_state = .{ .end = expr };
                                }

                                return .{ .save = res.save };
                            },
                            .end => |expr| {
                                switch (token) {
                                    .rparen => {
                                        try self.annotations.append(allocator, .{
                                            .name = annotation.name,
                                            .value = expr,
                                        });

                                        self.state = .empty;
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected rparen but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                        }
                    },
                    .access_provider => |*access_provider| {
                        switch (access_provider.*) {
                            .start => {
                                switch (token) {
                                    .word => |word| {
                                        if (!std.mem.eql(u8, word, "provider")) {
                                            std.log.err("unexpected token: expected word to equal \"provider\" but got \"{s}\"", .{word});
                                            return error.UnexpectedToken;
                                        }

                                        access_provider.* = .before_name;
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .before_name => {
                                switch (token) {
                                    inline .string, .word => |s| {
                                        access_provider.* = .{ .after_name = try allocator.dupe(u8, s) };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected string or word but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .after_name => |name| {
                                switch (token) {
                                    .lbrace => {
                                        access_provider.* = .{ .body = .{ .name = name } };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .body => |*body| {
                                switch (body.state) {
                                    .empty => {
                                        switch (token) {
                                            .semi, .eol => {},
                                            .word => |word| {
                                                if (std.meta.stringToEnum(enum { issuer, jwks_uri, role, ttl }, word)) |keyword| {
                                                    switch (keyword) {
                                                        .issuer => {
                                                            body.state = .issuer;
                                                        },
                                                        .jwks_uri => {
                                                            body.state = .jwks_uri;
                                                        },
                                                        .role => {
                                                            body.state = .{ .role = .start };
                                                        },
                                                        .ttl => {
                                                            body.state = .ttl;
                                                        },
                                                    }
                                                } else {
                                                    std.log.err("unexpected token: expected word to equal \"issuer\", \"jwks_uri\", \"role\" or \"ttl\" but got \"{s}\"", .{word});
                                                    return error.UnexpectedToken;
                                                }
                                            },
                                            .rbrace => {
                                                body.state = .end;
                                                return .{ .save = token };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected semi, eol, word or rbrace but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    inline .issuer, .jwks_uri, .ttl => |_, tag| {
                                        switch (token) {
                                            .string => |str| {
                                                try body.members.append(
                                                    allocator,
                                                    @unionInit(SchemaDefinition.AccessProvider.Member, @tagName(tag), try allocator.dupe(u8, str)),
                                                );

                                                body.state = .empty;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected string but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .role => |*role| {
                                        switch (role.*) {
                                            .start => {
                                                switch (token) {
                                                    inline .word, .string => |s| {
                                                        role.* = .{ .name = try allocator.dupe(u8, s) };
                                                    },
                                                    else => {
                                                        std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                                        return error.UnexpectedToken;
                                                    },
                                                }
                                            },
                                            .name => |name| {
                                                switch (token) {
                                                    .lbrace => {
                                                        role.* = .{ .block = name };
                                                    },
                                                    .eol, .semi => {
                                                        try body.members.append(allocator, .{ .role = .{ .name = name } });

                                                        body.state = .empty;
                                                    },
                                                    else => {
                                                        std.log.err("unexpected token: expected lbrace, eol or semi but got {s}", .{@tagName(token)});
                                                        return error.UnexpectedToken;
                                                    },
                                                }
                                            },
                                            .block => |name| {
                                                switch (token) {
                                                    .eol, .semi => {},
                                                    .word => |word| {
                                                        if (!std.mem.eql(u8, word, "predicate")) {
                                                            std.log.err("unexpected token: expected word to equal \"predicate\" but got \"{s}\"", .{word});
                                                            return error.UnexpectedToken;
                                                        }

                                                        role.* = .{ .predicate = .{ .name = name } };
                                                    },
                                                    else => {
                                                        std.log.err("unexpected token: expected eol, semi or word but got {s}", .{@tagName(token)});
                                                        return error.UnexpectedToken;
                                                    },
                                                }
                                            },
                                            .predicate => |*predicate| {
                                                const res = try predicate.expr.pushToken(allocator, token);
                                                if (res.expr) |expr| {
                                                    role.* = .{
                                                        .end = .{
                                                            .name = predicate.name,
                                                            .expr = expr,
                                                        },
                                                    };
                                                }

                                                return .{ .save = res.save };
                                            },
                                            .end => |end| {
                                                switch (token) {
                                                    .eol, .semi => {},
                                                    .rbrace => {
                                                        try body.members.append(allocator, .{
                                                            .role = .{
                                                                .name = end.name,
                                                                .predicate = end.expr,
                                                            },
                                                        });

                                                        body.state = .end;
                                                    },
                                                    else => {
                                                        std.log.err("unexpected token: expected rbrace but got {s}", .{@tagName(token)});
                                                        return error.UnexpectedToken;
                                                    },
                                                }
                                            },
                                        }
                                    },
                                    .end => {
                                        switch (token) {
                                            .semi, .eol => {
                                                body.state = .empty;
                                            },
                                            .rbrace => {
                                                self.state = .{
                                                    .end = .{
                                                        .access_provider = .{
                                                            .name = body.name,
                                                            .members = try body.members.toOwnedSlice(allocator),
                                                        },
                                                    },
                                                };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected word or rbrace but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                }
                            },
                        }
                    },
                    .collection => |*collection| {
                        if (collection.name == null) {
                            switch (token) {
                                inline .string, .word => |name| {
                                    collection.name = try allocator.dupe(u8, name);
                                },
                                else => {
                                    std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        } else if (collection.member_state == null) {
                            switch (token) {
                                .lbrace => {
                                    collection.member_state = .empty;
                                },
                                else => {
                                    std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        } else switch (collection.member_state.?) {
                            .empty => {
                                switch (token) {
                                    .eol, .semi => {},
                                    .word => |word| {
                                        if (std.meta.stringToEnum(enum { migrations, history_days, document_ttls, ttl_days, index, unique, check, compute }, word)) |keyword| {
                                            switch (keyword) {
                                                inline .history_days, .document_ttls, .ttl_days => |kw| collection.member_state = @field(State.Collection.Member, @tagName(kw)),
                                                inline else => |kw| collection.member_state = @unionInit(State.Collection.Member, @tagName(kw), .{}),
                                            }
                                        } else {
                                            collection.member_state = .{ .field = .{ .name = try allocator.dupe(u8, word) } };
                                        }
                                    },
                                    .string => |str| {
                                        collection.member_state = .{ .field = .{ .name = try allocator.dupe(u8, str) } };
                                    },
                                    .asterisk => {
                                        collection.member_state = .{ .field = .{ .name = try allocator.dupe(u8, "*") } };
                                    },
                                    .rbrace => {
                                        self.state = .{
                                            .end = .{
                                                .collection = .{
                                                    .name = collection.name.?,
                                                    .members = try collection.members.toOwnedSlice(allocator),
                                                },
                                            },
                                        };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected eol, semi, word or string but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .field => |*field| {
                                if (field.parser == null and field.type == null) {
                                    if (token == .colon) {
                                        field.parser = .{ .type = .{} };
                                    } else {
                                        std.log.err("unexpected token: expected colon or equal but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    }
                                } else if (field.parser == null) {
                                    switch (token) {
                                        .eol, .semi => {
                                            try collection.members.append(allocator, .{
                                                .field = .{
                                                    .name = field.name,
                                                    .type = field.type.?,
                                                },
                                            });

                                            collection.member_state = .empty;
                                        },
                                        .equal => {
                                            field.parser = .{ .expr = .{} };
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                } else switch (field.parser.?) {
                                    .type => |*parser| {
                                        const res = try parser.pushToken(allocator, token);
                                        if (res.type) |fql_type| {
                                            field.type = fql_type;
                                            field.parser = null;
                                        }

                                        return .{ .save = res.save };
                                    },
                                    .expr => |*parser| {
                                        const res = try parser.pushToken(allocator, token);
                                        if (res.expr) |expr| {
                                            try collection.members.append(allocator, .{
                                                .field = .{
                                                    .name = field.name,
                                                    .type = field.type.?,
                                                    .default = expr,
                                                },
                                            });

                                            collection.member_state = .empty;
                                        }

                                        return .{ .save = res.save };
                                    },
                                }
                            },
                            .migrations => |*migrations| {
                                switch (migrations.state) {
                                    .before_lbrace => {
                                        switch (token) {
                                            .lbrace => {
                                                migrations.state = .start;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .start => {
                                        switch (token) {
                                            .eol, .semi => {},
                                            .rbrace => {
                                                try collection.members.append(allocator, .{ .migrations = try migrations.statements.toOwnedSlice(allocator) });
                                                collection.member_state = .empty;
                                            },
                                            .word => |word| {
                                                if (std.meta.stringToEnum(std.meta.Tag(Collection.Member.Migration), word)) |tag| {
                                                    migrations.state = .{ .first_expr = .{ .tag = tag } };
                                                } else {
                                                    std.log.err("unexpected token: expected word to equal \"add\", \"backfill\", \"drop\", \"move\", \"move_conflicts\", \"move_wildcard\" or \"split\" but got \"{s}\"", .{@tagName(token)});
                                                    return error.UnexpectedToken;
                                                }
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected rbrace or word but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .first_expr => |*first_expr| {
                                        const res = try first_expr.parser.pushToken(allocator, token);
                                        if (res.expr) |expr| {
                                            switch (first_expr.tag) {
                                                inline else => |tag| {
                                                    if (std.meta.FieldType(SchemaDefinition.Collection.Member.Migration, tag) == FQLExpression) {
                                                        try migrations.statements.append(
                                                            allocator,
                                                            @unionInit(SchemaDefinition.Collection.Member.Migration, @tagName(tag), expr),
                                                        );
                                                        migrations.state = .start;
                                                    } else {
                                                        migrations.state = .{
                                                            .before_arrow = .{
                                                                .tag = std.meta.stringToEnum(@TypeOf(migrations.state.before_arrow.tag), @tagName(tag)).?,
                                                                .first_expr = expr,
                                                            },
                                                        };
                                                    }
                                                },
                                            }
                                        }

                                        return .{ .save = res.save };
                                    },
                                    .before_arrow => |before_arrow| {
                                        migrations.state = switch (before_arrow.tag) {
                                            inline else => |tag| @unionInit(
                                                @TypeOf(migrations.state),
                                                @tagName(tag),
                                                .{
                                                    .first_expr = before_arrow.first_expr,
                                                },
                                            ),
                                        };
                                    },
                                    inline .move, .backfill => |*move_or_backfill, tag| {
                                        const res = try move_or_backfill.parser.pushToken(allocator, token);
                                        if (res.expr) |expr| {
                                            try migrations.statements.append(
                                                allocator,
                                                switch (tag) {
                                                    .move => .{
                                                        .move = .{
                                                            .old_name = move_or_backfill.first_expr,
                                                            .new_name = expr,
                                                        },
                                                    },
                                                    .backfill => .{
                                                        .backfill = .{
                                                            .name = move_or_backfill.first_expr,
                                                            .value = expr,
                                                        },
                                                    },
                                                    else => unreachable,
                                                },
                                            );

                                            migrations.state = .start;
                                        }

                                        return .{ .save = res.save };
                                    },
                                    .split => |*split| {
                                        if (split.parser) |*parser| {
                                            const res = try parser.pushToken(allocator, token);
                                            if (res.expr) |expr| {
                                                try split.after.append(allocator, expr);
                                                split.parser = null;
                                            }

                                            return .{ .save = res.save };
                                        } else {
                                            switch (token) {
                                                .eol, .semi => {
                                                    try migrations.statements.append(allocator, .{
                                                        .split = .{
                                                            .old_name = split.first_expr,
                                                            .new_names = try split.after.toOwnedSlice(allocator),
                                                        },
                                                    });

                                                    migrations.state = .start;
                                                },
                                                .comma => {
                                                    split.parser = .{};
                                                },
                                                else => {
                                                    std.log.err("unexpected token: expected eol, semi or comma but got {s}", .{@tagName(token)});
                                                    return error.UnexpectedToken;
                                                },
                                            }
                                        }
                                    },
                                }
                            },
                            .history_days => {
                                switch (token) {
                                    .number => |num| {
                                        try collection.members.append(allocator, .{ .history_days = try allocator.dupe(u8, num) });
                                        collection.member_state = .empty;
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected number but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .document_ttls => {
                                switch (token) {
                                    .word => |word| {
                                        if (std.meta.stringToEnum(enum { true, false, null }, word)) |keyword| {
                                            try collection.members.append(allocator, .{
                                                .document_ttls = switch (keyword) {
                                                    .true => true,
                                                    .false => false,
                                                    .null => null,
                                                },
                                            });
                                            collection.member_state = .empty;
                                        } else {
                                            std.log.err("unexpected token: expected word to equal \"true\", \"false\" or \"null\" but got {s}", .{word});
                                            return error.UnexpectedToken;
                                        }
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .ttl_days => {
                                switch (token) {
                                    .number => |num| {
                                        try collection.members.append(allocator, .{
                                            .ttl_days = try allocator.dupe(u8, num),
                                        });
                                        collection.member_state = .empty;
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected number but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .index => |*index| {
                                if (index.name == null) {
                                    switch (token) {
                                        inline .string, .word => |name| {
                                            index.name = try allocator.dupe(u8, name);
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                } else {
                                    switch (index.state) {
                                        .before_lbrace => {
                                            switch (token) {
                                                .lbrace => {
                                                    index.state = .start;
                                                },
                                                else => {
                                                    std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                                    return error.UnexpectedToken;
                                                },
                                            }
                                        },
                                        .start => {
                                            switch (token) {
                                                .eol => {},
                                                .rbrace => {
                                                    try collection.members.append(allocator, .{
                                                        .index = .{
                                                            .name = index.name.?,
                                                            .members = try index.members.toOwnedSlice(allocator),
                                                        },
                                                    });

                                                    collection.member_state = .empty;
                                                },
                                                .word => |word| {
                                                    if (std.meta.stringToEnum(@TypeOf(index.state.property.type), word)) |keyword| {
                                                        index.state = .{ .property = .{ .type = keyword } };
                                                    } else {
                                                        std.log.err("unexpected token: expected word to equal \"terms\" or \"values\" but got {s}", .{word});
                                                        return error.UnexpectedToken;
                                                    }
                                                },
                                                else => {
                                                    std.log.err("unexpected token: expected eol, rbrace or word but got {s}", .{@tagName(token)});
                                                    return error.UnexpectedToken;
                                                },
                                            }
                                        },
                                        .property => |*property| {
                                            switch (property.state) {
                                                .before_lbracket => {
                                                    switch (token) {
                                                        .lbracket => {
                                                            property.state = .start;
                                                        },
                                                        else => {
                                                            std.log.err("unexpected token: expected lbracket but got {s}", .{@tagName(token)});
                                                            return error.UnexpectedToken;
                                                        },
                                                    }
                                                },
                                                .start => {
                                                    switch (token) {
                                                        .rbracket => {
                                                            switch (property.type) {
                                                                inline else => |tag| try index.members.append(
                                                                    allocator,
                                                                    @unionInit(
                                                                        SchemaDefinition.Collection.Member.Index.Member,
                                                                        @tagName(tag),
                                                                        try property.fields.toOwnedSlice(allocator),
                                                                    ),
                                                                ),
                                                            }

                                                            index.state = .start;
                                                        },
                                                        else => {
                                                            property.state = .{ .parsing = .{} };
                                                            return .{ .save = token };
                                                        },
                                                    }
                                                },
                                                .parsing => |*parser| {
                                                    const res = try parser.pushToken(allocator, token);
                                                    if (res.expr) |expr| {
                                                        try property.fields.append(allocator, expr);
                                                        property.state = .end;
                                                    }

                                                    return .{ .save = res.save };
                                                },
                                                .end => {
                                                    switch (token) {
                                                        .rbracket => {
                                                            property.state = .start;
                                                            return .{ .save = token };
                                                        },
                                                        .comma => {
                                                            property.state = .{ .parsing = .{} };
                                                        },
                                                        else => {
                                                            std.log.err("unexpected token: expected rbracket or comma but got {s}", .{@tagName(token)});
                                                            return error.UnexpectedToken;
                                                        },
                                                    }
                                                },
                                            }
                                        },
                                    }
                                }
                            },
                            .unique => |*unique| {
                                switch (unique.state) {
                                    .before_lbracket => {
                                        switch (token) {
                                            .lbracket => {
                                                unique.state = .start;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected lbracket but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .start => {
                                        switch (token) {
                                            .rbracket => {
                                                try collection.members.append(allocator, .{
                                                    .unique_constraint = .{
                                                        .terms = try unique.terms.toOwnedSlice(allocator),
                                                    },
                                                });
                                                collection.member_state = .empty;
                                            },
                                            else => {
                                                unique.state = .{ .parsing = .{} };
                                                return .{ .save = token };
                                            },
                                        }
                                    },
                                    .parsing => |*parser| {
                                        const res = try parser.pushToken(allocator, token);
                                        if (res.expr) |expr| {
                                            try unique.terms.append(allocator, expr);
                                            unique.state = .end;
                                        }

                                        return .{ .save = res.save };
                                    },
                                    .end => {
                                        switch (token) {
                                            .rbracket => {
                                                unique.state = .start;
                                                return .{ .save = token };
                                            },
                                            .comma => {
                                                unique.state = .start;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected number but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                }
                            },
                            .check => |*check| {
                                if (check.name == null) {
                                    switch (token) {
                                        inline .string, .word => |name| {
                                            check.name = try allocator.dupe(u8, name);
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                } else {
                                    const res = try check.parser.pushToken(allocator, token);
                                    if (res.expr) |expr| {
                                        try collection.members.append(allocator, .{
                                            .check_constraint = .{
                                                .name = check.name.?,
                                                .predicate = expr,
                                            },
                                        });

                                        collection.member_state = .empty;
                                    }

                                    return .{ .save = res.save };
                                }
                            },
                            .compute => |*compute| {
                                if (compute.name == null) {
                                    switch (token) {
                                        inline .string, .word => |name| {
                                            compute.name = try allocator.dupe(u8, name);
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                } else if (compute.parser == null) {
                                    if (compute.type == null and token == .colon) {
                                        compute.parser = .{ .type = .{} };
                                    } else if (token == .equal) {
                                        compute.parser = .{ .expr = .{} };
                                    } else {
                                        if (compute.type == null) {
                                            std.log.err("unexpected token: expected colon or equal but got {s}", .{@tagName(token)});
                                        } else {
                                            std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                        }

                                        return error.UnexpectedToken;
                                    }
                                } else {
                                    switch (compute.parser.?) {
                                        .type => |*parser| {
                                            const res = try parser.pushToken(allocator, token);
                                            if (res.type) |fql_type| {
                                                compute.type = fql_type;
                                                compute.parser = null;
                                            }

                                            return .{ .save = res.save };
                                        },
                                        .expr => |*parser| {
                                            const res = try parser.pushToken(allocator, token);
                                            if (res.expr) |expr| {
                                                try collection.members.append(allocator, .{
                                                    .computed_field = .{
                                                        .name = compute.name.?,
                                                        .type = compute.type,
                                                        .function = expr,
                                                    },
                                                });

                                                collection.member_state = .empty;
                                            }

                                            return .{ .save = res.save };
                                        },
                                    }
                                }
                            },
                        }
                    },
                    .role => |*role| {
                        if (role.name == null) {
                            switch (token) {
                                inline .string, .word => |name| {
                                    role.name = try allocator.dupe(u8, name);
                                },
                                else => {
                                    std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        } else if (role.member_state == null) {
                            switch (token) {
                                .lbrace => {
                                    role.member_state = .empty;
                                },
                                else => {
                                    std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        } else switch (role.member_state.?) {
                            .empty => {
                                switch (token) {
                                    .eol, .semi => {},
                                    .word => |word| {
                                        if (std.meta.stringToEnum(enum { membership, privileges }, word)) |keyword| {
                                            switch (keyword) {
                                                inline else => |kw| role.member_state = @field(@TypeOf(role.member_state.?), @tagName(kw)),
                                            }
                                        } else {
                                            std.log.err("unexpected token: word to equal \"membership\" or \"privileges\" but got {s}", .{word});
                                            return error.UnexpectedToken;
                                        }
                                    },
                                    .rbrace => {
                                        self.state = .{
                                            .end = .{
                                                .role = .{
                                                    .name = role.name.?,
                                                    .members = try role.members.toOwnedSlice(allocator),
                                                },
                                            },
                                        };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected eol, semi or word but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .membership => {
                                switch (token) {
                                    inline .string, .word => |collection| {
                                        role.member_state = .{ .membership_collection = try allocator.dupe(u8, collection) };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected string or word but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .membership_collection => |collection| {
                                switch (token) {
                                    .eol, .semi => {
                                        try role.members.append(allocator, .{ .membership = .{ .collection = collection } });
                                        role.member_state = .empty;
                                    },
                                    .lbrace => {
                                        role.member_state = .{ .membership_block = .{ .collection = collection } };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected eol, semi or lbrace but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .membership_block => |*membership_block| {
                                switch (membership_block.state) {
                                    .start => {
                                        switch (token) {
                                            .eol, .semi => {},
                                            .word => |word| {
                                                if (std.mem.eql(u8, word, "predicate")) {
                                                    membership_block.state = .{ .predicate = .{} };
                                                } else {
                                                    std.log.err("unexpected token: expected word to equal \"predicate\" but got {s}", .{word});
                                                    return error.UnexpectedToken;
                                                }
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected eol, semi or word but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .predicate => |*expr_parser| {
                                        const res = try expr_parser.pushToken(allocator, token);
                                        if (res.expr) |expr| {
                                            membership_block.state = .{ .end = expr };
                                        }
                                        return .{ .save = res.save };
                                    },
                                    .end => |predicate| {
                                        switch (token) {
                                            .eol, .semi => {},
                                            .rbrace => {
                                                try role.members.append(allocator, .{
                                                    .membership = .{
                                                        .collection = membership_block.collection,
                                                        .predicate = predicate,
                                                    },
                                                });
                                                role.member_state = .empty;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected rbrace but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                }
                            },
                            .privileges => {
                                switch (token) {
                                    inline .string, .word => |resource| {
                                        role.member_state = .{ .privileges_resource = try allocator.dupe(u8, resource) };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected string or word but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .privileges_resource => |resource| {
                                switch (token) {
                                    .lbrace => {
                                        role.member_state = .{ .privileges_block = .{ .resource = resource } };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .privileges_block => |*privileges_block| {
                                switch (privileges_block.state) {
                                    .start => {
                                        switch (token) {
                                            .eol, .semi => {},
                                            .rbrace => {
                                                try role.members.append(allocator, .{
                                                    .privileges = .{
                                                        .resource = privileges_block.resource,
                                                        .actions = try privileges_block.actions.toOwnedSlice(allocator),
                                                    },
                                                });
                                                role.member_state = .empty;
                                            },
                                            .word => |word| {
                                                if (std.meta.stringToEnum(SchemaDefinition.Role.Member.Privileges.Action.Action, word)) |action| {
                                                    privileges_block.state = .{ .action = action };
                                                } else {
                                                    std.log.err("unexpected token: expected word to equal \"predicate\" but got {s}", .{word});
                                                    return error.UnexpectedToken;
                                                }
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected eol, semi, rbrace or word but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .action => |action| {
                                        switch (token) {
                                            .eol, .semi => {
                                                try privileges_block.actions.append(allocator, .{ .action = action });
                                                privileges_block.state = .start;
                                            },
                                            .lbrace => {
                                                privileges_block.state = .{ .action_block = action };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected eol, semi or lbrace but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .action_block => |action| {
                                        switch (token) {
                                            .eol, .semi => {},
                                            .word => |word| {
                                                if (std.mem.eql(u8, word, "predicate")) {
                                                    privileges_block.state = .{ .action_predicate = .{ .action = action } };
                                                } else {
                                                    std.log.err("unexpected token: expected word to equal \"predicate\" but got {s}", .{word});
                                                    return error.UnexpectedToken;
                                                }
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected eol, semi or word but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .action_predicate => |*action_predicate| {
                                        const res = try action_predicate.expr_parser.pushToken(allocator, token);
                                        if (res.expr) |expr| {
                                            privileges_block.state = .{
                                                .action_end = .{
                                                    .action = action_predicate.action,
                                                    .predicate = expr,
                                                },
                                            };
                                        }

                                        return .{ .save = res.save };
                                    },
                                    .action_end => |action| {
                                        switch (token) {
                                            .eol, .semi => {},
                                            .rbrace => {
                                                try privileges_block.actions.append(allocator, action);
                                                privileges_block.state = .start;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected eol, semi or rbrace but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                }
                            },
                        }
                    },
                    .function => |*function| {
                        switch (function.*) {
                            .start => {
                                switch (token) {
                                    inline .string, .word => |s| {
                                        function.* = .{ .after_name = try allocator.dupe(u8, s) };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .after_name => |name| {
                                switch (token) {
                                    .lparen => {
                                        function.* = .{ .params = .{ .name = name } };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected lparen but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            },
                            .params => |*params| {
                                switch (params.param_state) {
                                    .start => {
                                        switch (token) {
                                            .word => |word| {
                                                params.param_state = .{ .after_name = try allocator.dupe(u8, word) };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .after_name => |name| {
                                        switch (token) {
                                            .colon => {
                                                params.param_state = .{ .before_type = .{ .name = name } };
                                            },
                                            .rparen, .comma => {
                                                try params.params.append(allocator, .{ .name = name });
                                                params.param_state = .end;
                                                return .{ .save = token };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected colon, rparen or comma but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                    .before_type => |*before_type| {
                                        const res = try before_type.type_parser.pushToken(allocator, token);
                                        if (res.type) |fql_type| {
                                            try params.params.append(allocator, .{
                                                .name = before_type.name,
                                                .type = fql_type,
                                            });
                                            params.param_state = .end;
                                        }

                                        return .{ .save = res.save };
                                    },
                                    .end => {
                                        switch (token) {
                                            .comma => {
                                                params.param_state = .start;
                                            },
                                            .rparen => {
                                                function.* = .{
                                                    .before_body = .{
                                                        .name = params.name,
                                                        .params = try params.params.toOwnedSlice(allocator),
                                                    },
                                                };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected rparen or comma but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    },
                                }
                            },
                            .return_type => |*return_type| {
                                const res = try return_type.type_parser.pushToken(allocator, token);
                                if (res.type) |fql_type| {
                                    function.* = .{
                                        .before_body = .{
                                            .name = return_type.name,
                                            .params = return_type.params,
                                            .return_type = fql_type,
                                        },
                                    };
                                }

                                return .{ .save = res.save };
                            },
                            .before_body => |before_body| {
                                if (before_body.return_type != null and token == .colon) {
                                    function.* = .{
                                        .return_type = .{
                                            .name = before_body.name,
                                            .params = before_body.params,
                                        },
                                    };
                                } else if (token == .lbrace) {
                                    function.* = .{
                                        .body = .{
                                            .name = before_body.name,
                                            .params = before_body.params,
                                            .return_type = before_body.return_type,
                                        },
                                    };
                                } else {
                                    if (before_body.return_type != null) {
                                        std.log.err("unexpected token: expected colon or lbrace but got {s}", .{@tagName(token)});
                                    } else {
                                        std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                    }

                                    return error.UnexpectedToken;
                                }
                            },
                            .body => |*body| {
                                if (body.expr_parser.state == .empty and token == .rbrace) {
                                    self.state = .{
                                        .end = .{
                                            .function = .{
                                                .name = body.name,
                                                .parameters = body.params,
                                                .return_type = body.return_type,
                                                .body = try body.exprs.toOwnedSlice(allocator),
                                            },
                                        },
                                    };
                                } else {
                                    const res = try body.expr_parser.pushToken(allocator, token);
                                    if (res.expr) |expr| {
                                        try body.exprs.append(allocator, expr);
                                    }

                                    return .{ .save = res.save };
                                }
                            },
                        }
                    },
                    .end => |*definition| {
                        defer {
                            self.annotations.clearRetainingCapacity();
                            self.state = .empty;
                        }

                        switch (definition.*) {
                            inline else => |*def, def_tag| {
                                const E = AnnotationEnum(def_tag);

                                for (self.annotations.items) |annotation| {
                                    defer allocator.free(annotation.name);

                                    if (std.meta.fields(E).len > 0) {
                                        if (std.meta.stringToEnum(E, annotation.name)) |name| {
                                            switch (name) {
                                                inline else => |tag| {
                                                    @field(def, @tagName(tag)) = annotation.value;
                                                },
                                            }
                                            continue;
                                        }
                                    }

                                    annotation.value.deinit(allocator);
                                    std.log.warn("unknown {s} annotation: @{s}", .{ @tagName(def_tag), annotation.name });
                                }
                            },
                        }

                        defer self.state = .empty;

                        return .{ .save = token, .definition = definition.* };
                    },
                }

                return .{};
            }
        };

        allocator: std.mem.Allocator,
        inner: Unmanaged = .{},

        pub fn init(allocator: std.mem.Allocator) Parser {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: Parser) void {
            self.inner.deinit(self.allocator);
        }

        pub fn reset(self: *Parser) void {
            self.deinit();
            self.inner = .{};
        }

        pub const PushResult = struct {
            save: ?tokenizer.Token = null,
            definition: ?SchemaDefinition = null,
        };

        pub fn push(self: *Parser, token: tokenizer.Token) !PushResult {
            return try self.inner.pushToken(self.allocator, token);
        }
    };

    pub fn parse(allocator: std.mem.Allocator, it: *tokenizer.TokenIterator) !SchemaDefinition {
        var parser = Parser.init(allocator);
        defer parser.deinit();

        while (true) {
            const token = try it.nextToken(allocator);
            defer token.deinit(allocator);

            const result = try parser.push(token);
            if (result.save) |save| {
                // std.debug.print("saving token: {any}\n", .{token});
                it.saveToken(try save.dupe(allocator));
            }

            if (result.definition) |definition| {
                return definition;
            }
        }
    }
};

pub fn parseDefinition(allocator: std.mem.Allocator, reader: std.io.AnyReader) !SchemaDefinition {
    var it = tokenizer.TokenIterator.init(reader);
    defer it.deinit(allocator);

    return SchemaDefinition.parse(allocator, &it);
}

fn expectParsedDefnEqual(str: []const u8, expected: SchemaDefinition) !void {
    var stream = std.io.fixedBufferStream(str);
    var actual = try parseDefinition(testing.allocator, stream.reader().any());
    defer actual.deinit(testing.allocator);

    // std.debug.print("actual: {any}\n", .{actual});

    try testing.expectEqualDeep(expected, actual);
}

test parseDefinition {
    try expectParsedDefnEqual(
        \\access provider someIssuer {
        \\    issuer "https://example.com/"
        \\    jwks_uri "https://example.com/.well-known/jwks.json"
        \\
        \\    role customer
        \\    role manager {
        \\        predicate (jwt => jwt!.scope.includes("manager"))
        \\    }
        \\}
    ,
        .{
            .access_provider = .{
                .name = "someIssuer",
                .members = &[_]SchemaDefinition.AccessProvider.Member{
                    .{ .issuer = "\"https://example.com/\"" },
                    .{ .jwks_uri = "\"https://example.com/.well-known/jwks.json\"" },
                    .{ .role = .{ .name = "customer" } },
                    .{
                        .role = .{
                            .name = "manager",
                            .predicate = .{
                                .isolated = &FQLExpression{
                                    .function = .{
                                        .parameters = .{
                                            .short = "jwt",
                                        },
                                        .body = &FQLExpression{
                                            .invocation = .{
                                                .function = &FQLExpression{
                                                    .field_access = .{
                                                        .value = &FQLExpression{
                                                            .field_access = .{
                                                                .value = &FQLExpression{
                                                                    .non_null_assertion = &FQLExpression{ .identifier = "jwt" },
                                                                },
                                                                .field = .{ .identifier = "scope" },
                                                            },
                                                        },
                                                        .field = .{ .identifier = "includes" },
                                                    },
                                                },
                                                .arguments = &[_]FQLExpression{
                                                    .{ .string_literal = "\"manager\"" },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
    );

    try expectParsedDefnEqual(
        \\@role(server)
        \\function inventory(name) {
        \\  Product.byName(name) {
        \\    name,
        \\    description,
        \\    quantity
        \\  }
        \\}
    ,
        .{
            .function = .{
                .role = .{ .identifier = "server" },
                .name = "inventory",
                .parameters = &[_]SchemaDefinition.Function.Parameter{
                    .{ .name = "name" },
                },
                .body = &[_]FQLExpression{
                    .{
                        .projection = .{
                            .expression = &FQLExpression{
                                .invocation = .{
                                    .function = &FQLExpression{
                                        .field_access = .{
                                            .value = &FQLExpression{ .identifier = "Product" },
                                            .field = .{ .identifier = "byName" },
                                        },
                                    },
                                    .arguments = &[_]FQLExpression{
                                        .{ .identifier = "name" },
                                    },
                                },
                            },
                            .fields = &[_]FQLExpression.Projection.Field{
                                .{ .short = "name" },
                                .{ .short = "description" },
                                .{ .short = "quantity" },
                            },
                        },
                    },
                },
            },
        },
    );

    try expectParsedDefnEqual(
        \\role manager {
        \\
        \\  // Assign the `manager` role to tokens with
        \\  // an identity document in the `Manager` collection.
        \\  membership Manager
        \\
        \\  // If the predicate is `true`,
        \\  // assign the `manager` role to tokens with
        \\  // an identity document in the `User` collection.
        \\  membership User {
        \\    // Check that the identity document's
        \\    // `accessLevel` field value is `manager`
        \\    predicate (user => user.accessLevel == 'manager')
        \\  }
        \\
        \\  // Grant full access to `Store` collection documents.
        \\  privileges Store {
        \\    create
        \\    read
        \\    write
        \\    delete
        \\  }
        \\
        \\  // Grant `read` access to `Customer` collection documents.
        \\  privileges Customer {
        \\    read
        \\  }
        \\
        \\  // If the predicate is `true`,
        \\  // grant `read` access to `Manager` collection documents.
        \\  privileges Manager {
        \\    read {
        \\      predicate (doc =>
        \\        // Check that the `ManagerProfile` document
        \\        // is the token's identity document.
        \\        // `Query.identity()` is `null` (falsy) for JWTs or keys.
        \\        Query.identity() == doc &&
        \\        // Check that it's a weekday.
        \\        Date.today().dayOfWeek < 6
        \\      )
        \\    }
        \\  }
        \\
        \\  // Grant the ability to call the user-defined
        \\  // `inventory()` function.
        \\  privileges inventory {
        \\    call
        \\  }
        \\
        \\  // If the predicate is `true`,
        \\  // grant the ability to call the user-defined
        \\  // `submitOrder()` function.
        \\  privileges submitOrder {
        \\    call {
        \\      // Check that the function's
        \\      // `customer` argument (`args[0]`) is
        \\      // the token's identity document.
        \\      predicate (args => Query.identity() == args[0])
        \\    }
        \\  }
        \\}
    ,
        .{
            .role = .{
                .name = "manager",
                .members = &[_]SchemaDefinition.Role.Member{
                    .{ .membership = .{ .collection = "Manager" } },
                    .{
                        .membership = .{
                            .collection = "User",
                            .predicate = .{
                                .isolated = &FQLExpression{
                                    .function = .{
                                        .parameters = .{ .short = "user" },
                                        .body = &FQLExpression{
                                            .binary_operation = .{
                                                .lhs = &FQLExpression{
                                                    .field_access = .{
                                                        .value = &FQLExpression{ .identifier = "user" },
                                                        .field = .{ .identifier = "accessLevel" },
                                                    },
                                                },
                                                .operator = .equality,
                                                .rhs = &FQLExpression{ .string_literal = "'manager'" },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .privileges = .{
                            .resource = "Store",
                            .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                                .{ .action = .create },
                                .{ .action = .read },
                                .{ .action = .write },
                                .{ .action = .delete },
                            },
                        },
                    },
                    .{
                        .privileges = .{
                            .resource = "Customer",
                            .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                                .{ .action = .read },
                            },
                        },
                    },
                    .{
                        .privileges = .{
                            .resource = "Manager",
                            .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                                .{
                                    .action = .read,
                                    .predicate = .{
                                        .isolated = &FQLExpression{
                                            .function = .{
                                                .parameters = .{ .short = "doc" },
                                                .body = &FQLExpression{
                                                    .binary_operation = .{
                                                        .lhs = &FQLExpression{
                                                            .binary_operation = .{
                                                                .lhs = &FQLExpression{
                                                                    .invocation = .{
                                                                        .function = &FQLExpression{
                                                                            .field_access = .{
                                                                                .value = &FQLExpression{ .identifier = "Query" },
                                                                                .field = .{ .identifier = "identity" },
                                                                            },
                                                                        },
                                                                        .arguments = &.{},
                                                                    },
                                                                },
                                                                .operator = .equality,
                                                                .rhs = &FQLExpression{ .identifier = "doc" },
                                                            },
                                                        },
                                                        .operator = .logical_and,
                                                        .rhs = &FQLExpression{
                                                            .binary_operation = .{
                                                                .lhs = &FQLExpression{
                                                                    .field_access = .{
                                                                        .value = &FQLExpression{
                                                                            .invocation = .{
                                                                                .function = &FQLExpression{
                                                                                    .field_access = .{
                                                                                        .value = &FQLExpression{ .identifier = "Date" },
                                                                                        .field = .{ .identifier = "today" },
                                                                                    },
                                                                                },
                                                                                .arguments = &.{},
                                                                            },
                                                                        },
                                                                        .field = .{ .identifier = "dayOfWeek" },
                                                                    },
                                                                },
                                                                .operator = .less_than,
                                                                .rhs = &FQLExpression{ .number_literal = "6" },
                                                            },
                                                        },
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .privileges = .{
                            .resource = "inventory",
                            .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                                .{ .action = .call },
                            },
                        },
                    },
                    .{
                        .privileges = .{
                            .resource = "submitOrder",
                            .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                                .{
                                    .action = .call,
                                    .predicate = .{
                                        .isolated = &FQLExpression{
                                            .function = .{
                                                .parameters = .{ .short = "args" },
                                                .body = &FQLExpression{
                                                    .binary_operation = .{
                                                        .lhs = &FQLExpression{
                                                            .invocation = .{
                                                                .function = &FQLExpression{
                                                                    .field_access = .{
                                                                        .value = &FQLExpression{ .identifier = "Query" },
                                                                        .field = .{ .identifier = "identity" },
                                                                    },
                                                                },
                                                                .arguments = &.{},
                                                            },
                                                        },
                                                        .operator = .equality,
                                                        .rhs = &FQLExpression{
                                                            .field_access = .{
                                                                .value = &FQLExpression{ .identifier = "args" },
                                                                .field = .{
                                                                    .expression = &FQLExpression{ .number_literal = "0" },
                                                                },
                                                            },
                                                        },
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
    );

    try expectParsedDefnEqual(
        \\collection Product {
        \\  name: String?
        \\  description: String?
        \\  price: Double = 0.00
        \\  quantity: Int = 0
        \\  store: Ref<Store>?
        \\  backorderLimit: Int?
        \\  backordered: Boolean?
        \\  categories: Array<String>?
        \\  creationTime: Time = Time.now()
        \\  creationTimeEpoch: Int?
        \\  typeConflicts: { *: Any }?
        \\
        \\  *: Any
        \\
        \\  migrations {
        \\    add .typeConflicts
        \\    add .quantity
        \\    backfill .quantity = 0
        \\    drop .internalDesc
        \\    move_conflicts .typeConflicts
        \\    move .desc -> .description
        \\    split .creationTime -> .creationTime, .creationTimeEpoch
        \\  }
        \\
        \\  unique [.name, .description, mva(.categories)]
        \\
        \\  index byName {
        \\    terms [.name]
        \\    values [desc(.quantity), desc(mva(.categories))]
        \\  }
        \\
        \\  check posQuantity ((doc) => doc.quantity >= 0)
        \\  compute InventoryValue: Number = (.quantity * .price)
        \\
        \\  history_days 3
        \\  document_ttls true
        \\  ttl_days 5
        \\}
    ,
        .{
            .collection = .{
                .name = "Product",
                .members = &[_]SchemaDefinition.Collection.Member{
                    .{
                        .field = .{
                            .name = "name",
                            .type = .{ .optional = &FQLType{ .named = "String" } },
                        },
                    },
                    .{
                        .field = .{
                            .name = "description",
                            .type = .{ .optional = &FQLType{ .named = "String" } },
                        },
                    },
                    .{
                        .field = .{
                            .name = "price",
                            .type = .{ .named = "Double" },
                            .default = .{ .number_literal = "0.00" },
                        },
                    },
                    .{
                        .field = .{
                            .name = "quantity",
                            .type = .{ .named = "Int" },
                            .default = .{ .number_literal = "0" },
                        },
                    },
                    .{
                        .field = .{
                            .name = "store",
                            .type = .{
                                .optional = &FQLType{
                                    .template = .{
                                        .name = "Ref",
                                        .parameters = &[_]FQLType{
                                            .{ .named = "Store" },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .field = .{
                            .name = "backorderLimit",
                            .type = .{ .optional = &FQLType{ .named = "Int" } },
                        },
                    },
                    .{
                        .field = .{
                            .name = "backordered",
                            .type = .{ .optional = &FQLType{ .named = "Boolean" } },
                        },
                    },
                    .{
                        .field = .{
                            .name = "categories",
                            .type = .{
                                .optional = &FQLType{
                                    .template = .{
                                        .name = "Array",
                                        .parameters = &[_]FQLType{
                                            .{ .named = "String" },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .field = .{
                            .name = "creationTime",
                            .type = .{ .named = "Time" },
                            .default = .{
                                .invocation = .{
                                    .function = &FQLExpression{
                                        .field_access = .{
                                            .value = &FQLExpression{ .identifier = "Time" },
                                            .field = .{ .identifier = "now" },
                                        },
                                    },
                                    .arguments = &.{},
                                },
                            },
                        },
                    },
                    .{
                        .field = .{
                            .name = "creationTimeEpoch",
                            .type = .{ .optional = &FQLType{ .named = "Int" } },
                        },
                    },
                    .{
                        .field = .{
                            .name = "typeConflicts",
                            .type = .{
                                .optional = &FQLType{
                                    .object = .{
                                        .fields = &[_]FQLType.Object.Field{
                                            .{
                                                .key = .wildcard,
                                                .type = .{ .named = "Any" },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .field = .{
                            .name = "*",
                            .type = .{ .named = "Any" },
                        },
                    },
                    .{
                        .migrations = &[_]SchemaDefinition.Collection.Member.Migration{
                            .{
                                .add = .{ .anonymous_field_access = .{ .identifier = "typeConflicts" } },
                            },
                            .{
                                .add = .{ .anonymous_field_access = .{ .identifier = "quantity" } },
                            },
                            .{
                                .backfill = .{
                                    .name = .{ .anonymous_field_access = .{ .identifier = "quantity" } },
                                    .value = .{ .number_literal = "0" },
                                },
                            },
                            .{
                                .drop = .{ .anonymous_field_access = .{ .identifier = "internalDesc" } },
                            },
                            .{
                                .move_conflicts = .{ .anonymous_field_access = .{ .identifier = "typeConflicts" } },
                            },
                            .{
                                .move = .{
                                    .old_name = .{ .anonymous_field_access = .{ .identifier = "desc" } },
                                    .new_name = .{ .anonymous_field_access = .{ .identifier = "description" } },
                                },
                            },
                            .{
                                .split = .{
                                    .old_name = .{ .anonymous_field_access = .{ .identifier = "creationTime" } },
                                    .new_names = &[_]FQLExpression{
                                        .{ .anonymous_field_access = .{ .identifier = "creationTime" } },
                                        .{ .anonymous_field_access = .{ .identifier = "creationTimeEpoch" } },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .unique_constraint = .{
                            .terms = &[_]FQLExpression{
                                .{ .anonymous_field_access = .{ .identifier = "name" } },
                                .{ .anonymous_field_access = .{ .identifier = "description" } },
                                .{
                                    .invocation = .{
                                        .function = &FQLExpression{ .identifier = "mva" },
                                        .arguments = &[_]FQLExpression{
                                            .{ .anonymous_field_access = .{ .identifier = "categories" } },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .index = .{
                            .name = "byName",
                            .members = &[_]SchemaDefinition.Collection.Member.Index.Member{
                                .{
                                    .terms = &[_]FQLExpression{
                                        .{ .anonymous_field_access = .{ .identifier = "name" } },
                                    },
                                },
                                .{
                                    .values = &[_]FQLExpression{
                                        .{
                                            .invocation = .{
                                                .function = &FQLExpression{ .identifier = "desc" },
                                                .arguments = &[_]FQLExpression{
                                                    .{ .anonymous_field_access = .{ .identifier = "quantity" } },
                                                },
                                            },
                                        },
                                        .{
                                            .invocation = .{
                                                .function = &FQLExpression{ .identifier = "desc" },
                                                .arguments = &[_]FQLExpression{
                                                    .{
                                                        .invocation = .{
                                                            .function = &FQLExpression{ .identifier = "mva" },
                                                            .arguments = &[_]FQLExpression{
                                                                .{ .anonymous_field_access = .{ .identifier = "categories" } },
                                                            },
                                                        },
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .check_constraint = .{
                            .name = "posQuantity",
                            .predicate = .{
                                .isolated = &FQLExpression{
                                    .function = .{
                                        .parameters = .{
                                            .long = .{
                                                .parameters = &.{"doc"},
                                            },
                                        },
                                        .body = &FQLExpression{
                                            .binary_operation = .{
                                                .lhs = &FQLExpression{
                                                    .field_access = .{
                                                        .value = &FQLExpression{ .identifier = "doc" },
                                                        .field = .{ .identifier = "quantity" },
                                                    },
                                                },
                                                .operator = .greater_than_or_equal,
                                                .rhs = &FQLExpression{ .number_literal = "0" },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .computed_field = .{
                            .name = "InventoryValue",
                            .type = .{ .named = "Number" },
                            .function = .{
                                .isolated = &FQLExpression{
                                    .binary_operation = .{
                                        .lhs = &FQLExpression{ .anonymous_field_access = .{ .identifier = "quantity" } },
                                        .operator = .multiply,
                                        .rhs = &FQLExpression{ .anonymous_field_access = .{ .identifier = "price" } },
                                    },
                                },
                            },
                        },
                    },
                    .{
                        .history_days = "3",
                    },
                    .{
                        .document_ttls = true,
                    },
                    .{
                        .ttl_days = "5",
                    },
                },
            },
        },
    );
}

pub const SchemaTree = struct {
    allocator: std.mem.Allocator,

    declarations: ?[]SchemaDefinition = null,

    fn deinit(self: @This()) void {
        if (self.declarations) |declarations| {
            for (declarations) |declaration| {
                declaration.deinit(self.allocator);
            }

            self.allocator.free(self.declarations);
        }
    }
};
