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

            if (token == .comment_line or token == .comment_block) {
                continue;
            }

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
    };

    pub const UnaryOperation = struct {
        pub const Operator = enum {
            logical_not,
            bitwise_not,
            arithmetic_not,
        };

        operator: Operator,
        operand: *const FQLExpression,

        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            self.operand.deinit(allocator);
            allocator.destroy(self.operand);
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
                    optional: bool,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.value.deinit(allocator);
                    }
                };

                const Invocation = struct {
                    function: FQLExpression,
                    arguments: std.ArrayListUnmanaged(FQLExpression) = .{},

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
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
                switch (self.state) {
                    .empty => switch (token) {
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
                                if (parent.state == .array_literal) {
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
                                        .rbrace => {
                                            parent.finalizeExpr(.{ .block_scope = try block_scope.toOwnedSlice(allocator) });
                                            return .{};
                                        },
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

            if (token == .comment_line or token == .comment_block) {
                continue;
            }

            // std.debug.print("pushing token: {any}\n", .{token});
            const result = try parser.push(token);
            // std.debug.print("parser state: {s} {s}\n", .{ @tagName(parser.inner.state), if (parser.inner.parent) |p| @tagName(p.state) else "" });
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
};
