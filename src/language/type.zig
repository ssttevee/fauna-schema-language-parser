const std = @import("std");
const testing = std.testing;

const Tokenizer = @import("../Tokenizer.zig");
const util = @import("../util.zig");
const parsing = @import("../parsing.zig");

pub const FQLType = union(enum) {
    pub const Object = struct {
        pub const Field = struct {
            const Key = union(enum) {
                identifier: []const u8,
                string: []const u8,
                wildcard,

                pub fn deinit(self: Key, allocator: std.mem.Allocator) void {
                    switch (self) {
                        inline .identifier, .string => |s| allocator.free(s),
                        else => {},
                    }
                }

                pub fn dupe(self: Key, allocator: std.mem.Allocator) std.mem.Allocator.Error!Key {
                    return switch (self) {
                        inline .identifier, .string => |s, tag| @unionInit(
                            @This(),
                            @tagName(tag),
                            try allocator.dupe(u8, s),
                        ),
                        else => .wildcard,
                    };
                }
            };

            key: Key,
            type: FQLType,

            pub fn deinit(self: Field, allocator: std.mem.Allocator) void {
                self.key.deinit(allocator);
                self.type.deinit(allocator);
            }

            pub fn dupe(self: Field, allocator: std.mem.Allocator) std.mem.Allocator.Error!Field {
                const key = try self.key.dupe(allocator);
                errdefer key.deinit(allocator);

                return .{
                    .key = key,
                    .type = try self.type.dupe(allocator),
                };
            }
        };

        fields: ?[]const Field = null,

        pub fn deinit(self: Object, allocator: std.mem.Allocator) void {
            if (self.fields) |fields| {
                for (fields) |field| {
                    field.deinit(allocator);
                }

                allocator.free(fields);
            }
        }

        pub fn dupe(self: Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!Object {
            return .{
                .fields = try util.slice.deepDupe(allocator, self.fields),
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
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

        pub fn deinit(self: Union, allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            allocator.destroy(self.lhs);
            self.rhs.deinit(allocator);
            allocator.destroy(self.rhs);
        }

        pub fn dupe(self: Union, allocator: std.mem.Allocator) std.mem.Allocator.Error!Union {
            const lhs = try self.lhs.dupePtr(allocator);
            errdefer {
                lhs.deinit(allocator);
                allocator.destroy(lhs);
            }

            return .{
                .lhs = lhs,
                .rhs = try self.rhs.dupePtr(allocator),
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
            try self.lhs.printCanonical(writer);
            try writer.writeAll(" | ");
            try self.rhs.printCanonical(writer);
        }
    };

    pub const Template = struct {
        name: []const u8,
        parameters: ?[]const FQLType = null,

        pub fn deinit(self: Template, allocator: std.mem.Allocator) void {
            if (self.parameters) |parameters| {
                for (parameters) |parameter| {
                    parameter.deinit(allocator);
                }

                allocator.free(parameters);
            }

            allocator.free(self.name);
        }

        pub fn dupe(self: Template, allocator: std.mem.Allocator) std.mem.Allocator.Error!Template {
            const name = try allocator.dupe(u8, self.name);
            errdefer allocator.free(name);

            return .{
                .name = name,
                .parameters = try util.slice.deepDupe(allocator, self.parameters),
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
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

        pub fn deinit(self: Tuple, allocator: std.mem.Allocator) void {
            if (self.types) |types| {
                for (types) |t| {
                    t.deinit(allocator);
                }

                allocator.free(types);
            }
        }

        pub fn dupe(self: Tuple, allocator: std.mem.Allocator) std.mem.Allocator.Error!Tuple {
            return .{
                .types = try util.slice.deepDupe(allocator, self.types),
                .parens = self.parens,
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
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

                pub fn deinit(self: Long, allocator: std.mem.Allocator) void {
                    if (self.types) |types| {
                        for (types) |t| {
                            t.deinit(allocator);
                        }

                        allocator.free(types);
                    }
                }

                pub fn dupe(self: Long, allocator: std.mem.Allocator) std.mem.Allocator.Error!Long {
                    return .{
                        .types = try util.slice.deepDupe(allocator, self.types),
                        .variadic = self.variadic,
                    };
                }
            };

            long: Long,
            short: *const FQLType,

            pub fn deinit(self: Parameters, allocator: std.mem.Allocator) void {
                switch (self) {
                    .long => |long| long.deinit(allocator),
                    .short => |short| {
                        short.deinit(allocator);
                        allocator.destroy(short);
                    },
                }
            }

            pub fn dupe(self: Parameters, allocator: std.mem.Allocator) std.mem.Allocator.Error!Parameters {
                return switch (self) {
                    .long => |long| .{ .long = try long.dupe(allocator) },
                    .short => |short| .{ .short = try short.dupePtr(allocator) },
                };
            }
        };

        parameters: Parameters,
        return_type: *const FQLType,

        pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
            self.parameters.deinit(allocator);
            self.return_type.deinit(allocator);
            allocator.destroy(self.return_type);
        }

        pub fn dupe(self: Function, allocator: std.mem.Allocator) std.mem.Allocator.Error!Function {
            const parameters = try self.parameters.dupe(allocator);
            errdefer parameters.deinit(allocator);

            return .{
                .parameters = parameters,
                .return_type = try self.return_type.dupePtr(allocator),
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
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

    pub fn dupe(self: FQLType, allocator: std.mem.Allocator) std.mem.Allocator.Error!FQLType {
        return switch (self) {
            inline .named, .string_literal, .number_literal => |name, tag| @unionInit(FQLType, @tagName(tag), try allocator.dupe(u8, name)),
            inline .object, .@"union", .template, .tuple, .function => |obj, tag| @unionInit(FQLType, @tagName(tag), try obj.dupe(allocator)),
            inline .optional, .isolated => |optional, tag| @unionInit(FQLType, @tagName(tag), try optional.dupePtr(allocator)),
        };
    }

    fn dupePtr(self: *const FQLType, allocator: std.mem.Allocator) !*FQLType {
        const ptr = try allocator.create(FQLType);
        errdefer allocator.destroy(ptr);

        ptr.* = try self.dupe(allocator);

        return ptr;
    }

    pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
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

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try self.printCanonical(writer);
    }

    pub fn toCanonicalString(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
        var str = std.ArrayList(u8).init(allocator);
        defer str.deinit();

        try self.printCanonical(str.writer().any());

        return try str.toOwnedSlice();
    }

    pub const Parser = parsing.ManagedParser(struct {
        const State = union(enum) {
            const Tuple = struct {
                types: std.ArrayListUnmanaged(FQLType) = .{},
                parens: bool = false,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    for (self.types.items) |fql_type| {
                        fql_type.deinit(allocator);
                    }

                    @constCast(&self.types).deinit(allocator);
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

                    @constCast(&self.fields).deinit(allocator);
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

                    @constCast(&self.parameters).deinit(allocator);
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
                    if (self.variadic_state) |variadic_state| {
                        switch (variadic_state) {
                            .start => {},
                            inline .after_type, .after_rparen => |t| t.deinit(allocator),
                        }
                    }

                    for (self.parameters) |parameter| {
                        parameter.deinit(allocator);
                    }

                    allocator.free(self.parameters);
                }
            };

            empty,
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
                    inline .tuple, .object, .template, .union_lhs, .short_function, .long_function, .end => |state| state.deinit(allocator),
                    .empty => {},
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

        inline fn finalizeType(self: *@This(), fql_type: FQLType) void {
            self.state = .{ .end = fql_type };
        }

        fn startChildState(self: *@This(), allocator: std.mem.Allocator) !void {
            self.* = .{ .parent = try util.mem.createCopy(@This(), allocator, self) };
        }

        pub const PushResult = struct {
            save: ?Tokenizer.Token = null,
            type: ?FQLType = null,
        };

        pub fn pushToken(self: *@This(), allocator: std.mem.Allocator, token: Tokenizer.Token) !PushResult {
            if (token == .comment_block or token == .comment_line or (token == .eol and self.state != .end and self.state != .identifier)) {
                return .{};
            }

            switch (self.state) {
                .empty => switch (token) {
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
                        switch (token) {
                            .word => |word| {
                                object_state.state = .{
                                    .after_key = .{
                                        .identifier = try allocator.dupe(u8, word),
                                    },
                                };
                            },
                            .string => |str| {
                                object_state.state = .{
                                    .after_key = .{
                                        .string = try allocator.dupe(u8, str),
                                    },
                                };
                            },
                            .asterisk => object_state.state = .{
                                .after_key = .wildcard,
                            },
                            .rbrace => {
                                self.state = .{
                                    .end = .{
                                        .object = .{
                                            .fields = try object_state.fields.toOwnedSlice(allocator),
                                        },
                                    },
                                };
                            },
                            else => {
                                std.log.err("unexpected token: expected word, string or asterisk but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
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
                            try object_state.fields.append(allocator, field);

                            self.state = .{
                                .end = .{
                                    .object = .{
                                        .fields = try object_state.fields.toOwnedSlice(allocator),
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
    });

    pub const parse = @as(fn (allocator: std.mem.Allocator, it: *Tokenizer.TokenIterator) anyerror!?@This(), Parser.parseIterator);
};

pub const parseType = FQLType.Parser.parseReader;

fn expectParsedTypeEqual(str: []const u8, expected: FQLType) !void {
    var stream = std.io.fixedBufferStream(str);
    var actual = (try parseType(testing.allocator, stream.reader().any())).?;
    defer actual.deinit(testing.allocator);

    try testing.expectEqualDeep(expected, actual);

    const canonical_string = try actual.toCanonicalString(testing.allocator);
    defer testing.allocator.free(canonical_string);

    try testing.expectEqualStrings(str, canonical_string);

    try parsing.checkForLeaks(FQLType.Parser, str);

    try parsing.testDupe(expected);
    try parsing.testDupe(actual);
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
