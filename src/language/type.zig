const std = @import("std");
const testing = std.testing;

const sourcemap = @import("../sourcemap.zig");
const Tokenizer = @import("../Tokenizer.zig");
const util = @import("../util.zig");
const parsing = @import("../parsing.zig");
const common = @import("../common.zig");

const TextNode = common.TextNode;
const Position = common.Position;
const SourceLocation = common.SourceLocation;

pub const FQLType = union(enum) {
    pub const Object = struct {
        pub const Field = struct {
            const Key = union(enum) {
                identifier: TextNode,
                string: TextNode,
                wildcard: ?SourceLocation,

                pub fn deinit(self: Key, allocator: std.mem.Allocator) void {
                    switch (self) {
                        inline .identifier, .string => |s| s.deinit(allocator),
                        else => {},
                    }
                }

                pub fn dupe(self: Key, allocator: std.mem.Allocator) std.mem.Allocator.Error!Key {
                    return switch (self) {
                        inline .identifier, .string => |s, tag| @unionInit(
                            @This(),
                            @tagName(tag),
                            try s.dupe(allocator),
                        ),
                        .wildcard => |loc| .{ .wildcard = loc },
                    };
                }

                fn location(self: Key) ?SourceLocation {
                    return switch (self) {
                        .wildcard => |loc| loc,
                        inline else => |k| k.location,
                    };
                }
            };

            key: Key,
            type: FQLType,
            location: ?SourceLocation = null,
            colon_position: ?Position = null,

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
                    .location = self.location,
                    .colon_position = self.colon_position,
                };
            }

            pub fn printCanonical(self: Field, writer: anytype) @TypeOf(writer).Error!void {
                switch (self.key) {
                    .wildcard => |key_loc| {
                        if (key_loc) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                        }

                        try writer.writeByte('*');
                    },
                    .identifier => |ident| try ident.printNamedCanonical(writer),
                    .string => |str| try str.printCanonical(writer),
                }

                if (self.location) |loc| {
                    if (self.colon_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                    }
                }

                try writer.writeByte(':');

                if (self.location) |loc| {
                    if (self.colon_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                    }
                }

                try writer.writeByte(' ');

                try self.type.printCanonical(writer);
            }
        };

        fields: ?[]const Field = null,
        location: ?SourceLocation = null,
        comma_positions: ?[]const Position = null,

        pub fn deinit(self: Object, allocator: std.mem.Allocator) void {
            if (self.fields) |fields| {
                for (fields) |field| {
                    field.deinit(allocator);
                }

                allocator.free(fields);
            }

            if (self.comma_positions) |comma_positions| {
                allocator.free(comma_positions);
            }
        }

        pub fn dupe(self: Object, allocator: std.mem.Allocator) std.mem.Allocator.Error!Object {
            return .{
                .fields = try util.slice.deepDupe(allocator, self.fields),
                .location = self.location,
                .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
            };
        }

        pub fn printCanonical(self: Object, writer: anytype) @TypeOf(writer).Error!void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte('{');

            if (self.fields) |fields| {
                if (fields.len > 0) {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(1), null);
                    }

                    try writer.writeByte(' ');

                    for (fields, 0..) |field, i| {
                        if (i > 0) {
                            if (self.location) |loc| {
                                if (self.comma_positions) |commas| {
                                    if (commas.len >= i) {
                                        sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1], null);
                                    }
                                }
                            }

                            try writer.writeByte(',');

                            if (self.location) |loc| {
                                if (self.comma_positions) |commas| {
                                    if (commas.len >= i) {
                                        sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1].bump(1), null);
                                    }
                                }
                            }

                            try writer.writeByte(' ');
                        }

                        try field.printCanonical(writer);
                    }

                    if (fields[fields.len - 1].location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte(' ');
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('}');
        }
    };

    pub const Union = struct {
        lhs: *const FQLType,
        rhs: *const FQLType,
        location: ?SourceLocation = null,
        pipe_position: ?Position = null,

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
                .location = self.location,
                .pipe_position = self.pipe_position,
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
            try self.lhs.printCanonical(writer);

            if (self.lhs.location()) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.pipe_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('|');

            if (self.location) |loc| {
                if (self.pipe_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                }
            }

            try writer.writeByte(' ');

            try self.rhs.printCanonical(writer);
        }
    };

    pub const Template = struct {
        name: TextNode,
        parameters: ?[]const FQLType = null,
        location: ?SourceLocation = null,
        larrow_position: ?Position = null,
        comma_positions: ?[]const Position = null,

        pub fn deinit(self: Template, allocator: std.mem.Allocator) void {
            if (self.parameters) |parameters| {
                for (parameters) |parameter| {
                    parameter.deinit(allocator);
                }

                allocator.free(parameters);
            }

            self.name.deinit(allocator);

            if (self.comma_positions) |comma_positions| {
                allocator.free(comma_positions);
            }
        }

        pub fn dupe(self: Template, allocator: std.mem.Allocator) std.mem.Allocator.Error!Template {
            const name = try self.name.dupe(allocator);
            errdefer name.deinit(allocator);

            return .{
                .name = name,
                .parameters = try util.slice.deepDupe(allocator, self.parameters),
                .location = self.location,
                .larrow_position = self.larrow_position,
                .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
            try self.name.printCanonical(writer);

            if (self.parameters) |parameters| {
                if (self.location) |loc| {
                    if (self.larrow_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                    }
                }

                try writer.writeByte('<');

                for (parameters, 0..) |parameter, i| {
                    if (i > 0) {
                        if (self.location) |loc| {
                            if (self.comma_positions) |commas| {
                                if (commas.len >= i) {
                                    sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1], null);
                                }
                            }
                        }

                        try writer.writeByte(',');

                        if (self.location) |loc| {
                            if (self.comma_positions) |commas| {
                                if (commas.len >= i) {
                                    sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1].bump(1), null);
                                }
                            }
                        }

                        try writer.writeByte(' ');
                    }

                    try parameter.printCanonical(writer);
                }

                if (self.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                }

                try writer.writeByte('>');
            }
        }
    };

    pub const Tuple = struct {
        types: ?[]const FQLType = null,
        parens: bool = false,
        location: ?SourceLocation = null,
        comma_positions: ?[]const Position = null,

        pub fn deinit(self: Tuple, allocator: std.mem.Allocator) void {
            if (self.types) |types| {
                for (types) |t| {
                    t.deinit(allocator);
                }

                allocator.free(types);
            }

            if (self.comma_positions) |comma_positions| {
                allocator.free(comma_positions);
            }
        }

        pub fn dupe(self: Tuple, allocator: std.mem.Allocator) std.mem.Allocator.Error!Tuple {
            return .{
                .types = try util.slice.deepDupe(allocator, self.types),
                .parens = self.parens,
                .location = self.location,
                .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
            };
        }

        pub fn printCanonical(self: @This(), writer: anytype) @TypeOf(writer).Error!void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte('[');

            if (self.types) |types| {
                for (types, 0..) |fql_type, i| {
                    if (i > 0) {
                        if (self.location) |loc| {
                            if (self.comma_positions) |commas| {
                                if (commas.len >= i) {
                                    sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1], null);
                                }
                            }
                        }

                        try writer.writeByte(',');

                        if (self.location) |loc| {
                            if (self.comma_positions) |commas| {
                                if (commas.len >= i) {
                                    sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1].bump(1), null);
                                }
                            }
                        }

                        try writer.writeByte(' ');
                    }

                    try fql_type.printCanonical(writer);
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte(']');
        }
    };

    pub const Function = struct {
        pub const Parameters = union(enum) {
            pub const Long = struct {
                types: ?[]const FQLType = null,
                variadic: bool = false,
                location: ?SourceLocation = null,
                comma_positions: ?[]const Position = null,
                dot3_position: ?Position = null,

                pub fn deinit(self: Long, allocator: std.mem.Allocator) void {
                    if (self.types) |types| {
                        for (types) |t| {
                            t.deinit(allocator);
                        }

                        allocator.free(types);
                    }

                    if (self.comma_positions) |comma_positions| {
                        allocator.free(comma_positions);
                    }
                }

                pub fn dupe(self: Long, allocator: std.mem.Allocator) std.mem.Allocator.Error!Long {
                    return .{
                        .types = try util.slice.deepDupe(allocator, self.types),
                        .variadic = self.variadic,
                        .location = self.location,
                        .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
                        .dot3_position = self.dot3_position,
                    };
                }

                pub fn printCanonical(self: Long, writer: anytype) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeByte('(');

                    if (self.types) |types| {
                        for (types, 0..) |fql_type, i| {
                            if (i > 0) {
                                if (self.location) |loc| {
                                    if (self.comma_positions) |commas| {
                                        if (commas.len >= i) {
                                            sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1], null);
                                        }
                                    }
                                }

                                try writer.writeByte(',');

                                if (self.location) |loc| {
                                    if (self.comma_positions) |commas| {
                                        if (commas.len >= i) {
                                            sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1].bump(1), null);
                                        }
                                    }
                                }

                                try writer.writeByte(' ');
                            }

                            if (i == types.len - 1 and self.variadic) {
                                if (self.location) |loc| {
                                    if (self.dot3_position) |pos| {
                                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                    }
                                }

                                try writer.writeAll("...");
                            }

                            try fql_type.printCanonical(writer);
                        }
                    }

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                    }

                    try writer.writeByte(')');
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

            pub fn location(self: Parameters) ?SourceLocation {
                return switch (self) {
                    .long => |long| long.location,
                    .short => |short| short.location(),
                };
            }

            pub fn printCanonical(self: Parameters, writer: anytype) @TypeOf(writer).Error!void {
                switch (self) {
                    inline else => |p| try p.printCanonical(writer),
                }
            }
        };

        parameters: Parameters,
        return_type: *const FQLType,
        location: ?SourceLocation = null,
        equal_rarrow_position: ?Position = null,

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
                .location = self.location,
                .equal_rarrow_position = self.equal_rarrow_position,
            };
        }

        pub fn printCanonical(self: Function, writer: anytype) @TypeOf(writer).Error!void {
            try self.parameters.printCanonical(writer);

            if (self.parameters.location()) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.equal_rarrow_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeAll("=>");

            if (self.location) |loc| {
                if (self.equal_rarrow_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(2), null);
                }
            }

            try writer.writeByte(' ');

            try self.return_type.printCanonical(writer);
        }
    };

    pub const Optional = struct {
        type: *const FQLType,
        location: ?SourceLocation = null,

        pub fn deinit(self: Optional, allocator: std.mem.Allocator) void {
            self.type.deinit(allocator);
            allocator.destroy(self.type);
        }

        pub fn dupe(self: Optional, allocator: std.mem.Allocator) std.mem.Allocator.Error!Optional {
            return .{
                .type = try self.type.dupePtr(allocator),
                .location = self.location,
            };
        }

        pub fn printCanonical(self: Optional, writer: anytype) @TypeOf(writer).Error!void {
            try self.type.printCanonical(writer);

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('?');
        }
    };

    pub const Isolated = struct {
        type: *const FQLType,
        location: ?SourceLocation = null,

        pub fn deinit(self: Isolated, allocator: std.mem.Allocator) void {
            self.type.deinit(allocator);
            allocator.destroy(self.type);
        }

        pub fn dupe(self: Isolated, allocator: std.mem.Allocator) std.mem.Allocator.Error!Isolated {
            return .{
                .type = try self.type.dupePtr(allocator),
                .location = self.location,
            };
        }

        pub fn printCanonical(self: Isolated, writer: anytype) @TypeOf(writer).Error!void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte('(');

            try self.type.printCanonical(writer);

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte(')');
        }
    };

    named: TextNode,
    object: Object,
    @"union": Union,
    optional: Optional,
    template: Template,
    tuple: Tuple,
    string_literal: TextNode,
    number_literal: TextNode,
    function: Function,
    isolated: Isolated,

    pub fn location(self: FQLType) ?SourceLocation {
        return switch (self) {
            inline else => |t| t.location,
        };
    }

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |obj| obj.deinit(allocator),
        }
    }

    pub fn dupe(self: FQLType, allocator: std.mem.Allocator) std.mem.Allocator.Error!FQLType {
        return switch (self) {
            inline else => |obj, tag| @unionInit(FQLType, @tagName(tag), try obj.dupe(allocator)),
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
            .named => |named| try named.printNamedCanonical(writer),
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
                start: Position,
                types: std.ArrayListUnmanaged(FQLType) = .{},
                parens: bool = false,
                comma_positions: std.ArrayListUnmanaged(Position) = .{},

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    for (self.types.items) |fql_type| {
                        fql_type.deinit(allocator);
                    }

                    @constCast(&self.types).deinit(allocator);
                    @constCast(&self.comma_positions).deinit(allocator);
                }
            };

            const Object = struct {
                start: Position,
                fields: std.ArrayListUnmanaged(FQLType.Object.Field) = .{},
                state: union(enum) {
                    before_key,
                    after_key: FQLType.Object.Field.Key,
                    after_colon: struct {
                        key: FQLType.Object.Field.Key,
                        colon_position: Position,
                    },
                    after_type: FQLType.Object.Field,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self) {
                            .before_key => {},
                            .after_colon => |after_colon| after_colon.key.deinit(allocator),
                            inline else => |s| s.deinit(allocator),
                        }
                    }
                } = .before_key,
                comma_positions: std.ArrayListUnmanaged(Position) = .{},

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    for (self.fields.items) |field| {
                        field.deinit(allocator);
                    }

                    @constCast(&self.fields).deinit(allocator);
                    @constCast(&self.comma_positions).deinit(allocator);
                    self.state.deinit(allocator);
                }
            };

            const Template = struct {
                start: Position,
                name: TextNode,
                larrow_position: Position,
                parameters: std.ArrayListUnmanaged(FQLType) = .{},
                comma_positions: std.ArrayListUnmanaged(Position) = .{},

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    for (self.parameters.items) |parameter| {
                        parameter.deinit(allocator);
                    }

                    @constCast(&self.parameters).deinit(allocator);
                    @constCast(&self.comma_positions).deinit(allocator);
                    self.name.deinit(allocator);
                }
            };

            const LongFunction = struct {
                start: Position,
                parameters: []const FQLType,
                param_end: ?Position,
                comma_positions: []const Position,
                variadic_state: ?union(enum) {
                    start,
                    after_type: FQLType,
                    after_rparen: FQLType,
                } = null,
                dot3_position: ?Position,
                equal_rarrow_position: ?Position,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.variadic_state) |variadic_state| {
                        switch (variadic_state) {
                            .start => {},
                            inline else => |t| t.deinit(allocator),
                        }
                    }

                    for (self.parameters) |parameter| {
                        parameter.deinit(allocator);
                    }

                    allocator.free(self.parameters);
                    allocator.free(self.comma_positions);
                }
            };

            const ShortFunction = struct {
                param_type: FQLType,
                equal_rarrow_position: Position,

                fn deinit(self: ShortFunction, allocator: std.mem.Allocator) void {
                    self.param_type.deinit(allocator);
                }
            };

            const UnionLHS = struct {
                type: FQLType,
                pipe_position: Position,

                fn deinit(self: UnionLHS, allocator: std.mem.Allocator) void {
                    self.type.deinit(allocator);
                }
            };

            empty,
            identifier: TextNode,
            union_lhs: UnionLHS,
            tuple: State.Tuple,
            object: State.Object,
            template: State.Template,
            short_function: ShortFunction,
            long_function: LongFunction,
            end: FQLType,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    .empty => {},
                    inline else => |state| state.deinit(allocator),
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
            save: ?Tokenizer.TokenWithLocation = null,
            type: ?FQLType = null,
        };

        pub fn pushToken(self: *@This(), allocator: std.mem.Allocator, token_with_location: Tokenizer.TokenWithLocation) !PushResult {
            const token = token_with_location.token;
            const loc = token_with_location.location.?;
            if (token == .comment_block or token == .comment_line or (token == .eol and self.state != .end and self.state != .identifier)) {
                return .{};
            }

            switch (self.state) {
                .empty => switch (token) {
                    .string => |str| {
                        self.finalizeType(.{
                            .string_literal = .{
                                .text = try allocator.dupe(u8, str),
                                .location = loc,
                            },
                        });
                    },
                    .number => |num| {
                        self.finalizeType(.{
                            .number_literal = .{
                                .text = try allocator.dupe(u8, num),
                                .location = loc,
                            },
                        });
                    },
                    .word => |word| {
                        self.state = .{
                            .identifier = .{
                                .text = try allocator.dupe(u8, word),
                                .location = loc,
                            },
                        };
                    },
                    .lbrace => {
                        self.state = .{
                            .object = .{
                                .start = loc.start,
                            },
                        };
                    },
                    .lbracket => {
                        self.state = .{
                            .tuple = .{
                                .start = loc.start,
                            },
                        };
                        try self.startChildState(allocator);
                    },
                    .lparen => {
                        self.state = .{
                            .tuple = .{
                                .parens = true,
                                .start = loc.start,
                            },
                        };
                        try self.startChildState(allocator);
                    },
                    else => {
                        if (self.parent) |parent| {
                            if (token == .dot3 and parent.state == .tuple and parent.state.tuple.parens) {
                                var tuple = parent.state.tuple;

                                parent.state = .{
                                    .long_function = .{
                                        .start = parent.state.tuple.start,
                                        .parameters = try tuple.types.toOwnedSlice(allocator),
                                        .param_end = null, // this is a placeholder
                                        .variadic_state = .start,
                                        .comma_positions = try tuple.comma_positions.toOwnedSlice(allocator),
                                        .dot3_position = loc.start,
                                        .equal_rarrow_position = null,
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
                        self.state = .{
                            .template = .{
                                .start = identifier.location.?.start,
                                .name = identifier,
                                .larrow_position = loc.start,
                            },
                        };
                        try self.startChildState(allocator);
                    },
                    else => {
                        self.finalizeType(.{ .named = identifier });
                        return .{ .save = token_with_location };
                    },
                },
                .tuple => |*tuple| switch (token) {
                    .comma => {
                        try tuple.comma_positions.append(allocator, loc.start);
                        try self.startChildState(allocator);
                    },
                    else => {
                        if ((tuple.parens and token == .rparen) or (!tuple.parens and token == .rbracket)) {
                            if (tuple.types.items.len == 1) {
                                self.finalizeType(.{
                                    .isolated = blk: {
                                        defer tuple.types.deinit(allocator);

                                        break :blk .{
                                            .type = try util.mem.createCopy(FQLType, allocator, &tuple.types.items[0]),
                                            .location = .{
                                                .source = loc.source,
                                                .start = tuple.start,
                                                .end = loc.end,
                                            },
                                        };
                                    },
                                });
                            } else {
                                self.finalizeType(.{
                                    .tuple = .{
                                        .types = try tuple.types.toOwnedSlice(allocator),
                                        .parens = tuple.parens,
                                        .location = .{
                                            .source = loc.source,
                                            .start = tuple.start,
                                            .end = loc.end,
                                        },
                                        .comma_positions = try tuple.comma_positions.toOwnedSlice(allocator),
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
                                        .identifier = .{
                                            .text = try allocator.dupe(u8, word),
                                            .location = loc,
                                        },
                                    },
                                };
                            },
                            .string => |str| {
                                object_state.state = .{
                                    .after_key = .{
                                        .string = .{
                                            .text = try allocator.dupe(u8, str),
                                            .location = loc,
                                        },
                                    },
                                };
                            },
                            .asterisk => object_state.state = .{
                                .after_key = .{ .wildcard = loc },
                            },
                            .rbrace => {
                                self.finalizeType(.{
                                    .object = .{
                                        .fields = try object_state.fields.toOwnedSlice(allocator),
                                        .location = .{
                                            .source = loc.source,
                                            .start = object_state.start,
                                            .end = loc.end,
                                        },
                                        .comma_positions = try object_state.comma_positions.toOwnedSlice(allocator),
                                    },
                                });
                            },
                            else => {
                                std.log.err("unexpected token: expected word, string or asterisk but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
                    },
                    .after_key => |key| switch (token) {
                        .colon => {
                            object_state.state = .{
                                .after_colon = .{
                                    .key = key,
                                    .colon_position = loc.start,
                                },
                            };

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
                            try object_state.comma_positions.append(allocator, loc.start);
                            object_state.state = .before_key;
                        },
                        .rbrace => {
                            try object_state.fields.append(allocator, field);

                            self.finalizeType(.{
                                .object = .{
                                    .fields = try object_state.fields.toOwnedSlice(allocator),
                                    .location = .{
                                        .source = loc.source,
                                        .start = object_state.start,
                                        .end = loc.end,
                                    },
                                    .comma_positions = try object_state.comma_positions.toOwnedSlice(allocator),
                                },
                            });
                        },
                        else => {
                            std.log.err("unexpected token: expected comma or rbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    else => unreachable,
                },
                .template => |*template| switch (token) {
                    .comma => {
                        try template.comma_positions.append(allocator, loc.start);
                        try self.startChildState(allocator);
                    },
                    .rarrow => {
                        self.finalizeType(.{
                            .template = .{
                                .name = template.name,
                                .parameters = try template.parameters.toOwnedSlice(allocator),
                                .location = .{
                                    .source = loc.source,
                                    .start = template.start,
                                    .end = loc.end,
                                },
                                .larrow_position = template.larrow_position,
                                .comma_positions = try template.comma_positions.toOwnedSlice(allocator),
                            },
                        });
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
                                        long_function.param_end = loc.end;
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
                                        long_function.equal_rarrow_position = loc.start;
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
                            self.finalizeType(.{
                                .optional = .{
                                    .type = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                    .location = .{
                                        .source = loc.source,
                                        .start = fql_type.location().?.start,
                                        .end = loc.end,
                                    },
                                },
                            });
                            return .{};
                        },
                        .pipe => {
                            self.state = .{
                                .union_lhs = .{
                                    .type = fql_type,
                                    .pipe_position = loc.start,
                                },
                            };
                            try self.startChildState(allocator);
                            return .{};
                        },
                        .equal_rarrow => {
                            self.state = .{
                                .short_function = .{
                                    .param_type = fql_type,
                                    .equal_rarrow_position = loc.start,
                                },
                            };
                            try self.startChildState(allocator);
                            return .{};
                        },
                        else => {},
                    }

                    if (token == .equal_rarrow) {
                        if (fql_type == .tuple) {
                            self.state = .{
                                .long_function = .{
                                    .start = fql_type.tuple.location.?.start,
                                    .parameters = fql_type.tuple.types.?,
                                    .param_end = fql_type.tuple.location.?.end,
                                    .equal_rarrow_position = loc.start,
                                    .comma_positions = fql_type.tuple.comma_positions.?,
                                    .dot3_position = null,
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
                                std.debug.assert(object_state.state == .after_colon);

                                const after_colon = object_state.state.after_colon;

                                object_state.state = .{
                                    .after_type = .{
                                        .key = after_colon.key,
                                        .type = fql_type,
                                        .location = .{
                                            .source = loc.source,
                                            .start = after_colon.key.location().?.start,
                                            .end = fql_type.location().?.end,
                                        },
                                        .colon_position = after_colon.colon_position,
                                    },
                                };
                            },
                            .template => |*template_state| {
                                try template_state.parameters.append(allocator, fql_type);
                            },
                            .union_lhs => |lhs| {
                                parent.finalizeType(.{
                                    .@"union" = .{
                                        .lhs = try util.mem.createCopy(FQLType, allocator, &lhs.type),
                                        .rhs = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                        .location = .{
                                            .source = loc.source,
                                            .start = lhs.type.location().?.start,
                                            .end = loc.end,
                                        },
                                        .pipe_position = lhs.pipe_position,
                                    },
                                });
                            },
                            .short_function => |short_function| {
                                parent.finalizeType(.{
                                    .function = .{
                                        .parameters = .{ .short = try util.mem.createCopy(FQLType, allocator, &short_function.param_type) },
                                        .return_type = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                        .location = .{
                                            .source = loc.source,
                                            .start = short_function.param_type.location().?.start,
                                            .end = loc.end,
                                        },
                                        .equal_rarrow_position = short_function.equal_rarrow_position,
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
                                                            .location = .{
                                                                .source = loc.source,
                                                                .start = long_function.start,
                                                                .end = long_function.param_end.?,
                                                            },
                                                            .comma_positions = long_function.comma_positions,
                                                            .dot3_position = long_function.dot3_position,
                                                        },
                                                    },
                                                    .return_type = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = long_function.start,
                                                        .end = loc.end,
                                                    },
                                                    .equal_rarrow_position = long_function.equal_rarrow_position,
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
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = long_function.start,
                                                        .end = long_function.param_end.?,
                                                    },
                                                },
                                            },
                                            .return_type = try util.mem.createCopy(FQLType, allocator, &fql_type),
                                            .location = .{
                                                .source = loc.source,
                                                .start = long_function.start,
                                                .end = loc.end,
                                            },
                                            .equal_rarrow_position = long_function.equal_rarrow_position.?,
                                        },
                                    });
                                }
                            },
                            else => std.debug.panic("invalid parser parent state: {s}", .{@tagName(parent.state)}),
                        }

                        return .{ .save = token_with_location };
                    }

                    defer self.* = .{};

                    return .{ .save = token_with_location, .type = fql_type };
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
    var actual = (try FQLType.Parser.parseString(testing.allocator, str, null)).?;
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
    try expectParsedTypeEqual("String", .{
        .named = .{
            .text = "String",
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 6,
                    .line = 0,
                    .column = 6,
                },
            },
        },
    });
    try expectParsedTypeEqual("Array<Date>", .{
        .template = .{
            .name = .{
                .text = "Array",
                .location = .{
                    .start = .{
                        .offset = 0,
                        .line = 0,
                        .column = 0,
                    },
                    .end = .{
                        .offset = 5,
                        .line = 0,
                        .column = 5,
                    },
                },
            },
            .parameters = &[_]FQLType{
                .{
                    .named = .{
                        .text = "Date",
                        .location = .{
                            .start = .{
                                .offset = 6,
                                .line = 0,
                                .column = 6,
                            },
                            .end = .{
                                .offset = 10,
                                .line = 0,
                                .column = 10,
                            },
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 11,
                    .line = 0,
                    .column = 11,
                },
            },
            .larrow_position = .{
                .offset = 5,
                .line = 0,
                .column = 5,
            },
            .comma_positions = &.{},
        },
    });
    try expectParsedTypeEqual("{ foo: Uuid, \"bar\": Boolean, *: Any }", .{
        .object = .{
            .fields = &[_]FQLType.Object.Field{
                .{
                    .key = .{
                        .identifier = .{
                            .text = "foo",
                            .location = .{
                                .start = .{
                                    .offset = 2,
                                    .line = 0,
                                    .column = 2,
                                },
                                .end = .{
                                    .offset = 5,
                                    .line = 0,
                                    .column = 5,
                                },
                            },
                        },
                    },
                    .type = .{
                        .named = .{
                            .text = "Uuid",
                            .location = .{
                                .start = .{
                                    .offset = 7,
                                    .line = 0,
                                    .column = 7,
                                },
                                .end = .{
                                    .offset = 11,
                                    .line = 0,
                                    .column = 11,
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 2,
                            .line = 0,
                            .column = 2,
                        },
                        .end = .{
                            .offset = 11,
                            .line = 0,
                            .column = 11,
                        },
                    },
                    .colon_position = .{
                        .offset = 5,
                        .line = 0,
                        .column = 5,
                    },
                },
                .{
                    .key = .{
                        .string = .{
                            .text = "\"bar\"",
                            .location = .{
                                .start = .{
                                    .offset = 13,
                                    .line = 0,
                                    .column = 13,
                                },
                                .end = .{
                                    .offset = 18,
                                    .line = 0,
                                    .column = 18,
                                },
                            },
                        },
                    },
                    .type = .{
                        .named = .{
                            .text = "Boolean",
                            .location = .{
                                .start = .{
                                    .offset = 20,
                                    .line = 0,
                                    .column = 20,
                                },
                                .end = .{
                                    .offset = 27,
                                    .line = 0,
                                    .column = 27,
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 13,
                            .line = 0,
                            .column = 13,
                        },
                        .end = .{
                            .offset = 27,
                            .line = 0,
                            .column = 27,
                        },
                    },
                    .colon_position = .{
                        .offset = 18,
                        .line = 0,
                        .column = 18,
                    },
                },
                .{
                    .key = .{
                        .wildcard = .{
                            .start = .{
                                .offset = 29,
                                .line = 0,
                                .column = 29,
                            },
                            .end = .{
                                .offset = 30,
                                .line = 0,
                                .column = 30,
                            },
                        },
                    },
                    .type = .{
                        .named = .{
                            .text = "Any",
                            .location = .{
                                .start = .{
                                    .offset = 32,
                                    .line = 0,
                                    .column = 32,
                                },
                                .end = .{
                                    .offset = 35,
                                    .line = 0,
                                    .column = 35,
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 29,
                            .line = 0,
                            .column = 29,
                        },
                        .end = .{
                            .offset = 35,
                            .line = 0,
                            .column = 35,
                        },
                    },
                    .colon_position = .{
                        .offset = 30,
                        .line = 0,
                        .column = 30,
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 37,
                    .line = 0,
                    .column = 37,
                },
            },
            .comma_positions = &.{
                .{
                    .offset = 11,
                    .line = 0,
                    .column = 11,
                },
                .{
                    .offset = 27,
                    .line = 0,
                    .column = 27,
                },
            },
        },
    });
    try expectParsedTypeEqual("[Bytes, Number]", .{
        .tuple = .{
            .types = &[_]FQLType{
                .{
                    .named = .{
                        .text = "Bytes",
                        .location = .{
                            .start = .{
                                .offset = 1,
                                .line = 0,
                                .column = 1,
                            },
                            .end = .{
                                .offset = 6,
                                .line = 0,
                                .column = 6,
                            },
                        },
                    },
                },
                .{
                    .named = .{
                        .text = "Number",
                        .location = .{
                            .start = .{
                                .offset = 8,
                                .line = 0,
                                .column = 8,
                            },
                            .end = .{
                                .offset = 14,
                                .line = 0,
                                .column = 14,
                            },
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 15,
                    .line = 0,
                    .column = 15,
                },
            },
            .comma_positions = &.{
                .{
                    .offset = 6,
                    .line = 0,
                    .column = 6,
                },
            },
        },
    });
    try expectParsedTypeEqual("Time | Null", .{
        .@"union" = .{
            .lhs = &FQLType{
                .named = .{
                    .text = "Time",
                    .location = .{
                        .start = .{
                            .offset = 0,
                            .line = 0,
                            .column = 0,
                        },
                        .end = .{
                            .offset = 4,
                            .line = 0,
                            .column = 4,
                        },
                    },
                },
            },
            .rhs = &FQLType{
                .named = .{
                    .text = "Null",
                    .location = .{
                        .start = .{
                            .offset = 7,
                            .line = 0,
                            .column = 7,
                        },
                        .end = .{
                            .offset = 11,
                            .line = 0,
                            .column = 11,
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 11,
                    .line = 0,
                    .column = 11,
                },
            },
            .pipe_position = .{
                .offset = 5,
                .line = 0,
                .column = 5,
            },
        },
    });
    try expectParsedTypeEqual("{ x: number } | string", .{
        .@"union" = .{
            .lhs = &FQLType{
                .object = .{
                    .fields = &[_]FQLType.Object.Field{
                        .{
                            .key = .{
                                .identifier = .{
                                    .text = "x",
                                    .location = .{
                                        .start = .{
                                            .offset = 2,
                                            .line = 0,
                                            .column = 2,
                                        },
                                        .end = .{
                                            .offset = 3,
                                            .line = 0,
                                            .column = 3,
                                        },
                                    },
                                },
                            },
                            .type = .{
                                .named = .{
                                    .text = "number",
                                    .location = .{
                                        .start = .{
                                            .offset = 5,
                                            .line = 0,
                                            .column = 5,
                                        },
                                        .end = .{
                                            .offset = 11,
                                            .line = 0,
                                            .column = 11,
                                        },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{
                                    .offset = 2,
                                    .line = 0,
                                    .column = 2,
                                },
                                .end = .{
                                    .offset = 11,
                                    .line = 0,
                                    .column = 11,
                                },
                            },
                            .colon_position = .{
                                .offset = 3,
                                .line = 0,
                                .column = 3,
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 0,
                            .line = 0,
                            .column = 0,
                        },
                        .end = .{
                            .offset = 13,
                            .line = 0,
                            .column = 13,
                        },
                    },
                    .comma_positions = &.{},
                },
            },
            .rhs = &FQLType{
                .named = .{
                    .text = "string",
                    .location = .{
                        .start = .{
                            .offset = 16,
                            .line = 0,
                            .column = 16,
                        },
                        .end = .{
                            .offset = 22,
                            .line = 0,
                            .column = 22,
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 22,
                    .line = 0,
                    .column = 22,
                },
            },
            .pipe_position = .{
                .offset = 14,
                .line = 0,
                .column = 14,
            },
        },
    });
    try expectParsedTypeEqual("'0' | 0", .{
        .@"union" = .{
            .lhs = &FQLType{
                .string_literal = .{
                    .text = "'0'",
                    .location = .{
                        .start = .{
                            .offset = 0,
                            .line = 0,
                            .column = 0,
                        },
                        .end = .{
                            .offset = 3,
                            .line = 0,
                            .column = 3,
                        },
                    },
                },
            },
            .rhs = &FQLType{
                .number_literal = .{
                    .text = "0",
                    .location = .{
                        .start = .{
                            .offset = 6,
                            .line = 0,
                            .column = 6,
                        },
                        .end = .{
                            .offset = 7,
                            .line = 0,
                            .column = 7,
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 7,
                    .line = 0,
                    .column = 7,
                },
            },
            .pipe_position = .{
                .offset = 4,
                .line = 0,
                .column = 4,
            },
        },
    });
    try expectParsedTypeEqual("ID?", .{
        .optional = .{
            .type = &FQLType{
                .named = .{
                    .text = "ID",
                    .location = .{
                        .start = .{
                            .offset = 0,
                            .line = 0,
                            .column = 0,
                        },
                        .end = .{
                            .offset = 2,
                            .line = 0,
                            .column = 2,
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 3,
                    .line = 0,
                    .column = 3,
                },
            },
        },
    });
    try expectParsedTypeEqual("String => String", .{
        .function = .{
            .parameters = .{
                .short = &FQLType{
                    .named = .{
                        .text = "String",
                        .location = .{
                            .start = .{
                                .offset = 0,
                                .line = 0,
                                .column = 0,
                            },
                            .end = .{
                                .offset = 6,
                                .line = 0,
                                .column = 6,
                            },
                        },
                    },
                },
            },
            .return_type = &FQLType{
                .named = .{
                    .text = "String",
                    .location = .{
                        .start = .{
                            .offset = 10,
                            .line = 0,
                            .column = 10,
                        },
                        .end = .{
                            .offset = 16,
                            .line = 0,
                            .column = 16,
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 16,
                    .line = 0,
                    .column = 16,
                },
            },
            .equal_rarrow_position = .{
                .offset = 7,
                .line = 0,
                .column = 7,
            },
        },
    });
    try expectParsedTypeEqual("(String, ...Number) => String", .{
        .function = .{
            .parameters = .{
                .long = .{
                    .types = &[_]FQLType{
                        .{
                            .named = .{
                                .text = "String",
                                .location = .{
                                    .start = .{
                                        .offset = 1,
                                        .line = 0,
                                        .column = 1,
                                    },
                                    .end = .{
                                        .offset = 7,
                                        .line = 0,
                                        .column = 7,
                                    },
                                },
                            },
                        },
                        .{
                            .named = .{
                                .text = "Number",
                                .location = .{
                                    .start = .{
                                        .offset = 12,
                                        .line = 0,
                                        .column = 12,
                                    },
                                    .end = .{
                                        .offset = 18,
                                        .line = 0,
                                        .column = 18,
                                    },
                                },
                            },
                        },
                    },
                    .variadic = true,
                    .location = .{
                        .start = .{
                            .offset = 0,
                            .line = 0,
                            .column = 0,
                        },
                        .end = .{
                            .offset = 19,
                            .line = 0,
                            .column = 19,
                        },
                    },
                    .comma_positions = &.{
                        .{
                            .offset = 7,
                            .line = 0,
                            .column = 7,
                        },
                    },
                    .dot3_position = .{
                        .offset = 9,
                        .line = 0,
                        .column = 9,
                    },
                },
            },
            .return_type = &FQLType{
                .named = .{
                    .text = "String",
                    .location = .{
                        .start = .{
                            .offset = 23,
                            .line = 0,
                            .column = 23,
                        },
                        .end = .{
                            .offset = 29,
                            .line = 0,
                            .column = 29,
                        },
                    },
                },
            },
            .location = .{
                .start = .{
                    .offset = 0,
                    .line = 0,
                    .column = 0,
                },
                .end = .{
                    .offset = 29,
                    .line = 0,
                    .column = 29,
                },
            },
            .equal_rarrow_position = .{
                .offset = 20,
                .line = 0,
                .column = 20,
            },
        },
    });
}
