const std = @import("std");
const testing = std.testing;

const sourcemap = @import("../sourcemap.zig");

const Tokenizer = @import("../Tokenizer.zig");
const util = @import("../util.zig");
const parsing = @import("../parsing.zig");
const common = @import("../common.zig");

const TextNode = common.TextNode;
const BooleanNode = common.BooleanNode;
const Position = common.Position;
const SourceLocation = common.SourceLocation;

const FQLType = @import("type.zig").FQLType;
const FQLExpression = @import("expression.zig").FQLExpression;

pub const SchemaDefinition = union(enum) {
    pub fn TaggedNode(comptime tag: @Type(.EnumLiteral), comptime T: type) type {
        return struct {
            node: T,
            location: ?SourceLocation = null,

            pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                if (@hasDecl(T, "deinit")) {
                    self.node.deinit(allocator);
                }
            }

            pub fn dupe(self: @This(), allocator: std.mem.Allocator) std.mem.Allocator.Error!@This() {
                return .{
                    .node = try self.node.dupe(allocator),
                    .location = self.location,
                };
            }

            pub fn printCanonical(self: @This(), writer: anytype, indent_str: []const u8, level: usize) @TypeOf(writer).Error!void {
                if (self.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                }

                try writer.writeAll(@tagName(tag));

                if (self.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(@intCast(@tagName(tag).len)), null);
                }

                try writer.writeByte(' ');

                const info = @typeInfo(@TypeOf(T.printCanonical)).Fn;
                if (info.params.len == 4) {
                    try self.node.printCanonical(writer, indent_str, level);
                } else if (info.params.len == 3) {
                    try self.node.printCanonical(writer, indent_str);
                } else if (info.params.len == 2) {
                    try self.node.printCanonical(writer);
                }
            }
        };
    }

    pub fn Annotation(comptime tag: @Type(.EnumLiteral)) type {
        return struct {
            value: FQLExpression,
            location: ?SourceLocation = null,
            lparen_position: ?Position = null,

            pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                self.value.deinit(allocator);
            }

            pub fn dupe(self: @This(), allocator: std.mem.Allocator) std.mem.Allocator.Error!@This() {
                return .{
                    .value = try self.value.dupe(allocator),
                    .location = self.location,
                    .lparen_position = self.lparen_position,
                };
            }

            pub fn printCanonical(self: @This(), writer: anytype, indent_str: []const u8, level: usize) @TypeOf(writer).Error!void {
                if (self.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                }

                try writer.writeAll("@" ++ @tagName(tag));

                if (self.location) |loc| {
                    if (self.lparen_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                    }
                }

                try writer.writeByte('(');

                try self.value.printCanonical(writer, indent_str, level + 1);

                if (self.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                }

                try writer.writeByte(')');
            }
        };
    }

    pub const AccessProvider = struct {
        pub const Member = union(enum) {
            pub const Role = struct {
                name: TextNode,
                predicate: ?FQLExpression = null,
                location: ?SourceLocation = null,
                lbrace_position: ?Position = null,
                predicate_position: ?Position = null,

                pub fn deinit(self: Member.Role, allocator: std.mem.Allocator) void {
                    self.name.deinit(allocator);
                    if (self.predicate) |predicate| {
                        predicate.deinit(allocator);
                    }
                }

                pub fn dupe(self: Member.Role, allocator: std.mem.Allocator) std.mem.Allocator.Error!Member.Role {
                    var predicate: ?FQLExpression = null;
                    if (self.predicate) |expr| {
                        predicate = try expr.dupe(allocator);
                    }

                    errdefer if (predicate) |expr| {
                        expr.deinit(allocator);
                    };

                    return .{
                        .name = try self.name.dupe(allocator),
                        .predicate = predicate,
                        .location = self.location,
                        .lbrace_position = self.lbrace_position,
                        .predicate_position = self.predicate_position,
                    };
                }

                pub fn printCanonical(self: Member.Role, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("role");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(4), null);
                    }

                    try writer.writeByte(' ');

                    try self.name.printNamedCanonical(writer);

                    if (self.predicate) |predicate| {
                        if (self.name.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }

                        try writer.writeByte(' ');
                        if (self.location) |loc| {
                            if (self.lbrace_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeByte('{');

                        if (self.location) |loc| {
                            if (self.lbrace_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                            }
                        }

                        try writer.writeByte('\n');

                        try writer.writeBytesNTimes(indent_str, 2);

                        if (self.location) |loc| {
                            if (self.predicate_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeAll("predicate");

                        if (self.name.location) |loc| {
                            if (self.predicate_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(9), null);
                            }
                        }

                        try writer.writeByte(' ');

                        try predicate.printCanonical(writer, indent_str, 2);

                        if (predicate.location()) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }

                        try writer.writeByte('\n');

                        try writer.writeAll(indent_str);

                        if (self.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                        }

                        try writer.writeByte('}');
                    }
                }
            };

            issuer: TaggedNode(.issuer, TextNode),
            jwks_uri: TaggedNode(.jwks_uri, TextNode),
            role: Member.Role,
            ttl: TaggedNode(.ttl, TextNode),

            pub fn deinit(self: Member, allocator: std.mem.Allocator) void {
                switch (self) {
                    inline else => |r| r.deinit(allocator),
                }
            }

            pub fn dupe(self: Member, allocator: std.mem.Allocator) std.mem.Allocator.Error!Member {
                return switch (self) {
                    inline else => |s, tag| @unionInit(Member, @tagName(tag), try s.dupe(allocator)),
                };
            }

            pub fn location(self: Member) ?SourceLocation {
                return switch (self) {
                    inline else => |member| member.location,
                };
            }

            pub fn printCanonical(self: Member, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                switch (self) {
                    .role => |role| try role.printCanonical(writer, indent_str),
                    inline else => |member| try member.printCanonical(writer, indent_str, 1),
                }
            }
        };

        name: TextNode,
        members: ?[]const Member = null,
        location: ?SourceLocation = null,
        provider_position: ?Position = null,
        lbrace_position: ?Position = null,

        pub fn deinit(self: AccessProvider, allocator: std.mem.Allocator) void {
            if (self.members) |members| {
                for (members) |member| {
                    member.deinit(allocator);
                }

                allocator.free(members);
            }

            self.name.deinit(allocator);
        }

        pub fn dupe(self: AccessProvider, allocator: std.mem.Allocator) std.mem.Allocator.Error!AccessProvider {
            const n = try self.name.dupe(allocator);
            errdefer n.deinit(allocator);

            return .{
                .name = n,
                .members = try util.slice.deepDupe(allocator, self.members),
                .location = self.location,
                .provider_position = self.provider_position,
                .lbrace_position = self.lbrace_position,
            };
        }

        pub fn printCanonical(self: AccessProvider, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeAll("access");

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(6), null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.provider_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeAll("provider");

            if (self.location) |loc| {
                if (self.provider_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(8), null);
                }
            }

            try writer.writeByte(' ');

            try self.name.printNamedCanonical(writer);

            if (self.name.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('{');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                }
            }

            try writer.writeByte('\n');

            if (self.members) |members| {
                for (members) |member| {
                    try writer.writeAll(indent_str);
                    try member.printCanonical(writer, indent_str);

                    if (member.location()) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte('\n');
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('}');

            try writer.writeByte('\n');
        }
    };

    pub const Collection = struct {
        pub const Member = union(enum) {
            pub const Field = struct {
                name: TextNode,
                type: FQLType,
                default: ?FQLExpression = null,
                location: ?SourceLocation = null,
                colon_position: ?Position = null,
                equal_position: ?Position = null,

                pub fn deinit(self: Field, allocator: std.mem.Allocator) void {
                    if (self.default) |default| {
                        default.deinit(allocator);
                    }

                    self.type.deinit(allocator);
                    self.name.deinit(allocator);
                }

                pub fn dupe(self: Field, allocator: std.mem.Allocator) std.mem.Allocator.Error!Field {
                    var default: ?FQLExpression = null;
                    if (self.default) |expr| {
                        default = try expr.dupe(allocator);
                    }

                    errdefer if (default) |expr| {
                        expr.deinit(allocator);
                    };

                    const n = try self.name.dupe(allocator);
                    errdefer n.deinit(allocator);

                    return .{
                        .name = n,
                        .type = try self.type.dupe(allocator),
                        .default = default,
                        .location = self.location,
                        .colon_position = self.colon_position,
                        .equal_position = self.equal_position,
                    };
                }

                pub fn printCanonical(self: Field, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    try self.name.printCanonical(writer);

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

                    if (self.default) |default| {
                        if (self.type.location()) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }

                        try writer.writeByte(' ');

                        if (self.location) |loc| {
                            if (self.equal_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeByte('=');

                        if (self.location) |loc| {
                            if (self.equal_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                            }
                        }

                        try writer.writeByte(' ');

                        try default.printCanonical(writer, indent_str, 2);
                    }
                }
            };

            pub const Index = struct {
                pub const Member = struct {
                    pub const Kind = enum { terms, values };

                    kind: Kind,
                    expressions: []const FQLExpression,
                    location: ?SourceLocation = null,
                    lbracket_position: ?Position = null,
                    comma_positions: ?[]const Position = null,

                    pub fn deinit(self: Index.Member, allocator: std.mem.Allocator) void {
                        for (self.expressions) |expr| {
                            expr.deinit(allocator);
                        }

                        allocator.free(self.expressions);

                        if (self.comma_positions) |comma_positions| {
                            allocator.free(comma_positions);
                        }
                    }

                    pub fn dupe(self: Index.Member, allocator: std.mem.Allocator) std.mem.Allocator.Error!Index.Member {
                        return .{
                            .kind = self.kind,
                            .expressions = try util.slice.deepDupe(allocator, self.expressions),
                            .location = self.location,
                            .lbracket_position = self.lbracket_position,
                            .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
                        };
                    }

                    pub fn printCanonical(self: Index.Member, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                        if (self.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                        }

                        try writer.writeAll(@tagName(self.kind));

                        if (self.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(@intCast(@tagName(self.kind).len)), null);
                        }

                        try writer.writeByte(' ');

                        if (self.location) |loc| {
                            if (self.lbracket_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeByte('[');

                        for (self.expressions, 0..) |expr, i| {
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

                            try expr.printCanonical(writer, indent_str, 3);
                        }

                        if (self.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                        }

                        try writer.writeAll("]");
                    }
                };

                name: TextNode,
                members: ?[]const Index.Member = null,
                location: ?SourceLocation = null,
                lbrace_position: ?Position = null,

                pub fn deinit(self: Index, allocator: std.mem.Allocator) void {
                    if (self.members) |members| {
                        for (members) |member| {
                            member.deinit(allocator);
                        }

                        allocator.free(members);
                    }

                    self.name.deinit(allocator);
                }

                pub fn dupe(self: Index, allocator: std.mem.Allocator) std.mem.Allocator.Error!Index {
                    const n = try self.name.dupe(allocator);
                    errdefer n.deinit(allocator);

                    return .{
                        .name = n,
                        .members = try util.slice.deepDupe(allocator, self.members),
                        .location = self.location,
                        .lbrace_position = self.lbrace_position,
                    };
                }

                pub fn printCanonical(self: Index, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("index");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(5), null);
                    }

                    try writer.writeByte(' ');

                    try self.name.printCanonical(writer);

                    if (self.name.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte(' ');

                    if (self.location) |loc| {
                        if (self.lbrace_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('{');

                    if (self.location) |loc| {
                        if (self.lbrace_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                        }
                    }

                    try writer.writeByte('\n');

                    if (self.members) |members| {
                        for (members) |member| {
                            try writer.writeBytesNTimes(indent_str, 2);

                            try member.printCanonical(writer, indent_str);

                            if (member.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte('\n');
                        }
                    }

                    try writer.writeAll(indent_str);

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                    }

                    try writer.writeAll("}");
                }
            };

            pub const Migrations = struct {
                pub const Statement = union(enum) {
                    pub const Backfill = struct {
                        name: FQLExpression,
                        value: FQLExpression,
                        location: ?SourceLocation = null,
                        equal_position: ?Position = null,

                        pub fn deinit(self: Backfill, allocator: std.mem.Allocator) void {
                            self.name.deinit(allocator);
                            self.value.deinit(allocator);
                        }

                        pub fn dupe(self: Backfill, allocator: std.mem.Allocator) std.mem.Allocator.Error!Backfill {
                            const n = try self.name.dupe(allocator);
                            errdefer n.deinit(allocator);

                            return .{
                                .name = n,
                                .value = try self.value.dupe(allocator),
                                .location = self.location,
                                .equal_position = self.equal_position,
                            };
                        }

                        pub fn printCanonical(self: Backfill, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                            }

                            try writer.writeAll("backfill");

                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(8), null);
                            }

                            try writer.writeByte(' ');

                            try self.name.printCanonical(writer, indent_str, 2);

                            if (self.name.location()) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte(' ');

                            if (self.location) |loc| {
                                if (self.equal_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                }
                            }

                            try writer.writeByte('=');

                            if (self.location) |loc| {
                                if (self.equal_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                                }
                            }

                            try writer.writeByte(' ');

                            try self.value.printCanonical(writer, indent_str, 2);
                        }
                    };

                    pub const Move = struct {
                        old_name: FQLExpression,
                        new_name: FQLExpression,
                        location: ?SourceLocation = null,
                        minus_rarrow_position: ?Position = null,

                        pub fn deinit(self: Move, allocator: std.mem.Allocator) void {
                            self.old_name.deinit(allocator);
                            self.new_name.deinit(allocator);
                        }

                        pub fn dupe(self: Move, allocator: std.mem.Allocator) std.mem.Allocator.Error!Move {
                            const old_name = try self.old_name.dupe(allocator);
                            errdefer old_name.deinit(allocator);

                            return .{
                                .old_name = old_name,
                                .new_name = try self.new_name.dupe(allocator),
                                .location = self.location,
                                .minus_rarrow_position = self.minus_rarrow_position,
                            };
                        }

                        pub fn printCanonical(self: Move, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                            }

                            try writer.writeAll("move");

                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(4), null);
                            }

                            try writer.writeByte(' ');

                            try self.old_name.printCanonical(writer, indent_str, 2);

                            if (self.old_name.location()) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte(' ');

                            if (self.location) |loc| {
                                if (self.minus_rarrow_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                }
                            }

                            try writer.writeAll("->");

                            if (self.location) |loc| {
                                if (self.minus_rarrow_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(2), null);
                                }
                            }

                            try writer.writeByte(' ');

                            try self.new_name.printCanonical(writer, indent_str, 2);
                        }
                    };

                    pub const Split = struct {
                        old_name: FQLExpression,
                        new_names: []const FQLExpression,
                        location: ?SourceLocation = null,
                        minus_rarrow_position: ?Position = null,
                        comma_positions: ?[]const Position = null,

                        pub fn deinit(self: Split, allocator: std.mem.Allocator) void {
                            for (self.new_names) |new_name| {
                                new_name.deinit(allocator);
                            }

                            allocator.free(self.new_names);
                            self.old_name.deinit(allocator);

                            if (self.comma_positions) |comma_positions| {
                                allocator.free(comma_positions);
                            }
                        }

                        pub fn dupe(self: Split, allocator: std.mem.Allocator) std.mem.Allocator.Error!Split {
                            const old_name = try self.old_name.dupe(allocator);
                            errdefer old_name.deinit(allocator);

                            return .{
                                .old_name = old_name,
                                .new_names = try util.slice.deepDupe(allocator, self.new_names),
                                .location = self.location,
                                .minus_rarrow_position = self.minus_rarrow_position,
                                .comma_positions = self.comma_positions,
                            };
                        }

                        pub fn printCanonical(self: Split, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                            }

                            try writer.writeAll("split");

                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(5), null);
                            }

                            try writer.writeByte(' ');

                            try self.old_name.printCanonical(writer, indent_str, 2);

                            if (self.old_name.location()) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte(' ');

                            if (self.location) |loc| {
                                if (self.minus_rarrow_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                }
                            }

                            try writer.writeAll("->");

                            if (self.location) |loc| {
                                if (self.minus_rarrow_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(2), null);
                                }
                            }

                            try writer.writeByte(' ');

                            for (self.new_names, 0..) |new_name, i| {
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

                                try new_name.printCanonical(writer, indent_str, 2);
                            }
                        }
                    };

                    add: TaggedNode(.add, FQLExpression),
                    backfill: Backfill,
                    drop: TaggedNode(.drop, FQLExpression),
                    move: Move,
                    move_conflicts: TaggedNode(.move_conflicts, FQLExpression),
                    move_wildcard: TaggedNode(.move_wildcard, FQLExpression),
                    split: Split,

                    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
                        switch (self) {
                            inline else => |e| e.deinit(allocator),
                        }
                    }

                    pub fn dupe(self: Statement, allocator: std.mem.Allocator) std.mem.Allocator.Error!Statement {
                        return switch (self) {
                            inline else => |m, tag| @unionInit(
                                Statement,
                                @tagName(tag),
                                try m.dupe(allocator),
                            ),
                        };
                    }

                    pub fn location(self: Statement) ?SourceLocation {
                        return switch (self) {
                            inline else => |m| m.location,
                        };
                    }

                    pub fn printCanonical(self: Statement, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                        switch (self) {
                            inline .backfill, .move, .split => |member| try member.printCanonical(writer, indent_str),
                            inline else => |member| try member.printCanonical(writer, indent_str, 2),
                        }
                    }
                };

                statements: []const Statement,
                location: ?SourceLocation = null,
                lbrace_position: ?Position = null,

                pub fn deinit(self: Migrations, allocator: std.mem.Allocator) void {
                    for (self.statements) |migration| {
                        migration.deinit(allocator);
                    }

                    allocator.free(self.statements);
                }

                pub fn dupe(self: Migrations, allocator: std.mem.Allocator) std.mem.Allocator.Error!Migrations {
                    return .{
                        .statements = try util.slice.deepDupe(allocator, self.statements),
                        .location = self.location,
                        .lbrace_position = self.lbrace_position,
                    };
                }

                pub fn printCanonical(self: Migrations, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("migrations");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(10), null);
                    }

                    try writer.writeByte(' ');

                    if (self.location) |loc| {
                        if (self.lbrace_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('{');

                    if (self.location) |loc| {
                        if (self.lbrace_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                        }
                    }

                    try writer.writeByte('\n');

                    for (self.statements) |migration| {
                        try writer.writeBytesNTimes(indent_str, 2);
                        try migration.printCanonical(writer, indent_str);

                        if (migration.location()) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }

                        try writer.writeByte('\n');
                    }

                    try writer.writeAll(indent_str);

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                    }

                    try writer.writeByte('}');
                }
            };

            pub const UniqueConstraint = struct {
                terms: ?[]const FQLExpression = null,
                location: ?SourceLocation = null,
                comma_positions: ?[]const Position = null,
                lbracket_position: ?Position = null,

                pub fn deinit(self: UniqueConstraint, allocator: std.mem.Allocator) void {
                    if (self.terms) |terms| {
                        for (terms) |term| {
                            term.deinit(allocator);
                        }

                        allocator.free(terms);
                    }

                    if (self.comma_positions) |comma_positions| {
                        allocator.free(comma_positions);
                    }
                }

                pub fn dupe(self: UniqueConstraint, allocator: std.mem.Allocator) std.mem.Allocator.Error!UniqueConstraint {
                    return .{
                        .terms = try util.slice.deepDupe(allocator, self.terms),
                        .location = self.location,
                        .lbracket_position = self.lbracket_position,
                        .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
                    };
                }

                pub fn printCanonical(self: UniqueConstraint, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("unique");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(6), null);
                    }

                    try writer.writeByte(' ');

                    if (self.location) |loc| {
                        if (self.lbracket_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('[');

                    if (self.terms) |terms| {
                        for (terms, 0..) |term, i| {
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

                            try term.printCanonical(writer, indent_str, 3);
                        }
                    }

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                    }

                    try writer.writeAll("]");
                }
            };

            pub const CheckConstraint = struct {
                name: TextNode,
                predicate: FQLExpression,
                location: ?SourceLocation = null,

                pub fn deinit(self: CheckConstraint, allocator: std.mem.Allocator) void {
                    self.predicate.deinit(allocator);
                    self.name.deinit(allocator);
                }

                pub fn dupe(self: CheckConstraint, allocator: std.mem.Allocator) std.mem.Allocator.Error!CheckConstraint {
                    const n = try self.name.dupe(allocator);
                    errdefer n.deinit(allocator);

                    return .{
                        .name = n,
                        .predicate = try self.predicate.dupe(allocator),
                        .location = self.location,
                    };
                }

                pub fn printCanonical(self: CheckConstraint, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("check");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(5), null);
                    }

                    try writer.writeByte(' ');

                    try self.name.printCanonical(writer);

                    if (self.name.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte(' ');

                    try self.predicate.printCanonical(writer, indent_str, 2);
                }
            };

            pub const ComputedField = struct {
                name: TextNode,
                type: ?FQLType,
                function: FQLExpression,
                location: ?SourceLocation = null,
                colon_position: ?Position = null,
                equal_position: ?Position = null,

                pub fn deinit(self: ComputedField, allocator: std.mem.Allocator) void {
                    if (self.type) |t| {
                        t.deinit(allocator);
                    }

                    self.function.deinit(allocator);
                    self.name.deinit(allocator);
                }

                pub fn dupe(self: ComputedField, allocator: std.mem.Allocator) std.mem.Allocator.Error!ComputedField {
                    const n = try self.name.dupe(allocator);
                    errdefer n.deinit(allocator);

                    var fql_type: ?FQLType = null;
                    if (self.type) |t| {
                        fql_type = try t.dupe(allocator);
                    }

                    errdefer if (fql_type) |t| {
                        t.deinit(allocator);
                    };

                    return .{
                        .name = n,
                        .type = fql_type,
                        .function = try self.function.dupe(allocator),
                        .location = self.location,
                        .colon_position = self.colon_position,
                        .equal_position = self.equal_position,
                    };
                }

                pub fn printCanonical(self: ComputedField, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("compute");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(7), null);
                    }

                    try writer.writeByte(' ');

                    try self.name.printCanonical(writer);

                    if (self.type) |fql_type| {
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

                        try fql_type.printCanonical(writer);

                        if (fql_type.location()) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }
                    } else {
                        if (self.name.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }
                    }

                    try writer.writeByte(' ');

                    if (self.location) |loc| {
                        if (self.equal_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('=');

                    if (self.location) |loc| {
                        if (self.equal_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                        }
                    }

                    try writer.writeByte(' ');

                    try self.function.printCanonical(writer, indent_str, 2);
                }
            };

            field: Field,
            migrations: Migrations,
            history_days: TaggedNode(.history_days, TextNode),
            document_ttls: TaggedNode(.document_ttls, BooleanNode),
            ttl_days: TaggedNode(.ttl_days, TextNode),
            index: Index,
            unique_constraint: UniqueConstraint,
            check_constraint: CheckConstraint,
            computed_field: ComputedField,

            pub fn deinit(self: Member, allocator: std.mem.Allocator) void {
                switch (self) {
                    inline else => |v| v.deinit(allocator),
                }
            }

            pub fn dupe(self: Member, allocator: std.mem.Allocator) std.mem.Allocator.Error!Member {
                return switch (self) {
                    inline else => |v, tag| @unionInit(Member, @tagName(tag), try v.dupe(allocator)),
                };
            }

            pub fn location(self: Member) ?SourceLocation {
                return switch (self) {
                    inline else => |member| member.location,
                };
            }

            pub fn printCanonical(self: Member, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                switch (self) {
                    inline .history_days, .document_ttls, .ttl_days => |member| try member.printCanonical(writer, indent_str, 1),
                    inline else => |prop| try prop.printCanonical(writer, indent_str),
                }
            }
        };

        alias: ?Annotation(.alias) = null,

        name: TextNode,
        members: ?[]const Member = null,
        location: ?SourceLocation = null,
        collection_position: ?Position = null,
        lbrace_position: ?Position = null,

        pub fn deinit(self: Collection, allocator: std.mem.Allocator) void {
            if (self.alias) |alias| {
                alias.deinit(allocator);
            }

            if (self.members) |members| {
                for (members) |member| {
                    member.deinit(allocator);
                }

                allocator.free(members);
            }

            self.name.deinit(allocator);
        }

        pub fn dupe(self: Collection, allocator: std.mem.Allocator) std.mem.Allocator.Error!Collection {
            const alias: ?Annotation(.alias) = if (self.alias) |annotation| try annotation.dupe(allocator) else null;
            errdefer if (alias) |annotation| {
                annotation.deinit(allocator);
            };

            const n = try self.name.dupe(allocator);
            errdefer n.deinit(allocator);

            return .{
                .alias = alias,
                .name = n,
                .members = try util.slice.deepDupe(allocator, self.members),
                .location = self.location,
                .collection_position = self.collection_position,
                .lbrace_position = self.lbrace_position,
            };
        }

        pub fn printCanonical(self: Collection, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
            if (self.alias) |alias| {
                try alias.printCanonical(writer, indent_str, 0);

                if (alias.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                }

                try writer.writeByte('\n');
            }

            if (self.location) |loc| {
                if (self.collection_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeAll("collection");

            if (self.location) |loc| {
                if (self.collection_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(10), null);
                }
            }

            try writer.writeByte(' ');

            try self.name.printCanonical(writer);

            if (self.name.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('{');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                }
            }

            try writer.writeByte('\n');

            if (self.members) |members| {
                for (members) |member| {
                    try writer.writeAll(indent_str);

                    try member.printCanonical(writer, indent_str);

                    if (member.location()) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte('\n');
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('}');
        }
    };

    pub const Role = struct {
        pub const Member = union(enum) {
            pub const Membership = struct {
                collection: TextNode,
                predicate: ?FQLExpression = null,
                location: ?SourceLocation = null,
                lbrace_position: ?Position = null,
                predicate_position: ?Position = null,

                pub fn deinit(self: Membership, allocator: std.mem.Allocator) void {
                    if (self.predicate) |predicate| {
                        predicate.deinit(allocator);
                    }

                    self.collection.deinit(allocator);
                }

                pub fn dupe(self: Membership, allocator: std.mem.Allocator) std.mem.Allocator.Error!Membership {
                    var predicate: ?FQLExpression = null;
                    if (self.predicate) |expr| {
                        predicate = try expr.dupe(allocator);
                    }

                    errdefer if (predicate) |expr| {
                        expr.deinit(allocator);
                    };

                    return .{
                        .collection = try self.collection.dupe(allocator),
                        .predicate = predicate,
                        .location = self.location,
                        .lbrace_position = self.lbrace_position,
                        .predicate_position = self.predicate_position,
                    };
                }

                pub fn printCanonical(self: Membership, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("membership");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(10), null);
                    }

                    try writer.writeByte(' ');

                    try self.collection.printCanonical(writer);

                    if (self.predicate) |predicate| {
                        if (self.collection.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }

                        try writer.writeByte(' ');

                        if (self.location) |loc| {
                            if (self.lbrace_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeByte('{');

                        if (self.location) |loc| {
                            if (self.lbrace_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                            }
                        }

                        try writer.writeByte('\n');
                        try writer.writeBytesNTimes(indent_str, 2);

                        if (self.location) |loc| {
                            if (self.predicate_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeAll("predicate");

                        if (self.location) |loc| {
                            if (self.predicate_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(9), null);
                            }
                        }

                        try writer.writeByte(' ');

                        try predicate.printCanonical(writer, indent_str, 2);

                        if (predicate.location()) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }

                        try writer.writeByte('\n');
                        try writer.writeAll(indent_str);

                        if (self.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                        }

                        try writer.writeByte('}');
                    }
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
                    location: ?SourceLocation = null,
                    lbrace_position: ?Position = null,
                    predicate_position: ?Position = null,

                    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.predicate) |predicate| {
                            predicate.deinit(allocator);
                        }
                    }

                    pub fn dupe(self: @This(), allocator: std.mem.Allocator) std.mem.Allocator.Error!@This() {
                        return .{
                            .action = self.action,
                            .predicate = if (self.predicate) |expr| try expr.dupe(allocator) else null,
                            .location = self.location,
                            .lbrace_position = self.lbrace_position,
                            .predicate_position = self.predicate_position,
                        };
                    }

                    pub fn printCanonical(self: @This(), writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                        if (self.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                        }

                        try writer.writeAll(@tagName(self.action));

                        if (self.predicate) |predicate| {
                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(@intCast(@tagName(self.action).len)), null);
                            }

                            try writer.writeByte(' ');

                            if (self.location) |loc| {
                                if (self.lbrace_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                }
                            }

                            try writer.writeByte('{');

                            if (self.location) |loc| {
                                if (self.lbrace_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                                }
                            }

                            try writer.writeByte('\n');

                            try writer.writeBytesNTimes(indent_str, 3);

                            if (self.location) |loc| {
                                if (self.predicate_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                }
                            }

                            try writer.writeAll("predicate");

                            if (self.location) |loc| {
                                if (self.predicate_position) |pos| {
                                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(9), null);
                                }
                            }

                            try writer.writeByte(' ');

                            try predicate.printCanonical(writer, indent_str, 3);

                            if (predicate.location()) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte('\n');
                            try writer.writeBytesNTimes(indent_str, 2);

                            if (self.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                            }

                            try writer.writeByte('}');
                        }
                    }
                };

                resource: TextNode,
                actions: ?[]const Action = null,
                location: ?SourceLocation = null,
                lbrace_position: ?Position = null,

                pub fn deinit(self: Privileges, allocator: std.mem.Allocator) void {
                    if (self.actions) |actions| {
                        for (actions) |action| {
                            action.deinit(allocator);
                        }

                        allocator.free(actions);
                    }

                    self.resource.deinit(allocator);
                }

                pub fn dupe(self: Privileges, allocator: std.mem.Allocator) std.mem.Allocator.Error!Privileges {
                    const resource = try self.resource.dupe(allocator);
                    errdefer resource.deinit(allocator);

                    return .{
                        .resource = resource,
                        .actions = try util.slice.deepDupe(allocator, self.actions),
                        .location = self.location,
                        .lbrace_position = self.lbrace_position,
                    };
                }

                pub fn printCanonical(self: Privileges, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeAll("privileges");

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(10), null);
                    }

                    try writer.writeByte(' ');

                    try self.resource.printCanonical(writer);

                    if (self.resource.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte(' ');

                    if (self.location) |loc| {
                        if (self.lbrace_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('{');

                    if (self.location) |loc| {
                        if (self.lbrace_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                        }
                    }

                    try writer.writeByte('\n');

                    if (self.actions) |actions| {
                        for (actions) |action| {
                            try writer.writeBytesNTimes(indent_str, 2);

                            try action.printCanonical(writer, indent_str);

                            if (action.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte('\n');
                        }
                    }

                    try writer.writeAll(indent_str);

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                    }

                    try writer.writeByte('}');
                }
            };

            membership: Membership,
            privileges: Privileges,

            pub fn deinit(self: Member, allocator: std.mem.Allocator) void {
                switch (self) {
                    inline else => |d| d.deinit(allocator),
                }
            }

            pub fn dupe(self: Member, allocator: std.mem.Allocator) std.mem.Allocator.Error!Member {
                return switch (self) {
                    inline else => |m, tag| @unionInit(Member, @tagName(tag), try m.dupe(allocator)),
                };
            }

            pub fn printCanonical(self: Member, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
                switch (self) {
                    inline else => |member| try member.printCanonical(writer, indent_str),
                }
            }

            pub fn location(self: Member) ?SourceLocation {
                return switch (self) {
                    inline else => |member| member.location,
                };
            }
        };

        name: TextNode,
        members: ?[]const Member = null,
        location: ?SourceLocation = null,
        lbrace_position: ?Position = null,

        pub fn deinit(self: Role, allocator: std.mem.Allocator) void {
            if (self.members) |members| {
                for (members) |member| {
                    member.deinit(allocator);
                }

                allocator.free(members);
            }

            self.name.deinit(allocator);
        }

        pub fn dupe(self: Role, allocator: std.mem.Allocator) std.mem.Allocator.Error!Role {
            const n = try self.name.dupe(allocator);
            errdefer n.deinit(allocator);

            return .{
                .name = n,
                .members = try util.slice.deepDupe(allocator, self.members),
                .location = self.location,
                .lbrace_position = self.lbrace_position,
            };
        }

        pub fn printCanonical(self: Role, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeAll("role");

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(4), null);
            }

            try writer.writeByte(' ');

            try self.name.printCanonical(writer);

            if (self.name.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('{');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                }
            }

            try writer.writeByte('\n');

            if (self.members) |members| {
                for (members) |member| {
                    try writer.writeAll(indent_str);
                    try member.printCanonical(writer, indent_str);

                    if (member.location()) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte('\n');
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('}');
        }
    };

    pub const Function = struct {
        pub const Parameter = struct {
            name: TextNode,
            type: ?FQLType = null,
            location: ?SourceLocation = null,
            colon_position: ?Position = null,

            pub fn deinit(self: Parameter, allocator: std.mem.Allocator) void {
                if (self.type) |t| {
                    t.deinit(allocator);
                }

                self.name.deinit(allocator);
            }

            pub fn dupe(self: Parameter, allocator: std.mem.Allocator) std.mem.Allocator.Error!Parameter {
                const n = try self.name.dupe(allocator);
                errdefer n.deinit(allocator);

                return .{
                    .name = n,
                    .type = if (self.type) |t| try t.dupe(allocator) else null,
                    .location = self.location,
                    .colon_position = self.colon_position,
                };
            }
        };

        role: ?Annotation(.role) = null,
        alias: ?Annotation(.alias) = null,

        name: TextNode,
        parameters: ?[]const Parameter = null,
        variadic_parameter: ?Parameter = null,
        return_type: ?FQLType = null,
        body: ?[]const FQLExpression = null,
        location: ?SourceLocation = null,
        function_position: ?Position = null,
        lparen_position: ?Position = null,
        comma_positions: ?[]const Position = null,
        dot3_position: ?Position = null,
        rparen_position: ?Position = null,
        colon_position: ?Position = null,
        lbrace_position: ?Position = null,

        pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
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

            if (self.variadic_parameter) |variadic_parameter| {
                variadic_parameter.deinit(allocator);
            }

            if (self.body) |exprs| {
                for (exprs) |expr| {
                    expr.deinit(allocator);
                }

                allocator.free(exprs);
            }

            if (self.comma_positions) |comma_positions| {
                allocator.free(comma_positions);
            }

            self.name.deinit(allocator);
        }

        pub fn dupe(self: Function, allocator: std.mem.Allocator) std.mem.Allocator.Error!Function {
            const alias: ?Annotation(.alias) = if (self.alias) |expr| try expr.dupe(allocator) else null;
            errdefer if (alias) |expr| {
                expr.deinit(allocator);
            };

            const role: ?Annotation(.role) = if (self.role) |expr| try expr.dupe(allocator) else null;
            errdefer if (role) |expr| {
                expr.deinit(allocator);
            };

            const n = try self.name.dupe(allocator);
            errdefer n.deinit(allocator);

            const parameters = try util.slice.deepDupe(allocator, self.parameters);
            errdefer if (parameters) |params| {
                for (params) |param| {
                    param.deinit(allocator);
                }

                allocator.free(params);
            };

            const return_type: ?FQLType = if (self.return_type) |fql_type| try fql_type.dupe(allocator) else null;
            errdefer if (return_type) |fql_type| {
                fql_type.deinit(allocator);
            };

            const variadic_parameter = if (self.variadic_parameter) |param| try param.dupe(allocator) else null;
            errdefer if (variadic_parameter) |param| {
                param.deinit(allocator);
            };

            return .{
                .alias = alias,
                .role = role,
                .name = n,
                .parameters = parameters,
                .variadic_parameter = variadic_parameter,
                .return_type = return_type,
                .body = try util.slice.deepDupe(allocator, self.body),
                .location = self.location,
                .function_position = self.function_position,
                .lparen_position = self.lparen_position,
                .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
                .rparen_position = self.rparen_position,
                .colon_position = self.colon_position,
                .dot3_position = self.dot3_position,
                .lbrace_position = self.lbrace_position,
            };
        }

        pub fn printCanonical(self: Function, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
            if (self.alias) |alias| {
                try alias.printCanonical(writer, indent_str, 0);

                if (alias.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                }

                try writer.writeByte('\n');
            }

            if (self.role) |role| {
                try role.printCanonical(writer, indent_str, 0);

                if (role.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                }

                try writer.writeByte('\n');
            }

            if (self.location) |loc| {
                if (self.function_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeAll("function");

            if (self.location) |loc| {
                if (self.function_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(8), null);
                }
            }

            try writer.writeByte(' ');

            try self.name.printCanonical(writer);

            if (self.location) |loc| {
                if (self.lparen_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('(');

            if (self.parameters) |parameters| {
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

                    try parameter.name.printCanonical(writer);

                    if (parameter.type) |param_type| {
                        if (parameter.location) |loc| {
                            if (parameter.colon_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeByte(':');

                        if (parameter.location) |loc| {
                            if (parameter.colon_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                            }
                        }

                        try writer.writeByte(' ');

                        try param_type.printCanonical(writer);
                    }
                }
            }

            if (self.variadic_parameter) |parameter| {
                if (self.parameters) |other_params| {
                    if (other_params.len > 0) {
                        if (self.location) |loc| {
                            if (self.comma_positions) |commas| {
                                if (commas.len >= other_params.len) {
                                    sourcemap.setNextWriteMapping(writer, loc.source, commas[other_params.len - 1], null);
                                }
                            }
                        }

                        try writer.writeByte(',');

                        if (self.location) |loc| {
                            if (self.comma_positions) |commas| {
                                if (commas.len >= other_params.len) {
                                    sourcemap.setNextWriteMapping(writer, loc.source, commas[other_params.len - 1].bump(1), null);
                                }
                            }
                        }

                        try writer.writeByte(' ');
                    }
                }

                if (self.location) |loc| {
                    if (self.dot3_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                    }
                }

                try writer.writeByteNTimes('.', 3);

                if (self.location) |loc| {
                    if (self.dot3_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(3), null);
                    }
                }

                try parameter.name.printCanonical(writer);

                if (parameter.type) |param_type| {
                    if (parameter.location) |loc| {
                        if (parameter.colon_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte(':');

                    if (parameter.location) |loc| {
                        if (parameter.colon_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                        }
                    }

                    try writer.writeByte(' ');

                    try param_type.printCanonical(writer);
                }
            }

            if (self.location) |loc| {
                if (self.rparen_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte(')');

            if (self.return_type) |return_type| {
                if (self.location) |loc| {
                    if (self.colon_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                    }
                }

                try writer.writeByte(':');

                if (self.location) |loc| {
                    if (self.rparen_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                    }
                }

                try writer.writeByte(' ');

                try return_type.printCanonical(writer);

                if (return_type.location()) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                }
            } else {
                if (self.location) |loc| {
                    if (self.rparen_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                    }
                }
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('{');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                }
            }

            try writer.writeByte('\n');

            if (self.body) |exprs| {
                for (exprs) |expr| {
                    try writer.writeAll(indent_str);
                    try expr.printCanonical(writer, indent_str, 1);

                    if (expr.location()) |field_loc| {
                        sourcemap.setNextWriteMapping(writer, field_loc.source, field_loc.end, null);
                    }

                    try writer.writeByte('\n');
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('}');
        }

        pub const ExprWalker = struct {
            allocator: std.mem.Allocator,
            function: *const Function,
            pos: usize = 0,
            walker: ?FQLExpression.Walker.Unmanaged = null,

            pub fn deinit(self: *ExprWalker) void {
                self.* = undefined;
            }

            pub fn next(self: *ExprWalker) !?*const FQLExpression {
                if (self.walker) |*walker| {
                    if (try walker.next(self.allocator)) |expr| {
                        return expr;
                    } else {
                        walker.deinit(self.allocator);
                        self.walker = null;
                    }
                }

                if (self.function.body) |body| {
                    if (self.pos < body.len) {
                        self.walker = .{ .root = &body[self.pos] };
                        self.pos += 1;

                        return self.next();
                    }
                }

                return null;
            }
        };

        pub fn walkBody(self: *const Function, allocator: std.mem.Allocator) ExprWalker {
            return .{
                .allocator = allocator,
                .function = self,
            };
        }
    };

    access_provider: AccessProvider,
    collection: Collection,
    role: Role,
    function: Function,

    pub fn name(self: SchemaDefinition) []const u8 {
        return switch (self) {
            inline else => |s| s.name.text,
        };
    }

    pub fn deinit(self: SchemaDefinition, allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |d| d.deinit(allocator),
        }
    }

    pub fn dupe(self: SchemaDefinition, allocator: std.mem.Allocator) std.mem.Allocator.Error!SchemaDefinition {
        return switch (self) {
            inline else => |d, tag| @unionInit(SchemaDefinition, @tagName(tag), try d.dupe(allocator)),
        };
    }

    pub fn printCanonical(self: SchemaDefinition, writer: anytype, indent_str: []const u8) @TypeOf(writer).Error!void {
        switch (self) {
            inline else => |def| try def.printCanonical(writer, indent_str),
        }
    }

    pub fn location(self: SchemaDefinition) ?SourceLocation {
        return switch (self) {
            inline else => |def| def.location,
        };
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

    pub const Parser = parsing.ManagedParser(struct {
        const State = union(enum) {
            const Annotation = struct {
                name: TextNode,
                expr_state: union(enum) {
                    start,
                    parsing: struct { lparen_position: Position, parser: FQLExpression.Parser.Unmanaged = .{} },
                    end: struct { lparen_position: Position, expr: FQLExpression },
                } = .start,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self.expr_state) {
                        .start => {},
                        .parsing => |p| p.parser.deinit(allocator),
                        .end => |end| end.expr.deinit(allocator),
                    }

                    self.name.deinit(allocator);
                }
            };

            const AccessProvider = union(enum) {
                start: Position,
                before_name: struct {
                    start_position: Position,
                    provider_position: Position,
                },
                after_name: struct {
                    start_position: Position,
                    provider_position: Position,
                    name: TextNode,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.name.deinit(allocator);
                    }
                },
                body: struct {
                    start_position: Position,
                    provider_position: Position,
                    lbrace_position: Position,
                    name: TextNode,
                    members: std.ArrayListUnmanaged(SchemaDefinition.AccessProvider.Member) = .{},
                    state: union(enum) {
                        empty,
                        issuer: Position,
                        jwks_uri: Position,
                        ttl: Position,
                        role: union(enum) {
                            start: Position,
                            after_name: struct {
                                start_position: Position,
                                name: TextNode,
                            },
                            block: struct {
                                start_position: Position,
                                name: TextNode,
                                lbrace_position: Position,
                            },
                            predicate: struct {
                                start_position: Position,
                                name: TextNode,
                                lbrace_position: Position,
                                predicate_position: Position,
                                expr: FQLExpression.Parser.Unmanaged = .{},
                            },
                            end: struct {
                                start_position: Position,
                                name: TextNode,
                                lbrace_position: Position,
                                predicate_position: Position,
                                expr: FQLExpression,
                            },
                        },
                        end,
                    } = .empty,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.members.items) |member| {
                            member.deinit(allocator);
                        }

                        switch (self.state) {
                            .role => |role| {
                                switch (role) {
                                    .start => {},
                                    inline .after_name, .block => |v| v.name.deinit(allocator),
                                    inline .predicate, .end => |v| {
                                        v.name.deinit(allocator);
                                        v.expr.deinit(allocator);
                                    },
                                }
                            },
                            else => {},
                        }

                        @constCast(&self.members).deinit(allocator);
                        self.name.deinit(allocator);
                    }
                },

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self) {
                        .before_name, .start => {},
                        inline else => |v| v.deinit(allocator),
                    }
                }
            };

            const Collection = struct {
                const Member = union(enum) {
                    empty,
                    field: struct {
                        start_position: Position,
                        name: TextNode,
                        type: ?FQLType = null,
                        colon_position: ?Position = null,
                        equal_position: ?Position = null,
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

                            self.name.deinit(allocator);
                        }
                    },
                    migrations: struct {
                        start_position: Position,
                        statements: std.ArrayListUnmanaged(SchemaDefinition.Collection.Member.Migrations.Statement) = .{},
                        state: union(enum) {
                            before_lbrace,
                            start,

                            first_expr: struct {
                                start_position: Position,
                                tag: std.meta.Tag(SchemaDefinition.Collection.Member.Migrations.Statement),
                                parser: FQLExpression.Parser.Unmanaged = .{},
                            },

                            before_arrow: struct {
                                start_position: Position,
                                tag: enum { move, backfill, split },
                                first_expr: FQLExpression,
                            },

                            move: struct {
                                start_position: Position,
                                first_expr: FQLExpression,
                                parser: FQLExpression.Parser.Unmanaged = .{},
                                minus_rarrow_position: Position,
                            },

                            backfill: struct {
                                start_position: Position,
                                first_expr: FQLExpression,
                                parser: FQLExpression.Parser.Unmanaged = .{},
                                equal_position: Position,
                            },

                            split: struct {
                                start_position: Position,
                                first_expr: FQLExpression,
                                after: std.ArrayListUnmanaged(FQLExpression) = .{},
                                parser: ?FQLExpression.Parser.Unmanaged = .{},
                                minus_rarrow_position: Position,
                                comma_positions: std.ArrayListUnmanaged(Position) = .{},
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

                                        @constCast(&split.comma_positions).deinit(allocator);
                                        @constCast(&split.after).deinit(allocator);
                                        split.first_expr.deinit(allocator);
                                    },
                                }
                            }
                        } = .before_lbrace,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            for (self.statements.items) |statement| {
                                statement.deinit(allocator);
                            }

                            @constCast(&self.statements).deinit(allocator);
                            self.state.deinit(allocator);
                        }
                    },
                    history_days: Position,
                    document_ttls: Position,
                    ttl_days: Position,
                    index: struct {
                        start_position: Position,
                        name: ?TextNode = null,
                        members: std.ArrayListUnmanaged(SchemaDefinition.Collection.Member.Index.Member) = .{},
                        lbrace_position: ?Position = null,
                        state: union(enum) {
                            before_lbrace,
                            start,
                            property: struct {
                                start_position: Position,
                                type: SchemaDefinition.Collection.Member.Index.Member.Kind,
                                fields: std.ArrayListUnmanaged(FQLExpression) = .{},
                                lbracket_position: ?Position = null,
                                comma_positions: std.ArrayListUnmanaged(Position) = .{},
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

                                    @constCast(&self.fields).deinit(allocator);
                                    @constCast(&self.comma_positions).deinit(allocator);
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
                            if (self.name) |n| {
                                n.deinit(allocator);
                            }

                            for (self.members.items) |member| {
                                member.deinit(allocator);
                            }

                            @constCast(&self.members).deinit(allocator);
                            self.state.deinit(allocator);
                        }
                    },
                    unique: struct {
                        start_position: Position,
                        terms: std.ArrayListUnmanaged(FQLExpression) = .{},
                        lbracket_position: ?Position = null,
                        comma_positions: std.ArrayListUnmanaged(Position) = .{},
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

                            @constCast(&self.terms).deinit(allocator);
                            @constCast(&self.comma_positions).deinit(allocator);
                        }
                    },
                    check: struct {
                        start_position: Position,
                        name: ?TextNode = null,
                        parser: FQLExpression.Parser.Unmanaged = .{},

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            if (self.name) |n| {
                                n.deinit(allocator);
                            }

                            self.parser.deinit(allocator);
                        }
                    },
                    compute: struct {
                        start_position: Position,
                        name: ?TextNode = null,
                        type: ?FQLType = null,
                        colon_position: ?Position = null,
                        equal_position: ?Position = null,
                        parser: ?union(enum) {
                            type: FQLType.Parser.Unmanaged,
                            expr: FQLExpression.Parser.Unmanaged,
                        } = null,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            if (self.name) |n| {
                                n.deinit(allocator);
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

                start_position: Position,
                name: ?TextNode = null,
                lbrace_position: ?Position = null,
                members: std.ArrayListUnmanaged(SchemaDefinition.Collection.Member) = .{},
                member_state: ?Member = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.name) |n| {
                        n.deinit(allocator);
                    }

                    if (self.member_state) |state| {
                        state.deinit(allocator);
                    }

                    for (self.members.items) |member| {
                        member.deinit(allocator);
                    }

                    @constCast(&self.members).deinit(allocator);
                }
            };

            const Role = struct {
                start_position: Position,
                name: ?TextNode = null,
                lbrace_position: ?Position = null,
                members: std.ArrayListUnmanaged(SchemaDefinition.Role.Member) = .{},
                member_state: ?union(enum) {
                    empty,
                    membership: Position,
                    membership_collection: struct {
                        start_position: Position,
                        collection: TextNode,
                    },
                    membership_block: struct {
                        start_position: Position,
                        collection: TextNode,
                        lbrace_position: Position,
                        state: union(enum) {
                            start,
                            predicate: struct {
                                predicate_position: Position,
                                parser: FQLExpression.Parser.Unmanaged = .{},
                            },
                            end: struct {
                                predicate_position: Position,
                                expr: FQLExpression,
                            },
                        } = .start,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            switch (self.state) {
                                .start => {},
                                .predicate => |s| s.parser.deinit(allocator),
                                .end => |s| s.expr.deinit(allocator),
                            }

                            self.collection.deinit(allocator);
                        }
                    },
                    privileges: Position,
                    privileges_resource: struct {
                        start_position: Position,
                        resource: TextNode,
                    },
                    privileges_block: struct {
                        start_position: Position,
                        resource: TextNode,
                        lbrace_position: Position,
                        actions: std.ArrayListUnmanaged(SchemaDefinition.Role.Member.Privileges.Action) = .{},
                        state: union(enum) {
                            start,
                            action: struct {
                                start_position: Position,
                                end_position: Position,
                                action: SchemaDefinition.Role.Member.Privileges.Action.Action,
                            },
                            action_block: struct {
                                start_position: Position,
                                action: SchemaDefinition.Role.Member.Privileges.Action.Action,
                                lbrace_position: Position,
                            },
                            action_predicate: struct {
                                start_position: Position,
                                action: SchemaDefinition.Role.Member.Privileges.Action.Action,
                                lbrace_position: Position,
                                predicate_position: Position,
                                parser: FQLExpression.Parser.Unmanaged = .{},
                            },
                            action_end: struct {
                                start_position: Position,
                                action: SchemaDefinition.Role.Member.Privileges.Action.Action,
                                lbrace_position: Position,
                                predicate_position: Position,
                                expr: FQLExpression,
                            },

                            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                                switch (self) {
                                    .start, .action, .action_block => {},
                                    .action_predicate => |action_predicate| action_predicate.parser.deinit(allocator),
                                    .action_end => |action_end| action_end.expr.deinit(allocator),
                                }
                            }
                        } = .start,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            for (self.actions.items) |action| {
                                action.deinit(allocator);
                            }

                            @constCast(&self.actions).deinit(allocator);
                            self.state.deinit(allocator);
                            self.resource.deinit(allocator);
                        }
                    },

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self) {
                            .empty, .membership, .privileges => {},
                            .membership_collection => |membership| membership.collection.deinit(allocator),
                            .privileges_resource => |privileges| privileges.resource.deinit(allocator),
                            inline else => |s| s.deinit(allocator),
                        }
                    }
                } = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.name) |n| {
                        n.deinit(allocator);
                    }

                    if (self.member_state) |state| {
                        state.deinit(allocator);
                    }

                    for (self.members.items) |member| {
                        member.deinit(allocator);
                    }

                    @constCast(&self.members).deinit(allocator);
                }
            };

            const Function = union(enum) {
                const VariadicParameter = struct {
                    dot3_position: Position,
                    param: SchemaDefinition.Function.Parameter,

                    fn deinit(self: VariadicParameter, allocator: std.mem.Allocator) void {
                        self.param.deinit(allocator);
                    }
                };

                start: Position,
                after_name: struct {
                    start_position: Position,
                    name: TextNode,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.name.deinit(allocator);
                    }
                },
                params: struct {
                    start_position: Position,
                    lparen_position: Position,
                    name: TextNode,
                    params: std.ArrayListUnmanaged(SchemaDefinition.Function.Parameter) = .{},
                    variadic_param: ?VariadicParameter = null,
                    comma_positions: std.ArrayListUnmanaged(Position) = .{},
                    param_state: union(enum) {
                        start,
                        after_dot3: Position,
                        after_name: struct { dot3_position: ?Position = null, name: TextNode },
                        after_colon: struct { dot3_position: ?Position = null, name: TextNode, colon_position: Position, type_parser: FQLType.Parser.Unmanaged = .{} },
                        end,
                    } = .start,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        switch (self.param_state) {
                            .start, .end, .after_dot3 => {},
                            .after_name => |after_name| after_name.name.deinit(allocator),
                            .after_colon => |before_type| {
                                before_type.name.deinit(allocator);
                                before_type.type_parser.deinit(allocator);
                            },
                        }

                        for (self.params.items) |param| {
                            param.deinit(allocator);
                        }

                        if (self.variadic_param) |param| {
                            param.deinit(allocator);
                        }

                        @constCast(&self.params).deinit(allocator);
                        @constCast(&self.comma_positions).deinit(allocator);
                        self.name.deinit(allocator);
                    }
                },
                return_type: struct {
                    start_position: Position,
                    lparen_position: Position,
                    comma_positions: []const Position,
                    rparen_position: Position,
                    colon_position: Position,
                    name: TextNode,
                    params: []const SchemaDefinition.Function.Parameter,
                    variadic_param: ?VariadicParameter = null,
                    type_parser: FQLType.Parser.Unmanaged = .{},

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        for (self.params) |param| {
                            param.deinit(allocator);
                        }

                        if (self.variadic_param) |param| {
                            param.deinit(allocator);
                        }

                        self.name.deinit(allocator);
                        allocator.free(self.params);
                        allocator.free(self.comma_positions);
                        self.type_parser.deinit(allocator);
                    }
                },
                before_body: struct {
                    start_position: Position,
                    lparen_position: Position,
                    comma_positions: []const Position,
                    rparen_position: Position,
                    colon_position: ?Position = null,
                    name: TextNode,
                    params: []const SchemaDefinition.Function.Parameter,
                    variadic_param: ?VariadicParameter = null,
                    return_type: ?FQLType = null,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.return_type) |return_type| {
                            return_type.deinit(allocator);
                        }

                        for (self.params) |param| {
                            param.deinit(allocator);
                        }

                        if (self.variadic_param) |param| {
                            param.deinit(allocator);
                        }

                        self.name.deinit(allocator);
                        allocator.free(self.params);
                        allocator.free(self.comma_positions);
                    }
                },
                body: struct {
                    start_position: Position,
                    lparen_position: Position,
                    comma_positions: []const Position,
                    rparen_position: Position,
                    colon_position: ?Position = null,
                    lbrace_position: Position,
                    name: TextNode,
                    params: []const SchemaDefinition.Function.Parameter,
                    variadic_param: ?VariadicParameter = null,
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

                        if (self.variadic_param) |param| {
                            param.deinit(allocator);
                        }

                        for (self.exprs.items) |expr| {
                            expr.deinit(allocator);
                        }

                        @constCast(&self.exprs).deinit(allocator);
                        self.expr_parser.deinit(allocator);
                        self.name.deinit(allocator);
                        allocator.free(self.params);
                        allocator.free(self.comma_positions);
                    }
                },

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self) {
                        .start => {},
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

        annotations: std.ArrayListUnmanaged(struct { name: []const u8, value: FQLExpression, lparen_position: Position, location: SourceLocation }) = .{},
        state: State = .empty,

        pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            for (self.annotations.items) |annotation| {
                annotation.value.deinit(allocator);
                allocator.free(annotation.name);
            }

            @constCast(&self.annotations).deinit(allocator);

            switch (self.state) {
                .empty => {},
                inline else => |s| s.deinit(allocator),
            }
        }

        pub const PushResult = struct {
            save: ?Tokenizer.TokenWithLocation = null,
            definition: ?SchemaDefinition = null,
        };

        inline fn finalizeDefinition(self: *@This(), defn: SchemaDefinition) void {
            self.state = .{ .end = defn };
        }

        pub fn pushToken(self: *@This(), allocator: std.mem.Allocator, token_with_location: Tokenizer.TokenWithLocation) !PushResult {
            // std.debug.print("schema parser state: {s}\n", .{@tagName(self.state)});
            // std.debug.print("got token: {any}\n", .{token});

            const loc = token_with_location.location.?;
            var token = token_with_location.token;
            if (token == .comment_line) {
                token = .eol;
            } else if (token == .comment_block) {
                return .{};
            }

            switch (self.state) {
                .empty => {
                    switch (token) {
                        .eof, .eol => {},
                        .annotation => |annotation| {
                            self.state = .{ .annotation = .{ .name = .{ .text = try allocator.dupe(u8, annotation), .location = loc } } };
                        },
                        .word => |word| {
                            if (std.meta.stringToEnum(enum { access, collection, role, function }, word)) |keyword| {
                                switch (keyword) {
                                    .access => {
                                        self.state = .{ .access_provider = .{ .start = loc.start } };
                                    },
                                    .collection => {
                                        self.state = .{ .collection = .{ .start_position = loc.start } };
                                    },
                                    .role => {
                                        self.state = .{ .role = .{ .start_position = loc.start } };
                                    },
                                    .function => {
                                        self.state = .{ .function = .{ .start = loc.start } };
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
                                    annotation.expr_state = .{
                                        .parsing = .{
                                            .lparen_position = loc.start,
                                        },
                                    };
                                },
                                else => {
                                    std.log.err("unexpected token: expected lparen but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .parsing => |*p| {
                            const res = try p.parser.pushToken(allocator, token_with_location);
                            if (res.expr) |expr| {
                                annotation.expr_state = .{
                                    .end = .{
                                        .lparen_position = p.lparen_position,
                                        .expr = expr,
                                    },
                                };
                            }

                            return .{ .save = res.save };
                        },
                        .end => |end| {
                            switch (token) {
                                .rparen => {
                                    try self.annotations.append(allocator, .{
                                        .name = annotation.name.text,
                                        .value = end.expr,
                                        .lparen_position = end.lparen_position,
                                        .location = .{
                                            .source = loc.source,
                                            .start = annotation.name.location.?.start,
                                            .end = loc.end,
                                        },
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
                        .start => |start_position| {
                            switch (token) {
                                .word => |word| {
                                    if (!std.mem.eql(u8, word, "provider")) {
                                        std.log.err("unexpected token: expected word to equal \"provider\" but got \"{s}\"", .{word});
                                        return error.UnexpectedToken;
                                    }

                                    access_provider.* = .{
                                        .before_name = .{
                                            .start_position = start_position,
                                            .provider_position = loc.start,
                                        },
                                    };
                                },
                                else => {
                                    std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .before_name => |before_name| {
                            switch (token) {
                                inline .string, .word => |s| {
                                    access_provider.* = .{
                                        .after_name = .{
                                            .name = .{
                                                .text = try allocator.dupe(u8, s),
                                                .location = loc,
                                            },
                                            .start_position = before_name.start_position,
                                            .provider_position = before_name.provider_position,
                                        },
                                    };
                                },
                                else => {
                                    std.log.err("unexpected token: expected string or word but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .after_name => |after_name| {
                            switch (token) {
                                .lbrace => {
                                    access_provider.* = .{
                                        .body = .{
                                            .name = after_name.name,
                                            .start_position = after_name.start_position,
                                            .provider_position = after_name.provider_position,
                                            .lbrace_position = loc.start,
                                        },
                                    };
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
                                                        body.state = .{ .issuer = loc.start };
                                                    },
                                                    .jwks_uri => {
                                                        body.state = .{ .jwks_uri = loc.start };
                                                    },
                                                    .role => {
                                                        body.state = .{ .role = .{ .start = loc.start } };
                                                    },
                                                    .ttl => {
                                                        body.state = .{ .ttl = loc.start };
                                                    },
                                                }
                                            } else {
                                                std.log.err("unexpected token: expected word to equal \"issuer\", \"jwks_uri\", \"role\" or \"ttl\" but got \"{s}\"", .{word});
                                                return error.UnexpectedToken;
                                            }
                                        },
                                        .rbrace => {
                                            body.state = .end;
                                            return .{ .save = token_with_location };
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
                                                @unionInit(SchemaDefinition.AccessProvider.Member, @tagName(tag), .{
                                                    .node = .{
                                                        .text = try allocator.dupe(u8, str),
                                                        .location = loc,
                                                    },
                                                }),
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
                                        .start => |start_position| {
                                            switch (token) {
                                                inline .word, .string => |s| {
                                                    role.* = .{
                                                        .after_name = .{
                                                            .name = .{
                                                                .text = try allocator.dupe(u8, s),
                                                                .location = loc,
                                                            },
                                                            .start_position = start_position,
                                                        },
                                                    };
                                                },
                                                else => {
                                                    std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                                    return error.UnexpectedToken;
                                                },
                                            }
                                        },
                                        .after_name => |after_name| {
                                            switch (token) {
                                                .lbrace => {
                                                    role.* = .{
                                                        .block = .{
                                                            .name = after_name.name,
                                                            .start_position = after_name.start_position,
                                                            .lbrace_position = loc.start,
                                                        },
                                                    };
                                                },
                                                .eol, .semi => {
                                                    try body.members.append(allocator, .{
                                                        .role = .{
                                                            .name = after_name.name,
                                                        },
                                                    });

                                                    body.state = .empty;
                                                },
                                                else => {
                                                    std.log.err("unexpected token: expected lbrace, eol or semi but got {s}", .{@tagName(token)});
                                                    return error.UnexpectedToken;
                                                },
                                            }
                                        },
                                        .block => |block| {
                                            switch (token) {
                                                .eol, .semi => {},
                                                .word => |word| {
                                                    if (!std.mem.eql(u8, word, "predicate")) {
                                                        std.log.err("unexpected token: expected word to equal \"predicate\" but got \"{s}\"", .{word});
                                                        return error.UnexpectedToken;
                                                    }

                                                    role.* = .{
                                                        .predicate = .{
                                                            .name = block.name,
                                                            .start_position = block.start_position,
                                                            .lbrace_position = block.lbrace_position,
                                                            .predicate_position = loc.start,
                                                        },
                                                    };
                                                },
                                                else => {
                                                    std.log.err("unexpected token: expected eol, semi or word but got {s}", .{@tagName(token)});
                                                    return error.UnexpectedToken;
                                                },
                                            }
                                        },
                                        .predicate => |*predicate| {
                                            const res = try predicate.expr.pushToken(allocator, token_with_location);
                                            if (res.expr) |expr| {
                                                role.* = .{
                                                    .end = .{
                                                        .name = predicate.name,
                                                        .expr = expr,
                                                        .start_position = predicate.start_position,
                                                        .lbrace_position = predicate.lbrace_position,
                                                        .predicate_position = predicate.predicate_position,
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
                                                            .location = .{
                                                                .source = loc.source,
                                                                .start = end.start_position,
                                                                .end = loc.end,
                                                            },
                                                            .lbrace_position = end.lbrace_position,
                                                            .predicate_position = end.predicate_position,
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
                                            self.finalizeDefinition(.{
                                                .access_provider = .{
                                                    .name = body.name,
                                                    .members = try body.members.toOwnedSlice(allocator),
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = body.start_position,
                                                        .end = loc.end,
                                                    },
                                                    .provider_position = body.provider_position,
                                                    .lbrace_position = body.lbrace_position,
                                                },
                                            });
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
                            inline .string, .word => |n| {
                                collection.name = .{ .text = try allocator.dupe(u8, n), .location = loc };
                            },
                            else => {
                                std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
                    } else if (collection.member_state == null) {
                        switch (token) {
                            .lbrace => {
                                collection.lbrace_position = loc.start;
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
                                            inline .history_days, .document_ttls, .ttl_days => |kw| collection.member_state = @unionInit(State.Collection.Member, @tagName(kw), loc.start),
                                            inline else => |kw| collection.member_state = @unionInit(State.Collection.Member, @tagName(kw), .{ .start_position = loc.start }),
                                        }
                                    } else {
                                        collection.member_state = .{
                                            .field = .{
                                                .start_position = loc.start,
                                                .name = .{
                                                    .text = try allocator.dupe(u8, word),
                                                    .location = loc,
                                                },
                                            },
                                        };
                                    }
                                },
                                .string => |str| {
                                    collection.member_state = .{
                                        .field = .{
                                            .start_position = loc.start,
                                            .name = .{
                                                .text = try allocator.dupe(u8, str),
                                                .location = loc,
                                            },
                                        },
                                    };
                                },
                                .asterisk => {
                                    collection.member_state = .{
                                        .field = .{
                                            .start_position = loc.start,
                                            .name = .{
                                                .text = try allocator.dupe(u8, "*"),
                                                .location = loc,
                                            },
                                        },
                                    };
                                },
                                .rbrace => {
                                    self.finalizeDefinition(.{
                                        .collection = .{
                                            .name = collection.name.?,
                                            .members = try collection.members.toOwnedSlice(allocator),
                                            .location = .{
                                                .source = loc.source,
                                                .start = collection.start_position,
                                                .end = loc.end,
                                            },
                                            .collection_position = collection.start_position,
                                            .lbrace_position = collection.lbrace_position,
                                        },
                                    });
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
                                    field.colon_position = loc.start;
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
                                                .location = .{
                                                    .source = loc.source,
                                                    .start = field.name.location.?.start,
                                                    .end = field.type.?.location().?.end,
                                                },
                                                .colon_position = field.colon_position,
                                            },
                                        });

                                        collection.member_state = .empty;
                                    },
                                    .equal => {
                                        field.equal_position = loc.start;
                                        field.parser = .{ .expr = .{} };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            } else switch (field.parser.?) {
                                .type => |*parser| {
                                    const res = try parser.pushToken(allocator, token_with_location);
                                    if (res.type) |fql_type| {
                                        field.type = fql_type;
                                        field.parser = null;
                                    }

                                    return .{ .save = res.save };
                                },
                                .expr => |*parser| {
                                    const res = try parser.pushToken(allocator, token_with_location);
                                    if (res.expr) |expr| {
                                        try collection.members.append(allocator, .{
                                            .field = .{
                                                .name = field.name,
                                                .type = field.type.?,
                                                .default = expr,
                                                .location = .{
                                                    .source = loc.source,
                                                    .start = field.name.location.?.start,
                                                    .end = expr.location().?.end,
                                                },
                                                .colon_position = field.colon_position,
                                                .equal_position = field.equal_position,
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
                                            try collection.members.append(allocator, .{
                                                .migrations = .{
                                                    .statements = try migrations.statements.toOwnedSlice(allocator),
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = migrations.start_position,
                                                        .end = loc.end,
                                                    },
                                                },
                                            });

                                            collection.member_state = .empty;
                                        },
                                        .word => |word| {
                                            if (std.meta.stringToEnum(std.meta.Tag(Collection.Member.Migrations.Statement), word)) |tag| {
                                                migrations.state = .{ .first_expr = .{ .tag = tag, .start_position = loc.start } };
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
                                    const res = try first_expr.parser.pushToken(allocator, token_with_location);
                                    if (res.expr) |expr| {
                                        switch (first_expr.tag) {
                                            inline .add, .drop, .move_conflicts, .move_wildcard => |tag| {
                                                try migrations.statements.append(
                                                    allocator,
                                                    @unionInit(SchemaDefinition.Collection.Member.Migrations.Statement, @tagName(tag), .{ .node = expr }),
                                                );
                                                migrations.state = .start;
                                            },
                                            inline else => |tag| {
                                                migrations.state = .{
                                                    .before_arrow = .{
                                                        .start_position = first_expr.start_position,
                                                        .tag = std.meta.stringToEnum(@TypeOf(migrations.state.before_arrow.tag), @tagName(tag)).?,
                                                        .first_expr = expr,
                                                    },
                                                };
                                            },
                                        }
                                    }

                                    return .{ .save = res.save };
                                },
                                .before_arrow => |before_arrow| {
                                    switch (before_arrow.tag) {
                                        .backfill => {
                                            if (token != .equal) {
                                                std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            }

                                            migrations.state = .{
                                                .backfill = .{
                                                    .start_position = before_arrow.start_position,
                                                    .first_expr = before_arrow.first_expr,
                                                    .equal_position = loc.start,
                                                },
                                            };
                                        },
                                        inline else => |tag| {
                                            if (token != .minus_rarrow) {
                                                std.log.err("unexpected token: expected minus_rarrow but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            }

                                            migrations.state = @unionInit(
                                                @TypeOf(migrations.state),
                                                @tagName(tag),
                                                .{
                                                    .start_position = before_arrow.start_position,
                                                    .first_expr = before_arrow.first_expr,
                                                    .minus_rarrow_position = loc.start,
                                                },
                                            );
                                        },
                                    }
                                },
                                inline .move, .backfill => |*move_or_backfill, tag| {
                                    const res = try move_or_backfill.parser.pushToken(allocator, token_with_location);
                                    if (res.expr) |expr| {
                                        try migrations.statements.append(
                                            allocator,
                                            switch (tag) {
                                                .move => .{
                                                    .move = .{
                                                        .old_name = move_or_backfill.first_expr,
                                                        .new_name = expr,
                                                        .location = .{
                                                            .source = loc.source,
                                                            .start = move_or_backfill.start_position,
                                                            .end = expr.location().?.end,
                                                        },
                                                        .minus_rarrow_position = move_or_backfill.minus_rarrow_position,
                                                    },
                                                },
                                                .backfill => .{
                                                    .backfill = .{
                                                        .name = move_or_backfill.first_expr,
                                                        .value = expr,
                                                        .location = .{
                                                            .source = loc.source,
                                                            .start = move_or_backfill.start_position,
                                                            .end = expr.location().?.end,
                                                        },
                                                        .equal_position = move_or_backfill.equal_position,
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
                                        const res = try parser.pushToken(allocator, token_with_location);
                                        if (res.expr) |expr| {
                                            try split.after.append(allocator, expr);
                                            split.parser = null;
                                        }

                                        return .{ .save = res.save };
                                    } else {
                                        switch (token) {
                                            .eol, .semi, .rbrace => {
                                                const new_names = try split.after.toOwnedSlice(allocator);

                                                try migrations.statements.append(allocator, .{
                                                    .split = .{
                                                        .old_name = split.first_expr,
                                                        .new_names = new_names,
                                                        .location = .{
                                                            .source = loc.source,
                                                            .start = split.start_position,
                                                            .end = new_names[new_names.len - 1].location().?.end,
                                                        },
                                                        .minus_rarrow_position = split.minus_rarrow_position,
                                                        .comma_positions = try split.comma_positions.toOwnedSlice(allocator),
                                                    },
                                                });

                                                migrations.state = .start;

                                                if (token == .rbrace) {
                                                    return .{ .save = token_with_location };
                                                }
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
                        .history_days => |start_position| {
                            switch (token) {
                                .number => |num| {
                                    try collection.members.append(allocator, .{
                                        .history_days = .{
                                            .node = .{
                                                .text = try allocator.dupe(u8, num),
                                                .location = loc,
                                            },
                                            .location = .{
                                                .source = loc.source,
                                                .start = start_position,
                                                .end = loc.end,
                                            },
                                        },
                                    });

                                    collection.member_state = .empty;
                                },
                                else => {
                                    std.log.err("unexpected token: expected number but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .document_ttls => |start_position| {
                            switch (token) {
                                .word => |word| {
                                    if (std.meta.stringToEnum(enum { true, false }, word)) |keyword| {
                                        try collection.members.append(allocator, .{
                                            .document_ttls = .{
                                                .node = .{ .value = keyword == .true, .location = loc },
                                                .location = .{
                                                    .source = loc.source,
                                                    .start = start_position,
                                                    .end = loc.end,
                                                },
                                            },
                                        });

                                        collection.member_state = .empty;
                                    } else {
                                        std.log.err("unexpected token: expected word to equal \"true\" or \"false\" but got {s}", .{word});
                                        return error.UnexpectedToken;
                                    }
                                },
                                else => {
                                    std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .ttl_days => |start_position| {
                            switch (token) {
                                .number => |num| {
                                    try collection.members.append(allocator, .{
                                        .ttl_days = .{
                                            .node = .{
                                                .text = try allocator.dupe(u8, num),
                                                .location = loc,
                                            },
                                            .location = .{
                                                .source = loc.source,
                                                .start = start_position,
                                                .end = loc.end,
                                            },
                                        },
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
                                    inline .string, .word => |n| {
                                        index.name = .{ .text = try allocator.dupe(u8, n), .location = loc };
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
                                                index.lbrace_position = loc.start;
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
                                                        .location = .{
                                                            .source = loc.source,
                                                            .start = index.start_position,
                                                            .end = loc.end,
                                                        },
                                                        .lbrace_position = index.lbrace_position,
                                                    },
                                                });

                                                collection.member_state = .empty;
                                            },
                                            .word => |word| {
                                                if (std.meta.stringToEnum(@TypeOf(index.state.property.type), word)) |keyword| {
                                                    index.state = .{
                                                        .property = .{
                                                            .start_position = loc.start,
                                                            .type = keyword,
                                                        },
                                                    };
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
                                                        property.lbracket_position = loc.start;
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
                                                        try index.members.append(
                                                            allocator,
                                                            .{
                                                                .kind = property.type,
                                                                .expressions = try property.fields.toOwnedSlice(allocator),
                                                                .location = .{
                                                                    .source = loc.source,
                                                                    .start = property.start_position,
                                                                    .end = loc.end,
                                                                },
                                                                .lbracket_position = property.lbracket_position,
                                                                .comma_positions = try property.comma_positions.toOwnedSlice(allocator),
                                                            },
                                                        );

                                                        index.state = .start;
                                                    },
                                                    else => {
                                                        property.state = .{ .parsing = .{} };
                                                        return .{ .save = token_with_location };
                                                    },
                                                }
                                            },
                                            .parsing => |*parser| {
                                                const res = try parser.pushToken(allocator, token_with_location);
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
                                                        return .{ .save = token_with_location };
                                                    },
                                                    .comma => {
                                                        try property.comma_positions.append(allocator, loc.start);
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
                                            unique.lbracket_position = loc.start;
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
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = unique.start_position,
                                                        .end = loc.end,
                                                    },
                                                    .comma_positions = try unique.comma_positions.toOwnedSlice(allocator),
                                                    .lbracket_position = unique.lbracket_position,
                                                },
                                            });
                                            collection.member_state = .empty;
                                        },
                                        else => {
                                            unique.state = .{ .parsing = .{} };
                                            return .{ .save = token_with_location };
                                        },
                                    }
                                },
                                .parsing => |*parser| {
                                    const res = try parser.pushToken(allocator, token_with_location);
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
                                            return .{ .save = token_with_location };
                                        },
                                        .comma => {
                                            try unique.comma_positions.append(allocator, loc.start);
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
                                    inline .string, .word => |n| {
                                        check.name = .{ .text = try allocator.dupe(u8, n), .location = loc };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            } else {
                                const res = try check.parser.pushToken(allocator, token_with_location);
                                if (res.expr) |expr| {
                                    try collection.members.append(allocator, .{
                                        .check_constraint = .{
                                            .name = check.name.?,
                                            .predicate = expr,
                                            .location = .{
                                                .source = loc.source,
                                                .start = check.start_position,
                                                .end = expr.location().?.end,
                                            },
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
                                    inline .string, .word => |n| {
                                        compute.name = .{ .text = try allocator.dupe(u8, n), .location = loc };
                                    },
                                    else => {
                                        std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                        return error.UnexpectedToken;
                                    },
                                }
                            } else if (compute.parser == null) {
                                if (compute.type == null and token == .colon) {
                                    compute.colon_position = loc.start;
                                    compute.parser = .{ .type = .{} };
                                } else if (token == .equal) {
                                    compute.equal_position = loc.start;
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
                                        const res = try parser.pushToken(allocator, token_with_location);
                                        if (res.type) |fql_type| {
                                            compute.type = fql_type;
                                            compute.parser = null;
                                        }

                                        return .{ .save = res.save };
                                    },
                                    .expr => |*parser| {
                                        const res = try parser.pushToken(allocator, token_with_location);
                                        if (res.expr) |expr| {
                                            try collection.members.append(allocator, .{
                                                .computed_field = .{
                                                    .name = compute.name.?,
                                                    .type = compute.type,
                                                    .function = expr,
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = compute.start_position,
                                                        .end = expr.location().?.end,
                                                    },
                                                    .colon_position = compute.colon_position,
                                                    .equal_position = compute.equal_position,
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
                            inline .string, .word => |n| {
                                role.name = .{ .text = try allocator.dupe(u8, n), .location = loc };
                            },
                            else => {
                                std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
                    } else if (role.member_state == null) {
                        switch (token) {
                            .lbrace => {
                                role.lbrace_position = loc.start;
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
                                            inline else => |kw| role.member_state = @unionInit(@TypeOf(role.member_state.?), @tagName(kw), loc.start),
                                        }
                                    } else {
                                        std.log.err("unexpected token: expected word to equal \"membership\" or \"privileges\" but got {s}", .{word});
                                        return error.UnexpectedToken;
                                    }
                                },
                                .rbrace => {
                                    self.finalizeDefinition(.{
                                        .role = .{
                                            .name = role.name.?,
                                            .members = try role.members.toOwnedSlice(allocator),
                                            .location = .{
                                                .source = loc.source,
                                                .start = role.start_position,
                                                .end = loc.end,
                                            },
                                            .lbrace_position = role.lbrace_position,
                                        },
                                    });
                                },
                                else => {
                                    std.log.err("unexpected token: expected eol, semi or word but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .membership => |start_position| {
                            switch (token) {
                                inline .string, .word => |collection| {
                                    role.member_state = .{
                                        .membership_collection = .{
                                            .collection = .{ .text = try allocator.dupe(u8, collection), .location = loc },
                                            .start_position = start_position,
                                        },
                                    };
                                },
                                else => {
                                    std.log.err("unexpected token: expected string or word but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .membership_collection => |membership| {
                            switch (token) {
                                .eol, .semi => {
                                    try role.members.append(allocator, .{
                                        .membership = .{
                                            .collection = membership.collection,
                                            .location = .{
                                                .source = loc.source,
                                                .start = membership.start_position,
                                                .end = membership.collection.location.?.end,
                                            },
                                        },
                                    });
                                    role.member_state = .empty;
                                },
                                .lbrace => {
                                    role.member_state = .{
                                        .membership_block = .{
                                            .collection = membership.collection,
                                            .start_position = membership.start_position,
                                            .lbrace_position = loc.start,
                                        },
                                    };
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
                                                membership_block.state = .{ .predicate = .{ .predicate_position = loc.start } };
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
                                .predicate => |*predicate| {
                                    const res = try predicate.parser.pushToken(allocator, token_with_location);
                                    if (res.expr) |expr| {
                                        membership_block.state = .{
                                            .end = .{
                                                .expr = expr,
                                                .predicate_position = predicate.predicate_position,
                                            },
                                        };
                                    }
                                    return .{ .save = res.save };
                                },
                                .end => |end| {
                                    switch (token) {
                                        .eol, .semi => {},
                                        .rbrace => {
                                            try role.members.append(allocator, .{
                                                .membership = .{
                                                    .collection = membership_block.collection,
                                                    .predicate = end.expr,
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = membership_block.start_position,
                                                        .end = loc.end,
                                                    },
                                                    .lbrace_position = membership_block.lbrace_position,
                                                    .predicate_position = end.predicate_position,
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
                        .privileges => |start_position| {
                            switch (token) {
                                inline .string, .word => |resource| {
                                    role.member_state = .{
                                        .privileges_resource = .{
                                            .resource = .{ .text = try allocator.dupe(u8, resource), .location = loc },
                                            .start_position = start_position,
                                        },
                                    };
                                },
                                else => {
                                    std.log.err("unexpected token: expected string or word but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .privileges_resource => |privileges| {
                            switch (token) {
                                .lbrace => {
                                    role.member_state = .{
                                        .privileges_block = .{
                                            .resource = privileges.resource,
                                            .start_position = privileges.start_position,
                                            .lbrace_position = loc.start,
                                        },
                                    };
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
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = privileges_block.start_position,
                                                        .end = loc.end,
                                                    },
                                                    .lbrace_position = privileges_block.lbrace_position,
                                                },
                                            });
                                            role.member_state = .empty;
                                        },
                                        .word => |word| {
                                            if (std.meta.stringToEnum(SchemaDefinition.Role.Member.Privileges.Action.Action, word)) |action| {
                                                privileges_block.state = .{
                                                    .action = .{
                                                        .action = action,
                                                        .start_position = loc.start,
                                                        .end_position = loc.end,
                                                    },
                                                };
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
                                            try privileges_block.actions.append(allocator, .{
                                                .action = action.action,
                                                .location = .{
                                                    .source = loc.source,
                                                    .start = action.start_position,
                                                    .end = action.end_position,
                                                },
                                            });
                                            privileges_block.state = .start;
                                        },
                                        .lbrace => {
                                            privileges_block.state = .{
                                                .action_block = .{
                                                    .start_position = action.start_position,
                                                    .action = action.action,
                                                    .lbrace_position = loc.start,
                                                },
                                            };
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
                                                privileges_block.state = .{
                                                    .action_predicate = .{
                                                        .action = action.action,
                                                        .start_position = action.start_position,
                                                        .lbrace_position = action.lbrace_position,
                                                        .predicate_position = loc.start,
                                                    },
                                                };
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
                                    const res = try action_predicate.parser.pushToken(allocator, token_with_location);
                                    if (res.expr) |expr| {
                                        privileges_block.state = .{
                                            .action_end = .{
                                                .action = action_predicate.action,
                                                .expr = expr,
                                                .start_position = action_predicate.start_position,
                                                .lbrace_position = action_predicate.lbrace_position,
                                                .predicate_position = action_predicate.predicate_position,
                                            },
                                        };
                                    }

                                    return .{ .save = res.save };
                                },
                                .action_end => |action_end| {
                                    switch (token) {
                                        .eol, .semi => {},
                                        .rbrace => {
                                            try privileges_block.actions.append(allocator, .{
                                                .action = action_end.action,
                                                .predicate = action_end.expr,
                                                .location = .{
                                                    .source = loc.source,
                                                    .start = action_end.start_position,
                                                    .end = loc.end,
                                                },
                                                .lbrace_position = action_end.lbrace_position,
                                                .predicate_position = action_end.predicate_position,
                                            });
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
                        .start => |start_position| {
                            switch (token) {
                                inline .string, .word => |s| {
                                    function.* = .{
                                        .after_name = .{
                                            .name = .{ .text = try allocator.dupe(u8, s), .location = loc },
                                            .start_position = start_position,
                                        },
                                    };
                                },
                                else => {
                                    std.log.err("unexpected token: expected word or string but got {s}", .{@tagName(token)});
                                    return error.UnexpectedToken;
                                },
                            }
                        },
                        .after_name => |after_name| {
                            switch (token) {
                                .lparen => {
                                    function.* = .{
                                        .params = .{
                                            .name = after_name.name,
                                            .start_position = after_name.start_position,
                                            .lparen_position = loc.start,
                                        },
                                    };
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
                                        .eol => {},
                                        .word => |word| {
                                            params.param_state = .{
                                                .after_name = .{
                                                    .name = .{
                                                        .text = try allocator.dupe(u8, word),
                                                        .location = loc,
                                                    },
                                                },
                                            };
                                        },
                                        .rparen => {
                                            params.param_state = .end;
                                            return .{ .save = token_with_location };
                                        },
                                        .dot3 => {
                                            params.param_state = .{
                                                .after_dot3 = loc.start,
                                            };
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word, dot3 or rparen but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                },
                                .after_dot3 => |p| {
                                    switch (token) {
                                        .word => |word| {
                                            params.param_state = .{
                                                .after_name = .{
                                                    .dot3_position = p,
                                                    .name = .{
                                                        .text = try allocator.dupe(u8, word),
                                                        .location = loc,
                                                    },
                                                },
                                            };
                                        },
                                        else => {
                                            std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                            return error.UnexpectedToken;
                                        },
                                    }
                                },
                                .after_name => |after_name| {
                                    if (after_name.dot3_position) |dot3_position| {
                                        switch (token) {
                                            .colon => {
                                                params.param_state = .{
                                                    .after_colon = .{
                                                        .dot3_position = dot3_position,
                                                        .name = after_name.name,
                                                        .colon_position = loc.start,
                                                    },
                                                };
                                            },
                                            .rparen => {
                                                try params.params.append(allocator, .{
                                                    .name = after_name.name,
                                                    .location = after_name.name.location,
                                                });
                                                params.param_state = .end;
                                                return .{ .save = token_with_location };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected colon or rparen but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    } else {
                                        switch (token) {
                                            .colon => {
                                                params.param_state = .{
                                                    .after_colon = .{
                                                        .dot3_position = after_name.dot3_position,
                                                        .name = after_name.name,
                                                        .colon_position = loc.start,
                                                    },
                                                };
                                            },
                                            .rparen, .comma => {
                                                try params.params.append(allocator, .{
                                                    .name = after_name.name,
                                                    .location = after_name.name.location,
                                                });
                                                params.param_state = .end;
                                                return .{ .save = token_with_location };
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected colon, rparen or comma but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    }
                                },
                                .after_colon => |*after_colon| {
                                    const res = try after_colon.type_parser.pushToken(allocator, token_with_location);
                                    if (res.type) |fql_type| {
                                        if (after_colon.dot3_position) |dot3_position| {
                                            params.variadic_param = .{
                                                .dot3_position = dot3_position,
                                                .param = .{
                                                    .name = after_colon.name,
                                                    .type = fql_type,
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = after_colon.name.location.?.start,
                                                        .end = fql_type.location().?.end,
                                                    },
                                                    .colon_position = after_colon.colon_position,
                                                },
                                            };
                                        } else {
                                            try params.params.append(allocator, .{
                                                .name = after_colon.name,
                                                .type = fql_type,
                                                .location = .{
                                                    .source = loc.source,
                                                    .start = after_colon.name.location.?.start,
                                                    .end = fql_type.location().?.end,
                                                },
                                                .colon_position = after_colon.colon_position,
                                            });
                                        }

                                        params.param_state = .end;
                                    }

                                    return .{ .save = res.save };
                                },
                                .end => {
                                    if (params.variadic_param) |variadic_param| {
                                        switch (token) {
                                            .rparen => {
                                                const new_state: State.Function = .{
                                                    .before_body = .{
                                                        .name = params.name,
                                                        .params = try params.params.toOwnedSlice(allocator),
                                                        .start_position = params.start_position,
                                                        .lparen_position = params.lparen_position,
                                                        .comma_positions = try params.comma_positions.toOwnedSlice(allocator),
                                                        .rparen_position = loc.start,
                                                        .variadic_param = variadic_param,
                                                    },
                                                };
                                                function.* = new_state;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected rparen but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    } else {
                                        switch (token) {
                                            .comma => {
                                                try params.comma_positions.append(allocator, loc.start);
                                                params.param_state = .start;
                                            },
                                            .rparen => {
                                                const new_state: State.Function = .{
                                                    .before_body = .{
                                                        .name = params.name,
                                                        .params = try params.params.toOwnedSlice(allocator),
                                                        .start_position = params.start_position,
                                                        .lparen_position = params.lparen_position,
                                                        .comma_positions = try params.comma_positions.toOwnedSlice(allocator),
                                                        .rparen_position = loc.start,
                                                    },
                                                };
                                                function.* = new_state;
                                            },
                                            else => {
                                                std.log.err("unexpected token: expected rparen or comma but got {s}", .{@tagName(token)});
                                                return error.UnexpectedToken;
                                            },
                                        }
                                    }
                                },
                            }
                        },
                        .return_type => |*return_type| {
                            const res = try return_type.type_parser.pushToken(allocator, token_with_location);
                            if (res.type) |fql_type| {
                                const new_state: State.Function = .{
                                    .before_body = .{
                                        .name = return_type.name,
                                        .params = return_type.params,
                                        .variadic_param = return_type.variadic_param,
                                        .return_type = fql_type,
                                        .start_position = return_type.start_position,
                                        .lparen_position = return_type.lparen_position,
                                        .comma_positions = return_type.comma_positions,
                                        .rparen_position = return_type.rparen_position,
                                        .colon_position = return_type.colon_position,
                                    },
                                };
                                function.* = new_state;
                            }

                            return .{ .save = res.save };
                        },
                        .before_body => |before_body| {
                            if (before_body.return_type == null and token == .colon) {
                                const new_state: State.Function = .{
                                    .return_type = .{
                                        .name = before_body.name,
                                        .params = before_body.params,
                                        .variadic_param = before_body.variadic_param,
                                        .start_position = before_body.start_position,
                                        .lparen_position = before_body.lparen_position,
                                        .comma_positions = before_body.comma_positions,
                                        .rparen_position = before_body.rparen_position,
                                        .colon_position = loc.start,
                                    },
                                };
                                function.* = new_state;
                            } else if (token == .lbrace) {
                                function.* = .{
                                    .body = .{
                                        .name = before_body.name,
                                        .params = before_body.params,
                                        .variadic_param = before_body.variadic_param,
                                        .return_type = before_body.return_type,
                                        .start_position = before_body.start_position,
                                        .lparen_position = before_body.lparen_position,
                                        .comma_positions = before_body.comma_positions,
                                        .rparen_position = before_body.rparen_position,
                                        .colon_position = before_body.colon_position,
                                        .lbrace_position = loc.start,
                                    },
                                };
                            } else {
                                if (before_body.return_type == null) {
                                    std.log.err("unexpected token: expected colon or lbrace but got {s}", .{@tagName(token)});
                                } else {
                                    std.log.err("unexpected token: expected lbrace but got {s}", .{@tagName(token)});
                                }

                                return error.UnexpectedToken;
                            }
                        },
                        .body => |*body| {
                            if (body.expr_parser.state == .empty and body.expr_parser.parent == null and token == .rbrace) {
                                self.finalizeDefinition(.{
                                    .function = .{
                                        .name = body.name,
                                        .parameters = body.params,
                                        .variadic_parameter = if (body.variadic_param) |param| param.param else null,
                                        .dot3_position = if (body.variadic_param) |param| param.dot3_position else null,
                                        .return_type = body.return_type,
                                        .body = try body.exprs.toOwnedSlice(allocator),
                                        .location = .{
                                            .source = loc.source,
                                            .start = body.start_position,
                                            .end = loc.end,
                                        },
                                        .function_position = body.start_position,
                                        .lparen_position = body.lparen_position,
                                        .comma_positions = body.comma_positions,
                                        .rparen_position = body.rparen_position,
                                        .colon_position = body.colon_position,
                                        .lbrace_position = body.lbrace_position,
                                    },
                                });
                            } else if (body.expr_parser.state == .empty and body.expr_parser.parent == null and token == .eof) {
                                std.log.err("unexpected token: eof", .{});
                                return error.UnexpectedToken;
                            } else {
                                const res = try body.expr_parser.pushToken(allocator, token_with_location);
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

                            var new_start_position: ?Position = null;
                            for (self.annotations.items) |annotation| {
                                defer allocator.free(annotation.name);

                                if (std.meta.fields(E).len > 0) {
                                    if (std.meta.stringToEnum(E, annotation.name)) |n| {
                                        switch (n) {
                                            inline else => |tag| {
                                                @field(def, @tagName(tag)) = .{
                                                    .value = annotation.value,
                                                    .location = annotation.location,
                                                    .lparen_position = annotation.lparen_position,
                                                };
                                            },
                                        }
                                        if (new_start_position) |pos| {
                                            if (annotation.location.start.offset < pos.offset) {
                                                new_start_position = annotation.location.start;
                                            }
                                        } else {
                                            new_start_position = annotation.location.start;
                                        }
                                        continue;
                                    }
                                }

                                annotation.value.deinit(allocator);
                                std.log.warn("unknown {s} annotation: @{s}", .{ @tagName(def_tag), annotation.name });
                            }

                            if (new_start_position) |pos| {
                                def.location.?.start = pos;
                            }
                        },
                    }

                    defer self.state = .empty;

                    return .{ .save = token_with_location, .definition = definition.* };
                },
            }

            return .{};
        }
    });

    pub const parse = @as(fn (allocator: std.mem.Allocator, it: *Tokenizer.TokenIterator) anyerror!?@This(), Parser.parseIterator);
};

pub const parseDefinition = SchemaDefinition.Parser.parseReader;

fn expectParsedDefnEqual(str: []const u8, expected: SchemaDefinition) !void {
    var stream = std.io.fixedBufferStream(str);
    var actual = (try parseDefinition(testing.allocator, stream.reader().any(), null)).?;
    defer actual.deinit(testing.allocator);

    // std.debug.print("actual: {any}\n", .{actual});

    try testing.expectEqualDeep(expected, actual);

    try parsing.checkForLeaks(SchemaDefinition.Parser, str);

    try parsing.testDupe(expected);
    try parsing.testDupe(actual);
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
    , .{
        .access_provider = .{
            .name = .{
                .text = "someIssuer",
                .location = .{
                    .start = .{ .offset = 16, .line = 0, .column = 16 },
                    .end = .{ .offset = 26, .line = 0, .column = 26 },
                },
            },
            .members = &[_]SchemaDefinition.AccessProvider.Member{
                .{
                    .issuer = .{
                        .node = .{
                            .text = "\"https://example.com/\"",
                            .location = .{
                                .start = .{ .offset = 40, .line = 1, .column = 11 },
                                .end = .{ .offset = 62, .line = 1, .column = 33 },
                            },
                        },
                    },
                },
                .{
                    .jwks_uri = .{
                        .node = .{
                            .text = "\"https://example.com/.well-known/jwks.json\"",
                            .location = .{
                                .start = .{ .offset = 76, .line = 2, .column = 13 },
                                .end = .{ .offset = 119, .line = 2, .column = 56 },
                            },
                        },
                    },
                },
                .{
                    .role = .{
                        .name = .{
                            .text = "customer",
                            .location = .{
                                .start = .{
                                    .offset = 130,
                                    .line = 4,
                                    .column = 9,
                                },
                                .end = .{
                                    .offset = 138,
                                    .line = 4,
                                    .column = 17,
                                },
                            },
                        },
                    },
                },
                .{
                    .role = .{
                        .name = .{
                            .text = "manager",
                            .location = .{
                                .start = .{ .offset = 148, .line = 5, .column = 9 },
                                .end = .{ .offset = 155, .line = 5, .column = 16 },
                            },
                        },
                        .predicate = .{
                            .isolated = .{
                                .expression = &FQLExpression{
                                    .function = .{
                                        .parameters = .{
                                            .short = .{
                                                .text = "jwt",
                                                .location = .{
                                                    .start = .{
                                                        .offset = 177,
                                                        .line = 6,
                                                        .column = 19,
                                                    },
                                                    .end = .{
                                                        .offset = 180,
                                                        .line = 6,
                                                        .column = 22,
                                                    },
                                                },
                                            },
                                        },
                                        .body = &FQLExpression{
                                            .invocation = .{
                                                .function = &FQLExpression{
                                                    .field_access = .{
                                                        .value = &FQLExpression{
                                                            .field_access = .{
                                                                .value = &FQLExpression{
                                                                    .non_null_assertion = .{
                                                                        .expression = &FQLExpression{
                                                                            .identifier = .{
                                                                                .text = "jwt",
                                                                                .location = .{
                                                                                    .start = .{
                                                                                        .offset = 184,
                                                                                        .line = 6,
                                                                                        .column = 26,
                                                                                    },
                                                                                    .end = .{
                                                                                        .offset = 187,
                                                                                        .line = 6,
                                                                                        .column = 29,
                                                                                    },
                                                                                },
                                                                            },
                                                                        },
                                                                        .location = .{
                                                                            .start = .{
                                                                                .offset = 184,
                                                                                .line = 6,
                                                                                .column = 26,
                                                                            },
                                                                            .end = .{
                                                                                .offset = 188,
                                                                                .line = 6,
                                                                                .column = 30,
                                                                            },
                                                                        },
                                                                    },
                                                                },
                                                                .field = .{
                                                                    .identifier = .{
                                                                        .text = "scope",
                                                                        .location = .{
                                                                            .start = .{
                                                                                .offset = 189,
                                                                                .line = 6,
                                                                                .column = 31,
                                                                            },
                                                                            .end = .{
                                                                                .offset = 194,
                                                                                .line = 6,
                                                                                .column = 36,
                                                                            },
                                                                        },
                                                                    },
                                                                },
                                                                .location = .{
                                                                    .start = .{
                                                                        .offset = 184,
                                                                        .line = 6,
                                                                        .column = 26,
                                                                    },
                                                                    .end = .{
                                                                        .offset = 194,
                                                                        .line = 6,
                                                                        .column = 36,
                                                                    },
                                                                },
                                                                .dot_position = .{
                                                                    .offset = 188,
                                                                    .line = 6,
                                                                    .column = 30,
                                                                },
                                                            },
                                                        },
                                                        .field = .{
                                                            .identifier = .{
                                                                .text = "includes",
                                                                .location = .{
                                                                    .start = .{
                                                                        .offset = 195,
                                                                        .line = 6,
                                                                        .column = 37,
                                                                    },
                                                                    .end = .{
                                                                        .offset = 203,
                                                                        .line = 6,
                                                                        .column = 45,
                                                                    },
                                                                },
                                                            },
                                                        },
                                                        .location = .{
                                                            .start = .{
                                                                .offset = 184,
                                                                .line = 6,
                                                                .column = 26,
                                                            },
                                                            .end = .{
                                                                .offset = 203,
                                                                .line = 6,
                                                                .column = 45,
                                                            },
                                                        },
                                                        .dot_position = .{
                                                            .offset = 194,
                                                            .line = 6,
                                                            .column = 36,
                                                        },
                                                    },
                                                },
                                                .arguments = &[_]FQLExpression{
                                                    .{
                                                        .string_literal = .{
                                                            .text = "\"manager\"",
                                                            .location = .{
                                                                .start = .{
                                                                    .offset = 204,
                                                                    .line = 6,
                                                                    .column = 46,
                                                                },
                                                                .end = .{
                                                                    .offset = 213,
                                                                    .line = 6,
                                                                    .column = 55,
                                                                },
                                                            },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{
                                                        .offset = 184,
                                                        .line = 6,
                                                        .column = 26,
                                                    },
                                                    .end = .{
                                                        .offset = 214,
                                                        .line = 6,
                                                        .column = 56,
                                                    },
                                                },
                                                .comma_positions = &.{},
                                                .lparen_position = .{
                                                    .offset = 203,
                                                    .line = 6,
                                                    .column = 45,
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{
                                                .offset = 177,
                                                .line = 6,
                                                .column = 19,
                                            },
                                            .end = .{
                                                .offset = 214,
                                                .line = 6,
                                                .column = 56,
                                            },
                                        },
                                        .equal_rarrow_position = .{
                                            .offset = 181,
                                            .line = 6,
                                            .column = 23,
                                        },
                                    },
                                },
                                .location = .{
                                    .start = .{
                                        .offset = 176,
                                        .line = 6,
                                        .column = 18,
                                    },
                                    .end = .{
                                        .offset = 215,
                                        .line = 6,
                                        .column = 57,
                                    },
                                },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 143, .line = 5, .column = 4 },
                            .end = .{ .offset = 221, .line = 7, .column = 5 },
                        },
                        .lbrace_position = .{ .offset = 156, .line = 5, .column = 17 },
                        .predicate_position = .{ .offset = 166, .line = 6, .column = 8 },
                    },
                },
            },
            .location = .{
                .start = .{ .offset = 0, .line = 0, .column = 0 },
                .end = .{ .offset = 223, .line = 8, .column = 1 },
            },
            .provider_position = .{ .offset = 7, .line = 0, .column = 7 },
            .lbrace_position = .{ .offset = 27, .line = 0, .column = 27 },
        },
    });

    try expectParsedDefnEqual(
        \\function MyFunction(x: Number): Number {
        \\  x + 2
        \\}
    , .{
        .function = .{
            .name = .{
                .text = "MyFunction",
                .location = .{
                    .start = .{ .offset = 9, .line = 0, .column = 9 },
                    .end = .{ .offset = 19, .line = 0, .column = 19 },
                },
            },
            .parameters = &[_]SchemaDefinition.Function.Parameter{
                .{
                    .name = .{
                        .text = "x",
                        .location = .{
                            .start = .{ .offset = 20, .line = 0, .column = 20 },
                            .end = .{ .offset = 21, .line = 0, .column = 21 },
                        },
                    },
                    .type = .{
                        .named = .{
                            .text = "Number",
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
                        .start = .{ .offset = 20, .line = 0, .column = 20 },
                        .end = .{ .offset = 29, .line = 0, .column = 29 },
                    },
                    .colon_position = .{ .offset = 21, .line = 0, .column = 21 },
                },
            },
            .return_type = .{
                .named = .{
                    .text = "Number",
                    .location = .{
                        .start = .{
                            .offset = 32,
                            .line = 0,
                            .column = 32,
                        },
                        .end = .{
                            .offset = 38,
                            .line = 0,
                            .column = 38,
                        },
                    },
                },
            },
            .body = &[_]FQLExpression{
                .{
                    .binary_operation = .{
                        .lhs = &FQLExpression{
                            .identifier = .{
                                .text = "x",
                                .location = .{
                                    .start = .{
                                        .offset = 43,
                                        .line = 1,
                                        .column = 2,
                                    },
                                    .end = .{
                                        .offset = 44,
                                        .line = 1,
                                        .column = 3,
                                    },
                                },
                            },
                        },
                        .operator = .add,
                        .rhs = &FQLExpression{
                            .number_literal = .{
                                .text = "2",
                                .location = .{
                                    .start = .{
                                        .offset = 47,
                                        .line = 1,
                                        .column = 6,
                                    },
                                    .end = .{
                                        .offset = 48,
                                        .line = 1,
                                        .column = 7,
                                    },
                                },
                            },
                        },
                        .location = .{
                            .start = .{
                                .offset = 43,
                                .line = 1,
                                .column = 2,
                            },
                            .end = .{
                                .offset = 48,
                                .line = 1,
                                .column = 7,
                            },
                        },
                        .operator_position = .{
                            .offset = 45,
                            .line = 1,
                            .column = 4,
                        },
                    },
                },
            },
            .location = .{
                .start = .{ .offset = 0, .line = 0, .column = 0 },
                .end = .{ .offset = 50, .line = 2, .column = 1 },
            },
            .lparen_position = .{ .offset = 19, .line = 0, .column = 19 },
            .comma_positions = &.{},
            .rparen_position = .{ .offset = 29, .line = 0, .column = 29 },
            .colon_position = .{ .offset = 30, .line = 0, .column = 30 },
            .lbrace_position = .{ .offset = 39, .line = 0, .column = 39 },
            .function_position = .{ .offset = 0, .line = 0, .column = 0 },
        },
    });

    try expectParsedDefnEqual(
        \\function noparams() {
        \\  "hi"
        \\}
    , .{
        .function = .{
            .name = .{
                .text = "noparams",
                .location = .{
                    .start = .{ .offset = 9, .line = 0, .column = 9 },
                    .end = .{ .offset = 17, .line = 0, .column = 17 },
                },
            },
            .parameters = &.{},
            .body = &[_]FQLExpression{
                .{
                    .string_literal = .{
                        .text = "\"hi\"",
                        .location = .{
                            .start = .{
                                .offset = 24,
                                .line = 1,
                                .column = 2,
                            },
                            .end = .{
                                .offset = 28,
                                .line = 1,
                                .column = 6,
                            },
                        },
                    },
                },
            },
            .location = .{
                .start = .{ .offset = 0, .line = 0, .column = 0 },
                .end = .{ .offset = 30, .line = 2, .column = 1 },
            },
            .lparen_position = .{ .offset = 17, .line = 0, .column = 17 },
            .comma_positions = &.{},
            .rparen_position = .{ .offset = 18, .line = 0, .column = 18 },
            .lbrace_position = .{ .offset = 20, .line = 0, .column = 20 },
            .function_position = .{ .offset = 0, .line = 0, .column = 0 },
        },
    });

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
                .role = .{
                    .value = .{
                        .identifier = .{
                            .text = "server",
                            .location = .{
                                .start = .{
                                    .offset = 6,
                                    .line = 0,
                                    .column = 6,
                                },
                                .end = .{
                                    .offset = 12,
                                    .line = 0,
                                    .column = 12,
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{ .offset = 0, .line = 0, .column = 0 },
                        .end = .{ .offset = 13, .line = 0, .column = 13 },
                    },
                    .lparen_position = .{ .offset = 5, .line = 0, .column = 5 },
                },
                .name = .{
                    .text = "inventory",
                    .location = .{
                        .start = .{ .offset = 23, .line = 1, .column = 9 },
                        .end = .{ .offset = 32, .line = 1, .column = 18 },
                    },
                },
                .parameters = &[_]SchemaDefinition.Function.Parameter{
                    .{
                        .name = .{
                            .text = "name",
                            .location = .{
                                .start = .{ .offset = 33, .line = 1, .column = 19 },
                                .end = .{ .offset = 37, .line = 1, .column = 23 },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 33, .line = 1, .column = 19 },
                            .end = .{ .offset = 37, .line = 1, .column = 23 },
                        },
                    },
                },
                .body = &[_]FQLExpression{
                    .{
                        .projection = .{
                            .expression = &FQLExpression{
                                .invocation = .{
                                    .function = &FQLExpression{
                                        .field_access = .{
                                            .value = &FQLExpression{
                                                .identifier = .{
                                                    .text = "Product",
                                                    .location = .{
                                                        .start = .{
                                                            .offset = 43,
                                                            .line = 2,
                                                            .column = 2,
                                                        },
                                                        .end = .{
                                                            .offset = 50,
                                                            .line = 2,
                                                            .column = 9,
                                                        },
                                                    },
                                                },
                                            },
                                            .field = .{
                                                .identifier = .{
                                                    .text = "byName",
                                                    .location = .{
                                                        .start = .{
                                                            .offset = 51,
                                                            .line = 2,
                                                            .column = 10,
                                                        },
                                                        .end = .{
                                                            .offset = 57,
                                                            .line = 2,
                                                            .column = 16,
                                                        },
                                                    },
                                                },
                                            },
                                            .location = .{
                                                .start = .{
                                                    .offset = 43,
                                                    .line = 2,
                                                    .column = 2,
                                                },
                                                .end = .{
                                                    .offset = 57,
                                                    .line = 2,
                                                    .column = 16,
                                                },
                                            },
                                            .dot_position = .{
                                                .offset = 50,
                                                .line = 2,
                                                .column = 9,
                                            },
                                        },
                                    },
                                    .arguments = &[_]FQLExpression{
                                        .{
                                            .identifier = .{
                                                .text = "name",
                                                .location = .{
                                                    .start = .{
                                                        .offset = 58,
                                                        .line = 2,
                                                        .column = 17,
                                                    },
                                                    .end = .{
                                                        .offset = 62,
                                                        .line = 2,
                                                        .column = 21,
                                                    },
                                                },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{
                                            .offset = 43,
                                            .line = 2,
                                            .column = 2,
                                        },
                                        .end = .{
                                            .offset = 63,
                                            .line = 2,
                                            .column = 22,
                                        },
                                    },
                                    .comma_positions = &.{},
                                    .lparen_position = .{
                                        .offset = 57,
                                        .line = 2,
                                        .column = 16,
                                    },
                                },
                            },
                            .fields = &[_]FQLExpression.Projection.Field{
                                .{
                                    .short = .{
                                        .text = "name",
                                        .location = .{
                                            .start = .{
                                                .offset = 70,
                                                .line = 3,
                                                .column = 4,
                                            },
                                            .end = .{
                                                .offset = 74,
                                                .line = 3,
                                                .column = 8,
                                            },
                                        },
                                    },
                                },
                                .{
                                    .short = .{
                                        .text = "description",
                                        .location = .{
                                            .start = .{
                                                .offset = 80,
                                                .line = 4,
                                                .column = 4,
                                            },
                                            .end = .{
                                                .offset = 91,
                                                .line = 4,
                                                .column = 15,
                                            },
                                        },
                                    },
                                },
                                .{
                                    .short = .{
                                        .text = "quantity",
                                        .location = .{
                                            .start = .{
                                                .offset = 97,
                                                .line = 5,
                                                .column = 4,
                                            },
                                            .end = .{
                                                .offset = 105,
                                                .line = 5,
                                                .column = 12,
                                            },
                                        },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{
                                    .offset = 43,
                                    .line = 2,
                                    .column = 2,
                                },
                                .end = .{
                                    .offset = 109,
                                    .line = 6,
                                    .column = 3,
                                },
                            },
                            .comma_positions = &.{
                                .{ .offset = 74, .line = 3, .column = 8 },
                                .{ .offset = 91, .line = 4, .column = 15 },
                            },
                            .lbrace_position = .{
                                .offset = 64,
                                .line = 2,
                                .column = 23,
                            },
                        },
                    },
                },
                .location = .{
                    .start = .{ .offset = 0, .line = 0, .column = 0 },
                    .end = .{ .offset = 111, .line = 7, .column = 1 },
                },
                .lparen_position = .{ .offset = 32, .line = 1, .column = 18 },
                .comma_positions = &.{},
                .rparen_position = .{ .offset = 37, .line = 1, .column = 23 },
                .lbrace_position = .{ .offset = 39, .line = 1, .column = 25 },
                .function_position = .{ .offset = 14, .line = 1, .column = 0 },
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
    , .{
        .role = .{
            .name = .{
                .text = "manager",
                .location = .{
                    .start = .{ .offset = 5, .line = 0, .column = 5 },
                    .end = .{ .offset = 12, .line = 0, .column = 12 },
                },
            },
            .members = &[_]SchemaDefinition.Role.Member{
                .{
                    .membership = .{
                        .collection = .{
                            .text = "Manager",
                            .location = .{
                                .start = .{ .offset = 130, .line = 4, .column = 13 },
                                .end = .{ .offset = 137, .line = 4, .column = 20 },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 119, .line = 4, .column = 2 },
                            .end = .{ .offset = 137, .line = 4, .column = 20 },
                        },
                    },
                },
                .{
                    .membership = .{
                        .collection = .{
                            .text = "User",
                            .location = .{
                                .start = .{ .offset = 283, .line = 9, .column = 13 },
                                .end = .{ .offset = 287, .line = 9, .column = 17 },
                            },
                        },
                        .predicate = .{
                            .isolated = .{
                                .expression = &FQLExpression{
                                    .function = .{
                                        .parameters = .{
                                            .short = .{
                                                .text = "user",
                                                .location = .{
                                                    .start = .{
                                                        .offset = 393,
                                                        .line = 12,
                                                        .column = 15,
                                                    },
                                                    .end = .{
                                                        .offset = 397,
                                                        .line = 12,
                                                        .column = 19,
                                                    },
                                                },
                                            },
                                        },
                                        .body = &FQLExpression{
                                            .binary_operation = .{
                                                .lhs = &FQLExpression{
                                                    .field_access = .{
                                                        .value = &FQLExpression{
                                                            .identifier = .{
                                                                .text = "user",
                                                                .location = .{
                                                                    .start = .{
                                                                        .offset = 401,
                                                                        .line = 12,
                                                                        .column = 23,
                                                                    },
                                                                    .end = .{
                                                                        .offset = 405,
                                                                        .line = 12,
                                                                        .column = 27,
                                                                    },
                                                                },
                                                            },
                                                        },
                                                        .field = .{
                                                            .identifier = .{
                                                                .text = "accessLevel",
                                                                .location = .{
                                                                    .start = .{
                                                                        .offset = 406,
                                                                        .line = 12,
                                                                        .column = 28,
                                                                    },
                                                                    .end = .{
                                                                        .offset = 417,
                                                                        .line = 12,
                                                                        .column = 39,
                                                                    },
                                                                },
                                                            },
                                                        },
                                                        .location = .{
                                                            .start = .{
                                                                .offset = 401,
                                                                .line = 12,
                                                                .column = 23,
                                                            },
                                                            .end = .{
                                                                .offset = 417,
                                                                .line = 12,
                                                                .column = 39,
                                                            },
                                                        },
                                                        .dot_position = .{
                                                            .offset = 405,
                                                            .line = 12,
                                                            .column = 27,
                                                        },
                                                    },
                                                },
                                                .operator = .equality,
                                                .rhs = &FQLExpression{
                                                    .string_literal = .{
                                                        .text = "'manager'",
                                                        .location = .{
                                                            .start = .{
                                                                .offset = 421,
                                                                .line = 12,
                                                                .column = 43,
                                                            },
                                                            .end = .{
                                                                .offset = 430,
                                                                .line = 12,
                                                                .column = 52,
                                                            },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{
                                                        .offset = 401,
                                                        .line = 12,
                                                        .column = 23,
                                                    },
                                                    .end = .{
                                                        .offset = 430,
                                                        .line = 12,
                                                        .column = 52,
                                                    },
                                                },
                                                .operator_position = .{
                                                    .offset = 418,
                                                    .line = 12,
                                                    .column = 40,
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{
                                                .offset = 393,
                                                .line = 12,
                                                .column = 15,
                                            },
                                            .end = .{
                                                .offset = 430,
                                                .line = 12,
                                                .column = 52,
                                            },
                                        },
                                        .equal_rarrow_position = .{
                                            .offset = 398,
                                            .line = 12,
                                            .column = 20,
                                        },
                                    },
                                },
                                .location = .{
                                    .start = .{
                                        .offset = 392,
                                        .line = 12,
                                        .column = 14,
                                    },
                                    .end = .{
                                        .offset = 431,
                                        .line = 12,
                                        .column = 53,
                                    },
                                },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 272, .line = 9, .column = 2 },
                            .end = .{ .offset = 435, .line = 13, .column = 3 },
                        },
                        .lbrace_position = .{ .offset = 288, .line = 9, .column = 18 },
                        .predicate_position = .{ .offset = 382, .line = 12, .column = 4 },
                    },
                },
                .{
                    .privileges = .{
                        .resource = .{
                            .text = "Store",
                            .location = .{
                                .start = .{ .offset = 506, .line = 16, .column = 13 },
                                .end = .{ .offset = 511, .line = 16, .column = 18 },
                            },
                        },
                        .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                            .{
                                .action = .create,
                                .location = .{
                                    .start = .{ .offset = 518, .line = 17, .column = 4 },
                                    .end = .{ .offset = 524, .line = 17, .column = 10 },
                                },
                            },
                            .{
                                .action = .read,
                                .location = .{
                                    .start = .{ .offset = 529, .line = 18, .column = 4 },
                                    .end = .{ .offset = 533, .line = 18, .column = 8 },
                                },
                            },
                            .{
                                .action = .write,
                                .location = .{
                                    .start = .{ .offset = 538, .line = 19, .column = 4 },
                                    .end = .{ .offset = 543, .line = 19, .column = 9 },
                                },
                            },
                            .{
                                .action = .delete,
                                .location = .{
                                    .start = .{ .offset = 548, .line = 20, .column = 4 },
                                    .end = .{ .offset = 554, .line = 20, .column = 10 },
                                },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 495, .line = 16, .column = 2 },
                            .end = .{ .offset = 558, .line = 21, .column = 3 },
                        },
                        .lbrace_position = .{ .offset = 512, .line = 16, .column = 19 },
                    },
                },
                .{
                    .privileges = .{
                        .resource = .{
                            .text = "Customer",
                            .location = .{
                                .start = .{ .offset = 634, .line = 24, .column = 13 },
                                .end = .{ .offset = 642, .line = 24, .column = 21 },
                            },
                        },
                        .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                            .{
                                .action = .read,
                                .location = .{
                                    .start = .{ .offset = 649, .line = 25, .column = 4 },
                                    .end = .{ .offset = 653, .line = 25, .column = 8 },
                                },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 623, .line = 24, .column = 2 },
                            .end = .{ .offset = 657, .line = 26, .column = 3 },
                        },
                        .lbrace_position = .{ .offset = 643, .line = 24, .column = 22 },
                    },
                },
                .{
                    .privileges = .{
                        .resource = .{
                            .text = "Manager",
                            .location = .{
                                .start = .{ .offset = 765, .line = 30, .column = 13 },
                                .end = .{ .offset = 772, .line = 30, .column = 20 },
                            },
                        },
                        .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                            .{
                                .action = .read,
                                .predicate = .{
                                    .isolated = .{
                                        .expression = &FQLExpression{
                                            .function = .{
                                                .parameters = .{
                                                    .short = .{
                                                        .text = "doc",
                                                        .location = .{
                                                            .start = .{ .offset = 803, .line = 32, .column = 17 },
                                                            .end = .{ .offset = 806, .line = 32, .column = 20 },
                                                        },
                                                    },
                                                },
                                                .body = &FQLExpression{
                                                    .binary_operation = .{
                                                        .lhs = &FQLExpression{
                                                            .binary_operation = .{
                                                                .lhs = &FQLExpression{
                                                                    .invocation = .{
                                                                        .function = &FQLExpression{
                                                                            .field_access = .{
                                                                                .value = &FQLExpression{
                                                                                    .identifier = .{
                                                                                        .text = "Query",
                                                                                        .location = .{
                                                                                            .start = .{ .offset = 981, .line = 36, .column = 8 },
                                                                                            .end = .{ .offset = 986, .line = 36, .column = 13 },
                                                                                        },
                                                                                    },
                                                                                },
                                                                                .field = .{
                                                                                    .identifier = .{
                                                                                        .text = "identity",
                                                                                        .location = .{
                                                                                            .start = .{ .offset = 987, .line = 36, .column = 14 },
                                                                                            .end = .{ .offset = 995, .line = 36, .column = 22 },
                                                                                        },
                                                                                    },
                                                                                },
                                                                                .location = .{
                                                                                    .start = .{ .offset = 981, .line = 36, .column = 8 },
                                                                                    .end = .{ .offset = 995, .line = 36, .column = 22 },
                                                                                },
                                                                                .dot_position = .{ .offset = 986, .line = 36, .column = 13 },
                                                                            },
                                                                        },
                                                                        .arguments = &.{},
                                                                        .location = .{
                                                                            .start = .{ .offset = 981, .line = 36, .column = 8 },
                                                                            .end = .{ .offset = 997, .line = 36, .column = 24 },
                                                                        },
                                                                        .comma_positions = &.{},
                                                                        .lparen_position = .{ .offset = 995, .line = 36, .column = 22 },
                                                                    },
                                                                },
                                                                .operator = .equality,
                                                                .rhs = &FQLExpression{
                                                                    .identifier = .{
                                                                        .text = "doc",
                                                                        .location = .{
                                                                            .start = .{ .offset = 1001, .line = 36, .column = 28 },
                                                                            .end = .{ .offset = 1004, .line = 36, .column = 31 },
                                                                        },
                                                                    },
                                                                },
                                                                .location = .{
                                                                    .source = null,
                                                                    .start = .{ .offset = 981, .line = 36, .column = 8 },
                                                                    .end = .{ .offset = 1004, .line = 36, .column = 31 },
                                                                },
                                                                .operator_position = .{ .offset = 998, .line = 36, .column = 25 },
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
                                                                                        .value = &FQLExpression{
                                                                                            .identifier = .{
                                                                                                .text = "Date",
                                                                                                .location = .{
                                                                                                    .start = .{ .offset = 1054, .line = 38, .column = 8 },
                                                                                                    .end = .{ .offset = 1058, .line = 38, .column = 12 },
                                                                                                },
                                                                                            },
                                                                                        },
                                                                                        .field = .{
                                                                                            .identifier = .{
                                                                                                .text = "today",
                                                                                                .location = .{
                                                                                                    .start = .{ .offset = 1059, .line = 38, .column = 13 },
                                                                                                    .end = .{ .offset = 1064, .line = 38, .column = 18 },
                                                                                                },
                                                                                            },
                                                                                        },
                                                                                        .location = .{
                                                                                            .start = .{ .offset = 1054, .line = 38, .column = 8 },
                                                                                            .end = .{ .offset = 1064, .line = 38, .column = 18 },
                                                                                        },
                                                                                        .dot_position = .{ .offset = 1058, .line = 38, .column = 12 },
                                                                                    },
                                                                                },
                                                                                .arguments = &.{},
                                                                                .location = .{
                                                                                    .start = .{ .offset = 1054, .line = 38, .column = 8 },
                                                                                    .end = .{ .offset = 1066, .line = 38, .column = 20 },
                                                                                },
                                                                                .comma_positions = &.{},
                                                                                .lparen_position = .{ .offset = 1064, .line = 38, .column = 18 },
                                                                            },
                                                                        },
                                                                        .field = .{
                                                                            .identifier = .{
                                                                                .text = "dayOfWeek",
                                                                                .location = .{
                                                                                    .start = .{ .offset = 1067, .line = 38, .column = 21 },
                                                                                    .end = .{ .offset = 1076, .line = 38, .column = 30 },
                                                                                },
                                                                            },
                                                                        },
                                                                        .location = .{
                                                                            .start = .{ .offset = 1054, .line = 38, .column = 8 },
                                                                            .end = .{ .offset = 1076, .line = 38, .column = 30 },
                                                                        },
                                                                        .dot_position = .{ .offset = 1066, .line = 38, .column = 20 },
                                                                    },
                                                                },
                                                                .operator = .less_than,
                                                                .rhs = &FQLExpression{
                                                                    .number_literal = .{
                                                                        .text = "6",
                                                                        .location = .{
                                                                            .start = .{ .offset = 1079, .line = 38, .column = 33 },
                                                                            .end = .{ .offset = 1080, .line = 38, .column = 34 },
                                                                        },
                                                                    },
                                                                },
                                                                .location = .{
                                                                    .start = .{ .offset = 1054, .line = 38, .column = 8 },
                                                                    .end = .{ .offset = 1080, .line = 38, .column = 34 },
                                                                },
                                                                .operator_position = .{ .offset = 1077, .line = 38, .column = 31 },
                                                            },
                                                        },
                                                        .location = .{
                                                            .start = .{ .offset = 981, .line = 36, .column = 8 },
                                                            .end = .{ .offset = 1080, .line = 38, .column = 34 },
                                                        },
                                                        .operator_position = .{ .offset = 1005, .line = 36, .column = 32 },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 803, .line = 32, .column = 17 },
                                                    .end = .{ .offset = 1080, .line = 38, .column = 34 },
                                                },
                                                .equal_rarrow_position = .{ .offset = 807, .line = 32, .column = 21 },
                                            },
                                        },
                                        .location = .{
                                            .start = .{ .offset = 802, .line = 32, .column = 16 },
                                            .end = .{ .offset = 1088, .line = 39, .column = 7 },
                                        },
                                    },
                                },
                                .location = .{
                                    .start = .{ .offset = 779, .line = 31, .column = 4 },
                                    .end = .{ .offset = 1094, .line = 40, .column = 5 },
                                },
                                .lbrace_position = .{ .offset = 784, .line = 31, .column = 9 },
                                .predicate_position = .{ .offset = 792, .line = 32, .column = 6 },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 754, .line = 30, .column = 2 },
                            .end = .{ .offset = 1098, .line = 41, .column = 3 },
                        },
                        .lbrace_position = .{ .offset = 773, .line = 30, .column = 21 },
                    },
                },
                .{
                    .privileges = .{
                        .resource = .{
                            .text = "inventory",
                            .location = .{
                                .start = .{ .offset = 1190, .line = 45, .column = 13 },
                                .end = .{ .offset = 1199, .line = 45, .column = 22 },
                            },
                        },
                        .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                            .{
                                .action = .call,
                                .location = .{
                                    .start = .{ .offset = 1206, .line = 46, .column = 4 },
                                    .end = .{ .offset = 1210, .line = 46, .column = 8 },
                                },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 1179, .line = 45, .column = 2 },
                            .end = .{ .offset = 1214, .line = 47, .column = 3 },
                        },
                        .lbrace_position = .{ .offset = 1200, .line = 45, .column = 23 },
                    },
                },
                .{
                    .privileges = .{
                        .resource = .{
                            .text = "submitOrder",
                            .location = .{
                                .start = .{ .offset = 1341, .line = 52, .column = 13 },
                                .end = .{ .offset = 1352, .line = 52, .column = 24 },
                            },
                        },
                        .actions = &[_]SchemaDefinition.Role.Member.Privileges.Action{
                            .{
                                .action = .call,
                                .predicate = .{
                                    .isolated = .{
                                        .expression = &FQLExpression{
                                            .function = .{
                                                .parameters = .{
                                                    .short = .{
                                                        .text = "args",
                                                        .location = .{
                                                            .start = .{ .offset = 1502, .line = 57, .column = 17 },
                                                            .end = .{ .offset = 1506, .line = 57, .column = 21 },
                                                        },
                                                    },
                                                },
                                                .body = &FQLExpression{
                                                    .binary_operation = .{
                                                        .lhs = &FQLExpression{
                                                            .invocation = .{
                                                                .function = &FQLExpression{
                                                                    .field_access = .{
                                                                        .value = &FQLExpression{
                                                                            .identifier = .{
                                                                                .text = "Query",
                                                                                .location = .{
                                                                                    .start = .{ .offset = 1510, .line = 57, .column = 25 },
                                                                                    .end = .{ .offset = 1515, .line = 57, .column = 30 },
                                                                                },
                                                                            },
                                                                        },
                                                                        .field = .{
                                                                            .identifier = .{
                                                                                .text = "identity",
                                                                                .location = .{
                                                                                    .start = .{ .offset = 1516, .line = 57, .column = 31 },
                                                                                    .end = .{ .offset = 1524, .line = 57, .column = 39 },
                                                                                },
                                                                            },
                                                                        },
                                                                        .location = .{
                                                                            .start = .{ .offset = 1510, .line = 57, .column = 25 },
                                                                            .end = .{ .offset = 1524, .line = 57, .column = 39 },
                                                                        },
                                                                        .dot_position = .{ .offset = 1515, .line = 57, .column = 30 },
                                                                    },
                                                                },
                                                                .arguments = &.{},
                                                                .comma_positions = &.{},
                                                                .location = .{
                                                                    .start = .{ .offset = 1510, .line = 57, .column = 25 },
                                                                    .end = .{ .offset = 1526, .line = 57, .column = 41 },
                                                                },
                                                                .lparen_position = .{ .offset = 1524, .line = 57, .column = 39 },
                                                            },
                                                        },
                                                        .operator = .equality,
                                                        .rhs = &FQLExpression{
                                                            .field_access = .{
                                                                .value = &FQLExpression{
                                                                    .identifier = .{
                                                                        .text = "args",
                                                                        .location = .{
                                                                            .start = .{ .offset = 1530, .line = 57, .column = 45 },
                                                                            .end = .{ .offset = 1534, .line = 57, .column = 49 },
                                                                        },
                                                                    },
                                                                },
                                                                .field = .{
                                                                    .expression = &FQLExpression{
                                                                        .number_literal = .{
                                                                            .text = "0",
                                                                            .location = .{
                                                                                .start = .{ .offset = 1535, .line = 57, .column = 50 },
                                                                                .end = .{ .offset = 1536, .line = 57, .column = 51 },
                                                                            },
                                                                        },
                                                                    },
                                                                },
                                                                .location = .{
                                                                    .start = .{ .offset = 1530, .line = 57, .column = 45 },
                                                                    .end = .{ .offset = 1537, .line = 57, .column = 52 },
                                                                },
                                                                .lbracket_position = .{ .offset = 1534, .line = 57, .column = 49 },
                                                            },
                                                        },
                                                        .location = .{
                                                            .start = .{ .offset = 1510, .line = 57, .column = 25 },
                                                            .end = .{ .offset = 1537, .line = 57, .column = 52 },
                                                        },
                                                        .operator_position = .{ .offset = 1527, .line = 57, .column = 42 },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 1502, .line = 57, .column = 17 },
                                                    .end = .{ .offset = 1537, .line = 57, .column = 52 },
                                                },
                                                .equal_rarrow_position = .{ .offset = 1507, .line = 57, .column = 22 },
                                            },
                                        },
                                        .location = .{
                                            .start = .{ .offset = 1501, .line = 57, .column = 16 },
                                            .end = .{ .offset = 1538, .line = 57, .column = 53 },
                                        },
                                    },
                                },
                                .location = .{
                                    .start = .{ .offset = 1359, .line = 53, .column = 4 },
                                    .end = .{ .offset = 1544, .line = 58, .column = 5 },
                                },
                                .lbrace_position = .{ .offset = 1364, .line = 53, .column = 9 },
                                .predicate_position = .{ .offset = 1491, .line = 57, .column = 6 },
                            },
                        },
                        .location = .{
                            .start = .{ .offset = 1330, .line = 52, .column = 2 },
                            .end = .{ .offset = 1548, .line = 59, .column = 3 },
                        },
                        .lbrace_position = .{ .offset = 1353, .line = 52, .column = 25 },
                    },
                },
            },
            .location = .{
                .start = .{ .offset = 0, .line = 0, .column = 0 },
                .end = .{ .offset = 1550, .line = 60, .column = 1 },
            },
            .lbrace_position = .{ .offset = 13, .line = 0, .column = 13 },
        },
    });

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
                .name = .{
                    .text = "Product",
                    .location = .{
                        .start = .{ .offset = 11, .line = 0, .column = 11 },
                        .end = .{ .offset = 18, .line = 0, .column = 18 },
                    },
                },
                .members = &[_]SchemaDefinition.Collection.Member{
                    .{
                        .field = .{
                            .name = .{
                                .text = "name",
                                .location = .{
                                    .start = .{ .offset = 23, .line = 1, .column = 2 },
                                    .end = .{ .offset = 27, .line = 1, .column = 6 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .named = .{
                                            .text = "String",
                                            .location = .{
                                                .start = .{ .offset = 29, .line = 1, .column = 8 },
                                                .end = .{ .offset = 35, .line = 1, .column = 14 },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 29, .line = 1, .column = 8 },
                                        .end = .{ .offset = 36, .line = 1, .column = 15 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 23, .line = 1, .column = 2 },
                                .end = .{ .offset = 36, .line = 1, .column = 15 },
                            },
                            .colon_position = .{ .offset = 27, .line = 1, .column = 6 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "description",
                                .location = .{
                                    .start = .{ .offset = 39, .line = 2, .column = 2 },
                                    .end = .{ .offset = 50, .line = 2, .column = 13 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .named = .{
                                            .text = "String",
                                            .location = .{
                                                .start = .{ .offset = 52, .line = 2, .column = 15 },
                                                .end = .{ .offset = 58, .line = 2, .column = 21 },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 52, .line = 2, .column = 15 },
                                        .end = .{ .offset = 59, .line = 2, .column = 22 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 39, .line = 2, .column = 2 },
                                .end = .{ .offset = 59, .line = 2, .column = 22 },
                            },
                            .colon_position = .{ .offset = 50, .line = 2, .column = 13 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "price",
                                .location = .{
                                    .start = .{ .offset = 62, .line = 3, .column = 2 },
                                    .end = .{ .offset = 67, .line = 3, .column = 7 },
                                },
                            },
                            .type = .{
                                .named = .{
                                    .text = "Double",
                                    .location = .{
                                        .start = .{ .offset = 69, .line = 3, .column = 9 },
                                        .end = .{ .offset = 75, .line = 3, .column = 15 },
                                    },
                                },
                            },
                            .default = .{
                                .number_literal = .{
                                    .text = "0.00",
                                    .location = .{
                                        .start = .{ .offset = 78, .line = 3, .column = 18 },
                                        .end = .{ .offset = 82, .line = 3, .column = 22 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 62, .line = 3, .column = 2 },
                                .end = .{ .offset = 82, .line = 3, .column = 22 },
                            },
                            .colon_position = .{ .offset = 67, .line = 3, .column = 7 },
                            .equal_position = .{ .offset = 76, .line = 3, .column = 16 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "quantity",
                                .location = .{
                                    .start = .{ .offset = 85, .line = 4, .column = 2 },
                                    .end = .{ .offset = 93, .line = 4, .column = 10 },
                                },
                            },
                            .type = .{
                                .named = .{
                                    .text = "Int",
                                    .location = .{
                                        .start = .{ .offset = 95, .line = 4, .column = 12 },
                                        .end = .{ .offset = 98, .line = 4, .column = 15 },
                                    },
                                },
                            },
                            .default = .{
                                .number_literal = .{
                                    .text = "0",
                                    .location = .{
                                        .start = .{ .offset = 101, .line = 4, .column = 18 },
                                        .end = .{ .offset = 102, .line = 4, .column = 19 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 85, .line = 4, .column = 2 },
                                .end = .{ .offset = 102, .line = 4, .column = 19 },
                            },
                            .colon_position = .{ .offset = 93, .line = 4, .column = 10 },
                            .equal_position = .{ .offset = 99, .line = 4, .column = 16 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "store",
                                .location = .{
                                    .start = .{ .offset = 105, .line = 5, .column = 2 },
                                    .end = .{ .offset = 110, .line = 5, .column = 7 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .template = .{
                                            .name = .{
                                                .text = "Ref",
                                                .location = .{
                                                    .start = .{ .offset = 112, .line = 5, .column = 9 },
                                                    .end = .{ .offset = 115, .line = 5, .column = 12 },
                                                },
                                            },
                                            .parameters = &[_]FQLType{
                                                .{
                                                    .named = .{
                                                        .text = "Store",
                                                        .location = .{
                                                            .start = .{ .offset = 116, .line = 5, .column = 13 },
                                                            .end = .{ .offset = 121, .line = 5, .column = 18 },
                                                        },
                                                    },
                                                },
                                            },
                                            .location = .{
                                                .start = .{ .offset = 112, .line = 5, .column = 9 },
                                                .end = .{ .offset = 122, .line = 5, .column = 19 },
                                            },
                                            .larrow_position = .{ .offset = 115, .line = 5, .column = 12 },
                                            .comma_positions = &.{},
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 112, .line = 5, .column = 9 },
                                        .end = .{ .offset = 123, .line = 5, .column = 20 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 105, .line = 5, .column = 2 },
                                .end = .{ .offset = 123, .line = 5, .column = 20 },
                            },
                            .colon_position = .{ .offset = 110, .line = 5, .column = 7 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "backorderLimit",
                                .location = .{
                                    .start = .{ .offset = 126, .line = 6, .column = 2 },
                                    .end = .{ .offset = 140, .line = 6, .column = 16 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .named = .{
                                            .text = "Int",
                                            .location = .{
                                                .start = .{ .offset = 142, .line = 6, .column = 18 },
                                                .end = .{ .offset = 145, .line = 6, .column = 21 },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 142, .line = 6, .column = 18 },
                                        .end = .{ .offset = 146, .line = 6, .column = 22 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 126, .line = 6, .column = 2 },
                                .end = .{ .offset = 146, .line = 6, .column = 22 },
                            },
                            .colon_position = .{ .offset = 140, .line = 6, .column = 16 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "backordered",
                                .location = .{
                                    .start = .{ .offset = 149, .line = 7, .column = 2 },
                                    .end = .{ .offset = 160, .line = 7, .column = 13 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .named = .{
                                            .text = "Boolean",
                                            .location = .{
                                                .start = .{ .offset = 162, .line = 7, .column = 15 },
                                                .end = .{ .offset = 169, .line = 7, .column = 22 },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 162, .line = 7, .column = 15 },
                                        .end = .{ .offset = 170, .line = 7, .column = 23 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 149, .line = 7, .column = 2 },
                                .end = .{ .offset = 170, .line = 7, .column = 23 },
                            },
                            .colon_position = .{ .offset = 160, .line = 7, .column = 13 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "categories",
                                .location = .{
                                    .start = .{ .offset = 173, .line = 8, .column = 2 },
                                    .end = .{ .offset = 183, .line = 8, .column = 12 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .template = .{
                                            .name = .{
                                                .text = "Array",
                                                .location = .{
                                                    .start = .{ .offset = 185, .line = 8, .column = 14 },
                                                    .end = .{ .offset = 190, .line = 8, .column = 19 },
                                                },
                                            },
                                            .parameters = &[_]FQLType{
                                                .{
                                                    .named = .{
                                                        .text = "String",
                                                        .location = .{
                                                            .start = .{ .offset = 191, .line = 8, .column = 20 },
                                                            .end = .{ .offset = 197, .line = 8, .column = 26 },
                                                        },
                                                    },
                                                },
                                            },
                                            .location = .{
                                                .start = .{ .offset = 185, .line = 8, .column = 14 },
                                                .end = .{ .offset = 198, .line = 8, .column = 27 },
                                            },
                                            .larrow_position = .{ .offset = 190, .line = 8, .column = 19 },
                                            .comma_positions = &.{},
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 185, .line = 8, .column = 14 },
                                        .end = .{ .offset = 199, .line = 8, .column = 28 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 173, .line = 8, .column = 2 },
                                .end = .{ .offset = 199, .line = 8, .column = 28 },
                            },
                            .colon_position = .{ .offset = 183, .line = 8, .column = 12 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "creationTime",
                                .location = .{
                                    .start = .{ .offset = 202, .line = 9, .column = 2 },
                                    .end = .{ .offset = 214, .line = 9, .column = 14 },
                                },
                            },
                            .type = .{
                                .named = .{
                                    .text = "Time",
                                    .location = .{
                                        .start = .{ .offset = 216, .line = 9, .column = 16 },
                                        .end = .{ .offset = 220, .line = 9, .column = 20 },
                                    },
                                },
                            },
                            .default = .{
                                .invocation = .{
                                    .function = &FQLExpression{
                                        .field_access = .{
                                            .value = &FQLExpression{
                                                .identifier = .{
                                                    .text = "Time",
                                                    .location = .{
                                                        .start = .{ .offset = 223, .line = 9, .column = 23 },
                                                        .end = .{ .offset = 227, .line = 9, .column = 27 },
                                                    },
                                                },
                                            },
                                            .field = .{
                                                .identifier = .{
                                                    .text = "now",
                                                    .location = .{
                                                        .start = .{ .offset = 228, .line = 9, .column = 28 },
                                                        .end = .{ .offset = 231, .line = 9, .column = 31 },
                                                    },
                                                },
                                            },
                                            .location = .{
                                                .start = .{ .offset = 223, .line = 9, .column = 23 },
                                                .end = .{ .offset = 231, .line = 9, .column = 31 },
                                            },
                                            .dot_position = .{ .offset = 227, .line = 9, .column = 27 },
                                        },
                                    },
                                    .arguments = &.{},
                                    .location = .{
                                        .start = .{ .offset = 223, .line = 9, .column = 23 },
                                        .end = .{ .offset = 233, .line = 9, .column = 33 },
                                    },
                                    .comma_positions = &.{},
                                    .lparen_position = .{ .offset = 231, .line = 9, .column = 31 },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 202, .line = 9, .column = 2 },
                                .end = .{ .offset = 233, .line = 9, .column = 33 },
                            },
                            .colon_position = .{ .offset = 214, .line = 9, .column = 14 },
                            .equal_position = .{ .offset = 221, .line = 9, .column = 21 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "creationTimeEpoch",
                                .location = .{
                                    .start = .{ .offset = 236, .line = 10, .column = 2 },
                                    .end = .{ .offset = 253, .line = 10, .column = 19 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .named = .{
                                            .text = "Int",
                                            .location = .{
                                                .start = .{ .offset = 255, .line = 10, .column = 21 },
                                                .end = .{ .offset = 258, .line = 10, .column = 24 },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 255, .line = 10, .column = 21 },
                                        .end = .{ .offset = 259, .line = 10, .column = 25 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 236, .line = 10, .column = 2 },
                                .end = .{ .offset = 259, .line = 10, .column = 25 },
                            },
                            .colon_position = .{ .offset = 253, .line = 10, .column = 19 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "typeConflicts",
                                .location = .{
                                    .start = .{ .offset = 262, .line = 11, .column = 2 },
                                    .end = .{ .offset = 275, .line = 11, .column = 15 },
                                },
                            },
                            .type = .{
                                .optional = .{
                                    .type = &FQLType{
                                        .object = .{
                                            .fields = &[_]FQLType.Object.Field{
                                                .{
                                                    .key = .{
                                                        .wildcard = .{
                                                            .start = .{ .offset = 279, .line = 11, .column = 19 },
                                                            .end = .{ .offset = 280, .line = 11, .column = 20 },
                                                        },
                                                    },
                                                    .type = .{
                                                        .named = .{
                                                            .text = "Any",
                                                            .location = .{
                                                                .start = .{ .offset = 282, .line = 11, .column = 22 },
                                                                .end = .{ .offset = 285, .line = 11, .column = 25 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 279, .line = 11, .column = 19 },
                                                        .end = .{ .offset = 285, .line = 11, .column = 25 },
                                                    },
                                                    .colon_position = .{ .offset = 280, .line = 11, .column = 20 },
                                                },
                                            },
                                            .location = .{
                                                .start = .{ .offset = 277, .line = 11, .column = 17 },
                                                .end = .{ .offset = 287, .line = 11, .column = 27 },
                                            },
                                            .comma_positions = &.{},
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 277, .line = 11, .column = 17 },
                                        .end = .{ .offset = 288, .line = 11, .column = 28 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 262, .line = 11, .column = 2 },
                                .end = .{ .offset = 288, .line = 11, .column = 28 },
                            },
                            .colon_position = .{ .offset = 275, .line = 11, .column = 15 },
                        },
                    },
                    .{
                        .field = .{
                            .name = .{
                                .text = "*",
                                .location = .{
                                    .start = .{ .offset = 292, .line = 13, .column = 2 },
                                    .end = .{ .offset = 293, .line = 13, .column = 3 },
                                },
                            },
                            .type = .{
                                .named = .{
                                    .text = "Any",
                                    .location = .{
                                        .start = .{ .offset = 295, .line = 13, .column = 5 },
                                        .end = .{ .offset = 298, .line = 13, .column = 8 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 292, .line = 13, .column = 2 },
                                .end = .{ .offset = 298, .line = 13, .column = 8 },
                            },
                            .colon_position = .{ .offset = 293, .line = 13, .column = 3 },
                        },
                    },
                    .{
                        .migrations = .{
                            .statements = &[_]SchemaDefinition.Collection.Member.Migrations.Statement{
                                .{
                                    .add = .{
                                        .node = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "typeConflicts",
                                                        .location = .{
                                                            .start = .{ .offset = 324, .line = 16, .column = 9 },
                                                            .end = .{ .offset = 337, .line = 16, .column = 22 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 323, .line = 16, .column = 8 },
                                                    .end = .{ .offset = 337, .line = 16, .column = 22 },
                                                },
                                            },
                                        },
                                    },
                                },
                                .{
                                    .add = .{
                                        .node = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "quantity",
                                                        .location = .{
                                                            .start = .{ .offset = 347, .line = 17, .column = 9 },
                                                            .end = .{ .offset = 355, .line = 17, .column = 17 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 346, .line = 17, .column = 8 },
                                                    .end = .{ .offset = 355, .line = 17, .column = 17 },
                                                },
                                            },
                                        },
                                    },
                                },
                                .{
                                    .backfill = .{
                                        .name = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "quantity",
                                                        .location = .{
                                                            .start = .{ .offset = 370, .line = 18, .column = 14 },
                                                            .end = .{ .offset = 378, .line = 18, .column = 22 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 369, .line = 18, .column = 13 },
                                                    .end = .{ .offset = 378, .line = 18, .column = 22 },
                                                },
                                            },
                                        },
                                        .value = .{
                                            .number_literal = .{
                                                .text = "0",
                                                .location = .{
                                                    .start = .{ .offset = 381, .line = 18, .column = 25 },
                                                    .end = .{ .offset = 382, .line = 18, .column = 26 },
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{ .offset = 360, .line = 18, .column = 4 },
                                            .end = .{ .offset = 382, .line = 18, .column = 26 },
                                        },
                                        .equal_position = .{ .offset = 379, .line = 18, .column = 23 },
                                    },
                                },
                                .{
                                    .drop = .{
                                        .node = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "internalDesc",
                                                        .location = .{
                                                            .start = .{ .offset = 393, .line = 19, .column = 10 },
                                                            .end = .{ .offset = 405, .line = 19, .column = 22 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 392, .line = 19, .column = 9 },
                                                    .end = .{ .offset = 405, .line = 19, .column = 22 },
                                                },
                                            },
                                        },
                                    },
                                },
                                .{
                                    .move_conflicts = .{
                                        .node = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "typeConflicts",
                                                        .location = .{
                                                            .start = .{ .offset = 426, .line = 20, .column = 20 },
                                                            .end = .{ .offset = 439, .line = 20, .column = 33 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 425, .line = 20, .column = 19 },
                                                    .end = .{ .offset = 439, .line = 20, .column = 33 },
                                                },
                                            },
                                        },
                                    },
                                },
                                .{
                                    .move = .{
                                        .old_name = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "desc",
                                                        .location = .{
                                                            .start = .{ .offset = 450, .line = 21, .column = 10 },
                                                            .end = .{ .offset = 454, .line = 21, .column = 14 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 449, .line = 21, .column = 9 },
                                                    .end = .{ .offset = 454, .line = 21, .column = 14 },
                                                },
                                            },
                                        },
                                        .new_name = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "description",
                                                        .location = .{
                                                            .start = .{ .offset = 459, .line = 21, .column = 19 },
                                                            .end = .{ .offset = 470, .line = 21, .column = 30 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 458, .line = 21, .column = 18 },
                                                    .end = .{ .offset = 470, .line = 21, .column = 30 },
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{ .offset = 444, .line = 21, .column = 4 },
                                            .end = .{ .offset = 470, .line = 21, .column = 30 },
                                        },
                                        .minus_rarrow_position = .{ .offset = 455, .line = 21, .column = 15 },
                                    },
                                },
                                .{
                                    .split = .{
                                        .old_name = .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "creationTime",
                                                        .location = .{
                                                            .start = .{ .offset = 482, .line = 22, .column = 11 },
                                                            .end = .{ .offset = 494, .line = 22, .column = 23 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 481, .line = 22, .column = 10 },
                                                    .end = .{ .offset = 494, .line = 22, .column = 23 },
                                                },
                                            },
                                        },
                                        .new_names = &[_]FQLExpression{
                                            .{
                                                .anonymous_field_access = .{
                                                    .field = .{
                                                        .identifier = .{
                                                            .text = "creationTime",
                                                            .location = .{
                                                                .start = .{ .offset = 499, .line = 22, .column = 28 },
                                                                .end = .{ .offset = 511, .line = 22, .column = 40 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 498, .line = 22, .column = 27 },
                                                        .end = .{ .offset = 511, .line = 22, .column = 40 },
                                                    },
                                                },
                                            },
                                            .{
                                                .anonymous_field_access = .{
                                                    .field = .{
                                                        .identifier = .{
                                                            .text = "creationTimeEpoch",
                                                            .location = .{
                                                                .start = .{ .offset = 514, .line = 22, .column = 43 },
                                                                .end = .{ .offset = 531, .line = 22, .column = 60 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 513, .line = 22, .column = 42 },
                                                        .end = .{ .offset = 531, .line = 22, .column = 60 },
                                                    },
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{ .offset = 475, .line = 22, .column = 4 },
                                            .end = .{ .offset = 531, .line = 22, .column = 60 },
                                        },
                                        .minus_rarrow_position = .{ .offset = 495, .line = 22, .column = 24 },
                                        .comma_positions = &.{},
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 302, .line = 15, .column = 2 },
                                .end = .{ .offset = 535, .line = 23, .column = 3 },
                            },
                        },
                    },
                    .{
                        .unique_constraint = .{
                            .terms = &[_]FQLExpression{
                                .{
                                    .anonymous_field_access = .{
                                        .field = .{
                                            .identifier = .{
                                                .text = "name",
                                                .location = .{
                                                    .start = .{ .offset = 548, .line = 25, .column = 11 },
                                                    .end = .{ .offset = 552, .line = 25, .column = 15 },
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{ .offset = 547, .line = 25, .column = 10 },
                                            .end = .{ .offset = 552, .line = 25, .column = 15 },
                                        },
                                    },
                                },
                                .{
                                    .anonymous_field_access = .{
                                        .field = .{
                                            .identifier = .{
                                                .text = "description",
                                                .location = .{
                                                    .start = .{ .offset = 555, .line = 25, .column = 18 },
                                                    .end = .{ .offset = 566, .line = 25, .column = 29 },
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{ .offset = 554, .line = 25, .column = 17 },
                                            .end = .{ .offset = 566, .line = 25, .column = 29 },
                                        },
                                    },
                                },
                                .{
                                    .invocation = .{
                                        .function = &FQLExpression{
                                            .identifier = .{
                                                .text = "mva",
                                                .location = .{
                                                    .start = .{ .offset = 568, .line = 25, .column = 31 },
                                                    .end = .{ .offset = 571, .line = 25, .column = 34 },
                                                },
                                            },
                                        },
                                        .arguments = &[_]FQLExpression{
                                            .{
                                                .anonymous_field_access = .{
                                                    .field = .{
                                                        .identifier = .{
                                                            .text = "categories",
                                                            .location = .{
                                                                .start = .{ .offset = 573, .line = 25, .column = 36 },
                                                                .end = .{ .offset = 583, .line = 25, .column = 46 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 572, .line = 25, .column = 35 },
                                                        .end = .{ .offset = 583, .line = 25, .column = 46 },
                                                    },
                                                },
                                            },
                                        },
                                        .location = .{ .start = .{ .offset = 568, .line = 25, .column = 31 }, .end = .{ .offset = 584, .line = 25, .column = 47 } },
                                        .comma_positions = &.{},
                                        .lparen_position = .{ .offset = 571, .line = 25, .column = 34 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 539, .line = 25, .column = 2 },
                                .end = .{ .offset = 585, .line = 25, .column = 48 },
                            },
                            .comma_positions = &.{
                                .{ .offset = 552, .line = 25, .column = 15 },
                                .{ .offset = 566, .line = 25, .column = 29 },
                            },
                            .lbracket_position = .{ .offset = 546, .line = 25, .column = 9 },
                        },
                    },
                    .{
                        .index = .{
                            .name = .{
                                .text = "byName",
                                .location = .{
                                    .start = .{ .offset = 595, .line = 27, .column = 8 },
                                    .end = .{ .offset = 601, .line = 27, .column = 14 },
                                },
                            },
                            .members = &[_]SchemaDefinition.Collection.Member.Index.Member{
                                .{
                                    .kind = .terms,
                                    .expressions = &[_]FQLExpression{
                                        .{
                                            .anonymous_field_access = .{
                                                .field = .{
                                                    .identifier = .{
                                                        .text = "name",
                                                        .location = .{
                                                            .start = .{ .offset = 616, .line = 28, .column = 12 },
                                                            .end = .{ .offset = 620, .line = 28, .column = 16 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 615, .line = 28, .column = 11 },
                                                    .end = .{ .offset = 620, .line = 28, .column = 16 },
                                                },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 608, .line = 28, .column = 4 },
                                        .end = .{ .offset = 621, .line = 28, .column = 17 },
                                    },
                                    .lbracket_position = .{ .offset = 614, .line = 28, .column = 10 },
                                    .comma_positions = &.{},
                                },
                                .{
                                    .kind = .values,
                                    .expressions = &[_]FQLExpression{
                                        .{
                                            .invocation = .{
                                                .function = &FQLExpression{
                                                    .identifier = .{
                                                        .text = "desc",
                                                        .location = .{
                                                            .start = .{ .offset = 634, .line = 29, .column = 12 },
                                                            .end = .{ .offset = 638, .line = 29, .column = 16 },
                                                        },
                                                    },
                                                },
                                                .arguments = &[_]FQLExpression{
                                                    .{
                                                        .anonymous_field_access = .{
                                                            .field = .{
                                                                .identifier = .{
                                                                    .text = "quantity",
                                                                    .location = .{
                                                                        .start = .{ .offset = 640, .line = 29, .column = 18 },
                                                                        .end = .{ .offset = 648, .line = 29, .column = 26 },
                                                                    },
                                                                },
                                                            },
                                                            .location = .{
                                                                .start = .{ .offset = 639, .line = 29, .column = 17 },
                                                                .end = .{ .offset = 648, .line = 29, .column = 26 },
                                                            },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 634, .line = 29, .column = 12 },
                                                    .end = .{ .offset = 649, .line = 29, .column = 27 },
                                                },
                                                .comma_positions = &.{},
                                                .lparen_position = .{ .offset = 638, .line = 29, .column = 16 },
                                            },
                                        },
                                        .{
                                            .invocation = .{
                                                .function = &FQLExpression{
                                                    .identifier = .{
                                                        .text = "desc",
                                                        .location = .{
                                                            .start = .{ .offset = 651, .line = 29, .column = 29 },
                                                            .end = .{ .offset = 655, .line = 29, .column = 33 },
                                                        },
                                                    },
                                                },
                                                .arguments = &[_]FQLExpression{
                                                    .{
                                                        .invocation = .{
                                                            .function = &FQLExpression{
                                                                .identifier = .{
                                                                    .text = "mva",
                                                                    .location = .{
                                                                        .start = .{ .offset = 656, .line = 29, .column = 34 },
                                                                        .end = .{ .offset = 659, .line = 29, .column = 37 },
                                                                    },
                                                                },
                                                            },
                                                            .arguments = &[_]FQLExpression{
                                                                .{
                                                                    .anonymous_field_access = .{
                                                                        .field = .{
                                                                            .identifier = .{
                                                                                .text = "categories",
                                                                                .location = .{
                                                                                    .start = .{ .offset = 661, .line = 29, .column = 39 },
                                                                                    .end = .{ .offset = 671, .line = 29, .column = 49 },
                                                                                },
                                                                            },
                                                                        },
                                                                        .location = .{
                                                                            .start = .{ .offset = 660, .line = 29, .column = 38 },
                                                                            .end = .{ .offset = 671, .line = 29, .column = 49 },
                                                                        },
                                                                    },
                                                                },
                                                            },
                                                            .location = .{
                                                                .start = .{ .offset = 656, .line = 29, .column = 34 },
                                                                .end = .{ .offset = 672, .line = 29, .column = 50 },
                                                            },
                                                            .comma_positions = &.{},
                                                            .lparen_position = .{ .offset = 659, .line = 29, .column = 37 },
                                                        },
                                                    },
                                                },
                                                .location = .{
                                                    .start = .{ .offset = 651, .line = 29, .column = 29 },
                                                    .end = .{ .offset = 673, .line = 29, .column = 51 },
                                                },
                                                .comma_positions = &.{},
                                                .lparen_position = .{ .offset = 655, .line = 29, .column = 33 },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 626, .line = 29, .column = 4 },
                                        .end = .{ .offset = 674, .line = 29, .column = 52 },
                                    },
                                    .lbracket_position = .{ .offset = 633, .line = 29, .column = 11 },
                                    .comma_positions = &.{
                                        .{ .offset = 649, .line = 29, .column = 27 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 589, .line = 27, .column = 2 },
                                .end = .{ .offset = 678, .line = 30, .column = 3 },
                            },
                            .lbrace_position = .{ .offset = 602, .line = 27, .column = 15 },
                        },
                    },
                    .{
                        .check_constraint = .{
                            .name = .{
                                .text = "posQuantity",
                                .location = .{
                                    .start = .{ .offset = 688, .line = 32, .column = 8 },
                                    .end = .{ .offset = 699, .line = 32, .column = 19 },
                                },
                            },
                            .predicate = .{
                                .isolated = .{
                                    .expression = &FQLExpression{
                                        .function = .{
                                            .parameters = .{
                                                .long = .{
                                                    .parameters = &.{
                                                        .{
                                                            .text = "doc",
                                                            .location = .{
                                                                .start = .{ .offset = 702, .line = 32, .column = 22 },
                                                                .end = .{ .offset = 705, .line = 32, .column = 25 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 701, .line = 32, .column = 21 },
                                                        .end = .{ .offset = 706, .line = 32, .column = 26 },
                                                    },
                                                    .comma_positions = &.{},
                                                },
                                            },
                                            .body = &FQLExpression{
                                                .binary_operation = .{
                                                    .lhs = &FQLExpression{
                                                        .field_access = .{
                                                            .value = &FQLExpression{
                                                                .identifier = .{
                                                                    .text = "doc",
                                                                    .location = .{
                                                                        .start = .{ .offset = 710, .line = 32, .column = 30 },
                                                                        .end = .{ .offset = 713, .line = 32, .column = 33 },
                                                                    },
                                                                },
                                                            },
                                                            .field = .{
                                                                .identifier = .{
                                                                    .text = "quantity",
                                                                    .location = .{
                                                                        .start = .{ .offset = 714, .line = 32, .column = 34 },
                                                                        .end = .{ .offset = 722, .line = 32, .column = 42 },
                                                                    },
                                                                },
                                                            },
                                                            .location = .{
                                                                .start = .{ .offset = 710, .line = 32, .column = 30 },
                                                                .end = .{ .offset = 722, .line = 32, .column = 42 },
                                                            },
                                                            .dot_position = .{ .offset = 713, .line = 32, .column = 33 },
                                                        },
                                                    },
                                                    .operator = .greater_than_or_equal,
                                                    .rhs = &FQLExpression{
                                                        .number_literal = .{
                                                            .text = "0",
                                                            .location = .{
                                                                .start = .{ .offset = 726, .line = 32, .column = 46 },
                                                                .end = .{ .offset = 727, .line = 32, .column = 47 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 710, .line = 32, .column = 30 },
                                                        .end = .{ .offset = 727, .line = 32, .column = 47 },
                                                    },
                                                    .operator_position = .{ .offset = 723, .line = 32, .column = 43 },
                                                },
                                            },
                                            .location = .{
                                                .start = .{ .offset = 701, .line = 32, .column = 21 },
                                                .end = .{ .offset = 727, .line = 32, .column = 47 },
                                            },
                                            .equal_rarrow_position = .{ .offset = 707, .line = 32, .column = 27 },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 700, .line = 32, .column = 20 },
                                        .end = .{ .offset = 728, .line = 32, .column = 48 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 682, .line = 32, .column = 2 },
                                .end = .{ .offset = 728, .line = 32, .column = 48 },
                            },
                        },
                    },
                    .{
                        .computed_field = .{
                            .name = .{
                                .text = "InventoryValue",
                                .location = .{
                                    .start = .{ .offset = 739, .line = 33, .column = 10 },
                                    .end = .{ .offset = 753, .line = 33, .column = 24 },
                                },
                            },
                            .type = .{
                                .named = .{
                                    .text = "Number",
                                    .location = .{
                                        .start = .{ .offset = 755, .line = 33, .column = 26 },
                                        .end = .{ .offset = 761, .line = 33, .column = 32 },
                                    },
                                },
                            },
                            .function = .{
                                .isolated = .{
                                    .expression = &FQLExpression{
                                        .binary_operation = .{
                                            .lhs = &FQLExpression{
                                                .anonymous_field_access = .{
                                                    .field = .{
                                                        .identifier = .{
                                                            .text = "quantity",
                                                            .location = .{
                                                                .start = .{ .offset = 766, .line = 33, .column = 37 },
                                                                .end = .{ .offset = 774, .line = 33, .column = 45 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 765, .line = 33, .column = 36 },
                                                        .end = .{ .offset = 774, .line = 33, .column = 45 },
                                                    },
                                                },
                                            },
                                            .operator = .multiply,
                                            .rhs = &FQLExpression{
                                                .anonymous_field_access = .{
                                                    .field = .{
                                                        .identifier = .{
                                                            .text = "price",
                                                            .location = .{
                                                                .start = .{ .offset = 778, .line = 33, .column = 49 },
                                                                .end = .{ .offset = 783, .line = 33, .column = 54 },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{ .offset = 777, .line = 33, .column = 48 },
                                                        .end = .{ .offset = 783, .line = 33, .column = 54 },
                                                    },
                                                },
                                            },
                                            .location = .{
                                                .start = .{ .offset = 765, .line = 33, .column = 36 },
                                                .end = .{ .offset = 783, .line = 33, .column = 54 },
                                            },
                                            .operator_position = .{ .offset = 775, .line = 33, .column = 46 },
                                        },
                                    },
                                    .location = .{
                                        .start = .{ .offset = 764, .line = 33, .column = 35 },
                                        .end = .{ .offset = 784, .line = 33, .column = 55 },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 731, .line = 33, .column = 2 },
                                .end = .{ .offset = 784, .line = 33, .column = 55 },
                            },
                            .colon_position = .{ .offset = 753, .line = 33, .column = 24 },
                            .equal_position = .{ .offset = 762, .line = 33, .column = 33 },
                        },
                    },
                    .{
                        .history_days = .{
                            .node = .{
                                .text = "3",
                                .location = .{
                                    .start = .{ .offset = 801, .line = 35, .column = 15 },
                                    .end = .{ .offset = 802, .line = 35, .column = 16 },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 788, .line = 35, .column = 2 },
                                .end = .{ .offset = 802, .line = 35, .column = 16 },
                            },
                        },
                    },
                    .{
                        .document_ttls = .{
                            .node = .{
                                .value = true,
                                .location = .{
                                    .start = .{ .offset = 819, .line = 36, .column = 16 },
                                    .end = .{ .offset = 823, .line = 36, .column = 20 },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 805, .line = 36, .column = 2 },
                                .end = .{ .offset = 823, .line = 36, .column = 20 },
                            },
                        },
                    },
                    .{
                        .ttl_days = .{
                            .node = .{
                                .text = "5",
                                .location = .{
                                    .start = .{ .offset = 835, .line = 37, .column = 11 },
                                    .end = .{ .offset = 836, .line = 37, .column = 12 },
                                },
                            },
                            .location = .{
                                .start = .{ .offset = 826, .line = 37, .column = 2 },
                                .end = .{ .offset = 836, .line = 37, .column = 12 },
                            },
                        },
                    },
                },
                .location = .{
                    .start = .{ .offset = 0, .line = 0, .column = 0 },
                    .end = .{ .offset = 838, .line = 38, .column = 1 },
                },
                .collection_position = .{ .offset = 0, .line = 0, .column = 0 },
                .lbrace_position = .{ .offset = 19, .line = 0, .column = 19 },
            },
        },
    );
}

test "function walk body" {
    {
        const function = SchemaDefinition.Function{
            .name = .{ .text = "foo" },
        };

        var walker = function.walkBody(testing.allocator);
        defer walker.deinit();

        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const function = SchemaDefinition.Function{
            .name = .{ .text = "foo" },
            .body = &[_]FQLExpression{},
        };

        var walker = function.walkBody(testing.allocator);
        defer walker.deinit();

        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const function = SchemaDefinition.Function{
            .name = .{ .text = "foo" },
            .body = &[_]FQLExpression{
                .{ .identifier = .{ .text = "hello" } },
                .{ .identifier = .{ .text = "world" } },
            },
        };

        var walker = function.walkBody(testing.allocator);
        defer walker.deinit();

        try testing.expectEqualDeep(&function.body.?[0], try walker.next());
        try testing.expectEqualDeep(&function.body.?[1], try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }
}
