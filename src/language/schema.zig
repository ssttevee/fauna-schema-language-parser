const std = @import("std");
const testing = std.testing;

const Tokenizer = @import("../Tokenizer.zig");
const util = @import("../util.zig");
const parsing = @import("../parsing.zig");

const FQLType = @import("type.zig").FQLType;
const FQLExpression = @import("expression.zig").FQLExpression;

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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll("role ");
                    try writer.writeAll(self.name);
                    if (self.predicate) |predicate| {
                        try writer.writeAll(" {\n");
                        try writer.writeBytesNTimes(indent_str, 2);
                        try writer.writeAll("predicate ");
                        try predicate.printCanonical(writer, indent_str, 2);
                        try writer.writeByte('\n');
                        try writer.writeAll(indent_str);
                        try writer.writeByte('}');
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

            pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                switch (self) {
                    .role => |role| try role.printCanonical(writer, indent_str),
                    inline else => |member, tag| {
                        try writer.writeAll(@tagName(tag));
                        try writer.writeByte(' ');
                        try writer.writeAll(member);
                    },
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

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
            try writer.writeAll("access provider ");
            try writer.writeAll(self.name);
            try writer.writeAll(" {\n");

            if (self.members) |members| {
                for (members) |member| {
                    try writer.writeAll(indent_str);
                    try member.printCanonical(writer, indent_str);
                    try writer.writeByte('\n');
                }
            }

            try writer.writeAll("}\n");
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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll(self.name);
                    try writer.writeAll(": ");
                    try self.type.printCanonical(writer);
                    if (self.default) |default| {
                        try default.printCanonical(writer, indent_str, 2);
                    }
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

                    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                        try writer.writeAll(@tagName(self));
                        try writer.writeAll(" [");
                        switch (self) {
                            inline else => |exprs| {
                                for (exprs, 0..) |expr, i| {
                                    if (i > 0) {
                                        try writer.writeAll(", ");
                                    }

                                    try expr.printCanonical(writer, indent_str, 3);
                                }
                            },
                        }
                        try writer.writeAll("]");
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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll("index ");
                    try writer.writeAll(self.name);
                    try writer.writeAll(" {\n");
                    if (self.members) |members| {
                        for (members) |member| {
                            try writer.writeBytesNTimes(indent_str, 2);
                            try member.printCanonical(writer, indent_str);
                            try writer.writeByte('\n');
                        }
                    }

                    try writer.writeAll(indent_str);
                    try writer.writeAll("}");
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

                    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                        try self.name.printCanonical(writer, indent_str, 2);
                        try writer.writeAll(" = ");
                        try self.value.printCanonical(writer, indent_str, 2);
                    }
                };

                pub const Move = struct {
                    old_name: FQLExpression,
                    new_name: FQLExpression,

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        self.old_name.deinit(allocator);
                        self.new_name.deinit(allocator);
                    }

                    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                        try self.old_name.printCanonical(writer, indent_str, 2);
                        try writer.writeAll(" -> ");
                        try self.new_name.printCanonical(writer, indent_str, 2);
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

                    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                        try self.old_name.printCanonical(writer, indent_str, 2);
                        try writer.writeAll(" -> ");
                        for (self.new_names, 0..) |new_name, i| {
                            if (i > 0) {
                                try writer.writeAll(", ");
                            }

                            try new_name.printCanonical(writer, indent_str, 2);
                        }
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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll(@tagName(self));
                    try writer.writeByte(' ');
                    switch (self) {
                        inline .backfill, .move, .split => |member| try member.printCanonical(writer, indent_str),
                        inline else => |member| try member.printCanonical(writer, indent_str, 2),
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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll("unique [");
                    if (self.terms) |terms| {
                        for (terms, 0..) |term, i| {
                            if (i > 0) {
                                try writer.writeAll(", ");
                            }

                            try term.printCanonical(writer, indent_str, 3);
                        }
                    }

                    try writer.writeAll("]");
                }
            };

            pub const CheckConstraint = struct {
                name: []const u8,
                predicate: FQLExpression,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    self.predicate.deinit(allocator);
                    allocator.free(self.name);
                }

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll("check ");
                    try writer.writeAll(self.name);
                    try writer.writeAll(" ");
                    try self.predicate.printCanonical(writer, indent_str, 2);
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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll("compute ");
                    try writer.writeAll(self.name);
                    if (self.type) |fql_type| {
                        try writer.writeAll(": ");
                        try fql_type.printCanonical(writer);
                    }
                    try writer.writeAll(" = ");
                    try self.function.printCanonical(writer, indent_str, 2);
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

            pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                switch (self) {
                    inline .field, .index, .unique_constraint, .check_constraint, .computed_field => |prop| try prop.printCanonical(writer, indent_str),
                    inline .history_days, .ttl_days => |s, tag| {
                        try writer.writeAll(@tagName(tag));
                        try writer.writeByte(' ');
                        try writer.writeAll(s);
                    },
                    .document_ttls => |document_ttls| try std.fmt.format(writer, "document_ttls {?}", .{document_ttls}),
                    .migrations => |migrations| {
                        try writer.writeAll("migrations {\n");
                        for (migrations) |migration| {
                            try writer.writeBytesNTimes(indent_str, 2);
                            try migration.printCanonical(writer, indent_str);
                            try writer.writeByte('\n');
                        }
                        try writer.writeAll(indent_str);
                        try writer.writeByte('}');
                    },
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

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
            if (self.alias) |alias| {
                try writer.writeAll("@alias(");
                try alias.printCanonical(writer, indent_str, 1);
                try writer.writeAll(")\n");
            }

            try writer.writeAll("collection ");
            try writer.writeAll(self.name);
            try writer.writeAll(" {\n");

            if (self.members) |members| {
                for (members) |member| {
                    try writer.writeAll(indent_str);
                    try member.printCanonical(writer, indent_str);
                    try writer.writeByte('\n');
                }
            }

            try writer.writeAll("}\n");
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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll(self.collection);

                    if (self.predicate) |predicate| {
                        try writer.writeAll(" {\n");
                        try writer.writeBytesNTimes(indent_str, 2);
                        try writer.writeAll("predicate ");
                        try predicate.printCanonical(writer, indent_str, 2);
                        try writer.writeByte('\n');
                        try writer.writeAll(indent_str);
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

                    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                        if (self.predicate) |predicate| {
                            predicate.deinit(allocator);
                        }
                    }

                    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                        try writer.writeAll(@tagName(self.action));
                        if (self.predicate) |predicate| {
                            try writer.writeAll(" {\n");
                            try writer.writeBytesNTimes(indent_str, 3);
                            try writer.writeAll("predicate ");
                            try predicate.printCanonical(writer, indent_str, 3);
                            try writer.writeByte('\n');
                            try writer.writeBytesNTimes(indent_str, 2);
                            try writer.writeByte('}');
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

                pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                    try writer.writeAll(self.resource);
                    try writer.writeAll(" {\n");

                    if (self.actions) |actions| {
                        for (actions) |action| {
                            try writer.writeBytesNTimes(indent_str, 2);
                            try action.printCanonical(writer, indent_str);
                            try writer.writeByte('\n');
                        }
                    }

                    try writer.writeAll(indent_str);
                    try writer.writeByte('}');
                }
            };

            membership: Membership,
            privileges: Privileges,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                switch (self) {
                    inline else => |d| d.deinit(allocator),
                }
            }

            pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
                try writer.writeAll(@tagName(self));
                try writer.writeByte(' ');

                switch (self) {
                    inline else => |member| try member.printCanonical(writer, indent_str),
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

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
            try writer.writeAll("role ");
            try writer.writeAll(self.name);
            try writer.writeAll(" {\n");

            if (self.members) |members| {
                for (members) |member| {
                    try writer.writeAll(indent_str);
                    try member.printCanonical(writer, indent_str);
                    try writer.writeByte('\n');
                }
            }

            try writer.writeAll("}\n");
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

        pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
            if (self.alias) |alias| {
                try writer.writeAll("@alias(");
                try alias.printCanonical(writer, indent_str, 1);
                try writer.writeAll(")\n");
            }

            if (self.role) |role| {
                try writer.writeAll("@role(");
                try role.printCanonical(writer, indent_str, 1);
                try writer.writeAll(")\n");
            }

            try writer.writeAll("function ");
            try writer.writeAll(self.name);
            try writer.writeByte('(');

            if (self.parameters) |parameters| {
                for (parameters, 0..) |parameter, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }

                    try writer.writeAll(parameter.name);
                    if (parameter.type) |param_type| {
                        try writer.writeAll(": ");
                        try param_type.printCanonical(writer);
                    }
                }
            }

            try writer.writeByte(')');
            if (self.return_type) |return_type| {
                try writer.writeAll(": ");
                try return_type.printCanonical(writer);
            }

            try writer.writeAll(" {\n");

            if (self.body) |exprs| {
                for (exprs) |expr| {
                    try writer.writeAll(indent_str);
                    try expr.printCanonical(writer, indent_str, 1);
                    try writer.writeByte('\n');
                }
            }

            try writer.writeAll("}\n");
        }

        pub const Walker = struct {
            allocator: std.mem.Allocator,
            function: *const Function,
            pos: usize = 0,
            walker: ?FQLExpression.Walker.Unmanaged = null,

            pub fn deinit(self: *Walker) void {
                self.* = undefined;
            }

            pub fn next(self: *Walker) !?*const FQLExpression {
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

        pub fn walkBody(self: *const Function, allocator: std.mem.Allocator) Walker {
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

    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |d| d.deinit(allocator),
        }
    }

    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter, indent_str: []const u8) !void {
        switch (self) {
            inline else => |def| try def.printCanonical(writer, indent_str),
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

    pub const Parser = parsing.ManagedParser(struct {
        const State = union(enum) {
            const Annotation = struct {
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

            const AccessProvider = union(enum) {
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
                        for (self.members.items) |member| {
                            member.deinit(allocator);
                        }

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

                        @constCast(&self.members).deinit(allocator);
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

            const Collection = struct {
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

                                    @constCast(&self.fields).deinit(allocator);
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

                            @constCast(&self.members).deinit(allocator);
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

                            @constCast(&self.terms).deinit(allocator);
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

                    @constCast(&self.members).deinit(allocator);
                }
            };

            const Role = struct {
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

                            @constCast(&self.actions).deinit(allocator);
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

                    @constCast(&self.members).deinit(allocator);
                }
            };

            const Function = union(enum) {
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

                        @constCast(&self.params).deinit(allocator);
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

                        @constCast(&self.exprs).deinit(allocator);
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

            @constCast(&self.annotations).deinit(allocator);

            switch (self.state) {
                .empty => {},
                inline else => |s| s.deinit(allocator),
            }
        }

        pub const PushResult = struct {
            save: ?Tokenizer.Token = null,
            definition: ?SchemaDefinition = null,
        };

        pub fn pushToken(self: *@This(), allocator: std.mem.Allocator, token_: Tokenizer.Token) !PushResult {
            // std.debug.print("schema parser state: {s}\n", .{@tagName(self.state)});
            // std.debug.print("got token: {any}\n", .{token});

            var token = token_;
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
                                            .eol, .semi, .rbrace => {
                                                try migrations.statements.append(allocator, .{
                                                    .split = .{
                                                        .old_name = split.first_expr,
                                                        .new_names = try split.after.toOwnedSlice(allocator),
                                                    },
                                                });

                                                migrations.state = .start;

                                                if (token == .rbrace) {
                                                    return .{ .save = .rbrace };
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
                                        .rparen => {
                                            params.param_state = .end;
                                            return .{ .save = token };
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
                            if (before_body.return_type == null and token == .colon) {
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
    });

    pub const parse = @as(fn (allocator: std.mem.Allocator, it: *Tokenizer.TokenIterator) anyerror!?@This(), Parser.parseIterator);
};

pub const parseDefinition = SchemaDefinition.Parser.parseReader;

fn expectParsedDefnEqual(str: []const u8, expected: SchemaDefinition) !void {
    try parsing.checkForLeaks(SchemaDefinition.Parser, str);

    var stream = std.io.fixedBufferStream(str);
    var actual = (try parseDefinition(testing.allocator, stream.reader().any())).?;
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
        \\function MyFunction(x: Number): Number {
        \\  x + 2
        \\}
    ,
        .{
            .function = .{
                .name = "MyFunction",
                .parameters = &[_]SchemaDefinition.Function.Parameter{
                    .{
                        .name = "x",
                        .type = .{ .named = "Number" },
                    },
                },
                .return_type = .{ .named = "Number" },
                .body = &[_]FQLExpression{
                    .{
                        .binary_operation = .{
                            .lhs = &FQLExpression{ .identifier = "x" },
                            .operator = .add,
                            .rhs = &FQLExpression{ .number_literal = "2" },
                        },
                    },
                },
            },
        },
    );

    try expectParsedDefnEqual(
        \\function noparams() {
        \\  "hi"
        \\}
    ,
        .{
            .function = .{
                .name = "noparams",
                .parameters = &.{},
                .body = &[_]FQLExpression{
                    .{ .string_literal = "\"hi\"" },
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

test "function walk body" {
    {
        const function = SchemaDefinition.Function{
            .name = "foo",
        };

        var walker = function.walkBody(testing.allocator);
        defer walker.deinit();

        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const function = SchemaDefinition.Function{
            .name = "foo",
            .body = &[_]FQLExpression{},
        };

        var walker = function.walkBody(testing.allocator);
        defer walker.deinit();

        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const function = SchemaDefinition.Function{
            .name = "foo",
            .body = &[_]FQLExpression{
                .{ .identifier = "hello" },
                .{ .identifier = "world" },
            },
        };

        var walker = function.walkBody(testing.allocator);
        defer walker.deinit();

        try testing.expectEqualDeep(&function.body.?[0], try walker.next());
        try testing.expectEqualDeep(&function.body.?[1], try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }
}
