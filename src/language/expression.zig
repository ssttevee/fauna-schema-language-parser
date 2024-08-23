const std = @import("std");
const testing = std.testing;

const sourcemap = @import("../sourcemap.zig");
const Tokenizer = @import("../Tokenizer.zig");
const util = @import("../util.zig");
const parsing = @import("../parsing.zig");
const common = @import("../common.zig");

const FQLType = @import("type.zig").FQLType;
const TextNode = common.TextNode;
const BooleanNode = common.BooleanNode;
const Position = common.Position;
const SourceLocation = common.SourceLocation;

pub const FQLExpression = union(enum) {
    pub const ObjectLiteral = struct {
        pub const Field = struct {
            pub const Key = union(enum) {
                identifier: TextNode,
                string: TextNode,

                pub fn deinit(self: Key, allocator: std.mem.Allocator) void {
                    switch (self) {
                        inline else => |s| s.deinit(allocator),
                    }
                }

                pub fn dupe(self: Key, allocator: std.mem.Allocator) std.mem.Allocator.Error!Key {
                    return switch (self) {
                        inline else => |k, tag| @unionInit(Key, @tagName(tag), try k.dupe(allocator)),
                    };
                }

                pub fn printCanonical(self: Key, writer: anytype, _: []const u8, _: usize) !void {
                    switch (self) {
                        .identifier => |ident| try ident.printNamedCanonical(writer),
                        .string => |str| try str.printCanonical(writer),
                    }
                }

                pub fn location(self: Key) ?SourceLocation {
                    return switch (self) {
                        inline else => |t| t.location,
                    };
                }
            };

            key: Key,
            value: *const FQLExpression,
            location: ?SourceLocation = null,
            colon_position: ?Position = null,

            pub fn deinit(self: Field, allocator: std.mem.Allocator) void {
                self.key.deinit(allocator);
                self.value.deinit(allocator);
                allocator.destroy(self.value);
            }

            pub fn dupe(self: Field, allocator: std.mem.Allocator) std.mem.Allocator.Error!Field {
                const key = try self.key.dupe(allocator);
                errdefer key.deinit(allocator);

                return .{
                    .key = key,
                    .value = try self.value.dupePtr(allocator),
                    .location = self.location,
                    .colon_position = self.colon_position,
                };
            }

            fn multilineCanonical(self: Field) bool {
                return self.value.multilineCanonical();
            }

            pub fn printCanonical(self: Field, writer: anytype, indent_str: []const u8, level: usize) !void {
                try self.key.printCanonical(writer, indent_str, level);

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

                try self.value.printCanonical(writer, indent_str, level);
            }
        };

        fields: ?[]const Field = null,
        location: ?SourceLocation = null,
        comma_positions: ?[]const Position = null,

        pub fn deinit(self: ObjectLiteral, allocator: std.mem.Allocator) void {
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

        pub fn dupe(self: ObjectLiteral, allocator: std.mem.Allocator) std.mem.Allocator.Error!ObjectLiteral {
            return .{
                .fields = try util.slice.deepDupe(allocator, self.fields),
                .location = self.location,
                .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
            };
        }

        fn multilineCanonical(self: ObjectLiteral) bool {
            return self.fields != null and util.slice.some(self.fields.?, Field.multilineCanonical);
        }

        pub fn printCanonical(self: ObjectLiteral, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte('{');

            if (self.fields) |fields| {
                if (fields.len > 0) {
                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(1), null);
                    }

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

                        try field.printCanonical(writer, indent_str, level + @intFromBool(is_multiline));

                        if (is_multiline) {
                            if (self.location) |loc| {
                                if (self.comma_positions) |commas| {
                                    if (commas.len > i) {
                                        sourcemap.setNextWriteMapping(writer, loc.source, commas[i], null);
                                    }
                                }
                            }

                            try writer.writeByte(',');

                            if (self.location != null and self.comma_positions != null and self.comma_positions.?.len > i) {
                                sourcemap.setNextWriteMapping(writer, self.location.?.source, self.comma_positions.?[i].bump(1), null);
                            } else if (field.location) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte('\n');
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    } else {
                        try writer.writeByte(' ');
                    }
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('}');
        }

        fn nthChild(self: *const ObjectLiteral, n: usize) ?*const FQLExpression {
            if (self.fields) |fields| {
                if (n < fields.len) {
                    return fields[n].value;
                }
            }

            return null;
        }
    };

    pub const ArrayLiteral = struct {
        elements: ?[]const FQLExpression = null,
        parens: bool = false,
        location: ?SourceLocation = null,
        comma_positions: ?[]const Position = null,

        pub fn deinit(self: ArrayLiteral, allocator: std.mem.Allocator) void {
            if (self.elements) |elems| {
                for (elems) |elem| {
                    elem.deinit(allocator);
                }

                allocator.free(elems);
            }

            if (self.comma_positions) |comma_positions| {
                allocator.free(comma_positions);
            }
        }

        pub fn dupe(self: ArrayLiteral, allocator: std.mem.Allocator) std.mem.Allocator.Error!ArrayLiteral {
            return .{
                .elements = try util.slice.deepDupe(allocator, self.elements),
                .parens = self.parens,
                .location = self.location,
                .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
            };
        }

        fn multilineCanonical(self: ArrayLiteral) bool {
            return self.elements != null and util.slice.some(self.elements.?, FQLExpression.multilineCanonical);
        }

        pub fn printCanonical(self: ArrayLiteral, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte(if (self.parens) '(' else '[');

            if (self.elements) |elems| {
                if (elems.len > 0) {
                    const is_multiline = elems.len > 1 or util.slice.some(elems, FQLExpression.multilineCanonical);
                    if (is_multiline) {
                        if (self.location) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(1), null);
                        }

                        try writer.writeByte('\n');
                    }

                    for (elems, 0..) |elem, i| {
                        if (is_multiline) {
                            try writer.writeBytesNTimes(indent_str, level + 1);
                        } else if (i > 0) {
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

                        try elem.printCanonical(writer, indent_str, level + @intFromBool(is_multiline));

                        if (is_multiline) {
                            if (self.location) |loc| {
                                if (self.comma_positions) |commas| {
                                    if (commas.len > i) {
                                        sourcemap.setNextWriteMapping(writer, loc.source, commas[i], null);
                                    }
                                }
                            }

                            try writer.writeByte(',');

                            if (self.location != null and self.comma_positions != null and self.comma_positions.?.len > i) {
                                sourcemap.setNextWriteMapping(writer, self.location.?.source, self.comma_positions.?[i].bump(1), null);
                            } else if (elem.location()) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte('\n');
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    }
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte(if (self.parens) ')' else ']');
        }

        fn nthChild(self: *const ArrayLiteral, n: usize) ?*const FQLExpression {
            if (self.elements) |elements| {
                if (n < elements.len) {
                    return &elements[n];
                }
            }

            return null;
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

            fn fromToken(token: Tokenizer.Token) ?Operator {
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

            pub fn tokenLen(self: Operator) usize {
                return self.toString().len;
            }
        };

        operator: Operator,
        lhs: *const FQLExpression,
        rhs: *const FQLExpression,
        location: ?SourceLocation = null,
        operator_position: ?Position = null,

        pub fn deinit(self: BinaryOperation, allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            self.rhs.deinit(allocator);
            allocator.destroy(self.lhs);
            allocator.destroy(self.rhs);
        }

        pub fn dupe(self: BinaryOperation, allocator: std.mem.Allocator) std.mem.Allocator.Error!BinaryOperation {
            const lhs = try self.lhs.dupePtr(allocator);
            errdefer {
                lhs.deinit(allocator);
                allocator.destroy(lhs);
            }

            return .{
                .operator = self.operator,
                .lhs = lhs,
                .rhs = try self.rhs.dupePtr(allocator),
                .location = self.location,
                .operator_position = self.operator_position,
            };
        }

        fn multilineCanonical(self: BinaryOperation) bool {
            return self.lhs.multilineCanonical() or self.rhs.multilineCanonical();
        }

        pub fn printCanonical(self: BinaryOperation, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.lhs.printCanonical(writer, indent_str, level);

            if (self.lhs.location()) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.operator_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeAll(self.operator.toString());

            if (self.location) |loc| {
                if (self.operator_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(@intCast(self.operator.tokenLen())), null);
                }
            }

            try writer.writeByte(' ');

            try self.rhs.printCanonical(writer, indent_str, level);
        }

        fn nthChild(self: *const BinaryOperation, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.lhs;
            }

            if (n == 1) {
                return self.rhs;
            }

            return null;
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
        location: ?SourceLocation = null,

        fn deinit(self: UnaryOperation, allocator: std.mem.Allocator) void {
            self.operand.deinit(allocator);
            allocator.destroy(self.operand);
        }

        pub fn dupe(self: UnaryOperation, allocator: std.mem.Allocator) std.mem.Allocator.Error!UnaryOperation {
            return .{
                .operator = self.operator,
                .operand = try self.operand.dupePtr(allocator),
                .location = self.location,
            };
        }

        fn multilineCanonical(self: UnaryOperation) bool {
            return self.operand.multilineCanonical();
        }

        pub fn printCanonical(self: UnaryOperation, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeAll(self.operator.toString());
            try self.operand.printCanonical(writer, indent_str, level);
        }

        fn nthChild(self: *const UnaryOperation, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.operand;
            }

            return null;
        }
    };

    pub const FieldAccessKey = union(enum) {
        identifier: TextNode,
        expression: *const FQLExpression,

        pub fn deinit(self: FieldAccessKey, allocator: std.mem.Allocator) void {
            switch (self) {
                .identifier => |identifier| identifier.deinit(allocator),
                .expression => |expression| {
                    expression.deinit(allocator);
                    allocator.destroy(expression);
                },
            }
        }

        pub fn dupe(self: FieldAccessKey, allocator: std.mem.Allocator) std.mem.Allocator.Error!FieldAccessKey {
            return switch (self) {
                .identifier => |ident| .{ .identifier = try ident.dupe(allocator) },
                .expression => |expr| .{ .expression = try expr.dupePtr(allocator) },
            };
        }
    };

    pub const FieldAccess = struct {
        value: *const FQLExpression,
        field: FieldAccessKey,
        optional: bool = false,
        location: ?SourceLocation = null,
        question_position: ?Position = null,
        dot_position: ?Position = null,
        lbracket_position: ?Position = null,

        pub fn deinit(self: FieldAccess, allocator: std.mem.Allocator) void {
            self.value.deinit(allocator);
            self.field.deinit(allocator);
            allocator.destroy(self.value);
        }

        pub fn dupe(self: FieldAccess, allocator: std.mem.Allocator) std.mem.Allocator.Error!FieldAccess {
            const field = try self.field.dupe(allocator);
            errdefer field.deinit(allocator);

            return .{
                .value = try self.value.dupePtr(allocator),
                .field = field,
                .optional = self.optional,
                .location = self.location,
                .question_position = self.question_position,
                .dot_position = self.dot_position,
                .lbracket_position = self.lbracket_position,
            };
        }

        fn multilineCanonical(self: FieldAccess) bool {
            return self.value.multilineCanonical() or (self.field == .expression and self.field.expression.multilineCanonical());
        }

        pub fn printCanonical(self: FieldAccess, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.value.printCanonical(writer, indent_str, level);

            if (self.optional) {
                if (self.location) |loc| {
                    if (self.question_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                    }
                }

                try writer.writeByte('?');
            }

            switch (self.field) {
                .identifier => |ident| {
                    if (self.location) |loc| {
                        if (self.dot_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('.');

                    try ident.printNamedCanonical(writer);
                },
                .expression => |expr| {
                    if (self.optional) {
                        if (self.location) |loc| {
                            if (self.dot_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                            }
                        }

                        try writer.writeByte('.');
                    }

                    if (self.location) |loc| {
                        if (self.lbracket_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('[');
                    if (expr.multilineCanonical()) {
                        if (self.location) |loc| {
                            if (self.lbracket_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                            }
                        }

                        try writer.writeByte('\n');
                        try writer.writeBytesNTimes(indent_str, level + 1);

                        try expr.printCanonical(writer, indent_str, level + 1);

                        if (expr.location()) |loc| {
                            sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                        }

                        try writer.writeByte('\n');
                        try writer.writeBytesNTimes(indent_str, level);
                    } else {
                        try expr.printCanonical(writer, indent_str, level);
                    }

                    if (self.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                    }

                    try writer.writeByte(']');
                },
            }
        }

        fn nthChild(self: *const FieldAccess, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.value;
            }

            if (n == 1 and self.field == .expression) {
                return self.field.expression;
            }

            return null;
        }
    };

    pub const Invocation = struct {
        function: *const FQLExpression,
        arguments: ?[]const FQLExpression = null,
        location: ?SourceLocation = null,
        comma_positions: ?[]const Position = null,
        lparen_position: ?Position = null,

        pub fn deinit(self: Invocation, allocator: std.mem.Allocator) void {
            if (self.arguments) |arguments| {
                for (arguments) |argument| {
                    argument.deinit(allocator);
                }

                allocator.free(arguments);
            }

            self.function.deinit(allocator);
            allocator.destroy(self.function);

            if (self.comma_positions) |comma_positions| {
                allocator.free(comma_positions);
            }
        }

        pub fn dupe(self: Invocation, allocator: std.mem.Allocator) std.mem.Allocator.Error!Invocation {
            const function = try self.function.dupePtr(allocator);
            errdefer {
                function.deinit(allocator);
                allocator.destroy(function);
            }

            return .{
                .function = function,
                .arguments = try util.slice.deepDupe(allocator, self.arguments),
                .location = self.location,
                .comma_positions = util.slice.deepDupe(allocator, self.comma_positions) catch unreachable,
                .lparen_position = self.lparen_position,
            };
        }

        fn multilineCanonical(self: Invocation) bool {
            return self.function.multilineCanonical() or (self.arguments != null and util.slice.some(self.arguments.?, FQLExpression.multilineCanonical));
        }

        pub fn printCanonical(self: Invocation, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.function.printCanonical(writer, indent_str, level);

            if (self.location) |loc| {
                if (self.lparen_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('(');

            if (self.arguments) |args| {
                if (args.len > 0) {
                    const is_multiline = util.slice.some(args, FQLExpression.multilineCanonical);
                    if (is_multiline) {
                        if (self.location) |loc| {
                            if (self.lparen_position) |pos| {
                                sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                            }
                        }

                        try writer.writeByte('\n');
                    }

                    for (args, 0..) |arg, i| {
                        if (is_multiline) {
                            try writer.writeBytesNTimes(indent_str, level + 1);
                        } else if (i > 0) {
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

                        try arg.printCanonical(writer, indent_str, level + @intFromBool(is_multiline));

                        if (is_multiline) {
                            if (self.location) |loc| {
                                if (self.comma_positions) |commas| {
                                    if (commas.len > i) {
                                        sourcemap.setNextWriteMapping(writer, loc.source, commas[i], null);
                                    }
                                }
                            }

                            try writer.writeByte(',');

                            if (self.location != null and self.comma_positions != null and self.comma_positions.?.len > i) {
                                sourcemap.setNextWriteMapping(writer, self.location.?.source, self.comma_positions.?[i].bump(1), null);
                            } else if (arg.location()) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte('\n');
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    }
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte(')');
        }

        fn nthChild(self: *const Invocation, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.function;
            }

            if (self.arguments) |arguments| {
                if (n - 1 < arguments.len) {
                    return &arguments[n - 1];
                }
            }

            return null;
        }
    };

    pub const VariableDeclaration = struct {
        name: TextNode,
        type: ?FQLType = null,
        value: *const FQLExpression,
        location: ?SourceLocation = null,
        colon_position: ?Position = null,
        equal_position: ?Position = null,

        pub fn deinit(self: VariableDeclaration, allocator: std.mem.Allocator) void {
            if (self.type) |fql_type| {
                fql_type.deinit(allocator);
            }

            self.name.deinit(allocator);
            self.value.deinit(allocator);
            allocator.destroy(self.value);
        }

        pub fn dupe(self: VariableDeclaration, allocator: std.mem.Allocator) std.mem.Allocator.Error!VariableDeclaration {
            const name = try self.name.dupe(allocator);
            errdefer name.deinit(allocator);

            var fql_type: ?FQLType = null;
            if (self.type) |t| {
                fql_type = try t.dupe(allocator);
            }

            errdefer if (fql_type) |t| {
                t.deinit(allocator);
            };

            return .{
                .name = name,
                .type = fql_type,
                .value = try self.value.dupePtr(allocator),
                .location = self.location,
                .colon_position = self.colon_position,
                .equal_position = self.equal_position,
            };
        }

        fn multilineCanonical(self: VariableDeclaration) bool {
            return self.value.multilineCanonical();
        }

        pub fn printCanonical(self: VariableDeclaration, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeAll("let");

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(3), null);
            }

            try writer.writeByte(' ');

            try self.name.printNamedCanonical(writer);

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

            try self.value.printCanonical(writer, indent_str, level);
        }

        fn nthChild(self: *const VariableDeclaration, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.value;
            }

            return null;
        }
    };

    pub const Conditional = struct {
        condition: *const FQLExpression,
        body: *const FQLExpression,
        @"else": ?*const FQLExpression = null,
        location: ?SourceLocation = null,
        lparen_position: ?Position = null,
        rparen_position: ?Position = null,
        else_position: ?Position = null,

        pub fn deinit(self: Conditional, allocator: std.mem.Allocator) void {
            if (self.@"else") |@"else"| {
                @"else".deinit(allocator);
                allocator.destroy(@"else");
            }

            self.body.deinit(allocator);
            allocator.destroy(self.body);
            self.condition.deinit(allocator);
            allocator.destroy(self.condition);
        }

        pub fn dupe(self: Conditional, allocator: std.mem.Allocator) std.mem.Allocator.Error!Conditional {
            var else_expr: ?*const FQLExpression = null;
            if (self.@"else") |expr| {
                else_expr = try expr.dupePtr(allocator);
            }

            errdefer if (else_expr) |expr| {
                expr.deinit(allocator);
                allocator.destroy(expr);
            };

            const body = try self.body.dupePtr(allocator);
            errdefer {
                body.deinit(allocator);
                allocator.destroy(body);
            }

            return .{
                .condition = try self.condition.dupePtr(allocator),
                .body = body,
                .@"else" = else_expr,
                .location = self.location,
                .lparen_position = self.lparen_position,
                .rparen_position = self.rparen_position,
                .else_position = self.else_position,
            };
        }

        fn multilineCanonical(self: Conditional) bool {
            return self.condition.multilineCanonical() or self.body.multilineCanonical() or (self.@"else" != null and self.@"else".?.multilineCanonical());
        }

        pub fn printCanonical(self: Conditional, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeAll("if");

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(2), null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.lparen_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('(');

            if (self.condition.multilineCanonical()) {
                if (self.location) |loc| {
                    if (self.lparen_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                    }
                }

                try writer.writeByte('\n');
                try writer.writeBytesNTimes(indent_str, level + 1);

                try self.condition.printCanonical(writer, indent_str, level + 1);

                if (self.condition.location()) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                }

                try writer.writeByte('\n');
                try writer.writeBytesNTimes(indent_str, level);
            } else {
                try self.condition.printCanonical(writer, indent_str, level);
            }

            if (self.location) |loc| {
                if (self.rparen_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte(')');

            if (self.location) |loc| {
                if (self.rparen_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                }
            }

            try writer.writeByte(' ');

            try self.body.printCanonical(writer, indent_str, level);

            if (self.@"else") |expr| {
                if (self.body.location()) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                }

                try writer.writeByte(' ');

                if (self.location) |loc| {
                    if (self.else_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                    }
                }

                try writer.writeAll("else");

                if (self.location) |loc| {
                    if (self.else_position) |pos| {
                        sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(4), null);
                    }
                }

                try writer.writeByte(' ');

                try expr.printCanonical(writer, indent_str, level);
            }
        }

        fn nthChild(self: *const Conditional, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.condition;
            }

            if (n == 1) {
                return self.body;
            }

            if (n == 2) {
                if (self.@"else") |expr| {
                    return expr;
                }
            }

            return null;
        }
    };

    pub const Function = struct {
        pub const Parameters = union(enum) {
            long: struct {
                parameters: ?[]const TextNode = null,
                variadic: bool = false,
                location: ?SourceLocation = null,
                comma_positions: ?[]const Position = null,
                dot3_position: ?Position = null,
            },
            short: TextNode,

            pub fn deinit(self: Parameters, allocator: std.mem.Allocator) void {
                switch (self) {
                    .long => |long| {
                        if (long.parameters) |params| {
                            for (params) |param| {
                                param.deinit(allocator);
                            }

                            allocator.free(params);
                        }

                        if (long.comma_positions) |comma_positions| {
                            allocator.free(comma_positions);
                        }
                    },
                    .short => |param| param.deinit(allocator),
                }
            }

            pub fn dupe(self: Parameters, allocator: std.mem.Allocator) std.mem.Allocator.Error!Parameters {
                return switch (self) {
                    .long => |long| .{
                        .long = .{
                            .parameters = try util.slice.deepDupe(allocator, long.parameters),
                            .variadic = long.variadic,
                            .location = long.location,
                            .comma_positions = util.slice.deepDupe(allocator, long.comma_positions) catch unreachable,
                            .dot3_position = long.dot3_position,
                        },
                    },
                    .short => |short| .{ .short = try short.dupe(allocator) },
                };
            }

            pub fn location(self: Parameters) ?SourceLocation {
                return switch (self) {
                    .long => |long| long.location,
                    .short => |short| short.location,
                };
            }
        };

        parameters: Parameters,
        body: *const FQLExpression,
        location: ?SourceLocation = null,
        equal_rarrow_position: ?Position = null,

        pub fn deinit(self: Function, allocator: std.mem.Allocator) void {
            self.parameters.deinit(allocator);
            self.body.deinit(allocator);
            allocator.destroy(self.body);
        }

        pub fn dupe(self: Function, allocator: std.mem.Allocator) std.mem.Allocator.Error!Function {
            const parameters = try self.parameters.dupe(allocator);
            errdefer parameters.deinit(allocator);

            return .{
                .parameters = parameters,
                .body = try self.body.dupePtr(allocator),
                .location = self.location,
                .equal_rarrow_position = self.equal_rarrow_position,
            };
        }

        fn multilineCanonical(self: Function) bool {
            return self.body.multilineCanonical();
        }

        pub fn printCanonical(self: Function, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            switch (self.parameters) {
                .long => |long| {
                    if (long.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                    }

                    try writer.writeByte('(');

                    if (long.parameters) |params| {
                        for (params, 0..) |param, i| {
                            if (i > 0) {
                                if (long.location) |loc| {
                                    if (long.comma_positions) |commas| {
                                        if (commas.len >= i) {
                                            sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1], null);
                                        }
                                    }
                                }

                                try writer.writeByte(',');

                                if (long.location) |loc| {
                                    if (long.comma_positions) |commas| {
                                        if (commas.len >= i) {
                                            sourcemap.setNextWriteMapping(writer, loc.source, commas[i - 1].bump(1), null);
                                        }
                                    }
                                }

                                try writer.writeByte(' ');
                            }

                            if (long.variadic and i == params.len - 1) {
                                if (long.location) |loc| {
                                    if (long.dot3_position) |pos| {
                                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                    }
                                }

                                try writer.writeAll("...");
                            }

                            try param.printNamedCanonical(writer);
                        }
                    }

                    if (long.location) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
                    }

                    try writer.writeByte(')');
                },
                .short => |s| try s.printNamedCanonical(writer),
            }

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

            try self.body.printCanonical(writer, indent_str, level);
        }

        fn nthChild(self: *const Function, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.body;
            }

            return null;
        }
    };

    pub const Projection = struct {
        pub const Field = union(enum) {
            short: TextNode,
            long: struct {
                key: TextNode, // apparently this can be an asterisk? not sure what it does though...
                value: *const FQLExpression,
                colon_position: ?Position = null,

                pub fn location(self: @This()) ?SourceLocation {
                    if (self.key.location) |key_loc| {
                        if (self.value.location()) |value_loc| {
                            return .{
                                .source = key_loc.source,
                                .start = key_loc.start,
                                .end = value_loc.end,
                            };
                        }
                    }

                    return null;
                }
            },

            pub fn deinit(self: Field, allocator: std.mem.Allocator) void {
                switch (self) {
                    .short => |s| s.deinit(allocator),
                    .long => |l| {
                        l.key.deinit(allocator);
                        l.value.deinit(allocator);
                        allocator.destroy(l.value);
                    },
                }
            }

            pub fn dupe(self: Field, allocator: std.mem.Allocator) std.mem.Allocator.Error!Field {
                return switch (self) {
                    .short => |short| .{ .short = try short.dupe(allocator) },
                    .long => |long| .{
                        .long = blk: {
                            const key = try long.key.dupe(allocator);
                            errdefer key.deinit(allocator);

                            break :blk .{
                                .key = key,
                                .value = try long.value.dupePtr(allocator),
                                .colon_position = long.colon_position,
                            };
                        },
                    },
                };
            }

            fn multilineCanonical(self: Field) bool {
                return self == .long and self.long.value.multilineCanonical();
            }

            pub fn location(self: Field) ?SourceLocation {
                return switch (self) {
                    .short => |short| short.location,
                    .long => |long| long.location(),
                };
            }
        };

        expression: *const FQLExpression,
        fields: ?[]const Field = null,
        location: ?SourceLocation = null,
        comma_positions: ?[]const Position = null,
        lbrace_position: ?Position = null,

        pub fn deinit(self: Projection, allocator: std.mem.Allocator) void {
            if (self.fields) |fields| {
                for (fields) |field| {
                    field.deinit(allocator);
                }

                allocator.free(fields);
            }

            if (self.comma_positions) |comma_positions| {
                allocator.free(comma_positions);
            }

            self.expression.deinit(allocator);
            allocator.destroy(self.expression);
        }

        pub fn dupe(self: Projection, allocator: std.mem.Allocator) std.mem.Allocator.Error!Projection {
            const expr = try self.expression.dupePtr(allocator);
            errdefer {
                expr.deinit(allocator);
                allocator.destroy(expr);
            }

            return .{
                .expression = expr,
                .fields = try util.slice.deepDupe(allocator, self.fields),
                .location = self.location,
                .comma_positions = try util.slice.deepDupe(allocator, self.comma_positions),
                .lbrace_position = self.lbrace_position,
            };
        }

        fn multilineCanonical(self: Projection) bool {
            return self.expression.multilineCanonical() or (self.fields != null and util.slice.some(self.fields.?, Field.multilineCanonical));
        }

        pub fn printCanonical(self: Projection, writer: std.io.AnyWriter, indent_str: []const u8, level: usize) !void {
            try self.expression.printCanonical(writer, indent_str, level);

            if (self.expression.location()) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
            }

            try writer.writeByte(' ');

            if (self.location) |loc| {
                if (self.lbrace_position) |pos| {
                    sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                }
            }

            try writer.writeByte('{');

            if (self.fields) |fields| {
                if (fields.len > 0) {
                    if (self.location) |loc| {
                        if (self.lbrace_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                        }
                    }

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

                        switch (field) {
                            .short => |short| try short.printNamedCanonical(writer),
                            .long => |long| {
                                try long.key.printCanonical(writer);

                                if (self.location) |loc| {
                                    if (long.colon_position) |pos| {
                                        sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                                    }
                                }

                                try writer.writeByte(':');

                                if (self.location) |loc| {
                                    if (long.colon_position) |pos| {
                                        sourcemap.setNextWriteMapping(writer, loc.source, pos.bump(1), null);
                                    }
                                }

                                try writer.writeByte(' ');

                                try long.value.printCanonical(writer, indent_str, level + 1);
                            },
                        }

                        if (is_multiline) {
                            if (self.location) |loc| {
                                if (self.comma_positions) |commas| {
                                    if (commas.len > i) {
                                        sourcemap.setNextWriteMapping(writer, loc.source, commas[i], null);
                                    }
                                }
                            }

                            try writer.writeByte(',');

                            if (self.location != null and self.comma_positions != null and self.comma_positions.?.len > i) {
                                sourcemap.setNextWriteMapping(writer, self.location.?.source, self.comma_positions.?[i].bump(1), null);
                            } else if (field.location()) |loc| {
                                sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                            }

                            try writer.writeByte('\n');
                        }
                    }

                    if (is_multiline) {
                        try writer.writeBytesNTimes(indent_str, level);
                    } else {
                        try writer.writeByte(' ');
                    }
                }
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeAll("}");
        }

        fn nthChild(self: *const Projection, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.expression;
            }

            if (self.fields) |fields| {
                var i: usize = 0;
                for (fields) |field| {
                    if (field == .long) {
                        if (i == n - 1) {
                            return field.long.value;
                        }

                        i += 1;
                    }
                }
            }

            return null;
        }
    };

    pub const BlockScope = struct {
        statements: []const FQLExpression,
        location: ?SourceLocation = null,

        pub fn deinit(self: BlockScope, allocator: std.mem.Allocator) void {
            for (self.statements) |expr| {
                expr.deinit(allocator);
            }

            allocator.free(self.statements);
        }

        pub fn dupe(self: BlockScope, allocator: std.mem.Allocator) std.mem.Allocator.Error!BlockScope {
            return .{
                .statements = try util.slice.deepDupe(allocator, self.statements),
                .location = self.location,
            };
        }

        fn multilineCanonical(self: BlockScope) bool {
            return self.statements.len > 0;
        }

        pub fn printCanonical(self: BlockScope, writer: anytype, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte('{');

            if (self.statements.len > 0) {
                if (self.location) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.start.bump(1), null);
                }

                try writer.writeByte('\n');

                for (self.statements) |expr| {
                    try writer.writeBytesNTimes(indent_str, level + 1);

                    try expr.printCanonical(writer, indent_str, level + 1);

                    if (expr.location()) |loc| {
                        sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                    }

                    try writer.writeByte('\n');
                }

                try writer.writeBytesNTimes(indent_str, level);
            }

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeAll("}");
        }

        fn nthChild(self: *const BlockScope, n: usize) ?*const FQLExpression {
            if (n < self.statements.len) {
                return &self.statements[n];
            }

            return null;
        }
    };

    pub const AnonymousFieldAccess = struct {
        field: FieldAccessKey,
        location: ?SourceLocation = null,
        lbracket_position: ?Position = null,

        pub fn deinit(self: AnonymousFieldAccess, allocator: std.mem.Allocator) void {
            self.field.deinit(allocator);
        }

        pub fn dupe(self: AnonymousFieldAccess, allocator: std.mem.Allocator) std.mem.Allocator.Error!AnonymousFieldAccess {
            return .{
                .field = try self.field.dupe(allocator),
                .location = self.location,
                .lbracket_position = self.lbracket_position,
            };
        }

        fn multilineCanonical(self: AnonymousFieldAccess) bool {
            return switch (self.field) {
                .identifier => false,
                .expression => |expr| expr.multilineCanonical(),
            };
        }

        pub fn printCanonical(self: AnonymousFieldAccess, writer: anytype, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte('.');

            switch (self.field) {
                .identifier => |ident| try ident.printNamedCanonical(writer),
                .expression => |expr| {
                    if (self.location) |loc| {
                        if (self.lbracket_position) |pos| {
                            sourcemap.setNextWriteMapping(writer, loc.source, pos, null);
                        }
                    }

                    try writer.writeByte('[');
                    try expr.printCanonical(writer, indent_str, level);
                    try writer.writeByte(']');
                },
            }
        }

        fn nthChild(self: *const AnonymousFieldAccess, n: usize) ?*const FQLExpression {
            if (n == 0 and self.field == .expression) {
                return self.field.expression;
            }

            return null;
        }
    };

    pub const Isolated = struct {
        expression: *const FQLExpression,
        location: ?SourceLocation = null,

        pub fn deinit(self: Isolated, allocator: std.mem.Allocator) void {
            self.expression.deinit(allocator);
            allocator.destroy(self.expression);
        }

        pub fn dupe(self: Isolated, allocator: std.mem.Allocator) std.mem.Allocator.Error!Isolated {
            return .{
                .expression = try self.expression.dupePtr(allocator),
                .location = self.location,
            };
        }

        fn multilineCanonical(self: Isolated) bool {
            return self.expression.multilineCanonical();
        }

        pub fn printCanonical(self: Isolated, writer: anytype, indent_str: []const u8, level: usize) !void {
            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
            }

            try writer.writeByte('(');

            try self.expression.printCanonical(writer, indent_str, level);

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte(')');
        }

        fn nthChild(self: *const Isolated, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.expression;
            }

            return null;
        }
    };

    pub const NonNullAssertion = struct {
        expression: *const FQLExpression,
        location: ?SourceLocation = null,

        pub fn deinit(self: NonNullAssertion, allocator: std.mem.Allocator) void {
            self.expression.deinit(allocator);
            allocator.destroy(self.expression);
        }

        pub fn dupe(self: NonNullAssertion, allocator: std.mem.Allocator) std.mem.Allocator.Error!NonNullAssertion {
            return .{
                .expression = try self.expression.dupePtr(allocator),
                .location = self.location,
            };
        }

        fn multilineCanonical(self: NonNullAssertion) bool {
            return self.expression.multilineCanonical();
        }

        pub fn printCanonical(self: NonNullAssertion, writer: anytype, indent_str: []const u8, level: usize) !void {
            try self.expression.printCanonical(writer, indent_str, level);

            if (self.location) |loc| {
                sourcemap.setNextWriteMapping(writer, loc.source, loc.end.bump(-1), null);
            }

            try writer.writeByte('!');
        }

        fn nthChild(self: *const NonNullAssertion, n: usize) ?*const FQLExpression {
            if (n == 0) {
                return self.expression;
            }

            return null;
        }
    };

    null: ?SourceLocation,
    identifier: TextNode,
    number_literal: TextNode,
    string_literal: TextNode,
    boolean_literal: BooleanNode,
    array_literal: ArrayLiteral,
    object_literal: ObjectLiteral,
    unary_operation: UnaryOperation,
    binary_operation: BinaryOperation,
    field_access: FieldAccess,
    invocation: Invocation,
    variable_declaration: VariableDeclaration,
    block_scope: BlockScope,
    conditional: Conditional,
    isolated: Isolated,
    function: Function,
    anonymous_field_access: AnonymousFieldAccess,
    projection: Projection,
    non_null_assertion: NonNullAssertion,

    pub fn deinit(self: FQLExpression, allocator: std.mem.Allocator) void {
        // std.debug.print("deinit FQLExpression.{s}\n", .{@tagName(self)});
        switch (self) {
            .null, .boolean_literal => {},
            inline else => |expr| expr.deinit(allocator),
        }
    }

    pub fn dupe(self: FQLExpression, allocator: std.mem.Allocator) std.mem.Allocator.Error!FQLExpression {
        return switch (self) {
            inline else => |expr, tag| @unionInit(FQLExpression, @tagName(tag), try expr.dupe(allocator)),
            inline .null => |v, tag| @unionInit(FQLExpression, @tagName(tag), v),
        };
    }

    fn dupePtr(self: *const FQLExpression, allocator: std.mem.Allocator) !*FQLExpression {
        const ptr = try allocator.create(FQLExpression);
        errdefer allocator.destroy(ptr);

        ptr.* = try self.dupe(allocator);

        return ptr;
    }

    pub fn location(self: FQLExpression) ?SourceLocation {
        return switch (self) {
            .null => |n| n,
            inline else => |t| t.location,
        };
    }

    fn multilineCanonical(self: FQLExpression) bool {
        return switch (self) {
            .identifier, .number_literal, .string_literal, .null, .boolean_literal => false,
            inline else => |e| e.multilineCanonical(),
        };
    }

    pub fn printCanonical(self: FQLExpression, writer: anytype, indent_str: []const u8, level: usize) @TypeOf(writer).Error!void {
        switch (self) {
            inline .number_literal, .string_literal => |s| try s.printCanonical(writer),
            .identifier => |s| try s.printNamedCanonical(writer),
            .null => {
                if (self.null) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
                }

                try writer.writeAll("null");
            },
            .boolean_literal => |b| try b.printCanonical(writer),
            inline else => |e| try e.printCanonical(writer, indent_str, level),
        }
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;

        comptime var fmt_end = fmt.len;

        comptime var level: usize = 0;
        comptime {
            if (std.mem.indexOfScalar(u8, fmt, '.')) |pos| {
                level = try std.fmt.parseInt(usize, fmt[0..pos], 10);
                fmt_end = pos;
            }
        }

        const index_str: []const u8 = " " ** (std.fmt.parseInt(usize, fmt[0..fmt_end], 10) catch 4);

        try self.printCanonical(writer, index_str, level);
    }

    pub fn toCanonicalString(self: @This(), allocator: std.mem.Allocator) ![]const u8 {
        var str = std.ArrayList(u8).init(allocator);
        defer str.deinit();

        try self.printCanonical(str.writer().any(), "    ", 0);

        return try str.toOwnedSlice();
    }

    fn nthChild(self: *const FQLExpression, n: usize) ?*const FQLExpression {
        return switch (self.*) {
            .null, .identifier, .number_literal, .string_literal, .boolean_literal => null,
            inline else => |*expr| expr.nthChild(n),
        };
    }

    pub const Walker = struct {
        pub const Unmanaged = struct {
            root: *const FQLExpression,
            initialized: bool = false,
            stack: std.ArrayListUnmanaged(struct { expr: *const FQLExpression, next_child: usize = 0 }) = .{},

            pub fn deinit(self: *Unmanaged, allocator: std.mem.Allocator) void {
                self.stack.deinit(allocator);
                self.* = undefined;
            }

            pub fn next(self: *Unmanaged, allocator: std.mem.Allocator) !?*const FQLExpression {
                if (!self.initialized) {
                    self.initialized = true;
                    try self.stack.append(allocator, .{ .expr = self.root, .next_child = 0 });
                    return self.root;
                }

                while (self.stack.items.len > 0) {
                    const last_node = &self.stack.items[self.stack.items.len - 1];
                    if (last_node.expr.nthChild(last_node.next_child)) |child_expr| {
                        last_node.next_child += 1;
                        try self.stack.append(allocator, .{ .expr = child_expr });
                        return child_expr;
                    } else {
                        self.stack.items.len -= 1;
                    }
                } else {
                    return null;
                }
            }
        };

        allocator: std.mem.Allocator,
        inner: Unmanaged,

        pub fn deinit(self: *Walker) void {
            self.inner.deinit(self.allocator);
            self.* = undefined;
        }

        pub fn next(self: *Walker) !?*const FQLExpression {
            return try self.inner.next(self.allocator);
        }
    };

    pub fn walk(self: *const FQLExpression, allocator: std.mem.Allocator) Walker {
        return .{
            .allocator = allocator,
            .inner = .{
                .root = self,
            },
        };
    }

    fn isIdentifier(self: FQLExpression) bool {
        return self == .identifier;
    }

    pub const Parser = parsing.ManagedParser(struct {
        const State = union(enum) {
            const VariableDeclaration = struct {
                start_position: Position,
                name: ?TextNode = null,
                colon_position: ?Position = null,
                type: ?union(enum) { parser: FQLType.Parser.Unmanaged, type: FQLType } = null,
                equal_position: ?Position = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.name) |name| {
                        name.deinit(allocator);
                    }

                    if (self.type) |type_state| {
                        switch (type_state) {
                            inline else => |v| v.deinit(allocator),
                        }
                    }
                }
            };

            const Conditional = struct {
                start_position: Position,
                state: union(enum) {
                    const AfterCondition = struct {
                        lparen_position: Position,
                        condition: FQLExpression,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            self.condition.deinit(allocator);
                        }
                    };

                    const AfterRParen = struct {
                        lparen_position: Position,
                        condition: FQLExpression,
                        rparen_position: Position,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            self.condition.deinit(allocator);
                        }
                    };

                    const AfterBody = struct {
                        condition: FQLExpression,
                        body: FQLExpression,
                        lparen_position: Position,
                        rparen_position: Position,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            self.condition.deinit(allocator);
                            self.body.deinit(allocator);
                        }
                    };

                    const AfterElse = struct {
                        condition: FQLExpression,
                        body: FQLExpression,
                        lparen_position: Position,
                        rparen_position: Position,
                        else_position: Position,

                        fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                            self.condition.deinit(allocator);
                            self.body.deinit(allocator);
                        }
                    };

                    start,
                    after_lparen: Position,
                    after_condition: AfterCondition,
                    after_rparen: AfterRParen,
                    after_body: AfterBody,
                    after_else: AfterElse,
                },

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self.state) {
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
                operator_position: Position,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    self.lhs.deinit(allocator);
                }
            };

            const ArrayLiteral = struct {
                start_position: Position,
                elements: std.ArrayListUnmanaged(FQLExpression) = .{},
                comma_positions: std.ArrayListUnmanaged(Position) = .{},
                parens: bool = false,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    for (self.elements.items) |expr| {
                        expr.deinit(allocator);
                    }

                    @constCast(&self.elements).deinit(allocator);
                    @constCast(&self.comma_positions).deinit(allocator);
                }
            };

            const ObjectLiteral = struct {
                fields: std.ArrayListUnmanaged(FQLExpression.ObjectLiteral.Field) = .{},
                start_position: Position,
                comma_positions: std.ArrayListUnmanaged(Position) = .{},
                state: union(enum) {
                    start,
                    after_key: FQLExpression.ObjectLiteral.Field.Key,
                    after_colon: struct {
                        key: FQLExpression.ObjectLiteral.Field.Key,
                        colon_position: Position,
                    },
                    end,
                } = .start,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    for (self.fields.items) |item| {
                        item.deinit(allocator);
                    }

                    @constCast(&self.fields).deinit(allocator);
                    @constCast(&self.comma_positions).deinit(allocator);
                    switch (self.state) {
                        .after_key => |key| key.deinit(allocator),
                        .after_colon => |after_colon| after_colon.key.deinit(allocator),
                        else => {},
                    }
                }
            };

            const FieldAccess = struct {
                value: FQLExpression,
                optional: bool = false,
                question_position: ?Position = null,
                dot_position: ?Position = null,
                lbracket_position: ?Position = null,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    self.value.deinit(allocator);
                }
            };

            const Invocation = struct {
                function: FQLExpression,
                arguments: std.ArrayListUnmanaged(FQLExpression) = .{},
                comma_positions: std.ArrayListUnmanaged(Position) = .{},
                lparen_position: Position,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    for (self.arguments.items) |expr| {
                        expr.deinit(allocator);
                    }

                    @constCast(&self.arguments).deinit(allocator);
                    @constCast(&self.comma_positions).deinit(allocator);
                    self.function.deinit(allocator);
                }
            };

            const Projection = struct {
                expression: FQLExpression,
                fields: std.ArrayListUnmanaged(FQLExpression.Projection.Field) = .{},
                comma_positions: std.ArrayListUnmanaged(Position) = .{},
                lbrace_position: Position,
                state: union(enum) {
                    start,
                    after_identifier: TextNode,
                    after_colon: struct {
                        identifier: TextNode,
                        colon_position: Position,
                    },
                    end,
                } = .start,

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    switch (self.state) {
                        .after_identifier => |after_identifier| after_identifier.deinit(allocator),
                        .after_colon => |after_colon| after_colon.identifier.deinit(allocator),
                        else => {},
                    }

                    for (self.fields.items) |field| {
                        field.deinit(allocator);
                    }

                    @constCast(&self.comma_positions).deinit(allocator);
                    @constCast(&self.fields).deinit(allocator);
                    self.expression.deinit(allocator);
                }
            };

            const LongFunction = struct {
                parameters: []const TextNode,
                start_position: Position,
                comma_positions: std.ArrayListUnmanaged(Position) = .{},
                rparen_position: ?Position = null,
                dot3_position: ?Position = null,
                equal_rarrow_position: ?Position = null,
                variadic_state: ?union(enum) {
                    start,
                    after_param: TextNode,
                    after_rparen: TextNode,
                } = null,

                fn exprsToParams(allocator: std.mem.Allocator, exprs: []const FQLExpression) ![]TextNode {
                    const params = try allocator.alloc(TextNode, exprs.len);
                    for (exprs, 0..) |elem, i| {
                        params[i] = elem.identifier;
                    }

                    return params;
                }

                fn fromExprs(allocator: std.mem.Allocator, exprs: []const FQLExpression, dot3_position: ?Position) !LongFunction {
                    const params = try allocator.alloc(TextNode, exprs.len);
                    for (exprs, 0..) |elem, i| {
                        params[i] = elem.identifier;
                    }

                    return .{
                        .parameters = try exprsToParams(allocator, exprs),
                        .dot3_position = dot3_position,
                        .variadic_state = if (dot3_position != null) .start else null,
                    };
                }

                fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                    if (self.variadic_state) |variadic_state| {
                        switch (variadic_state) {
                            .start => {},
                            inline else => |s| s.deinit(allocator),
                        }
                    }

                    for (self.parameters) |str| {
                        str.deinit(allocator);
                    }

                    allocator.free(self.parameters);
                    @constCast(&self.comma_positions).deinit(allocator);
                }
            };

            const BlockScope = struct {
                start_position: Position,
                statements: std.ArrayListUnmanaged(FQLExpression) = .{},

                fn deinit(self: State.BlockScope, allocator: std.mem.Allocator) void {
                    for (self.statements.items) |expr| {
                        expr.deinit(allocator);
                    }

                    @constCast(&self.statements).deinit(allocator);
                }
            };

            const AfterLBrace = struct {
                position: Position,
                key: ?FQLExpression.ObjectLiteral.Field.Key = null,

                fn deinit(self: AfterLBrace, allocator: std.mem.Allocator) void {
                    if (self.key) |key| {
                        key.deinit(allocator);
                    }
                }
            };

            const ShortFunction = struct {
                param: TextNode,
                equal_rarrow_position: ?Position = null,

                fn deinit(self: ShortFunction, allocator: std.mem.Allocator) void {
                    self.param.deinit(allocator);
                }
            };

            const AnonymousFieldAccess = struct {
                start_position: Position,
                key: ?FQLExpression = null,
                lbracket_position: ?Position = null,

                fn deinit(self: State.AnonymousFieldAccess, allocator: std.mem.Allocator) void {
                    if (self.key) |key| {
                        key.deinit(allocator);
                    }
                }
            };

            const UnaryOperation = struct {
                operator: FQLExpression.UnaryOperation.Operator,
                start_position: Position,
            };

            empty,
            after_lbrace: AfterLBrace,
            after_identifier: TextNode,
            variable_declaration: State.VariableDeclaration,
            conditional: State.Conditional,
            unary_operation: State.UnaryOperation,
            binary_operation: State.BinaryOperation,
            array_literal: State.ArrayLiteral,
            object_literal: State.ObjectLiteral,
            field_access: State.FieldAccess,
            anonymous_field_access: State.AnonymousFieldAccess,
            invocation: State.Invocation,
            projection: State.Projection,
            block_scope: State.BlockScope,
            long_function: LongFunction,
            short_function: ShortFunction,
            end: FQLExpression,

            fn deinit(self: @This(), allocator: std.mem.Allocator) void {
                // std.debug.print("deinit FQLExpression.Parser.Unmanaged.State.{s}\n", .{@tagName(self)});
                switch (self) {
                    .empty, .unary_operation => {},
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

        pub const PushResult = struct {
            save: ?Tokenizer.TokenWithLocation = null,
            expr: ?FQLExpression = null,
        };

        pub fn pushToken(self: *@This(), allocator: std.mem.Allocator, token_with_location: Tokenizer.TokenWithLocation) !PushResult {
            const token = token_with_location.token;
            const loc = token_with_location.location.?;

            // std.debug.print("expression parser state: {s} {s}\n", .{ @tagName(self.state), if (self.parent) |p| @tagName(p.state) else "" });
            // std.debug.print("got token: {any}\n", .{token});

            if (token == .comment_block or token == .comment_line) {
                return .{};
            }

            switch (self.state) {
                .empty => switch (token) {
                    .eof, .eol, .semi => {},

                    // number literal
                    .number => |num| self.finalizeExpr(.{ .number_literal = .{ .text = try allocator.dupe(u8, num), .location = loc } }),

                    // string literal
                    .string => |str| self.finalizeExpr(.{ .string_literal = .{ .text = try allocator.dupe(u8, str), .location = loc } }),

                    // word indicates an identifier, null/boolean literal or a let/if statement
                    .word => |word| {
                        if (std.meta.stringToEnum(enum { let, @"if", null, true, false }, word)) |keyword| switch (keyword) {
                            .@"if" => self.state = .{ .conditional = .{ .start_position = loc.start, .state = .start } },
                            .let => self.state = .{ .variable_declaration = .{ .start_position = loc.start } },
                            .null => self.finalizeExpr(.{ .null = loc }),
                            .true => self.finalizeExpr(.{ .boolean_literal = .{ .value = true, .location = loc } }),
                            .false => self.finalizeExpr(.{ .boolean_literal = .{ .value = false, .location = loc } }),
                        } else {
                            self.state = .{ .after_identifier = .{ .text = try allocator.dupe(u8, word), .location = loc } };
                        }
                    },

                    // bang indicates a logical-not unary operation
                    .bang => {
                        self.state = .{
                            .unary_operation = .{
                                .operator = .logical_not,
                                .start_position = loc.start,
                            },
                        };
                        try self.startChildState(allocator);
                    },

                    // tilde indicates a bitwise-not unary operation
                    .tilde => {
                        self.state = .{
                            .unary_operation = .{
                                .operator = .bitwise_not,
                                .start_position = loc.start,
                            },
                        };
                        try self.startChildState(allocator);
                    },

                    // minus indicates an arithmetic-not unary operation
                    .minus => {
                        self.state = .{
                            .unary_operation = .{
                                .operator = .arithmetic_not,
                                .start_position = loc.start,
                            },
                        };
                        try self.startChildState(allocator);
                    },

                    // lbracket indicates an array literal (aka a tuple)
                    .lbracket => {
                        self.state = .{
                            .array_literal = .{
                                .start_position = loc.start,
                            },
                        };
                        try self.startChildState(allocator);
                    },

                    // dot indicates an anonymous field access (i.e. `.foo` or `.["foo"]`)
                    .dot => self.state = .{ .anonymous_field_access = .{ .start_position = loc.start } },

                    // lparen indicates either an isolated (parenthesized) expression, parenthesized tuple or a long form anonymous function (i.e. `() => {}`)
                    .lparen => {
                        // pretend it's a parenthesized tuple until proved otherwise
                        self.state = .{
                            .array_literal = .{
                                .start_position = loc.start,
                                .parens = true,
                            },
                        };
                        try self.startChildState(allocator);
                    },

                    // lbrace indicates either a block scope or an object literal
                    .lbrace => self.state = .{ .after_lbrace = .{ .position = loc.start } },
                    else => {
                        if (self.parent) |parent| {
                            switch (parent.state) {
                                .array_literal => |array_literal| {
                                    if (token == .rbracket) {
                                        defer allocator.destroy(parent);
                                        self.* = parent.*;

                                        return .{ .save = token_with_location };
                                    }

                                    if (array_literal.parens and token == .dot3) {
                                        if (util.slice.every(array_literal.elements, isIdentifier)) {
                                            defer @constCast(&array_literal.elements).deinit(allocator);

                                            parent.state = .{
                                                .long_function = .{
                                                    .parameters = try State.LongFunction.exprsToParams(allocator, array_literal.elements.items),
                                                    .start_position = array_literal.start_position,
                                                    .comma_positions = array_literal.comma_positions,
                                                    .dot3_position = loc.start,
                                                    .variadic_state = .start,
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

                                        return .{ .save = token_with_location };
                                    }
                                },
                                .invocation => {
                                    if (token == .rparen) {
                                        defer allocator.destroy(parent);
                                        self.* = parent.*;

                                        return .{ .save = token_with_location };
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
                            self.state = .{
                                .short_function = .{
                                    .param = identifier,
                                    .equal_rarrow_position = loc.start,
                                },
                            };
                            try self.startChildState(allocator);
                        },
                        else => {
                            self.finalizeExpr(.{ .identifier = identifier });
                            return .{ .save = token_with_location };
                        },
                    }
                },
                .variable_declaration => |*variable_declaration| {
                    if (variable_declaration.name == null) {
                        switch (token) {
                            .word => |word| {
                                variable_declaration.name = .{
                                    .text = try allocator.dupe(u8, word),
                                    .location = loc,
                                };
                            },
                            else => {
                                std.log.err("unexpected token: expected word but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
                    } else if (variable_declaration.type == null) {
                        switch (token) {
                            .equal => {
                                variable_declaration.equal_position = loc.start;
                                try self.startChildState(allocator);
                            },
                            .colon => {
                                variable_declaration.colon_position = loc.start;
                                variable_declaration.type = .{ .parser = .{} };
                            },
                            else => {
                                std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
                    } else if (variable_declaration.type.? == .parser) {
                        const result = try variable_declaration.type.?.parser.pushToken(allocator, token_with_location);
                        if (result.type) |fql_type| {
                            variable_declaration.type = .{ .type = fql_type };
                        }

                        return .{ .save = result.save };
                    } else {
                        switch (token) {
                            .equal => {
                                variable_declaration.equal_position = loc.start;
                                try self.startChildState(allocator);
                            },
                            else => {
                                std.log.err("unexpected token: expected equal but got {s}", .{@tagName(token)});
                                return error.UnexpectedToken;
                            },
                        }
                    }
                },
                .conditional => |*conditional| switch (conditional.state) {
                    .start => switch (token) {
                        .lparen => {
                            conditional.state = .{ .after_lparen = loc.start };
                            try self.startChildState(allocator);
                        },
                        else => {
                            std.log.err("unexpected token: expected lparen but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .after_condition => |after_condition| switch (token) {
                        .rparen => {
                            conditional.state = .{
                                .after_rparen = .{
                                    .lparen_position = after_condition.lparen_position,
                                    .condition = after_condition.condition,
                                    .rparen_position = loc.start,
                                },
                            };
                            try self.startChildState(allocator);
                        },
                        else => {
                            std.log.err("unexpected token: expected rparen but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .after_body => |after_body| {
                        if (token == .word and std.mem.eql(u8, token.word, "else")) {
                            conditional.state = .{
                                .after_else = .{
                                    .lparen_position = after_body.lparen_position,
                                    .condition = after_body.condition,
                                    .rparen_position = after_body.rparen_position,
                                    .body = after_body.body,
                                    .else_position = loc.start,
                                },
                            };
                            try self.startChildState(allocator);
                        } else {
                            self.finalizeExpr(.{
                                .conditional = .{
                                    .condition = try util.mem.createCopy(FQLExpression, allocator, &after_body.condition),
                                    .body = try util.mem.createCopy(FQLExpression, allocator, &after_body.body),
                                    .location = .{
                                        .source = loc.source,
                                        .start = conditional.start_position,
                                        .end = after_body.body.location().?.end,
                                    },
                                    .lparen_position = after_body.lparen_position,
                                    .rparen_position = after_body.rparen_position,
                                },
                            });

                            return .{ .save = token_with_location };
                        }
                    },
                    else => unreachable,
                },
                .array_literal => |*array_literal| switch (token) {
                    .eol => {},
                    .comma => {
                        try array_literal.comma_positions.append(allocator, loc.start);

                        try self.startChildState(allocator);
                    },
                    else => {
                        if (array_literal.parens and token == .rparen and array_literal.elements.items.len == 1) {
                            var elems = array_literal.elements;
                            defer elems.deinit(allocator);

                            // parenthesized tuples with exactly 1 element are actually not tuples at all! :o
                            self.finalizeExpr(.{
                                .isolated = .{
                                    .expression = try util.mem.createCopy(FQLExpression, allocator, &elems.items[0]),
                                    .location = .{
                                        .source = loc.source,
                                        .start = array_literal.start_position,
                                        .end = loc.end,
                                    },
                                },
                            });

                            return .{};
                        }

                        if ((array_literal.parens and token == .rparen) or (!array_literal.parens and token == .rbracket)) {
                            self.finalizeExpr(.{
                                .array_literal = .{
                                    .elements = try array_literal.elements.toOwnedSlice(allocator),
                                    .location = .{
                                        .source = loc.source,
                                        .start = array_literal.start_position,
                                        .end = loc.end,
                                    },
                                    .comma_positions = try array_literal.comma_positions.toOwnedSlice(allocator),
                                },
                            });

                            return .{};
                        }

                        std.log.err("unexpected token: expected comma or {s} but got {s}", .{ if (array_literal.parens) "rparen" else "rbracket", @tagName(token) });
                        return error.UnexpectedToken;
                    },
                },
                .field_access => |*field_access| switch (token) {
                    .word => |word| {
                        self.finalizeExpr(.{
                            .field_access = .{
                                .value = try util.mem.createCopy(FQLExpression, allocator, &field_access.value),
                                .field = .{ .identifier = .{ .text = try allocator.dupe(u8, word), .location = loc } },
                                .optional = field_access.optional,
                                .location = .{
                                    .source = loc.source,
                                    .start = field_access.value.location().?.start,
                                    .end = loc.end,
                                },
                                .question_position = field_access.question_position,
                                .dot_position = field_access.dot_position,
                                .lbracket_position = field_access.lbracket_position,
                            },
                        });
                    },
                    .lbracket => {
                        field_access.lbracket_position = loc.start;
                        try self.startChildState(allocator);
                    },
                    else => {
                        std.log.err("unexpected token: expected word or lbracket but got {s}", .{@tagName(token)});
                        return error.UnexpectedToken;
                    },
                },
                .anonymous_field_access => |anonymous_field_access| {
                    if (anonymous_field_access.key) |key| {
                        switch (token) {
                            .rbracket => {
                                self.finalizeExpr(.{
                                    .anonymous_field_access = .{
                                        .field = .{
                                            .expression = try util.mem.createCopy(FQLExpression, allocator, &key),
                                        },
                                        .location = .{
                                            .source = loc.source,
                                            .start = anonymous_field_access.start_position,
                                            .end = loc.end,
                                        },
                                        .lbracket_position = anonymous_field_access.lbracket_position,
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
                                        .field = .{
                                            .identifier = .{ .text = try allocator.dupe(u8, word), .location = loc },
                                        },
                                        .location = .{
                                            .source = loc.source,
                                            .start = anonymous_field_access.start_position,
                                            .end = loc.end,
                                        },
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
                        .word => |word| projection.state = .{
                            .after_identifier = .{
                                .text = try allocator.dupe(u8, word),
                                .location = loc,
                            },
                        },
                        .rbrace => {
                            projection.state = .end;
                            return .{ .save = token_with_location };
                        },
                        else => {
                            std.log.err("unexpected token: expected word or rbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .after_identifier => |identifier| switch (token) {
                        .eol => {},
                        .colon => {
                            projection.state = .{
                                .after_colon = .{
                                    .identifier = identifier,
                                    .colon_position = loc.start,
                                },
                            };
                            try self.startChildState(allocator);
                        },
                        .comma => {
                            try projection.comma_positions.append(allocator, loc.start);
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
                            return .{ .save = token_with_location };
                        },
                        else => {
                            std.log.err("unexpected token: expected colon, comma or rbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .end => switch (token) {
                        .comma => {
                            try projection.comma_positions.append(allocator, loc.start);
                            projection.state = .start;
                        },
                        .rbrace => {
                            self.finalizeExpr(.{
                                .projection = .{
                                    .expression = try util.mem.createCopy(FQLExpression, allocator, &projection.expression),
                                    .fields = try projection.fields.toOwnedSlice(allocator),
                                    .location = .{
                                        .source = loc.source,
                                        .start = projection.expression.location().?.start,
                                        .end = loc.end,
                                    },
                                    .lbrace_position = projection.lbrace_position,
                                    .comma_positions = try projection.comma_positions.toOwnedSlice(allocator),
                                },
                            });
                        },
                        else => {
                            std.log.err("unexpected token: expected colon, comma or rbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    else => {
                        std.debug.panic("invalid parser state: projection: {s}", .{@tagName(projection.state)});
                    },
                },
                .after_lbrace => |after_lbrace| {
                    // determine if we're dealing with a block scope or an object literal
                    if (after_lbrace.key) |key| {
                        switch (token) {
                            .eol => {},
                            .colon => {
                                self.state = .{
                                    .object_literal = .{
                                        .start_position = after_lbrace.position,
                                        .state = .{
                                            .after_colon = .{
                                                .key = key,
                                                .colon_position = loc.start,
                                            },
                                        },
                                    },
                                };
                                try self.startChildState(allocator);
                            },
                            else => {
                                defer key.deinit(allocator);

                                // it's not an object literal...
                                self.state = .{
                                    .block_scope = .{
                                        .start_position = after_lbrace.position,
                                    },
                                };

                                try self.startChildState(allocator);

                                const result = try self.pushToken(allocator, switch (key) {
                                    .identifier => |ident| .{ .token = .{ .word = ident.text }, .location = ident.location },
                                    .string => |str| .{ .token = .{ .string = str.text }, .location = str.location },
                                });

                                std.debug.assert(result.save == null);

                                return .{ .save = token_with_location, .expr = result.expr };
                            },
                        }
                    } else {
                        switch (token) {
                            .eol => {},
                            .word => |word| {
                                // it still could be either...
                                self.state.after_lbrace.key = .{
                                    .identifier = .{
                                        .text = try allocator.dupe(u8, word),
                                        .location = loc,
                                    },
                                };
                            },
                            .string => |str| {
                                // it still could be either...
                                self.state.after_lbrace.key = .{
                                    .string = .{
                                        .text = try allocator.dupe(u8, str),
                                        .location = loc,
                                    },
                                };
                            },
                            else => {
                                // it's probably a block scope
                                self.state = .{
                                    .block_scope = .{
                                        .start_position = after_lbrace.position,
                                    },
                                };
                                try self.startChildState(allocator);
                                return .{ .save = token_with_location };
                            },
                        }
                    }
                },
                .object_literal => |*object_literal| switch (object_literal.state) {
                    .start => switch (token) {
                        .eol => {},
                        .word => |word| {
                            object_literal.state = .{
                                .after_key = .{
                                    .identifier = .{
                                        .text = try allocator.dupe(u8, word),
                                        .location = loc,
                                    },
                                },
                            };
                        },
                        .string => |str| {
                            object_literal.state = .{
                                .after_key = .{
                                    .string = .{
                                        .text = try allocator.dupe(u8, str),
                                        .location = loc,
                                    },
                                },
                            };
                        },
                        .rbrace => {
                            object_literal.state = .end;
                            return .{ .save = token_with_location };
                        },
                        else => {
                            std.log.err("unexpected token: expected word, string or rbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    .after_key => |key| {
                        switch (token) {
                            .colon => {
                                object_literal.state = .{
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
                        }
                    },
                    .end => switch (token) {
                        .eol => {},
                        .comma => {
                            try object_literal.comma_positions.append(allocator, loc.start);
                            object_literal.state = .start;
                        },
                        .rbrace => {
                            self.finalizeExpr(.{
                                .object_literal = .{
                                    .fields = try object_literal.fields.toOwnedSlice(allocator),
                                    .location = .{
                                        .source = loc.source,
                                        .start = object_literal.start_position,
                                        .end = loc.end,
                                    },
                                    .comma_positions = try object_literal.comma_positions.toOwnedSlice(allocator),
                                },
                            });
                        },
                        else => {
                            std.log.err("unexpected token: expected comma or rbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    },
                    else => {
                        std.debug.panic("invalid parser state: object_literal: {s}", .{@tagName(object_literal.state)});
                    },
                },
                .invocation => |*invocation| {
                    if (token != .eol) {
                        if (token != .rparen) {
                            try self.startChildState(allocator);
                            return .{ .save = token_with_location };
                        }

                        self.finalizeExpr(.{
                            .invocation = .{
                                .function = try util.mem.createCopy(FQLExpression, allocator, &invocation.function),
                                .arguments = try invocation.arguments.toOwnedSlice(allocator),
                                .location = .{
                                    .source = loc.source,
                                    .start = invocation.function.location().?.start,
                                    .end = loc.end,
                                },
                                .comma_positions = try invocation.comma_positions.toOwnedSlice(allocator),
                                .lparen_position = invocation.lparen_position,
                            },
                        });
                    }
                },
                .block_scope => |*block_scope| {
                    switch (token) {
                        .eol => {},
                        .rbrace => {
                            self.finalizeExpr(.{
                                .block_scope = .{
                                    .statements = try block_scope.statements.toOwnedSlice(allocator),
                                    .location = .{
                                        .source = loc.source,
                                        .start = block_scope.start_position,
                                        .end = loc.end,
                                    },
                                },
                            });
                        },
                        else => {
                            std.log.err("unexpected token: expected rbrace but got {s}", .{@tagName(token)});
                            return error.UnexpectedToken;
                        },
                    }
                },
                .end => |expr| {
                    switch (token) {
                        .dot, .question_dot => {
                            self.state = .{
                                .field_access = .{
                                    .value = expr,
                                    .optional = token == .question_dot,
                                    .question_position = if (token == .question_dot) loc.start else null,
                                    .dot_position = if (token == .question_dot) loc.start.bump(1) else loc.start,
                                },
                            };
                            return .{};
                        },
                        .lbracket => {
                            self.state = .{
                                .field_access = .{
                                    .value = expr,
                                    .lbracket_position = loc.start,
                                },
                            };
                            try self.startChildState(allocator);
                            return .{};
                        },
                        .lparen => {
                            self.state = .{
                                .invocation = .{
                                    .function = expr,
                                    .lparen_position = loc.start,
                                },
                            };
                            return .{};
                        },
                        .lbrace => {
                            self.state = .{
                                .projection = .{
                                    .expression = expr,
                                    .lbrace_position = loc.start,
                                },
                            };
                            return .{};
                        },
                        .bang => {
                            self.finalizeExpr(.{
                                .non_null_assertion = .{
                                    .expression = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                    .location = .{
                                        .source = loc.source,
                                        .start = expr.location().?.start,
                                        .end = loc.end,
                                    },
                                },
                            });
                            return .{};
                        },
                        else => {
                            if (BinaryOperation.Operator.fromToken(token)) |operator| {
                                self.state = .{
                                    .binary_operation = .{
                                        .lhs = expr,
                                        .operator = operator,
                                        .operator_position = loc.start,
                                    },
                                };
                                try self.startChildState(allocator);
                                return .{};
                            }

                            if (token == .equal_rarrow) {
                                // could this actually be a function? :suprised_pikachu:

                                switch (expr) {
                                    .isolated => |isolated| {
                                        if (isolated.expression.* == .identifier) {
                                            defer allocator.destroy(isolated.expression);

                                            self.state = .{
                                                .long_function = .{
                                                    .parameters = try State.LongFunction.exprsToParams(allocator, &.{isolated.expression.*}),
                                                    .start_position = isolated.location.?.start,
                                                    .rparen_position = isolated.location.?.end.bump(-1),
                                                    .equal_rarrow_position = loc.start,
                                                },
                                            };

                                            try self.startChildState(allocator);
                                            return .{};
                                        }
                                    },
                                    .array_literal => |array_literal| {
                                        if (array_literal.elements == null or util.slice.every(array_literal.elements.?, isIdentifier)) {
                                            const elems = array_literal.elements orelse try allocator.alloc(FQLExpression, 0);
                                            defer allocator.free(elems);

                                            self.state = .{
                                                .long_function = .{
                                                    .parameters = try State.LongFunction.exprsToParams(allocator, elems),
                                                    .comma_positions = std.ArrayListUnmanaged(Position).fromOwnedSlice(@constCast(array_literal.comma_positions.?)),
                                                    .start_position = array_literal.location.?.start,
                                                    .rparen_position = array_literal.location.?.end.bump(-1),
                                                    .equal_rarrow_position = loc.start,
                                                },
                                            };

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

                    if (token == .eol) {
                        return .{};
                    }

                    if (self.parent) |parent| {
                        defer allocator.destroy(parent);
                        defer self.* = parent.*;

                        switch (parent.state) {
                            .unary_operation => |unary_operation| {
                                parent.finalizeExpr(.{
                                    .unary_operation = .{
                                        .operator = unary_operation.operator,
                                        .operand = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        .location = .{
                                            .source = loc.source,
                                            .start = unary_operation.start_position,
                                            .end = expr.location().?.end,
                                        },
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
                                                    .operator_position = binary_operation.operator_position,
                                                    .location = .{
                                                        .source = loc.source,
                                                        .start = binary_operation.lhs.location().?.start,
                                                        .end = expr.binary_operation.lhs.location().?.end,
                                                    },
                                                },
                                            }),
                                            .operator = expr.binary_operation.operator,
                                            .rhs = expr.binary_operation.rhs,
                                            .operator_position = expr.binary_operation.operator_position,
                                            .location = .{
                                                .source = loc.source,
                                                .start = binary_operation.lhs.location().?.start,
                                                .end = expr.binary_operation.rhs.location().?.end,
                                            },
                                        },
                                    });
                                } else {
                                    parent.finalizeExpr(.{
                                        .binary_operation = .{
                                            .operator = binary_operation.operator,
                                            .lhs = try util.mem.createCopy(FQLExpression, allocator, &binary_operation.lhs),
                                            .rhs = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            .operator_position = binary_operation.operator_position,
                                            .location = .{
                                                .source = loc.source,
                                                .start = binary_operation.lhs.location().?.start,
                                                .end = expr.location().?.end,
                                            },
                                        },
                                    });
                                }
                            },
                            .array_literal => |*array_literal| {
                                try array_literal.elements.append(allocator, expr);
                            },
                            .invocation => |*invocation| switch (token) {
                                .comma => {
                                    try invocation.comma_positions.append(allocator, loc.start);
                                    try invocation.arguments.append(allocator, expr);
                                    return .{};
                                },
                                .rparen => {
                                    try invocation.arguments.append(allocator, expr);

                                    parent.finalizeExpr(.{
                                        .invocation = .{
                                            .function = try util.mem.createCopy(FQLExpression, allocator, &invocation.function),
                                            .arguments = try invocation.arguments.toOwnedSlice(allocator),
                                            .location = .{
                                                .source = loc.source,
                                                .start = invocation.function.location().?.start,
                                                .end = loc.end,
                                            },
                                            .comma_positions = try invocation.comma_positions.toOwnedSlice(allocator),
                                            .lparen_position = invocation.lparen_position,
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

                                parent.finalizeExpr(.{
                                    .variable_declaration = .{
                                        .name = variable_declaration.name.?,
                                        .type = if (variable_declaration.type) |type_state| blk: {
                                            if (type_state != .type) {
                                                std.debug.panic("invalid parser parent state: variable_declaration: type is {s}", .{@tagName(type_state)});
                                            }

                                            break :blk type_state.type;
                                        } else null,
                                        .value = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        .location = .{
                                            .source = expr.location().?.source,
                                            .start = variable_declaration.start_position,
                                            .end = expr.location().?.end,
                                        },
                                        .colon_position = variable_declaration.colon_position,
                                        .equal_position = variable_declaration.equal_position,
                                    },
                                });
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
                                        .location = .{
                                            .source = loc.source,
                                            .start = field_access.value.location().?.start,
                                            .end = loc.end,
                                        },
                                        .question_position = field_access.question_position,
                                        .dot_position = field_access.dot_position,
                                        .lbracket_position = field_access.lbracket_position,
                                    },
                                });

                                return .{};
                            },
                            .object_literal => |*object_literal| {
                                switch (object_literal.state) {
                                    .after_colon => |after_colon| {
                                        try object_literal.fields.append(allocator, .{
                                            .key = after_colon.key,
                                            .value = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            .location = .{
                                                .source = loc.source,
                                                .start = after_colon.key.location().?.start,
                                                .end = expr.location().?.end,
                                            },
                                            .colon_position = after_colon.colon_position,
                                        });
                                        object_literal.state = .end;
                                    },
                                    else => std.debug.panic("invalid parser parent state: object_literal: {s}", .{@tagName(object_literal.state)}),
                                }
                            },
                            .conditional => |*conditional| switch (conditional.state) {
                                .after_lparen => |pos| {
                                    conditional.state = .{
                                        .after_condition = .{
                                            .lparen_position = pos,
                                            .condition = expr,
                                        },
                                    };
                                },
                                .after_rparen => |after_rparen| {
                                    conditional.state = .{
                                        .after_body = .{
                                            .lparen_position = after_rparen.lparen_position,
                                            .condition = after_rparen.condition,
                                            .rparen_position = after_rparen.rparen_position,
                                            .body = expr,
                                        },
                                    };
                                },
                                .after_else => |after_else| {
                                    parent.finalizeExpr(.{
                                        .conditional = .{
                                            .condition = try util.mem.createCopy(FQLExpression, allocator, &after_else.condition),
                                            .body = try util.mem.createCopy(FQLExpression, allocator, &after_else.body),
                                            .@"else" = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                            .location = .{
                                                .source = loc.source,
                                                .start = conditional.start_position,
                                                .end = expr.location().?.end,
                                            },
                                            .lparen_position = after_else.lparen_position,
                                            .rparen_position = after_else.rparen_position,
                                            .else_position = after_else.else_position,
                                        },
                                    });
                                },
                                else => std.debug.panic("invalid parser parent state: conditional statement: {s}", .{@tagName(conditional.state)}),
                            },
                            .block_scope => |*block_scope| {
                                try block_scope.statements.append(allocator, expr);

                                switch (token) {
                                    .rbrace => {
                                        return .{ .save = token_with_location };
                                    },
                                    else => {
                                        try parent.startChildState(allocator);
                                    },
                                }
                            },
                            .long_function => |*long_function| {
                                if (long_function.variadic_state != null and long_function.variadic_state.? != .after_rparen) {
                                    std.debug.panic("invalid parser parent state: long function: {s}", .{@tagName(long_function.variadic_state.?)});
                                }

                                parent.finalizeExpr(.{
                                    .function = .{
                                        .parameters = .{
                                            .long = .{
                                                .parameters = if (long_function.variadic_state) |variadic_state| blk: {
                                                    const params = try allocator.realloc(@constCast(long_function.parameters), long_function.parameters.len + 1);
                                                    params[params.len - 1] = variadic_state.after_rparen;
                                                    break :blk params;
                                                } else long_function.parameters,
                                                .variadic = long_function.variadic_state != null,
                                                .location = .{
                                                    .source = loc.source,
                                                    .start = long_function.start_position,
                                                    .end = long_function.rparen_position.?.bump(1),
                                                },
                                                .comma_positions = try long_function.comma_positions.toOwnedSlice(allocator),
                                                .dot3_position = long_function.dot3_position,
                                            },
                                        },
                                        .body = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        .location = .{
                                            .source = loc.source,
                                            .start = long_function.start_position,
                                            .end = expr.location().?.end,
                                        },
                                        .equal_rarrow_position = long_function.equal_rarrow_position,
                                    },
                                });
                            },
                            .short_function => |short_function| {
                                parent.finalizeExpr(.{
                                    .function = .{
                                        .parameters = .{
                                            .short = short_function.param,
                                        },
                                        .body = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        .location = .{
                                            .source = short_function.param.location.?.source,
                                            .start = short_function.param.location.?.start,
                                            .end = expr.location().?.end,
                                        },
                                        .equal_rarrow_position = short_function.equal_rarrow_position,
                                    },
                                });
                            },
                            .projection => |*projection| {
                                if (projection.state != .after_colon) {
                                    std.debug.panic("invalid parser parent state: projection: {s}", .{@tagName(projection.state)});
                                }

                                try projection.fields.append(allocator, .{
                                    .long = .{
                                        .key = projection.state.after_colon.identifier,
                                        .value = try util.mem.createCopy(FQLExpression, allocator, &expr),
                                        .colon_position = projection.state.after_colon.colon_position,
                                    },
                                });

                                projection.state = .end;
                            },
                            else => std.debug.panic("invalid parser parent state: {s}", .{@tagName(parent.state)}),
                        }

                        // ensure token is not consumed at this point
                        return .{ .save = token_with_location };
                    }

                    self.* = .{};

                    return .{ .save = token_with_location, .expr = expr };
                },
                else => {
                    if (self.state == .long_function and self.state.long_function.variadic_state != null) {
                        switch (self.state.long_function.variadic_state.?) {
                            .start => {
                                switch (token) {
                                    .word => |word| {
                                        self.state.long_function.variadic_state = .{
                                            .after_param = .{
                                                .text = try allocator.dupe(u8, word),
                                                .location = loc,
                                            },
                                        };
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
                                        self.state.long_function.rparen_position = loc.start;
                                        self.state.long_function.variadic_state = .{ .after_rparen = param };
                                        return .{};
                                    },
                                    .comma => {
                                        try self.state.long_function.comma_positions.append(allocator, loc.start);
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
                                        self.state.long_function.equal_rarrow_position = loc.start;
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
    });

    pub const parse = @as(fn (allocator: std.mem.Allocator, it: *Tokenizer.TokenIterator) anyerror!?@This(), Parser.parseIterator);
};

pub const parseExpression = FQLExpression.Parser.parseReader;

fn expectParsedExprEqual(str: []const u8, expected: FQLExpression) !void {
    var stream = std.io.fixedBufferStream(str);
    var actual = (try parseExpression(testing.allocator, stream.reader().any(), null)).?;
    defer actual.deinit(testing.allocator);

    // std.debug.print("actual: {any}\n", .{actual});

    try testing.expectEqualDeep(expected, actual);

    const canonical_string = try actual.toCanonicalString(testing.allocator);
    defer testing.allocator.free(canonical_string);

    try testing.expectEqualStrings(str, canonical_string);

    try parsing.checkForLeaks(FQLExpression.Parser, str);

    try parsing.testDupe(expected);
    try parsing.testDupe(actual);
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
            .name = .{
                .text = "myArray",
                .location = .{
                    .start = .{
                        .offset = 4,
                        .line = 0,
                        .column = 4,
                    },
                    .end = .{
                        .offset = 11,
                        .line = 0,
                        .column = 11,
                    },
                },
            },
            .type = .{
                .template = .{
                    .name = .{
                        .text = "Array",
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
                    .parameters = &[_]FQLType{
                        .{
                            .named = .{
                                .text = "Any",
                                .location = .{
                                    .start = .{
                                        .offset = 19,
                                        .line = 0,
                                        .column = 19,
                                    },
                                    .end = .{
                                        .offset = 22,
                                        .line = 0,
                                        .column = 22,
                                    },
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
                            .offset = 23,
                            .line = 0,
                            .column = 23,
                        },
                    },
                    .larrow_position = .{
                        .offset = 18,
                        .line = 0,
                        .column = 18,
                    },
                    .comma_positions = &.{},
                },
            },
            .value = &FQLExpression{
                .array_literal = .{
                    .elements = &[_]FQLExpression{
                        .{
                            .string_literal = .{
                                .text = "\"Hello\"",
                                .location = .{
                                    .start = .{
                                        .offset = 32,
                                        .line = 1,
                                        .column = 4,
                                    },
                                    .end = .{
                                        .offset = 39,
                                        .line = 1,
                                        .column = 11,
                                    },
                                },
                            },
                        },
                        .{
                            .number_literal = .{
                                .text = "45",
                                .location = .{
                                    .start = .{
                                        .offset = 45,
                                        .line = 2,
                                        .column = 4,
                                    },
                                    .end = .{
                                        .offset = 47,
                                        .line = 2,
                                        .column = 6,
                                    },
                                },
                            },
                        },
                        .{
                            .boolean_literal = .{
                                .value = true,
                                .location = .{
                                    .start = .{
                                        .offset = 53,
                                        .line = 3,
                                        .column = 4,
                                    },
                                    .end = .{
                                        .offset = 57,
                                        .line = 3,
                                        .column = 8,
                                    },
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 26,
                            .line = 0,
                            .column = 26,
                        },
                        .end = .{
                            .offset = 60,
                            .line = 4,
                            .column = 1,
                        },
                    },
                    .comma_positions = &.{
                        .{
                            .offset = 39,
                            .line = 1,
                            .column = 11,
                        },
                        .{
                            .offset = 47,
                            .line = 2,
                            .column = 6,
                        },
                        .{
                            .offset = 57,
                            .line = 3,
                            .column = 8,
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
                    .offset = 60,
                    .line = 4,
                    .column = 1,
                },
            },
            .colon_position = .{
                .offset = 11,
                .line = 0,
                .column = 11,
            },
            .equal_position = .{
                .offset = 24,
                .line = 0,
                .column = 24,
            },
        },
    });

    try expectParsedExprEqual("(1 + 3) * 2", .{
        .binary_operation = .{
            .lhs = &FQLExpression{
                .isolated = .{
                    .expression = &FQLExpression{
                        .binary_operation = .{
                            .lhs = &FQLExpression{
                                .number_literal = .{
                                    .text = "1",
                                    .location = .{
                                        .start = .{
                                            .offset = 1,
                                            .line = 0,
                                            .column = 1,
                                        },
                                        .end = .{
                                            .offset = 2,
                                            .line = 0,
                                            .column = 2,
                                        },
                                    },
                                },
                            },
                            .operator = .add,
                            .rhs = &FQLExpression{
                                .number_literal = .{
                                    .text = "3",
                                    .location = .{
                                        .start = .{
                                            .offset = 5,
                                            .line = 0,
                                            .column = 5,
                                        },
                                        .end = .{
                                            .offset = 6,
                                            .line = 0,
                                            .column = 6,
                                        },
                                    },
                                },
                            },
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
                            .operator_position = .{
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
                            .offset = 7,
                            .line = 0,
                            .column = 7,
                        },
                    },
                },
            },
            .operator = .multiply,
            .rhs = &FQLExpression{
                .number_literal = .{
                    .text = "2",
                    .location = .{
                        .start = .{
                            .offset = 10,
                            .line = 0,
                            .column = 10,
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
            .operator_position = .{
                .offset = 8,
                .line = 0,
                .column = 8,
            },
        },
    });

    try expectParsedExprEqual(
        \\{
        \\    []
        \\}
    , .{
        .block_scope = .{
            .statements = &[_]FQLExpression{
                .{
                    .array_literal = .{
                        .elements = &[_]FQLExpression{},
                        .location = .{
                            .start = .{
                                .offset = 6,
                                .line = 1,
                                .column = 4,
                            },
                            .end = .{
                                .offset = 8,
                                .line = 1,
                                .column = 6,
                            },
                        },
                        .comma_positions = &.{},
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
                    .offset = 10,
                    .line = 2,
                    .column = 1,
                },
            },
        },
    });

    try expectParsedExprEqual(
        \\"Hello " + [
        \\    "world",
        \\    "!",
        \\]
    , .{
        .binary_operation = .{
            .lhs = &FQLExpression{
                .string_literal = .{
                    .text = "\"Hello \"",
                    .location = .{
                        .start = .{
                            .offset = 0,
                            .line = 0,
                            .column = 0,
                        },
                        .end = .{
                            .offset = 8,
                            .line = 0,
                            .column = 8,
                        },
                    },
                },
            },
            .operator = .add,
            .rhs = &FQLExpression{
                .array_literal = .{
                    .elements = &[_]FQLExpression{
                        .{
                            .string_literal = .{
                                .text = "\"world\"",
                                .location = .{
                                    .start = .{
                                        .offset = 17,
                                        .line = 1,
                                        .column = 4,
                                    },
                                    .end = .{
                                        .offset = 24,
                                        .line = 1,
                                        .column = 11,
                                    },
                                },
                            },
                        },
                        .{
                            .string_literal = .{
                                .text = "\"!\"",
                                .location = .{
                                    .start = .{
                                        .offset = 30,
                                        .line = 2,
                                        .column = 4,
                                    },
                                    .end = .{
                                        .offset = 33,
                                        .line = 2,
                                        .column = 7,
                                    },
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 11,
                            .line = 0,
                            .column = 11,
                        },
                        .end = .{
                            .offset = 36,
                            .line = 3,
                            .column = 1,
                        },
                    },
                    .comma_positions = &.{
                        .{
                            .offset = 24,
                            .line = 1,
                            .column = 11,
                        },
                        .{
                            .offset = 33,
                            .line = 2,
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
                    .offset = 36,
                    .line = 3,
                    .column = 1,
                },
            },
            .operator_position = .{
                .offset = 9,
                .line = 0,
                .column = 9,
            },
        },
    });

    try expectParsedExprEqual("hello[\"world\"]", .{
        .field_access = .{
            .value = &FQLExpression{
                .identifier = .{
                    .text = "hello",
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
            },
            .field = .{
                .expression = &FQLExpression{
                    .string_literal = .{
                        .text = "\"world\"",
                        .location = .{
                            .start = .{
                                .offset = 6,
                                .line = 0,
                                .column = 6,
                            },
                            .end = .{
                                .offset = 13,
                                .line = 0,
                                .column = 13,
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
                    .offset = 14,
                    .line = 0,
                    .column = 14,
                },
            },
            .lbracket_position = .{
                .offset = 5,
                .line = 0,
                .column = 5,
            },
        },
    });

    try expectParsedExprEqual("\"This is a string\"?.at(0)", .{
        .invocation = .{
            .function = &FQLExpression{
                .field_access = .{
                    .value = &FQLExpression{
                        .string_literal = .{
                            .text = "\"This is a string\"",
                            .location = .{
                                .start = .{
                                    .offset = 0,
                                    .line = 0,
                                    .column = 0,
                                },
                                .end = .{
                                    .offset = 18,
                                    .line = 0,
                                    .column = 18,
                                },
                            },
                        },
                    },
                    .optional = true,
                    .field = .{
                        .identifier = .{
                            .text = "at",
                            .location = .{
                                .start = .{
                                    .offset = 20,
                                    .line = 0,
                                    .column = 20,
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
                    .question_position = .{
                        .offset = 18,
                        .line = 0,
                        .column = 18,
                    },
                    .dot_position = .{
                        .offset = 19,
                        .line = 0,
                        .column = 19,
                    },
                },
            },
            .arguments = &[_]FQLExpression{
                .{
                    .number_literal = .{
                        .text = "0",
                        .location = .{
                            .start = .{
                                .offset = 23,
                                .line = 0,
                                .column = 23,
                            },
                            .end = .{
                                .offset = 24,
                                .line = 0,
                                .column = 24,
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
                    .offset = 25,
                    .line = 0,
                    .column = 25,
                },
            },
            .comma_positions = &.{},
            .lparen_position = .{
                .offset = 22,
                .line = 0,
                .column = 22,
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
                    .key = .{
                        .identifier = .{
                            .text = "myKey",
                            .location = .{
                                .start = .{
                                    .offset = 6,
                                    .line = 1,
                                    .column = 4,
                                },
                                .end = .{
                                    .offset = 11,
                                    .line = 1,
                                    .column = 9,
                                },
                            },
                        },
                    },
                    .value = &FQLExpression{
                        .string_literal = .{
                            .text = "\"myValue\"",
                            .location = .{
                                .start = .{
                                    .offset = 13,
                                    .line = 1,
                                    .column = 11,
                                },
                                .end = .{
                                    .offset = 22,
                                    .line = 1,
                                    .column = 20,
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 6,
                            .line = 1,
                            .column = 4,
                        },
                        .end = .{
                            .offset = 22,
                            .line = 1,
                            .column = 20,
                        },
                    },
                    .colon_position = .{
                        .offset = 11,
                        .line = 1,
                        .column = 9,
                    },
                },
                .{
                    .key = .{
                        .string = .{
                            .text = "\"myOtherKey\"",
                            .location = .{
                                .start = .{
                                    .offset = 28,
                                    .line = 2,
                                    .column = 4,
                                },
                                .end = .{
                                    .offset = 40,
                                    .line = 2,
                                    .column = 16,
                                },
                            },
                        },
                    },
                    .value = &FQLExpression{
                        .number_literal = .{
                            .text = "4",
                            .location = .{
                                .start = .{
                                    .offset = 42,
                                    .line = 2,
                                    .column = 18,
                                },
                                .end = .{
                                    .offset = 43,
                                    .line = 2,
                                    .column = 19,
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 28,
                            .line = 2,
                            .column = 4,
                        },
                        .end = .{
                            .offset = 43,
                            .line = 2,
                            .column = 19,
                        },
                    },
                    .colon_position = .{
                        .offset = 40,
                        .line = 2,
                        .column = 16,
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
                    .offset = 46,
                    .line = 3,
                    .column = 1,
                },
            },
            .comma_positions = &.{
                .{
                    .offset = 22,
                    .line = 1,
                    .column = 20,
                },
                .{
                    .offset = 43,
                    .line = 2,
                    .column = 19,
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
                            .value = &FQLExpression{
                                .identifier = .{
                                    .text = "house",
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
                            },
                            .field = .{
                                .identifier = .{
                                    .text = "size",
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
                            .location = .{
                                .start = .{
                                    .offset = 0,
                                    .line = 0,
                                    .column = 0,
                                },
                                .end = .{
                                    .offset = 10,
                                    .line = 0,
                                    .column = 10,
                                },
                            },
                            .dot_position = .{
                                .offset = 5,
                                .line = 0,
                                .column = 5,
                            },
                        },
                    },
                    .operator = .equality,
                    .rhs = &FQLExpression{
                        .string_literal = .{
                            .text = "\"big\"",
                            .location = .{
                                .start = .{
                                    .offset = 14,
                                    .line = 0,
                                    .column = 14,
                                },
                                .end = .{
                                    .offset = 19,
                                    .line = 0,
                                    .column = 19,
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
                            .offset = 19,
                            .line = 0,
                            .column = 19,
                        },
                    },
                    .operator_position = .{
                        .offset = 11,
                        .line = 0,
                        .column = 11,
                    },
                },
            },
            .operator = .logical_and,
            .rhs = &FQLExpression{
                .binary_operation = .{
                    .lhs = &FQLExpression{
                        .field_access = .{
                            .value = &FQLExpression{
                                .identifier = .{
                                    .text = "house",
                                    .location = .{
                                        .start = .{
                                            .offset = 23,
                                            .line = 0,
                                            .column = 23,
                                        },
                                        .end = .{
                                            .offset = 28,
                                            .line = 0,
                                            .column = 28,
                                        },
                                    },
                                },
                            },
                            .field = .{
                                .identifier = .{
                                    .text = "color",
                                    .location = .{
                                        .start = .{
                                            .offset = 29,
                                            .line = 0,
                                            .column = 29,
                                        },
                                        .end = .{
                                            .offset = 34,
                                            .line = 0,
                                            .column = 34,
                                        },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{
                                    .offset = 23,
                                    .line = 0,
                                    .column = 23,
                                },
                                .end = .{
                                    .offset = 34,
                                    .line = 0,
                                    .column = 34,
                                },
                            },
                            .dot_position = .{
                                .offset = 28,
                                .line = 0,
                                .column = 28,
                            },
                        },
                    },
                    .operator = .equality,
                    .rhs = &FQLExpression{
                        .string_literal = .{
                            .text = "\"blue\"",
                            .location = .{
                                .start = .{
                                    .offset = 38,
                                    .line = 0,
                                    .column = 38,
                                },
                                .end = .{
                                    .offset = 44,
                                    .line = 0,
                                    .column = 44,
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 23,
                            .line = 0,
                            .column = 23,
                        },
                        .end = .{
                            .offset = 44,
                            .line = 0,
                            .column = 44,
                        },
                    },
                    .operator_position = .{
                        .offset = 35,
                        .line = 0,
                        .column = 35,
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
                    .offset = 44,
                    .line = 0,
                    .column = 44,
                },
            },
            .operator_position = .{
                .offset = 20,
                .line = 0,
                .column = 20,
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
                    .parameters = &.{
                        .{
                            .text = "x",
                            .location = .{
                                .start = .{
                                    .offset = 1,
                                    .line = 0,
                                    .column = 1,
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
                    .comma_positions = &.{},
                },
            },
            .body = &FQLExpression{
                .block_scope = .{
                    .statements = &[_]FQLExpression{
                        .{
                            .variable_declaration = .{
                                .name = .{
                                    .text = "greeting",
                                    .location = .{
                                        .start = .{
                                            .offset = 17,
                                            .line = 1,
                                            .column = 8,
                                        },
                                        .end = .{
                                            .offset = 25,
                                            .line = 1,
                                            .column = 16,
                                        },
                                    },
                                },
                                .value = &FQLExpression{
                                    .string_literal = .{
                                        .text = "\"hello\"",
                                        .location = .{
                                            .start = .{
                                                .offset = 28,
                                                .line = 1,
                                                .column = 19,
                                            },
                                            .end = .{
                                                .offset = 35,
                                                .line = 1,
                                                .column = 26,
                                            },
                                        },
                                    },
                                },
                                .location = .{
                                    .start = .{
                                        .offset = 13,
                                        .line = 1,
                                        .column = 4,
                                    },
                                    .end = .{
                                        .offset = 35,
                                        .line = 1,
                                        .column = 26,
                                    },
                                },
                                .equal_position = .{
                                    .offset = 26,
                                    .line = 1,
                                    .column = 17,
                                },
                            },
                        },
                        .{
                            .identifier = .{
                                .text = "greeting",
                                .location = .{
                                    .start = .{
                                        .offset = 40,
                                        .line = 2,
                                        .column = 4,
                                    },
                                    .end = .{
                                        .offset = 48,
                                        .line = 2,
                                        .column = 12,
                                    },
                                },
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 7,
                            .line = 0,
                            .column = 7,
                        },
                        .end = .{
                            .offset = 50,
                            .line = 3,
                            .column = 1,
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
                    .offset = 50,
                    .line = 3,
                    .column = 1,
                },
            },
            .equal_rarrow_position = .{
                .offset = 4,
                .line = 0,
                .column = 4,
            },
        },
    });

    try expectParsedExprEqual("x => y => z => x + y + z", .{
        .function = .{
            .parameters = .{
                .short = .{
                    .text = "x",
                    .location = .{
                        .start = .{
                            .offset = 0,
                            .line = 0,
                            .column = 0,
                        },
                        .end = .{
                            .offset = 1,
                            .line = 0,
                            .column = 1,
                        },
                    },
                },
            },
            .body = &FQLExpression{
                .function = .{
                    .parameters = .{
                        .short = .{
                            .text = "y",
                            .location = .{
                                .start = .{
                                    .offset = 5,
                                    .line = 0,
                                    .column = 5,
                                },
                                .end = .{
                                    .offset = 6,
                                    .line = 0,
                                    .column = 6,
                                },
                            },
                        },
                    },
                    .body = &FQLExpression{
                        .function = .{
                            .parameters = .{
                                .short = .{
                                    .text = "z",
                                    .location = .{
                                        .start = .{
                                            .offset = 10,
                                            .line = 0,
                                            .column = 10,
                                        },
                                        .end = .{
                                            .offset = 11,
                                            .line = 0,
                                            .column = 11,
                                        },
                                    },
                                },
                            },
                            .body = &FQLExpression{
                                .binary_operation = .{
                                    .lhs = &FQLExpression{
                                        .binary_operation = .{
                                            .lhs = &FQLExpression{
                                                .identifier = .{
                                                    .text = "x",
                                                    .location = .{
                                                        .start = .{
                                                            .offset = 15,
                                                            .line = 0,
                                                            .column = 15,
                                                        },
                                                        .end = .{
                                                            .offset = 16,
                                                            .line = 0,
                                                            .column = 16,
                                                        },
                                                    },
                                                },
                                            },
                                            .operator = .add,
                                            .rhs = &FQLExpression{
                                                .identifier = .{
                                                    .text = "y",
                                                    .location = .{
                                                        .start = .{
                                                            .offset = 19,
                                                            .line = 0,
                                                            .column = 19,
                                                        },
                                                        .end = .{
                                                            .offset = 20,
                                                            .line = 0,
                                                            .column = 20,
                                                        },
                                                    },
                                                },
                                            },
                                            .location = .{
                                                .start = .{
                                                    .offset = 15,
                                                    .line = 0,
                                                    .column = 15,
                                                },
                                                .end = .{
                                                    .offset = 20,
                                                    .line = 0,
                                                    .column = 20,
                                                },
                                            },
                                            .operator_position = .{
                                                .offset = 17,
                                                .line = 0,
                                                .column = 17,
                                            },
                                        },
                                    },
                                    .operator = .add,
                                    .rhs = &FQLExpression{
                                        .identifier = .{
                                            .text = "z",
                                            .location = .{
                                                .start = .{
                                                    .offset = 23,
                                                    .line = 0,
                                                    .column = 23,
                                                },
                                                .end = .{
                                                    .offset = 24,
                                                    .line = 0,
                                                    .column = 24,
                                                },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{
                                            .offset = 15,
                                            .line = 0,
                                            .column = 15,
                                        },
                                        .end = .{
                                            .offset = 24,
                                            .line = 0,
                                            .column = 24,
                                        },
                                    },
                                    .operator_position = .{
                                        .offset = 21,
                                        .line = 0,
                                        .column = 21,
                                    },
                                },
                            },
                            .location = .{
                                .start = .{
                                    .offset = 10,
                                    .line = 0,
                                    .column = 10,
                                },
                                .end = .{
                                    .offset = 24,
                                    .line = 0,
                                    .column = 24,
                                },
                            },
                            .equal_rarrow_position = .{
                                .offset = 12,
                                .line = 0,
                                .column = 12,
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 5,
                            .line = 0,
                            .column = 5,
                        },
                        .end = .{
                            .offset = 24,
                            .line = 0,
                            .column = 24,
                        },
                    },
                    .equal_rarrow_position = .{
                        .offset = 7,
                        .line = 0,
                        .column = 7,
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
                    .offset = 24,
                    .line = 0,
                    .column = 24,
                },
            },
            .equal_rarrow_position = .{
                .offset = 2,
                .line = 0,
                .column = 2,
            },
        },
    });

    try expectParsedExprEqual("(symbol, ...amounts) => symbol + amounts.reduce((prev, cur) => prev + cur).toString()", .{
        .function = .{
            .parameters = .{
                .long = .{
                    .parameters = &.{
                        .{
                            .text = "symbol",
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
                        .{
                            .text = "amounts",
                            .location = .{
                                .start = .{
                                    .offset = 12,
                                    .line = 0,
                                    .column = 12,
                                },
                                .end = .{
                                    .offset = 19,
                                    .line = 0,
                                    .column = 19,
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
                            .offset = 20,
                            .line = 0,
                            .column = 20,
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
            .body = &FQLExpression{
                .binary_operation = .{
                    .lhs = &FQLExpression{
                        .identifier = .{
                            .text = "symbol",
                            .location = .{
                                .start = .{
                                    .offset = 24,
                                    .line = 0,
                                    .column = 24,
                                },
                                .end = .{
                                    .offset = 30,
                                    .line = 0,
                                    .column = 30,
                                },
                            },
                        },
                    },
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
                                                                .parameters = &.{
                                                                    .{
                                                                        .text = "prev",
                                                                        .location = .{
                                                                            .start = .{
                                                                                .offset = 49,
                                                                                .line = 0,
                                                                                .column = 49,
                                                                            },
                                                                            .end = .{
                                                                                .offset = 53,
                                                                                .line = 0,
                                                                                .column = 53,
                                                                            },
                                                                        },
                                                                    },
                                                                    .{
                                                                        .text = "cur",
                                                                        .location = .{
                                                                            .start = .{
                                                                                .offset = 55,
                                                                                .line = 0,
                                                                                .column = 55,
                                                                            },
                                                                            .end = .{
                                                                                .offset = 58,
                                                                                .line = 0,
                                                                                .column = 58,
                                                                            },
                                                                        },
                                                                    },
                                                                },
                                                                .location = .{
                                                                    .start = .{
                                                                        .offset = 48,
                                                                        .line = 0,
                                                                        .column = 48,
                                                                    },
                                                                    .end = .{
                                                                        .offset = 59,
                                                                        .line = 0,
                                                                        .column = 59,
                                                                    },
                                                                },
                                                                .comma_positions = &.{
                                                                    .{
                                                                        .offset = 53,
                                                                        .line = 0,
                                                                        .column = 53,
                                                                    },
                                                                },
                                                            },
                                                        },
                                                        .body = &FQLExpression{
                                                            .binary_operation = .{
                                                                .lhs = &FQLExpression{
                                                                    .identifier = .{
                                                                        .text = "prev",
                                                                        .location = .{
                                                                            .start = .{
                                                                                .offset = 63,
                                                                                .line = 0,
                                                                                .column = 63,
                                                                            },
                                                                            .end = .{
                                                                                .offset = 67,
                                                                                .line = 0,
                                                                                .column = 67,
                                                                            },
                                                                        },
                                                                    },
                                                                },
                                                                .operator = .add,
                                                                .rhs = &FQLExpression{
                                                                    .identifier = .{
                                                                        .text = "cur",
                                                                        .location = .{
                                                                            .start = .{
                                                                                .offset = 70,
                                                                                .line = 0,
                                                                                .column = 70,
                                                                            },
                                                                            .end = .{
                                                                                .offset = 73,
                                                                                .line = 0,
                                                                                .column = 73,
                                                                            },
                                                                        },
                                                                    },
                                                                },
                                                                .location = .{
                                                                    .start = .{
                                                                        .offset = 63,
                                                                        .line = 0,
                                                                        .column = 63,
                                                                    },
                                                                    .end = .{
                                                                        .offset = 73,
                                                                        .line = 0,
                                                                        .column = 73,
                                                                    },
                                                                },
                                                                .operator_position = .{
                                                                    .offset = 68,
                                                                    .line = 0,
                                                                    .column = 68,
                                                                },
                                                            },
                                                        },
                                                        .location = .{
                                                            .start = .{
                                                                .offset = 48,
                                                                .line = 0,
                                                                .column = 48,
                                                            },
                                                            .end = .{
                                                                .offset = 73,
                                                                .line = 0,
                                                                .column = 73,
                                                            },
                                                        },
                                                        .equal_rarrow_position = .{
                                                            .offset = 60,
                                                            .line = 0,
                                                            .column = 60,
                                                        },
                                                    },
                                                },
                                            },
                                            .function = &FQLExpression{
                                                .field_access = .{
                                                    .value = &FQLExpression{
                                                        .identifier = .{
                                                            .text = "amounts",
                                                            .location = .{
                                                                .start = .{
                                                                    .offset = 33,
                                                                    .line = 0,
                                                                    .column = 33,
                                                                },
                                                                .end = .{
                                                                    .offset = 40,
                                                                    .line = 0,
                                                                    .column = 40,
                                                                },
                                                            },
                                                        },
                                                    },
                                                    .field = .{
                                                        .identifier = .{
                                                            .text = "reduce",
                                                            .location = .{
                                                                .start = .{
                                                                    .offset = 41,
                                                                    .line = 0,
                                                                    .column = 41,
                                                                },
                                                                .end = .{
                                                                    .offset = 47,
                                                                    .line = 0,
                                                                    .column = 47,
                                                                },
                                                            },
                                                        },
                                                    },
                                                    .location = .{
                                                        .start = .{
                                                            .offset = 33,
                                                            .line = 0,
                                                            .column = 33,
                                                        },
                                                        .end = .{
                                                            .offset = 47,
                                                            .line = 0,
                                                            .column = 47,
                                                        },
                                                    },
                                                    .dot_position = .{
                                                        .offset = 40,
                                                        .line = 0,
                                                        .column = 40,
                                                    },
                                                },
                                            },
                                            .location = .{
                                                .start = .{
                                                    .offset = 33,
                                                    .line = 0,
                                                    .column = 33,
                                                },
                                                .end = .{
                                                    .offset = 74,
                                                    .line = 0,
                                                    .column = 74,
                                                },
                                            },
                                            .comma_positions = &.{},
                                            .lparen_position = .{
                                                .offset = 47,
                                                .line = 0,
                                                .column = 47,
                                            },
                                        },
                                    },
                                    .field = .{
                                        .identifier = .{
                                            .text = "toString",
                                            .location = .{
                                                .start = .{
                                                    .offset = 75,
                                                    .line = 0,
                                                    .column = 75,
                                                },
                                                .end = .{
                                                    .offset = 83,
                                                    .line = 0,
                                                    .column = 83,
                                                },
                                            },
                                        },
                                    },
                                    .location = .{
                                        .start = .{
                                            .offset = 33,
                                            .line = 0,
                                            .column = 33,
                                        },
                                        .end = .{
                                            .offset = 83,
                                            .line = 0,
                                            .column = 83,
                                        },
                                    },
                                    .dot_position = .{
                                        .offset = 74,
                                        .line = 0,
                                        .column = 74,
                                    },
                                },
                            },
                            .arguments = &.{},
                            .location = .{
                                .start = .{
                                    .offset = 33,
                                    .line = 0,
                                    .column = 33,
                                },
                                .end = .{
                                    .offset = 85,
                                    .line = 0,
                                    .column = 85,
                                },
                            },
                            .comma_positions = &.{},
                            .lparen_position = .{
                                .offset = 83,
                                .line = 0,
                                .column = 83,
                            },
                        },
                    },
                    .location = .{
                        .start = .{
                            .offset = 24,
                            .line = 0,
                            .column = 24,
                        },
                        .end = .{
                            .offset = 85,
                            .line = 0,
                            .column = 85,
                        },
                    },
                    .operator_position = .{
                        .offset = 31,
                        .line = 0,
                        .column = 31,
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
                    .offset = 85,
                    .line = 0,
                    .column = 85,
                },
            },
            .equal_rarrow_position = .{
                .offset = 21,
                .line = 0,
                .column = 21,
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
                .non_null_assertion = .{
                    .expression = &FQLExpression{
                        .invocation = .{
                            .function = &FQLExpression{
                                .field_access = .{
                                    .value = &FQLExpression{
                                        .identifier = .{
                                            .text = "Store",
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
                                    },
                                    .field = .{
                                        .identifier = .{
                                            .text = "all",
                                            .location = .{
                                                .start = .{
                                                    .offset = 6,
                                                    .line = 0,
                                                    .column = 6,
                                                },
                                                .end = .{
                                                    .offset = 9,
                                                    .line = 0,
                                                    .column = 9,
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
                                            .offset = 9,
                                            .line = 0,
                                            .column = 9,
                                        },
                                    },
                                    .dot_position = .{
                                        .offset = 5,
                                        .line = 0,
                                        .column = 5,
                                    },
                                },
                            },
                            .arguments = &.{},
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
                            .comma_positions = &.{},
                            .lparen_position = .{
                                .offset = 9,
                                .line = 0,
                                .column = 9,
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
                            .offset = 12,
                            .line = 0,
                            .column = 12,
                        },
                    },
                },
            },
            .fields = &[_]FQLExpression.Projection.Field{
                .{
                    .short = .{
                        .text = "name",
                        .location = .{
                            .start = .{
                                .offset = 19,
                                .line = 1,
                                .column = 4,
                            },
                            .end = .{
                                .offset = 23,
                                .line = 1,
                                .column = 8,
                            },
                        },
                    },
                },
                .{
                    .long = .{
                        .key = .{
                            .text = "myCity",
                            .location = .{
                                .start = .{
                                    .offset = 29,
                                    .line = 2,
                                    .column = 4,
                                },
                                .end = .{
                                    .offset = 35,
                                    .line = 2,
                                    .column = 10,
                                },
                            },
                        },
                        .value = &FQLExpression{
                            .field_access = .{
                                .value = &FQLExpression{
                                    .anonymous_field_access = .{
                                        .field = .{
                                            .identifier = .{
                                                .text = "address",
                                                .location = .{
                                                    .start = .{
                                                        .offset = 38,
                                                        .line = 2,
                                                        .column = 13,
                                                    },
                                                    .end = .{
                                                        .offset = 45,
                                                        .line = 2,
                                                        .column = 20,
                                                    },
                                                },
                                            },
                                        },
                                        .location = .{
                                            .start = .{
                                                .offset = 37,
                                                .line = 2,
                                                .column = 12,
                                            },
                                            .end = .{
                                                .offset = 45,
                                                .line = 2,
                                                .column = 20,
                                            },
                                        },
                                    },
                                },
                                .field = .{
                                    .identifier = .{
                                        .text = "city",
                                        .location = .{
                                            .start = .{
                                                .offset = 46,
                                                .line = 2,
                                                .column = 21,
                                            },
                                            .end = .{
                                                .offset = 50,
                                                .line = 2,
                                                .column = 25,
                                            },
                                        },
                                    },
                                },
                                .location = .{
                                    .start = .{
                                        .offset = 37,
                                        .line = 2,
                                        .column = 12,
                                    },
                                    .end = .{
                                        .offset = 50,
                                        .line = 2,
                                        .column = 25,
                                    },
                                },
                                .dot_position = .{
                                    .offset = 45,
                                    .line = 2,
                                    .column = 20,
                                },
                            },
                        },
                        .colon_position = .{
                            .offset = 35,
                            .line = 2,
                            .column = 10,
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
                    .offset = 53,
                    .line = 3,
                    .column = 1,
                },
            },
            .comma_positions = &.{
                .{
                    .offset = 23,
                    .line = 1,
                    .column = 8,
                },
                .{
                    .offset = 50,
                    .line = 2,
                    .column = 25,
                },
            },
            .lbrace_position = .{
                .offset = 13,
                .line = 0,
                .column = 13,
            },
        },
    });

    try expectParsedExprEqual(
        \\((x) => x)(
        \\    {
        \\        "hi"
        \\    },
        \\)
    , .{
        .invocation = .{
            .function = &FQLExpression{
                .isolated = .{
                    .expression = &FQLExpression{
                        .function = .{
                            .parameters = .{
                                .long = .{
                                    .parameters = &.{
                                        .{
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
                                    .location = .{
                                        .start = .{
                                            .offset = 1,
                                            .line = 0,
                                            .column = 1,
                                        },
                                        .end = .{
                                            .offset = 4,
                                            .line = 0,
                                            .column = 4,
                                        },
                                    },
                                    .comma_positions = &.{},
                                },
                            },
                            .body = &FQLExpression{
                                .identifier = .{
                                    .text = "x",
                                    .location = .{
                                        .start = .{
                                            .offset = 8,
                                            .line = 0,
                                            .column = 8,
                                        },
                                        .end = .{
                                            .offset = 9,
                                            .line = 0,
                                            .column = 9,
                                        },
                                    },
                                },
                            },
                            .location = .{
                                .start = .{
                                    .offset = 1,
                                    .line = 0,
                                    .column = 1,
                                },
                                .end = .{
                                    .offset = 9,
                                    .line = 0,
                                    .column = 9,
                                },
                            },
                            .equal_rarrow_position = .{
                                .offset = 5,
                                .line = 0,
                                .column = 5,
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
                            .offset = 10,
                            .line = 0,
                            .column = 10,
                        },
                    },
                },
            },
            .arguments = &[_]FQLExpression{
                .{
                    .block_scope = .{
                        .statements = &[_]FQLExpression{
                            .{
                                .string_literal = .{
                                    .text = "\"hi\"",
                                    .location = .{
                                        .start = .{
                                            .offset = 26,
                                            .line = 2,
                                            .column = 8,
                                        },
                                        .end = .{
                                            .offset = 30,
                                            .line = 2,
                                            .column = 12,
                                        },
                                    },
                                },
                            },
                        },
                        .location = .{
                            .start = .{
                                .offset = 16,
                                .line = 1,
                                .column = 4,
                            },
                            .end = .{
                                .offset = 36,
                                .line = 3,
                                .column = 5,
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
                    .offset = 39,
                    .line = 4,
                    .column = 1,
                },
            },
            .comma_positions = &.{
                .{
                    .offset = 36,
                    .line = 3,
                    .column = 5,
                },
            },
            .lparen_position = .{
                .offset = 10,
                .line = 0,
                .column = 10,
            },
        },
    });
}

test "walk" {
    {
        const expr: FQLExpression = .null;
        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqualDeep(&expr, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .array_literal = .{
                .elements = &[_]FQLExpression{
                    .{ .boolean_literal = true },
                    .{ .boolean_literal = false },
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(&expr.array_literal.elements.?[0], try walker.next());
        try testing.expectEqual(&expr.array_literal.elements.?[1], try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .array_literal = .{
                .elements = &[_]FQLExpression{
                    .{ .boolean_literal = true },
                    .{ .boolean_literal = false },
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(&expr.array_literal.elements.?[0], try walker.next());
        try testing.expectEqual(&expr.array_literal.elements.?[1], try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .object_literal = .{
                .fields = &[_]FQLExpression.ObjectLiteral.Field{
                    .{
                        .key = .{ .identifier = "hello" },
                        .value = &FQLExpression{ .string_literal = "world" },
                    },
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.object_literal.fields.?[0].value, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .unary_operation = .{
                .operand = &FQLExpression{ .boolean_literal = false },
                .operator = .logical_not,
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.unary_operation.operand, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .unary_operation = .{
                .operand = &FQLExpression{ .boolean_literal = false },
                .operator = .logical_not,
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.unary_operation.operand, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .binary_operation = .{
                .lhs = &FQLExpression{ .string_literal = "foo" },
                .operator = .add,
                .rhs = &FQLExpression{ .string_literal = "bar" },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.binary_operation.lhs, try walker.next());
        try testing.expectEqual(expr.binary_operation.rhs, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .field_access = .{
                .value = &FQLExpression{ .string_literal = "foo" },
                .field = .{
                    .identifier = "length",
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.field_access.value, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .field_access = .{
                .value = &FQLExpression{ .string_literal = "foo" },
                .field = .{
                    .expression = &FQLExpression{ .string_literal = "length" },
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.field_access.value, try walker.next());
        try testing.expectEqual(expr.field_access.field.expression, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .invocation = .{
                .function = &FQLExpression{ .identifier = "log" },
                .arguments = &[_]FQLExpression{
                    .{ .string_literal = "foo" },
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.invocation.function, try walker.next());
        try testing.expectEqual(&expr.invocation.arguments.?[0], try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .variable_declaration = .{
                .name = "foo",
                .value = &FQLExpression{
                    .string_literal = "bar",
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.variable_declaration.value, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .block_scope = &[_]FQLExpression{
                .{ .string_literal = "foo" },
                .{ .string_literal = "bar" },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(&expr.block_scope[0], try walker.next());
        try testing.expectEqual(&expr.block_scope[1], try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .conditional = .{
                .condition = &FQLExpression{ .boolean_literal = true },
                .body = &FQLExpression{ .number_literal = "42" },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.conditional.condition, try walker.next());
        try testing.expectEqual(expr.conditional.body, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .conditional = .{
                .condition = &FQLExpression{ .identifier = "foo" },
                .body = &FQLExpression{ .number_literal = "42" },
                .@"else" = &FQLExpression{ .number_literal = "69" },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.conditional.condition, try walker.next());
        try testing.expectEqual(expr.conditional.body, try walker.next());
        try testing.expectEqual(expr.conditional.@"else".?, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .isolated = &FQLExpression{ .identifier = "foo" },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.isolated, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .function = .{
                .parameters = .{ .long = .{} },
                .body = &FQLExpression{ .string_literal = "deez nuts" },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.function.body, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .anonymous_field_access = .{
                .identifier = "foo",
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .anonymous_field_access = .{
                .expression = &FQLExpression{ .string_literal = "foo" },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.anonymous_field_access.expression, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .projection = .{
                .expression = &FQLExpression{ .identifier = "foo" },
                .fields = &[_]FQLExpression.Projection.Field{
                    .{ .short = "length" },
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.projection.expression, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .projection = .{
                .expression = &FQLExpression{ .identifier = "foo" },
                .fields = &[_]FQLExpression.Projection.Field{
                    .{ .short = "length" },
                    .{
                        .long = .{
                            .key = "zero",
                            .value = &FQLExpression{ .number_literal = "0" },
                        },
                    },
                },
            },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.projection.expression, try walker.next());
        try testing.expectEqual(expr.projection.fields.?[1].long.value, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }

    {
        const expr: FQLExpression = .{
            .non_null_assertion = &FQLExpression{ .identifier = "foo" },
        };

        var walker = expr.walk(testing.allocator);
        defer walker.deinit();

        try testing.expectEqual(&expr, try walker.next());
        try testing.expectEqual(expr.non_null_assertion, try walker.next());
        try testing.expectEqualDeep(null, try walker.next());
    }
}
