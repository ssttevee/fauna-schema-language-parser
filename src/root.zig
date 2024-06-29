const std = @import("std");
const testing = std.testing;

const Tokenizer = @import("Tokenizer.zig");
const util = @import("util.zig");

pub const Token = Tokenizer.Token;
pub const TokenIterator = Tokenizer.TokenIterator;

pub const FQLType = @import("language/type.zig").FQLType;
pub const FQLExpression = @import("language/expression.zig").FQLExpression;
pub const SchemaDefinition = @import("language/schema.zig").SchemaDefinition;

pub const parseType = @import("language/type.zig").parseType;
pub const parseExpression = @import("language/expression.zig").parseExpression;
pub const parseDefinition = @import("language/schema.zig").parseDefinition;

test {
    _ = Tokenizer;
    _ = @import("language/type.zig");
    _ = @import("language/expression.zig");
    _ = @import("language/schema.zig");
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
        var it = Tokenizer.TokenIterator.init(reader);
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
