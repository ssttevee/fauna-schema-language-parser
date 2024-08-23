const std = @import("std");
const testing = std.testing;

const Tokenizer = @import("Tokenizer.zig");
const util = @import("util.zig");
const common = @import("common.zig");
const sourcemap = @import("sourcemap.zig");

pub const Token = Tokenizer.Token;
pub const TokenIterator = Tokenizer.TokenIterator;

pub const FQLType = @import("language/type.zig").FQLType;
pub const FQLExpression = @import("language/expression.zig").FQLExpression;
pub const SchemaDefinition = @import("language/schema.zig").SchemaDefinition;

pub const parseType = @import("language/type.zig").parseType;
pub const parseExpression = @import("language/expression.zig").parseExpression;
pub const parseDefinition = @import("language/schema.zig").parseDefinition;

pub const Position = common.Position;
pub const SourceLocation = common.SourceLocation;
pub const TextNode = common.TextNode;
pub const SharedPtr = common.SharedPtrUnmanaged;
pub const SourceMapWriter = sourcemap.SourceMapWriter;

test {
    _ = Tokenizer;
    _ = @import("language/type.zig");
    _ = @import("language/expression.zig");
    _ = @import("language/schema.zig");
    _ = @import("sourcemap.zig");
}

pub const QueryTree = struct {
    allocator: std.mem.Allocator,

    expressions: ?[]FQLExpression = null,

    // convenient place to keep things like source location file names that are
    // carried over when merged
    extras: ?[]common.SharedPtrUnmanaged([]const u8) = null,

    pub fn deinit(self: QueryTree) void {
        if (self.expressions) |expressions| {
            for (expressions) |expr| {
                expr.deinit(self.allocator);
            }

            self.allocator.free(expressions);
        }

        if (self.extras) |extras| {
            for (extras) |extra| {
                extra.deinit(self.allocator);
            }

            self.allocator.free(extras);
        }
    }

    pub fn dupe(self: QueryTree, allocator: std.mem.Allocator) !QueryTree {
        return .{
            .allocator = allocator,
            .expressions = try util.slice.deepDupe(allocator, self.expressions),
            .extras = try util.slice.deepDupe(allocator, self.extras),
        };
    }

    pub fn parseFile(allocator: std.mem.Allocator, filename: []const u8) !SchemaTree {
        var f = try std.fs.cwd().openFile(filename, .{});
        defer f.close();

        return try parse(allocator, f.reader().any(), filename);
    }

    pub fn parse(allocator: std.mem.Allocator, reader: std.io.AnyReader, filename: []const u8) !QueryTree {
        const duped_filename = try allocator.dupe(u8, filename);
        var it = Tokenizer.TokenIterator.init(reader, duped_filename);
        defer it.deinit(allocator);

        var exprs = std.ArrayList(FQLExpression).init(allocator);
        defer {
            for (exprs.items) |expr| {
                expr.deinit(allocator);
            }

            exprs.deinit();
        }

        while (FQLExpression.parse(allocator, &it) catch |err| {
            if (err == error.UnexpectedToken) {
                std.log.err("at {s}:{d}:{d}", .{ filename, it.tokenizer.current_line + 1, it.tokenizer.current_col + 1 });
            }

            return err;
        }) |expr| {
            try exprs.append(expr);
        }

        return .{
            .allocator = allocator,
            .expressions = try exprs.toOwnedSlice(),
            .extras = try allocator.dupe(
                common.SharedPtrUnmanaged([]const u8),
                &.{
                    try common.SharedPtrUnmanaged([]const u8).init(allocator, duped_filename),
                },
            ),
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

    // convenient place to keep things like source location file names that are
    // carried over when merged
    extras: ?[]common.SharedPtrUnmanaged([]const u8) = null,

    pub fn deinit(self: SchemaTree) void {
        if (self.declarations) |declarations| {
            for (declarations) |declaration| {
                declaration.deinit(self.allocator);
            }

            self.allocator.free(declarations);
        }

        if (self.extras) |extras| {
            for (extras) |extra| {
                extra.deinit(self.allocator);
            }

            self.allocator.free(extras);
        }
    }

    pub fn dupe(self: SchemaTree, allocator: std.mem.Allocator) !SchemaTree {
        return .{
            .allocator = allocator,
            .declarations = try util.slice.deepDupe(allocator, self.declarations),
            .extras = try util.slice.deepDupe(allocator, self.extras),
        };
    }

    pub fn parseFile(allocator: std.mem.Allocator, filename: []const u8) !SchemaTree {
        var f = try std.fs.cwd().openFile(filename, .{});
        defer f.close();

        return try parse(allocator, f.reader().any(), filename);
    }

    pub fn parse(allocator: std.mem.Allocator, reader: std.io.AnyReader, filename: []const u8) !SchemaTree {
        const duped_filename = try allocator.dupe(u8, filename);
        var it = Tokenizer.TokenIterator.init(reader, duped_filename);
        defer it.deinit(allocator);

        var decls = std.ArrayList(SchemaDefinition).init(allocator);
        defer {
            for (decls.items) |decl| {
                decl.deinit(allocator);
            }

            decls.deinit();
        }

        while (SchemaDefinition.parse(allocator, &it) catch |err| {
            if (err == error.UnexpectedToken) {
                std.log.err("at {s}:{d}:{d}", .{ duped_filename, it.tokenizer.current_line + 1, it.tokenizer.current_col + 1 });
            }

            return err;
        }) |decl| {
            try decls.append(decl);
        }

        return .{
            .allocator = allocator,
            .declarations = try decls.toOwnedSlice(),
            .extras = try allocator.dupe(
                common.SharedPtrUnmanaged([]const u8),
                &.{
                    try common.SharedPtrUnmanaged([]const u8).init(allocator, duped_filename),
                },
            ),
        };
    }

    pub fn printCanonical(self: @This(), writer: std.io.AnyWriter) !void {
        if (self.declarations) |decls| {
            for (decls) |decl| {
                try decl.printCanonical(writer, "    ");
                if (decl.location()) |loc| {
                    sourcemap.setNextWriteMapping(writer, loc.source, loc.end, null);
                }
                try writer.writeAll("\n\n");
            }
        }
    }
};
