const std = @import("std");
const util = @import("util.zig");
const Tokenizer = @import("Tokenizer.zig");
const sourcemap = @import("sourcemap.zig");

pub const Position = struct {
    /// zero-based byte offset
    offset: u64,

    /// zero-based line number
    line: u32,

    /// zero-based column number
    column: u64,

    pub fn dupe(self: Position, _: std.mem.Allocator) error{}!Position {
        return .{
            .offset = self.offset,
            .line = self.line,
            .column = self.column,
        };
    }

    pub fn bump(self: Position, change: i8) Position {
        const magnitude: u8 = @intCast(@abs(change));
        if (change > 0) {
            return .{
                .offset = self.offset + magnitude,
                .line = self.line,
                .column = self.column + magnitude,
            };
        }

        return .{
            .offset = self.offset - magnitude,
            .line = self.line,
            .column = self.column - magnitude,
        };
    }
};

pub const SourceLocation = struct {
    /// this pointer is not owned by the AST, DO NOT FREE
    source: ?[]const u8 = null,
    start: Position,
    end: Position,

    pub fn format(self: SourceLocation, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try std.fmt.format(writer, "SourceLocation{{ ", .{});
        if (self.source) |source| {
            try std.fmt.format(writer, ".source = \"{s}\", ", .{source});
        }

        try std.fmt.format(writer, ".start = .{{ .offset = {d}, .line = {d}, .column = {d} }}, ", .{ self.start.offset, self.start.line, self.start.column });
        try std.fmt.format(writer, ".end = .{{ .offset = {d}, .line = {d}, .column = {d} }}", .{ self.end.offset, self.end.line, self.end.column });

        try std.fmt.format(writer, " }}", .{});
    }
};

pub const TextNode = struct {
    text: []const u8,
    location: ?SourceLocation = null,

    pub fn deinit(self: TextNode, allocator: std.mem.Allocator) void {
        return allocator.free(self.text);
    }

    pub fn dupe(self: TextNode, allocator: std.mem.Allocator) std.mem.Allocator.Error!TextNode {
        return .{
            .text = try allocator.dupe(u8, self.text),
            .location = self.location,
        };
    }

    pub fn printCanonical(self: TextNode, writer: anytype) @TypeOf(writer).Error!void {
        if (self.location) |loc| {
            sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
        }

        try writer.writeAll(self.text);
    }

    pub fn printNamedCanonical(self: TextNode, writer: anytype) @TypeOf(writer).Error!void {
        if (self.location) |loc| {
            sourcemap.setNextWriteMapping(writer, loc.source, loc.start, self.text);
        }

        try writer.writeAll(self.text);
    }

    pub fn format(self: TextNode, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.writeAll(self.text);
    }
};

pub const BooleanNode = struct {
    value: bool,
    location: ?SourceLocation = null,

    pub fn dupe(self: BooleanNode, _: std.mem.Allocator) !BooleanNode {
        return self;
    }

    pub fn printCanonical(self: BooleanNode, writer: anytype) @TypeOf(writer).Error!void {
        if (self.location) |loc| {
            sourcemap.setNextWriteMapping(writer, loc.source, loc.start, null);
        }

        try writer.writeAll(if (self.value) "true" else "false");
    }

    pub fn format(self: BooleanNode, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try std.fmt.format(writer, "{}", .{self.value});
    }
};

/// simple non-thread-safe ref counted pointer
pub fn SharedPtrUnmanaged(comptime T: type) type {
    return struct {
        counter: *usize,
        value: T,

        pub fn init(allocator: std.mem.Allocator, value: T) !@This() {
            const counter = try allocator.create(usize);
            counter.* = 1;
            return .{
                .counter = counter,
                .value = value,
            };
        }

        pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
            if (self.counter.* > 1) {
                self.counter.* -= 1;
                return;
            }

            allocator.destroy(self.counter);

            switch (@typeInfo(T)) {
                .Pointer => |info| switch (info.size) {
                    .One => return allocator.destroy(self.value),
                    .Slice => return allocator.free(self.value),
                    else => {},
                },
                .Struct, .Enum, .Union => {
                    if (@hasDecl(T, "deinit")) {
                        return @constCast(&self.value).deinit();
                    }
                },
                else => {},
            }

            std.debug.panic("unable to deinit value {any}", .{T});
        }

        pub inline fn get(self: @This()) T {
            return self.value;
        }

        pub inline fn set(self: @This(), value: T) void {
            self.value = value;
        }

        pub inline fn dupe(self: @This(), _: std.mem.Allocator) !@This() {
            self.counter.* += 1;
            return self;
        }

        pub inline fn dupeSlice(slice: anytype, allocator: std.mem.Allocator) !@TypeOf(slice) {
            return try util.slice.deepDupe(allocator, slice);
        }
    };
}
