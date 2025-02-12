const std = @import("std");
const testing = std.testing;

const Tokenizer = @import("Tokenizer.zig");
const util = @import("../util.zig");

pub fn ManagedParser(comptime UnmanagedParser: type) type {
    const T = @typeInfo(std.meta.fields(UnmanagedParser.PushResult)[1].type).Optional.child;
    return struct {
        pub const Unmanaged = UnmanagedParser;

        allocator: std.mem.Allocator,
        inner: Unmanaged = .{},

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: Self) void {
            self.inner.deinit(self.allocator);
        }

        pub fn reset(self: *Self) void {
            self.deinit();
            self.inner = .{};
        }

        pub const PushResult = UnmanagedParser.PushResult;

        pub fn pushToken(self: *Self, token: Tokenizer.TokenWithLocation) !PushResult {
            return try self.inner.pushToken(self.allocator, token);
        }

        pub fn parseIterator(allocator: std.mem.Allocator, it: *Tokenizer.TokenIterator) !?T {
            var parser = Self.init(allocator);
            defer parser.deinit();

            while (true) {
                const token = try it.nextToken(allocator);
                defer token.deinit(allocator);

                // std.debug.print("{} pushing token {s} {s}\n", .{ T, @tagName(token.token), token.token.bytes() });

                const result = try parser.pushToken(token);
                if (result.save) |save| {
                    // std.debug.print("{} saving token {s} {s}\n", .{ T, @tagName(save.token), token.token.bytes() });

                    it.saveToken(try save.dupe(allocator));
                }

                if (@field(result, std.meta.fieldNames(PushResult)[1])) |final| {
                    // std.debug.print("{} emitting {s} {s}\n", .{ T, std.meta.fields(PushResult)[1].name, @tagName(final) });
                    return final;
                }

                if (token.token == .eof and parser.inner.state == .empty and (!@hasField(Unmanaged, "parent") or parser.inner.parent == null)) {
                    return null;
                }
            }
        }

        pub fn parseReader(allocator: std.mem.Allocator, reader: std.io.AnyReader, source_file: ?[]const u8) !?T {
            var it = Tokenizer.TokenIterator.init(reader, source_file);
            defer it.deinit(allocator);

            return try parseIterator(allocator, &it);
        }

        pub fn parseString(allocator: std.mem.Allocator, str: []const u8, source_file: ?[]const u8) !?T {
            var stream = std.io.fixedBufferStream(str);
            return try parseReader(allocator, stream.reader().any(), source_file);
        }
    };
}

pub fn checkForLeaks(comptime P: type, str: []const u8) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ .backing_allocator = testing.allocator };
    const allocator = gpa.allocator();

    var limit: usize = 0;
    while (true) {
        // std.debug.print("checking for leak after {d} tokens\n", .{limit});
        var parser = P.init(allocator);
        defer parser.deinit();

        var stream = std.io.fixedBufferStream(str);
        var it = Tokenizer.TokenIterator.init(stream.reader().any(), null);
        defer it.deinit(allocator);

        var i: usize = 0;
        while (try it.next(allocator)) |token| {
            defer token.deinit(allocator);

            const result = try parser.pushToken(token);
            if (result.save) |save| {
                // std.debug.print("saving token: {s}\n", .{@tagName(save)});
                it.saveToken(try save.dupe(allocator));
            }

            if (@field(result, std.meta.fieldNames(P.PushResult)[1])) |final| {
                final.deinit(allocator);
            }

            if (i >= limit) {
                limit += 1;
                break;
            }

            i += 1;
        } else {
            break;
        }
    }

    if (gpa.deinit() == .leak) {
        return error.TestLeaked;
    }
}

pub fn testDupe(value: anytype) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){ .backing_allocator = testing.allocator };
    const allocator = gpa.allocator();

    const duped = try value.dupe(allocator);

    testing.expectEqualDeep(value, duped) catch |err| {
        duped.deinit(allocator);

        return err;
    };

    duped.deinit(allocator);

    if (gpa.deinit() == .leak) {
        return error.TestLeaked;
    }
}
