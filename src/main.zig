const std = @import("std");

const fauna = @import("root.zig");

fn HashWriter(comptime T: type) type {
    return std.io.Writer(
        *T,
        error{},
        struct {
            fn writeFn(hash: *T, bytes: []const u8) error{}!usize {
                hash.update(bytes);
                return bytes.len;
            }
        }.writeFn,
    );
}

const TeeWriter = struct {
    writers: []const std.io.AnyWriter,

    fn writeFn(self: TeeWriter, bytes: []const u8) anyerror!usize {
        for (self.writers) |w| {
            try w.writeAll(bytes);
        }

        return bytes.len;
    }

    pub fn writer(self: TeeWriter) std.io.Writer(TeeWriter, anyerror, writeFn) {
        return .{ .context = self };
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.detectLeaks();

    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn();
    defer stdin.close();

    const stdout = std.io.getStdOut();
    defer stdout.close();

    var hash = std.crypto.hash.sha2.Sha256.init(.{});

    const w = (TeeWriter{ .writers = &.{ stdout.writer().any(), (HashWriter(@TypeOf(hash)){ .context = &hash }).any() } }).writer();

    var it = fauna.TokenIterator.init(stdin.reader().any());
    defer it.deinit(allocator);

    while (try it.next(allocator)) |token| {
        it.saveToken(token);

        const expr = try fauna.FQLExpression.parse(allocator, &it);
        defer expr.deinit(allocator);

        try expr.printCanonical(w.any(), "  ", 0);
        try w.writeByte('\n');
    }

    for (hash.finalResult()) |byte| {
        std.debug.print("{x:0>2}", .{byte});
    }
    std.debug.print("\n", .{});
}
