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

    const infile, const filename = blk: {
        var args = std.process.args();
        _ = args.next();
        if (args.next()) |file| {
            if (file.len != 1 or file[0] != '-') {
                break :blk .{ try std.fs.cwd().openFile(file, .{}), file };
            }
        }

        break :blk .{ std.io.getStdIn(), "-" };
    };
    defer infile.close();

    const stdout = std.io.getStdOut();
    defer stdout.close();

    var hash = std.crypto.hash.sha2.Sha256.init(.{});

    const w = (TeeWriter{ .writers = &.{ stdout.writer().any(), (HashWriter(@TypeOf(hash)){ .context = &hash }).any() } }).writer();

    var it = fauna.TokenIterator.init(infile.reader().any());
    defer it.deinit(allocator);

    while (fauna.FQLExpression.parse(allocator, &it) catch |err| {
        if (err == error.UnexpectedToken) {
            std.log.err("at {s}:{d}:{d}", .{ filename, it.tokenizer.current_line + 1, it.tokenizer.current_col + 1 });
        }

        return err;
    }) |expr| {
        defer expr.deinit(allocator);

        try expr.printCanonical(w.any(), "  ", 0);
        try w.writeByte('\n');
    }

    for (hash.finalResult()) |byte| {
        std.debug.print("{x:0>2}", .{byte});
    }
    std.debug.print("\n", .{});
}
