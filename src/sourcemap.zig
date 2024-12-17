const std = @import("std");
const testing = std.testing;

const sourcemap = @import("sourcemap");
const util = @import("util.zig");

const Position = @import("common.zig").Position;

fn isWhitespaceOrLineTerminator(char: u8) bool {
    return util.isWhitespace(char) or util.isLineTerminator(char);
}

fn Base64Writer(comptime Writer: type, comptime Encoder: std.base64.Base64Encoder) type {
    return struct {
        w: Writer,
        saved: [2]u8 = undefined,
        saved_len: u2 = 0,
        closed: bool = false,

        fn writeFn(self: *@This(), bytes: []const u8) Writer.Error!usize {
            if (self.closed) {
                @setCold(true);
                std.debug.panic("cannot write after close", .{});
            }

            // std.debug.print("writing {d} bytes: {s}\n", .{ bytes.len, bytes });

            if (self.saved_len + bytes.len < 3) {
                // std.debug.print("fewer than 3 bytes...\n", .{});
                std.mem.copyForwards(u8, self.saved[self.saved_len..], bytes);
                self.saved_len = @intCast(self.saved_len + bytes.len);
                return bytes.len;
            }

            var dest: [4]u8 = undefined;

            var i: usize = 0;

            if (self.saved_len > 0) {
                // std.debug.print("found {d} saved bytes: {s}\n", .{ self.saved_len, self.saved[0..self.saved_len] });

                var source: [3]u8 = undefined;
                std.mem.copyForwards(u8, &source, self.saved[0..self.saved_len]);
                std.mem.copyForwards(u8, source[self.saved_len..], bytes[0 .. 3 - self.saved_len]);

                try self.w.writeAll(Encoder.encode(&dest, &source));

                i = 3 - self.saved_len;
                self.saved_len = 0;
            } else {
                // std.debug.print("no saved bytes\n", .{});
            }

            while (i + 2 < bytes.len) : (i += 3) {
                try self.w.writeAll(Encoder.encode(&dest, bytes[i .. i + 3]));
            }

            if (i < bytes.len) {
                const remaining = bytes.len - i;
                std.debug.assert(remaining < 3);
                std.mem.copyForwards(u8, &self.saved, bytes[i..]);
                self.saved_len = @intCast(remaining);
            }

            return bytes.len;
        }

        pub fn flush(self: *@This()) Writer.Error!void {
            if (self.closed) {
                return;
            }

            self.closed = true;

            if (self.saved_len == 0) {
                return;
            }

            // std.debug.print("flushing {d} saved bytes: {s}\n", .{ self.saved_len, self.saved[0..self.saved_len] });

            var dest: [4]u8 = undefined;
            try self.w.writeAll(Encoder.encode(&dest, self.saved[0..self.saved_len]));
        }

        pub fn writer(self: *@This()) std.io.Writer(*@This(), Writer.Error, writeFn) {
            return .{ .context = self };
        }
    };
}

fn base64Writer(writer: anytype) Base64Writer(@TypeOf(writer), std.base64.standard.Encoder) {
    return .{ .w = writer };
}

fn testBase64Writer(expected: []const u8, input: []const u8) !void {
    // std.debug.print("testing base64 {s} -> {s}\n", .{ input, expected });
    for (1..input.len + 1) |chunk_size| {
        // std.debug.print("using chunk_size {d}\n", .{chunk_size});

        var out = std.ArrayList(u8).init(testing.allocator);
        defer out.deinit();

        var w = base64Writer(out.writer());
        var i: usize = 0;
        while (i < input.len) : (i += chunk_size) {
            try w.writer().writeAll(input[i..@min(i + chunk_size, input.len)]);
        }
        try w.flush();

        try testing.expectEqualStrings(expected, out.items);
    }
}

test base64Writer {
    try testBase64Writer("YQ==", "a");
    try testBase64Writer("YWI=", "ab");
    try testBase64Writer("YWJj", "abc");
    try testBase64Writer("YWJjZA==", "abcd");
    try testBase64Writer("YWJjZGU=", "abcde");
    try testBase64Writer("YWJjZGVm", "abcdef");
    try testBase64Writer("YWJjZGVmZw==", "abcdefg");
    try testBase64Writer("YWJjZGVmZ2g=", "abcdefgh");
    try testBase64Writer("YWJjZGVmZ2hp", "abcdefghi");
}

pub const SourceMapWriter = struct {
    allocator: std.mem.Allocator,
    stream: std.io.AnyWriter,
    name_map: std.StringArrayHashMapUnmanaged([]const u8) = .{},
    state: *struct {
        generator: sourcemap.Generator,
        current_position: sourcemap.Position,
        next_mapping: std.meta.FieldType(sourcemap.Mapping, .original) = null,
        saved_mapping: std.meta.FieldType(sourcemap.Mapping, .original) = null,
    },

    pub fn init(allocator: std.mem.Allocator, w: std.io.AnyWriter, source_file: []const u8, source_root: []const u8) !SourceMapWriter {
        const state = try allocator.create(@typeInfo(std.meta.FieldType(SourceMapWriter, .state)).Pointer.child);
        state.* = .{
            .generator = try sourcemap.Generator.init(allocator, source_file, source_root),
            .current_position = .{
                .line = 0,
                .column = 0,
            },
        };
        return .{
            .allocator = allocator,
            .stream = w,
            .state = state,
        };
    }

    pub fn deinit(self: SourceMapWriter) void {
        self.state.generator.deinit();

        self.allocator.destroy(self.state);
    }

    pub fn setSourceContent(self: SourceMapWriter, source_file: []const u8, source_content: ?[]const u8) !void {
        try self.state.generator.setSourceContent(source_file, source_content);
    }

    pub fn addMapping(self: SourceMapWriter, source: ?[]const u8, position: Position, name: ?[]const u8) void {
        std.debug.assert(self.state.next_mapping == null);

        self.state.next_mapping = .{
            .source = source orelse "memory",
            .position = .{
                .line = position.line,
                .column = position.column,
            },
            .name = blk: {
                if (name) |mangled_name| {
                    if (self.name_map.get(mangled_name)) |original_name| {
                        break :blk original_name;
                    }
                }

                break :blk null;
            },
        };
    }

    fn anyWriteFn(ptr: *const anyopaque, bytes: []const u8) anyerror!usize {
        const self: *const SourceMapWriter = @alignCast(@ptrCast(ptr));

        return self.writeFn(bytes);
    }

    fn writeFn(self: *const SourceMapWriter, bytes: []const u8) anyerror!usize {
        if (self.state.next_mapping == null and util.slice.every(bytes, isWhitespaceOrLineTerminator)) {
            // skip add mapping when all bytes are whitespace to group `writeXNTimes` calls
        } else {
            try self.state.generator.addMapping(.{
                .generated = self.state.current_position,
                .original = self.state.next_mapping,
            });

            self.state.saved_mapping = self.state.next_mapping;
            self.state.next_mapping = null;
        }

        var line_it = std.mem.splitScalar(u8, bytes, '\n');
        var last_line = line_it.next().?;

        var new_lines: usize = 0;
        while (line_it.next()) |line| {
            if (new_lines > 0) {
                if (!util.slice.every(last_line, isWhitespaceOrLineTerminator)) {
                    self.state.saved_mapping = null;
                }

                try self.state.generator.addMapping(.{
                    .generated = .{
                        .line = self.state.current_position.line + @as(u32, @intCast(new_lines)),
                        .column = 0,
                    },
                    .original = self.state.saved_mapping,
                });
            }

            new_lines += 1;
            last_line = line;
        }

        // if (last_line.len > 0) {
        //     if (!util.slice.every(last_line, isWhitespaceOrLineTerminator)) {
        //         self.state.saved_mapping = null;
        //     }

        //     try self.state.generator.addMapping(.{
        //         .generated = .{
        //             .line = self.state.current_position.line + @as(u32, @intCast(new_lines)),
        //             .column = 0,
        //         },
        //         .original = self.state.saved_mapping,
        //     });
        // }

        if (new_lines > 0) {
            self.state.current_position.column = @intCast(last_line.len);
            self.state.current_position.line += @intCast(new_lines);
        }

        for (last_line) |c| {
            if (c == '\r') {
                // ignore
                continue;
            }

            self.state.current_position.column += 1;
        }

        return self.stream.write(bytes);
    }

    pub fn writer(self: *const SourceMapWriter) std.io.Writer(*const SourceMapWriter, anyerror, writeFn) {
        return .{ .context = self };
    }

    pub fn anyWriter(self: *const SourceMapWriter) std.io.AnyWriter {
        return .{ .context = self, .writeFn = anyWriteFn };
    }

    pub fn writeInlineSourceMap(self: *const SourceMapWriter) !void {
        try self.stream.writeAll("\n//# sourceMappingURL=data:application/json;charset=utf-8;base64,");

        var b64w = base64Writer(self.stream);
        var jsonwriter = std.json.writeStream(b64w.writer(), .{});
        try self.state.generator.jsonStringify(&jsonwriter);
        try b64w.flush();

        try self.stream.writeByte('\n');
    }
};

pub fn setNextWriteMapping(writer: anytype, source: ?[]const u8, position: Position, name: ?[]const u8) void {
    if (@hasField(@TypeOf(writer), "context") and @TypeOf(writer.context) == *const SourceMapWriter) {
        return writer.context.addMapping(source, position, name);
    }

    if (@TypeOf(writer) == std.io.AnyWriter and writer.writeFn == SourceMapWriter.anyWriteFn) {
        return @as(*const SourceMapWriter, @alignCast(@ptrCast(writer.context))).addMapping(source, position, name);
    }
}
