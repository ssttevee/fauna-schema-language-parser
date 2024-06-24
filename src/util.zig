const std = @import("std");
const testing = std.testing;

fn PredicateElementType(comptime predicate: anytype) type {
    std.debug.assert(@typeInfo(@TypeOf(predicate)) == .Fn);
    std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.calling_convention == .Unspecified);
    std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.is_generic == false);
    std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.is_var_args == false);
    std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.return_type == bool);
    std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.params.len == 1 or @typeInfo(@TypeOf(predicate)).Fn.params.len == 2);
    std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.params[0].is_generic == false);
    std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.params[0].is_noalias == false);
    if (@typeInfo(@TypeOf(predicate)).Fn.params.len == 2) {
        std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.params[1].is_generic == false);
        std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.params[1].is_noalias == false);
        std.debug.assert(@typeInfo(@TypeOf(predicate)).Fn.params[1].type == usize);
    }

    return @typeInfo(@TypeOf(predicate)).Fn.params[0].type.?;
}

pub const slice = struct {
    pub fn indexOfPosFn(elements: anytype, pos: usize, predicate: anytype) ?usize {
        const T = PredicateElementType(predicate);
        const s = blk: {
            if (@TypeOf(elements) == std.ArrayList(T) or @TypeOf(elements) == std.ArrayListUnmanaged(T)) {
                break :blk elements.items;
            } else if (@TypeOf(elements) == []T or @TypeOf(elements) == []const T) {
                break :blk elements;
            }

            @compileError(std.fmt.comptimePrint("unexpected elements type ({any}) for predicate type ({any})", .{ @TypeOf(elements), T }));
        };

        if (@TypeOf(predicate) == fn (elem: T, index: usize) bool) {
            for (pos..s.len) |i| {
                if (predicate(s[i], i)) {
                    return i;
                }
            }
        } else if (@TypeOf(predicate) == fn (elem: T) bool) {
            for (pos..s.len) |i| {
                if (predicate(s[i])) {
                    return i;
                }
            }
        }

        return null;
    }

    pub fn indexOfFn(elements: anytype, predicate: anytype) ?usize {
        return indexOfPosFn(elements, 0, predicate);
    }

    pub fn some(elements: anytype, predicate: anytype) bool {
        return indexOfFn(elements, predicate) != null;
    }

    pub fn every(elements: anytype, predicate: anytype) bool {
        return indexOfFn(elements, predicateNegation(predicate)) == null;
    }
};

pub fn predicateNegation(comptime predicate: anytype) @TypeOf(predicate) {
    const ElementType = PredicateElementType(predicate);

    if (@typeInfo(@TypeOf(predicate)).Fn.params.len == 2) {
        return struct {
            fn negation(elem: ElementType, index: usize) bool {
                return !predicate(elem, index);
            }
        }.negation;
    }

    return struct {
        fn negation(elem: ElementType) bool {
            return !predicate(elem);
        }
    }.negation;
}

pub const mem = struct {
    pub fn createCopy(comptime T: type, allocator: std.mem.Allocator, p: *const T) !*T {
        const ptr = try allocator.create(T);
        ptr.* = p.*;
        return ptr;
    }
};
