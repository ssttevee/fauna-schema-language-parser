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

    pub inline fn indexOfNonePosFn(elements: anytype, pos: usize, predicate: anytype) ?usize {
        return indexOfPosFn(elements, pos, predicateNegation(predicate));
    }

    pub inline fn indexOfFn(elements: anytype, predicate: anytype) ?usize {
        return indexOfPosFn(elements, 0, predicate);
    }

    pub inline fn indexOfNoneFn(elements: anytype, predicate: anytype) ?usize {
        return indexOfNonePosFn(elements, 0, predicate);
    }

    pub inline fn some(elements: anytype, predicate: anytype) bool {
        return indexOfFn(elements, predicate) != null;
    }

    pub inline fn every(elements: anytype, predicate: anytype) bool {
        return indexOfNoneFn(elements, predicate) == null;
    }

    pub fn deepDupe(allocator: std.mem.Allocator, elems: anytype) std.mem.Allocator.Error!@TypeOf(elems) {
        const orig_elems = blk: {
            if (@typeInfo(@TypeOf(elems)) == .Optional) {
                break :blk elems orelse return null;
            } else {
                break :blk elems;
            }
        };

        var i: usize = 0;
        const new_elems = try allocator.alloc(@TypeOf(orig_elems[0]), orig_elems.len);
        errdefer {
            for (new_elems[0..i]) |elem| {
                elem.deinit(allocator);
            }

            allocator.free(new_elems);
        }

        for (orig_elems) |orig_elem| {
            new_elems[i] = try orig_elem.dupe(allocator);
            i += 1;
        }

        return new_elems;
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

/// https://docs.fauna.com/fauna/current/reference/fql_reference/lexical#whitespace
pub fn isWhitespace(char: u8) bool {
    // character tab            : u+0009
    // line tab                 : u+000B
    // form feed                : u+000C
    // space                    : u+0020
    // no-break space           : u+00A0
    // zero-width no-break space: u+feff (unhandled)
    return char == '\x09' or char == '\x0B' or char == '\x0C' or char == '\x20' or char == '\xA0';
}

/// https://docs.fauna.com/fauna/current/reference/fql_reference/lexical#line-terminators
pub fn isLineTerminator(char: u8) bool {
    // line-feed          : u+000A
    // carriage return    : u+000D
    // line separator     : u+2028 (unhandled)
    // paragraph separator: u+2029 (unhandled)
    return char == '\x0A' or char == '\x0D';
}
