const std = @import("std");

const file_buffer_err = error{
    UnexpectedEndOfBuffer,
};

const file_buffer = struct {
    bytes: []const u8,
    cursor: usize,

    // Takes utf-8/wtf-8 path name
    pub fn init(path_relative: []const u8, alloc: std.mem.Allocator) !file_buffer {
        var file = try std.fs.cwd().openFile(
            path_relative,
            .{ .mode = .read_only },
        );
        defer file.close();
        const file_stat = try file.stat();
        return .{
            .bytes = try file.readToEndAlloc(
                alloc,
                file_stat.size,
            ),
            .cursor = 0,
        };
    }

    // Compile time check to be sure the type can be read
    pub fn readable(comptime T: type) void {
        switch (@typeInfo(T)) {
            .Struct => |Struct| {
                inline for (Struct.fields) |field| {
                    file_buffer.readable(field.type);
                }
            },
            .Enum => |Enum| {
                readable(Enum.tag_type);
            },
            .Int, .Float, .Array, .Bool => {},
            else => @compileError("File buffer cannot read type '" ++ @typeName(T) ++ "'"),
        }
    }

    pub fn read(self: *file_buffer, comptime T: type) !T {
        file_buffer.readable(T);
        if (self.bytes.len - self.cursor < @sizeOf(T)) {
            return file_buffer_err.UnexpectedEndOfBuffer;
        }
        switch (@typeInfo(T)) {
            .Struct => |info| {
                if (info.layout != std.builtin.Type.ContainerLayout.@"packed") {
                    var data: T = undefined;
                    inline for (info.fields) |field| {
                        // TODO:@field to access and read the fields
                        const data_ptr: [*]u8 = @ptrCast(&data);
                        @memcpy(
                            data_ptr + @offsetOf(T, field.name),
                            (self.bytes.ptr + self.cursor)[0..@sizeOf(field.type)],
                        );
                        self.cursor += @sizeOf(field.type);
                    }
                    return data;
                }
            },
            else => {},
        }

        defer self.cursor += @sizeOf(T);
        return std.mem.bytesToValue(T, (self.bytes.ptr + self.cursor));
    }

    pub fn read_array(self: *file_buffer, comptime T: type, out: []T) !void {
        file_buffer.readable(T);
        if (self.cursor + @sizeOf(T) * out.len > self.bytes.len) {
            return file_buffer_err.UnexpectedEndOfBuffer;
        }

        switch (@typeInfo(T)) {
            .Struct => |info| {
                if (info.layout != std.builtin.Type.ContainerLayout.@"packed") {
                    for (out) |*out_element| {
                        out_element.* = self.read(T);
                    }
                }
            },
            else => {
                @memcpy(
                    @as([*]u8, @ptrCast(out.ptr)),
                    (self.bytes.ptr + self.cursor)[0 .. @sizeOf(T) * out.len],
                );
                self.cursor += @sizeOf(T) * out.len;
            },
        }
    }

    pub fn read_utf8_alloc(self: *file_buffer, alloc: std.mem.Allocator, size: usize) ![]u8 {
        const utf8 = try alloc.alloc(u8, size);
        @memcpy(utf8, self.bytes.ptr + self.cursor);
        self.cursor += size;
        return utf8;
    }

    pub fn read_utf16Le_to_utf8_alloc(self: *file_buffer, alloc: std.mem.Allocator, size: usize) ![]u8 {
        const utf16le = try alloc.alloc(u16, size / 2);
        defer {
            self.cursor += size;
            alloc.free(utf16le);
        }
        @memcpy(
            @as([*]u8, @ptrCast(utf16le)),
            (self.bytes.ptr + self.cursor)[0..size],
        );
        return try std.unicode.utf16LeToUtf8Alloc(alloc, utf16le);
    }

    pub fn print_info(self: file_buffer) void {
        std.debug.print(
            "Total Bytes: {d}\n" ++
                "Cursor Position: {d}\n",
            .{
                self.bytes.len,
                self.cursor,
            },
        );
    }
};

const pmx_global = enum(u8) {
    TEXT_ENCODE = 0,
    ADDITIONAL_VEC4,
    VERTEX_INDEX_SIZE,
    TEXTURE_INDEX_SIZE,
    MATERIAL_INDEX_SIZE,
    BONE_INDEX_SIZE,
    MORPH_INDEX_SIZE,
    RIGID_BODY_INDEX_SIZE,
};

const pmx_utf = enum(u8) {
    utf8 = 1,
    utf16 = 0,
};

const pmx_err = error{
    IndexSizeWrong,
    MalformedVertexDeformType,
};

const pmx_header = struct {
    signature: [4]u8,
    version: f32,
    globals: []u8,

    name_local: []u8,
    name_global: []u8,

    comments_local: []u8,
    comments_global: []u8,
};

// 4 is additional vec4 max
const pmx_vertex = struct {
    const deform_type = enum(u8) {
        LinearBlend,
        SphericalBlend,
        DualQuatBlend,
    };

    position: [3]f32,
    normal: [3]f32,
    uv: [2]f32,
    additional: [4][4]f32,
    deform_type: deform_type,
    bones: [4]u32,
    weights: [4]f32,
    edge: f32,
};

const pmx_material = struct {
    const flags = packed struct {
        NoCull: bool,
        GroundShadow: bool,
        DrawShadow: bool,
        RecieveShadow: bool,
        HasEdge: bool,
        VertexColor: bool,
        PointDrawing: bool,
        LineDrawing: bool,
    };

    const environment_blend = enum(u8) {
        Disabled = 0,
        Multiply = 1,
        Additive = 2,
        VertColor = 3,
    };

    name_local: []u8,
    name_global: []u8,
    diffuse: [4]f32,
    specular: [3]f32,
    spec: f32,
    ambient: [3]f32,
    flags: flags,
    edge_color: [4]f32,
    edge_scale: f32,
    tex_index: u32,
    env_index: u32,
    env_blend: environment_blend,
    toon_ext: bool,
    toon_ref: u32,
    meta_data: []u8,
    indices: u32,
};

const pmx_bone = struct {
    const flags = packed struct {
        TailPositionIndexed: bool,
        Rotatable: bool,
        Translatable: bool,
        IsVisible: bool,
        Enabled: bool,
        InverseKinematics: bool,
        pad0: u2,
        InheritRotation: bool,
        InheritTranslation: bool,
        FixedAxis: bool,
        LocalCoordinate: bool,
        PhysAfterDeform: bool,
        ExternalParentDeform: bool,
        pad1: u2,
    };

    const tail = union(enum) {
        position: [3]f32,
        bone: u32,
    };

    const inherit = struct {
        parent: u32,
        weight: f32,
    };

    const local_coords = struct {
        x: [3]f32,
        z: [3]f32,
    };

    const ik = struct {
        const angle_limit = struct {
            min: [3]f32,
            max: [3]f32,
        };

        const link = struct {
            bone: u32,
            limit: ?angle_limit,
        };

        target: u32,
        loop_count: u32,
        limit_radian: f32,
        links: []link,
    };

    name_local: []u8,
    name_global: []u8,
    position: [3]f32,
    parent: u32,
    layer: i32,
    flags: flags,
    tail: tail,

    inherit: ?inherit = null,
    fixed_axis: ?[3]f32 = null,
    local_coords: ?local_coords = null,
    external_parent: ?u32 = null, //Is this a "bone index" or a signed 32bit integer
    ik: ?ik = null,
};

const pmx_morph = struct {
    const group = struct {
        morph: u32,
        weight: f32,
    };
    const vertex = struct {
        vertex: u32,
        translation: [3]f32,
    };
    const bone = struct {
        bone: u32,
        translation: [3]f32,
        rotation: [4]f32,
    };
    const UV = struct {
        vertex: u32,
        data: [4]f32,
    };
    const material = struct {
        material: u32,
        unknown: u8,
        diffuse: [4]f32,
        specular: [3]f32,
        spec: f32,
        ambient: [3]f32,
        edge_color: [4]f32,
        edge_size: f32,
        texture_tint: [4]f32,
        environment_tint: [4]f32,
        toon_tint: [4]f32,
    };
    const flip = struct {
        morph: u32,
        weight: f32,
    };
    const impulse = struct {
        rigid_body: u32,
        local: bool,
        speed: [3]f32,
        torque: [3]f32,
    };
    const panel_type = enum(u8) {
        Hidden = 0,
        Eyebrows,
        Eyes,
        Mouth,
        Other,
    };
    const morph_type = enum(u8) {
        Group = 0, // Group of morphs
        Vertex,
        Bone,
        UV,
        UV1,
        UV2,
        UV3,
        UV4,
        Material,
        Flip,
        Impulse,
    };

    const morph_data = union(morph_type) {
        Group: []group,
        Vertex: []vertex,
        Bone: []bone,
        UV: []UV,
        UV1: []UV,
        UV2: []UV,
        UV3: []UV,
        UV4: []UV,
        Material: []material,
        Flip: []flip,
        Impulse: []impulse,

        pub fn init(tag: morph_type, count: usize, alloc: std.mem.Allocator) !morph_data {
            return switch (tag) {
                .Group => morph_data{ .Group = try alloc.alloc(pmx_morph.group, count) },
                .Vertex => morph_data{ .Vertex = try alloc.alloc(pmx_morph.vertex, count) },
                .Bone => morph_data{ .Bone = try alloc.alloc(pmx_morph.bone, count) },
                .UV => morph_data{ .UV = try alloc.alloc(pmx_morph.UV, count) },
                .UV1 => morph_data{ .UV1 = try alloc.alloc(pmx_morph.UV, count) },
                .UV2 => morph_data{ .UV2 = try alloc.alloc(pmx_morph.UV, count) },
                .UV3 => morph_data{ .UV3 = try alloc.alloc(pmx_morph.UV, count) },
                .UV4 => morph_data{ .UV4 = try alloc.alloc(pmx_morph.UV, count) },
                .Material => morph_data{ .Material = try alloc.alloc(pmx_morph.material, count) },
                .Flip => morph_data{ .Flip = try alloc.alloc(pmx_morph.flip, count) },
                .Impulse => morph_data{ .Impulse = try alloc.alloc(pmx_morph.impulse, count) },
            };
        }
    };

    name_local: []u8,
    name_global: []u8,
    panel: panel_type,
    data: morph_data,
};

const pmx_display = struct {
    const frame_type = enum(u1) {
        Bone,
        Morph,
    };
    const frame = union(frame_type) {
        Bone: u32,
        Morph: u32,
    };
    name_local: []u8,
    name_global: []u8,
    special: bool,
    frames: []frame,
};

const pmx_data = struct {
    header: pmx_header,

    vertices: []pmx_vertex,
    indices: []u32,
    textures: [][]u8,
    materials: []pmx_material,
    bones: []pmx_bone,
    morphs: []pmx_morph,
    displays: []pmx_display,

    pub fn init(buffer: *file_buffer, alloc: std.mem.Allocator) !pmx_data {
        var data: pmx_data = undefined;
        try data.read_pmx_header(buffer, alloc);
        try data.read_pmx_vertices(buffer, alloc);
        try data.read_pmx_indices(buffer, alloc);
        try data.read_pmx_textures(buffer, alloc);
        try data.read_pmx_materials(buffer, alloc);
        try data.read_pmx_bones(buffer, alloc);
        try data.read_pmx_morphs(buffer, alloc);
        try data.read_pmx_displays(buffer, alloc);
        return data;
    }

    pub fn read_text(self: pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) ![]u8 {
        const text_size = @as(usize, @intCast(try buffer.read(i32)));
        return switch (self.utf()) {
            .utf8 => try buffer.read_utf8_alloc(alloc, text_size),
            .utf16 => try buffer.read_utf16Le_to_utf8_alloc(alloc, text_size),
        };
    }

    pub fn read_pmx_header(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        self.header.signature = try buffer.read(@TypeOf(self.header.signature));
        self.header.version = try buffer.read(f32);
        self.header.globals = try alloc.alloc(u8, @as(usize, try buffer.read(u8)));
        try buffer.read_array(u8, self.header.globals);

        self.header.name_local = try self.read_text(buffer, alloc);
        self.header.name_global = try self.read_text(buffer, alloc);

        self.header.comments_local = try self.read_text(buffer, alloc);
        self.header.comments_global = try self.read_text(buffer, alloc);
    }

    pub fn read_pmx_vertices(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        const vertex_count = @as(usize, @intCast(try buffer.read(i32)));
        self.vertices = try alloc.alloc(pmx_vertex, vertex_count);
        for (self.vertices) |*vertex| {
            vertex.position = try buffer.read([3]f32);
            vertex.normal = try buffer.read([3]f32);
            vertex.uv = try buffer.read([2]f32);
            for (0..self.vec4_count()) |i| {
                vertex.additional[i] = try buffer.read([4]f32);
            }
            vertex.bones = [4]u32{ 0, 0, 0, 0 };
            vertex.weights = [4]f32{ 0, 0, 0, 0 };

            // Read the bone and deformation data
            switch (try buffer.read(u8)) {
                0 => {
                    vertex.bones[0] = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    vertex.weights[0] = 1;
                    vertex.deform_type = .LinearBlend;
                },
                1, 3 => |val| {
                    vertex.bones[0] = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    vertex.bones[1] = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    vertex.weights[0] = try buffer.read(f32);
                    vertex.weights[1] = 1 - vertex.weights[0];
                    vertex.deform_type = .LinearBlend;
                    if (val == 3) {
                        _ = try buffer.read([3][3]f32);
                        vertex.deform_type = .SphericalBlend;
                    }
                },
                2, 4 => |val| {
                    vertex.bones[0] = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    vertex.bones[1] = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    vertex.bones[2] = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    vertex.bones[3] = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    vertex.weights[0] = try buffer.read(f32);
                    vertex.weights[0] = try buffer.read(f32);
                    vertex.weights[0] = try buffer.read(f32);
                    vertex.weights[0] = try buffer.read(f32);
                    vertex.deform_type = if (val == 2) .LinearBlend else .DualQuatBlend;
                },
                else => return pmx_err.MalformedVertexDeformType,
            }
            vertex.edge = try buffer.read(f32);
        }
    }

    pub fn read_pmx_indices(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        const index_count = @as(usize, @intCast(try buffer.read(i32)));
        self.indices = try alloc.alloc(u32, index_count);
        for (self.indices) |*index| {
            index.* = try self.read_index(buffer, .VERTEX_INDEX_SIZE);
        }
    }

    pub fn read_pmx_textures(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        const texture_count = @as(usize, @intCast(try buffer.read(i32)));
        self.textures = try alloc.alloc([]u8, texture_count);
        for (self.textures) |*texture| {
            texture.* = try self.read_text(buffer, alloc);
        }
    }

    pub fn read_pmx_materials(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        const material_count = @as(usize, @intCast(try buffer.read(i32)));
        self.materials = try alloc.alloc(pmx_material, material_count);
        for (self.materials) |*material| {
            material.name_local = try self.read_text(buffer, alloc);
            material.name_global = try self.read_text(buffer, alloc);
            material.diffuse = try buffer.read([4]f32);
            material.specular = try buffer.read([3]f32);
            material.spec = try buffer.read(f32);
            material.ambient = try buffer.read([3]f32);
            material.flags = try buffer.read(pmx_material.flags);
            material.edge_color = try buffer.read([4]f32);
            material.edge_scale = try buffer.read(f32);
            material.tex_index = try self.read_index(buffer, .TEXTURE_INDEX_SIZE);
            material.env_index = try self.read_index(buffer, .TEXTURE_INDEX_SIZE);
            material.env_blend = try buffer.read(pmx_material.environment_blend);
            material.toon_ext = try buffer.read(bool);
            if (material.toon_ext) {
                material.toon_ref = try self.read_index(buffer, .TEXTURE_INDEX_SIZE);
            } else {
                material.toon_ref = @as(u32, try buffer.read(u8));
            }
            material.meta_data = try self.read_text(buffer, alloc);
            material.indices = @bitCast(try buffer.read(i32));
        }
    }

    pub fn read_pmx_bones(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        const bone_count = @as(usize, @intCast(try buffer.read(i32)));
        self.bones = try alloc.alloc(pmx_bone, bone_count);
        for (self.bones) |*bone| {
            bone.name_local = try self.read_text(buffer, alloc);
            bone.name_global = try self.read_text(buffer, alloc);
            bone.position = try buffer.read([3]f32);
            bone.parent = try self.read_index(buffer, .BONE_INDEX_SIZE);
            bone.layer = try buffer.read(i32);
            bone.flags = try buffer.read(pmx_bone.flags);

            bone.tail = if (bone.flags.TailPositionIndexed) pmx_bone.tail{
                .bone = try self.read_index(buffer, .BONE_INDEX_SIZE),
            } else pmx_bone.tail{
                .position = try buffer.read([3]f32),
            };

            bone.inherit = if (bone.flags.InheritRotation or bone.flags.InheritTranslation) pmx_bone.inherit{
                .parent = try self.read_index(buffer, .BONE_INDEX_SIZE),
                .weight = try buffer.read(f32),
            } else null;

            bone.fixed_axis = if (bone.flags.FixedAxis) try buffer.read([3]f32) else null;

            bone.local_coords = if (bone.flags.LocalCoordinate) pmx_bone.local_coords{
                .x = try buffer.read([3]f32),
                .z = try buffer.read([3]f32),
            } else null;

            bone.external_parent = if (bone.flags.ExternalParentDeform)
                try self.read_index(buffer, .BONE_INDEX_SIZE)
            else
                null;

            if (bone.flags.InverseKinematics) {
                const target = try self.read_index(buffer, .BONE_INDEX_SIZE);
                const loop_count = try buffer.read(u32);
                const limit_radian = try buffer.read(f32);
                const link_count = @as(usize, @intCast(try buffer.read(i32)));
                const links = try alloc.alloc(pmx_bone.ik.link, link_count);
                for (links) |*link| {
                    link.bone = try self.read_index(buffer, .BONE_INDEX_SIZE);
                    link.limit = if (try buffer.read(bool)) pmx_bone.ik.angle_limit{
                        .max = try buffer.read([3]f32),
                        .min = try buffer.read([3]f32),
                    } else null;
                }
                bone.ik = pmx_bone.ik{
                    .target = target,
                    .loop_count = loop_count,
                    .limit_radian = limit_radian,
                    .links = links,
                };
            } else bone.ik = null;
        }
    }

    pub fn read_pmx_morphs(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        const morph_count = @as(usize, @intCast(try buffer.read(i32)));
        self.morphs = try alloc.alloc(pmx_morph, morph_count);
        for (self.morphs) |*morph| {
            morph.name_local = try self.read_text(buffer, alloc);
            morph.name_global = try self.read_text(buffer, alloc);
            morph.panel = try buffer.read(pmx_morph.panel_type);
            const morph_type = try buffer.read(pmx_morph.morph_type);
            const data_count = @as(usize, @intCast(try buffer.read(i32)));
            morph.data = try pmx_morph.morph_data.init(
                morph_type,
                data_count,
                alloc,
            );
            switch (morph.data) {
                .Group => |val| {
                    for (val) |*group| {
                        group.morph = try self.read_index(buffer, .MORPH_INDEX_SIZE);
                        group.weight = try buffer.read(f32);
                    }
                },
                .Vertex => |val| {
                    for (val) |*vertex| {
                        vertex.vertex = try self.read_index(buffer, .VERTEX_INDEX_SIZE);
                        vertex.translation = try buffer.read([3]f32);
                    }
                },
                .Bone => |val| {
                    for (val) |*bone| {
                        bone.bone = try self.read_index(buffer, .BONE_INDEX_SIZE);
                        bone.translation = try buffer.read([3]f32);
                        bone.rotation = try buffer.read([4]f32);
                    }
                },
                .UV => |val| {
                    for (val) |*uv| {
                        uv.vertex = try self.read_index(buffer, .VERTEX_INDEX_SIZE);
                        uv.data = try buffer.read([4]f32);
                    }
                },
                .UV1 => |val| {
                    for (val) |*uv| {
                        uv.vertex = try self.read_index(buffer, .VERTEX_INDEX_SIZE);
                        uv.data = try buffer.read([4]f32);
                    }
                },
                .UV2 => |val| {
                    for (val) |*uv| {
                        uv.vertex = try self.read_index(buffer, .VERTEX_INDEX_SIZE);
                        uv.data = try buffer.read([4]f32);
                    }
                },
                .UV3 => |val| {
                    for (val) |*uv| {
                        uv.vertex = try self.read_index(buffer, .VERTEX_INDEX_SIZE);
                        uv.data = try buffer.read([4]f32);
                    }
                },
                .UV4 => |val| {
                    for (val) |*uv| {
                        uv.vertex = try self.read_index(buffer, .VERTEX_INDEX_SIZE);
                        uv.data = try buffer.read([4]f32);
                    }
                },
                .Material => |val| {
                    for (val) |*material| {
                        material.material = try self.read_index(buffer, .MATERIAL_INDEX_SIZE);
                        material.unknown = try buffer.read(u8);
                        material.diffuse = try buffer.read([4]f32);
                        material.specular = try buffer.read([3]f32);
                        material.spec = try buffer.read(f32);
                        material.ambient = try buffer.read([3]f32);
                        material.edge_color = try buffer.read([4]f32);
                        material.edge_size = try buffer.read(f32);
                        material.texture_tint = try buffer.read([4]f32);
                        material.environment_tint = try buffer.read([4]f32);
                        material.toon_tint = try buffer.read([4]f32);
                    }
                },
                .Flip => |val| {
                    for (val) |*flip| {
                        flip.morph = try self.read_index(buffer, .MORPH_INDEX_SIZE);
                        flip.weight = try buffer.read(f32);
                    }
                },
                .Impulse => |val| {
                    for (val) |*impulse| {
                        impulse.rigid_body = try self.read_index(buffer, .RIGID_BODY_INDEX_SIZE);
                        impulse.local = try buffer.read(bool);
                        impulse.speed = try buffer.read([3]f32);
                        impulse.torque = try buffer.read([3]f32);
                    }
                },
            }
        }
    }

    pub fn read_pmx_displays(self: *pmx_data, buffer: *file_buffer, alloc: std.mem.Allocator) !void {
        const display_count = @as(usize, @intCast(try buffer.read(i32)));
        self.displays = try alloc.alloc(pmx_display, display_count);
        for (self.displays) |*display| {
            display.name_local = try self.read_text(buffer, alloc);
            display.name_global = try self.read_text(buffer, alloc);
            display.special = try buffer.read(bool);
            const frame_count = @as(usize, @intCast(try buffer.read(i32)));
            display.frames = try alloc.alloc(pmx_display.frame, frame_count);
            for (0..frame_count) |i| {
                display.frames[i] = switch (try buffer.read(pmx_display.frame_type)) {
                    .Bone => .{ .Bone = try self.read_index(buffer, .BONE_INDEX_SIZE) },
                    .Morph => .{ .Morph = try self.read_index(buffer, .MORPH_INDEX_SIZE) },
                };
            }
        }
    }

    // Call after init
    pub fn utf(self: pmx_data) pmx_utf {
        return @enumFromInt(self.header.globals[@intFromEnum(pmx_global.TEXT_ENCODE)]);
    }

    pub fn get_global(self: pmx_data, global: pmx_global) u8 {
        return self.header.globals[@intFromEnum(global)];
    }

    // Call after init
    pub fn vec4_count(self: pmx_data) u8 {
        return self.header.globals[@intFromEnum(pmx_global.ADDITIONAL_VEC4)];
    }

    // TODO: Handle cases where the returned value is -1 to show non-presence.
    pub fn read_index(self: pmx_data, buffer: *file_buffer, global: pmx_global) !u32 {
        return @as(u32, switch (self.get_global(global)) {
            1 => try buffer.read(u8),
            2 => try buffer.read(u16),
            4 => try buffer.read(u32),
            else => unreachable,
        });
    }

    pub fn print_header(self: pmx_data) void {
        std.debug.print(
            "{s}{d}\n",
            .{
                self.header.signature,
                self.header.version,
            },
        );
        for (0..self.header.globals.len) |i| {
            std.debug.print(
                "Globals[{d}]: {d}\n",
                .{ i, self.header.globals[i] },
            );
        }
        std.debug.print("Name local: {s}\n", .{self.header.name_local});
        std.debug.print("Name Global: {s}\n", .{self.header.name_global});
        std.debug.print("Comments local: {s}\n", .{self.header.comments_local});
        std.debug.print("Comments global: {s}\n", .{self.header.comments_global});
    }

    pub fn print_textures(self: pmx_data) void {
        for (self.textures, 0..) |texture, i| {
            std.debug.print(
                "Texture {d}: {s}\n",
                .{ i, texture },
            );
        }
    }

    pub fn print_materials(self: pmx_data) void {
        for (self.materials, 0..) |material, i| {
            std.debug.print(
                "Material {d}:\n" ++
                    "Name local: {s}\n" ++
                    "Name global: {s}\n",
                // "Diffuse  rgba:{any}\n" ++
                // "Specular rgb :{any}\n" ++
                // "Spec:        :{d}\n" ++
                // "Ambient  rgb :{any}\n" ++
                // "EdgeCol  rgba:{any}\n",
                .{
                    i,
                    material.name_local,
                    material.name_global,
                    // material.diffuse,
                    // material.specular,
                    // material.spec,
                    // material.ambient,
                    // material.edge_color,
                },
            );
        }
    }

    pub fn print_bones(self: pmx_data) void {
        for (self.bones, 0..) |bone, i| {
            std.debug.print(
                "Bone {d}:\n" ++
                    "Name Local: {s}\n" ++
                    "Name Global: {s}\n",
                .{
                    i,
                    bone.name_local,
                    bone.name_global,
                },
            );
        }
    }

    pub fn print_morphs(self: pmx_data) void {
        for (self.morphs, 0..) |morph, i| {
            std.debug.print(
                "Morph: {d}\n" ++
                    "Local: {s}\n" ++
                    "Global: {s}\n",
                .{
                    i,
                    morph.name_local,
                    morph.name_global,
                },
            );
        }
    }

    pub fn print_displays(self: pmx_data) void {
        for (self.displays, 0..) |display, i| {
            std.debug.print(
                "Display: {d}\n" ++
                    "Local: {s}\n" ++
                    "Global: {s}\n",
                .{
                    i,
                    display.name_local,
                    display.name_global,
                },
            );
        }
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const paths = [_][]const u8{"Path to pmx file"};

    for (paths) |path| {
        var buff = try file_buffer.init(
            path,
            alloc,
        );

        const data = pmx_data.init(&buff, alloc) catch |err| {
            std.debug.print("Error {}", .{err});
            continue;
        };

        data.print_header();
        data.print_textures();
        data.print_materials();
        data.print_bones();
        data.print_morphs();
        data.print_displays();
    }
}
