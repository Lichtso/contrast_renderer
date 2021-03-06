const uint MAX_DASH_INTERVALS = 4;
const uint MAX_OPTION_GROUPS = 241;

struct DynamicStrokeDescriptor {
    float gap_start[MAX_DASH_INTERVALS / 4];
    float gap_end[MAX_DASH_INTERVALS / 4];
    uint caps;
    uint meta;
    float phase;
};

layout(set=1, binding=0) uniform StencilStrokeUniforms {
    DynamicStrokeDescriptor groups[MAX_OPTION_GROUPS];
};

bool cap(vec2 texcoord, uint cap_type) {
    switch(cap_type & 0xF) {
        case 0: // Square
            return texcoord.y > 0.5;
        case 1: // Round
            return dot(texcoord, texcoord) < 0.25;
        case 2: // Out
            return 0.5 - texcoord.y > abs(texcoord.x);
        case 3: // In
            return texcoord.y < abs(texcoord.x);
        case 4: // Right
            return 0.5 - texcoord.y > texcoord.x;
        case 5: // Left
            return texcoord.y - 0.5 < texcoord.x;
        default: // Butt
            return texcoord.y < 0.0;
    }
}

bool stroke_dashed(uint path_index, vec2 texcoord) {
    uint last_interval_index = groups[path_index].meta >> 3;
    float pattern_length = groups[path_index].gap_end[last_interval_index];
    uint interval_index = 0;
    float gap_start, gap_end;
    float position_in_pattern = mod(texcoord.y - groups[path_index].phase, pattern_length);
    while((gap_end = groups[path_index].gap_end[interval_index] - position_in_pattern) < 0.0 && interval_index < last_interval_index)
        ++ interval_index;
    gap_start = position_in_pattern - groups[path_index].gap_start[interval_index];
    if(gap_start > 0.0) {
        uint caps = groups[path_index].caps >> (interval_index * 8);
        bool start_cap = cap(vec2(texcoord.x, gap_start), caps >> 4);
        bool end_cap = cap(vec2(texcoord.x, gap_end), caps);
        return start_cap || end_cap;
    } else
        return true;
}