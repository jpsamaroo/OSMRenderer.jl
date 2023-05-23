import CImGui
import CImGui: ImVec2
import CImGui: ImGuiGLFWBackend
if CImGui.opengl_major_version == 2
import CImGui: ImGuiOpenGL2Backend
const OpenGLBackend = ImGuiOpenGL2Backend
const CreateImageTexture = ImGuiOpenGL2Backend.CreateImageTexture
const UpdateImageTexture = ImGuiOpenGL2Backend.UpdateImageTexture
elseif CImGui.opengl_major_version >= 3
import CImGui: ImGuiOpenGLBackend
const OpenGLBackend = ImGuiOpenGLBackend
const CreateImageTexture = ImGuiOpenGLBackend.ImGui_ImplOpenGL3_CreateImageTexture
const UpdateImageTexture = ImGuiOpenGLBackend.ImGui_ImplOpenGL3_UpdateImageTexture
end
using .ImGuiGLFWBackend: LibGLFW
import CImGui: LibCImGui
using GLFW
using ModernGL

import CSyntax: @c, @cstatic

function color_code(features::Tuple{String,String})
    feat, subfeat = features
    if feat == "power"
        0xff33dddd
    elseif feat == "highway" ||
           subfeat == "neighbourhood" ||
           subfeat == "parking" ||
           subfeat == "car"
        0xffdd3333
    elseif feat == "railway"
        0xff3333dd
    elseif feat == "waterway"
        0xffddaa33
    elseif feat == "barrier"
        0xff3388dd
    else
        # Other
        0xff33dd33
    end
end
# No features
color_code(::Nothing) = 0x88aaaaaa

distance(pos1, pos2) =
    sqrt((pos1[1] - pos2[1])^2 + (pos1[2] - pos2[2])^2)
is_mouse_near_point(mpos, pos; radius=5) =
    distance(mpos, pos) <= radius
distance(ppos, lpos1, lpos2) =
    abs(((lpos2[1] - lpos1[1])*(lpos1[2] - ppos[2])) - ((lpos1[1] - ppos[1])*(lpos2[2] - lpos1[2]))) / distance(lpos1, lpos2)
is_mouse_near_line(mpos, lpos1, lpos2; radius=5) =
    distance(mpos, lpos1, lpos2) <= radius

function map_to_plot(ctx::MapContext, loc::LLA, plot_size::Tuple)
    bounds = viewable_bounds(ctx)
    x = ((loc.lon - bounds.min_x) / (bounds.max_x - bounds.min_x)) * plot_size[1]
    y = (1 - ((loc.lat - bounds.min_y) / (bounds.max_y - bounds.min_y))) * plot_size[2]
    return (x, y)
end
function plot_to_map(ctx::MapContext, xy::Tuple, plot_size::Tuple)
    bounds = viewable_bounds(ctx)
    x, y = xy
    lon = ((x / plot_size[1]) * (bounds.max_x - bounds.min_x)) + bounds.min_x
    lat = ((1 - (y / plot_size[2])) * (bounds.max_y - bounds.min_y)) + bounds.min_y
    return LLA(lat, lon)
end

function draw_controls(ctx::MapContext)
    radius = @cstatic radius=Cfloat(10) begin
        @c CImGui.SliderFloat("Mouse Radius", &radius, 0f0, 50f0)
        radius
    end
    attr_show = @cstatic attr_show=true begin
        @c CImGui.Checkbox("Show Attractiveness", &attr_show)
        attr_show
    end
    attr_alpha = @cstatic attr_alpha=Cint(100) begin
        @c CImGui.SliderInt("Attractiveness Alpha", &attr_alpha, 0f0, 255)
        attr_alpha
    end
    attr_granularity = @cstatic attr_granularity=Cint(16) begin
        @c CImGui.SliderInt("Attractiveness Granularity", &attr_granularity, 1, 20)
        attr_granularity
    end
    attr_scale = @cstatic attr_scale=Cint(1) begin
        @c CImGui.SliderInt("Attractiveness Scale", &attr_scale, 1, 20)
        attr_scale
    end
    # TODO: Memoize names
    attr_names = unique(map(v->v.class, values(ctx.attract_config[2])))
    local attr_name
    @cstatic attr_name_idx=Cint(0) begin
        @c CImGui.Combo("Attractiveness Metric", &attr_name_idx, attr_names, length(attr_names))
        attr_name = Symbol(attr_names[attr_name_idx+1])
    end
    return (;radius,
             attr_show,
             attr_alpha,
             attr_granularity,
             attr_scale,
             attr_name)
end
function draw_map(ctx::MapContext, control_state)
    (;xmls, images) = lock(ctx.lock) do
        return viewable_maps(ctx)
    end
    window_size = CImGui.GetWindowSize()
    width = window_size.x - 30
    height = window_size.y - 20
    plot_size = (width, height)
    cpos = CImGui.GetCursorScreenPos()
    cpos_xy = (cpos.x, cpos.y)
    mpos = unsafe_load(CImGui.GetIO().MousePos)
    mpos_xy = (mpos.x, mpos.y)
    draw_list = CImGui.GetWindowDrawList()
    wiki_tags = Set{String}()

    # Draw mouse radius circle
    radius = get(control_state, :radius, Cfloat(10))
    CImGui.AddCircle(draw_list, mpos, radius, 0xffffffff, 20, 1.0)

    for (tile_key, image) in images
        image === nothing && continue

        # Get/generate OpenGL texture from image
        tex_id = lock(ctx.lock) do
            get!(ctx.image_textures, tile_key) do
                width, height = size(image)
                tex_id = CreateImageTexture(width, height)
                # N.B. We inline transpose
                buf = zeros(UInt32, height, width)
                for j in 1:height
                    for i in 1:width
                        rgb = image[i,j]
                        color = (UInt32(reinterpret(UInt8, rgb.r)) << 24) |
                                (UInt32(reinterpret(UInt8, rgb.g)) << 16) |
                                (UInt32(reinterpret(UInt8, rgb.b)) << 8)  |
                                0xff
                        buf[j,i] = color
                    end
                end
                UpdateImageTexture(tex_id, buf, width, height)
                return tex_id
            end
        end

        # Render tile texture
        zoom, tile_x, tile_y = tile_key
        tile_bounds = slippy_to_bounds(zoom, tile_x, tile_y)
        loc_upperleft = LLA(tile_bounds.max_y, tile_bounds.min_x)
        loc_lowerright = LLA(tile_bounds.min_y, tile_bounds.max_x)
        image_xy_upperleft = map_to_plot(ctx, loc_upperleft, plot_size) .+ cpos_xy
        image_xy_lowerright = map_to_plot(ctx, loc_lowerright, plot_size) .+ cpos_xy
        CImGui.AddImage(draw_list, tex_id,
                        ImVec2(image_xy_upperleft...),
                        ImVec2(image_xy_lowerright...))
    end

    # Get attractiveness config
    attr_show = get(control_state, :attr_show, false)
    attr_alpha = UInt32(get(control_state, :attr_alpha, Cint(100))) << 24
    attr_gran = get(control_state, :attr_granularity, 16)
    attr_scale = get(control_state, :attr_scale, 1)
    attr_name = get(control_state, :attr_name, :tourism)

    attractiveness = viewable_maps(ctx, :attractiveness; zoom=attr_gran)
    for (tile_key, attract) in attractiveness
        # Overlay attractiveness data
        attr_show || continue
        attract === nothing && continue
        attract_value = get(attract, attr_name, nothing)
        attract_value === nothing && continue

        # Compute tile vertices
        tile_bounds = slippy_to_bounds(tile_key...)
        loc_upperleft = LLA(tile_bounds.max_y, tile_bounds.min_x)
        loc_lowerright = LLA(tile_bounds.min_y, tile_bounds.max_x)
        image_xy_upperleft = map_to_plot(ctx, loc_upperleft, plot_size) .+ cpos_xy
        image_xy_lowerright = map_to_plot(ctx, loc_lowerright, plot_size) .+ cpos_xy

        # Shade tile according to attractiveness
        if attract_value > 0
            color = attr_alpha | (UInt16(round(UInt8, clamp(attract_value * attr_scale, 0, 255))) << 8)
            CImGui.AddRectFilled(draw_list,
                                 ImVec2(image_xy_upperleft...),
                                 ImVec2(image_xy_lowerright...),
                                 color, 0.0, 0)
        end
        # Draw tile boundary
        CImGui.AddRect(draw_list,
                       ImVec2(image_xy_upperleft...),
                       ImVec2(image_xy_lowerright...),
                       0x880000ff, 0.0, 0, 3.0)
    end

    function draw_way(ctx, xml, way, pulse_color)
        last_lla = xml.nodes[first(way.nodes)]
        last_xy = map_to_plot(ctx, last_lla, plot_size) .+ cpos_xy
        features = get(xml.features, way.id, nothing)
        if features === nothing
            # Try to find a feature among our nodes
            for node in way.nodes
                features = get(xml.features, node, nothing)
                if features !== nothing
                    break
                end
            end
        end
        color = color_code(features)
        for node in way.nodes
            lla = xml.nodes[node]
            node_xy = map_to_plot(ctx, lla, plot_size) .+ cpos_xy
            # FIXME: if is_mouse_near_line(mpos_xy, node_xy, last_xy; radius)
            if is_mouse_near_point(mpos_xy, node_xy; radius)
                color = 0xff000000 | pulse_color
                #=
                if haskey(way.tags, "brand:wikipedia")
                    push!(wiki_tags, way.tags["brand:wikipedia"])
                end
                =#
            end
            CImGui.AddLine(draw_list, ImVec2(last_xy...), ImVec2(node_xy...), color, 1)
            last_xy = node_xy
        end
    end
    function draw_node(ctx, xml, node, loc::LLA)
        node_xy = map_to_plot(ctx, loc, plot_size) .+ cpos_xy
        feature = get(xml.features, node, nothing)
        color = color_code(feature)
        CImGui.AddCircleFilled(draw_list, ImVec2(node_xy...), 2, color, 20)
    end

    #= Overlay XML data
    for (_, xml) in xmls
        xml === nothing && continue

        # Draw all ways
        way_nodes = Set{Int}()
        pulse_time = @cstatic pulse_time=Cfloat(0) pulse_time += 0.5
        pulse_color = round(UInt32, clamp(sin(pulse_time) / 2 + 0.5, 0.3, 1.0) * 0xff)
        for way in xml.ways
            draw_way(ctx, xml, way, pulse_color)
            foreach(node->push!(way_nodes, node), way.nodes)
        end

        # Draw all nodes
        for (node, lla) in xml.nodes
            if node in way_nodes
                # Skip nodes that are already part of a way
                continue
            end
            draw_node(ctx, xml, node, lla)
        end
    end
    =#

    #= Draw tooltip
    if length(wiki_tags) > 0
        CImGui.BeginTooltip()
        for tag in sort(collect(wiki_tags))
            CImGui.Text(tag)
        end
        CImGui.EndTooltip()
    end
    =#

    # Handle mouse drag
    io = CImGui.GetIO()
    if CImGui.c_get(io.MouseDownDuration, 0) >= 0
        mouse_offset_x = -1 * unsafe_load(io.MouseDelta).x
        mouse_offset_y = -1 * unsafe_load(io.MouseDelta).y
        if mouse_offset_x != 0.0 || mouse_offset_y != 0.0
            center = ctx.active_center[]
            new_center = plot_to_map(ctx,
                                     (mouse_offset_x + (plot_size[1]/2),
                                      mouse_offset_y + (plot_size[2]/2)),
                                     plot_size)
            ctx.active_center[] = new_center
            @debug "Dragged map to $new_center ($mouse_offset_x, $mouse_offset_y)"
        end
    end

    # Handle keyboard input
    if CImGui.IsKeyPressed(Int('Q'))
        @debug "Exiting"
        exit()
    end
    W_key, A_key, S_key, D_key = Int.(('W', 'A', 'S', 'D'))
    shift_amount = 1.0
    if CImGui.IsKeyPressed(W_key)
        shift_center!(ctx, 0.0, shift_amount)
        @debug "Centered map on $(ctx.active_center[])"
    elseif CImGui.IsKeyPressed(A_key)
        shift_center!(ctx, -shift_amount, 0.0)
        @debug "Centered map on $(ctx.active_center[])"
    elseif CImGui.IsKeyPressed(S_key)
        shift_center!(ctx, 0.0, -shift_amount)
        @debug "Centered map on $(ctx.active_center[])"
    elseif CImGui.IsKeyPressed(D_key)
        shift_center!(ctx, shift_amount, 0.0)
        @debug "Centered map on $(ctx.active_center[])"
    end
    # TODO: LRUD keys

    Minus_key, Equal_key = Int.(('-', '='))
    scale_amount = 2.0
    if CImGui.IsKeyPressed(Minus_key)
        scale_view!(ctx, scale_amount)
        @debug "Zoomed map to $(ctx.view_size[])"
    elseif CImGui.IsKeyPressed(Equal_key)
        scale_view!(ctx, 1/scale_amount)
        @debug "Zoomed map to $(ctx.view_size[])"
    end

    CImGui.Dummy(ImVec2(width, height))
end

function _run_gui(map_ctx::MapContext)
    CLEAR_COLOR = Cfloat[0.45, 0.55, 0.60, 1.00]
    glsl_version = 130

    # setup GLFW error callback
    #? error_callback(err::GLFW.GLFWError) = @error "GLFW ERROR: code $(err.code) msg: $(err.description)"
    #? GLFW.SetErrorCallback(error_callback)

    # create window
    window = GLFW.Window(;name="OSMRenderer", windowhints=[], contexthints=[
        (GLFW.CONTEXT_VERSION_MAJOR, 2),
        (GLFW.CONTEXT_VERSION_MINOR, 1),
    ])
    GLFW.MakeContextCurrent(window)
    GLFW.SwapInterval(1)  # enable vsync

    # setup Dear ImGui context
    ctx = CImGui.CreateContext()

    # setup Dear ImGui style
    CImGui.StyleColorsDark()

    # load Fonts
    fonts = unsafe_load(CImGui.GetIO().Fonts)
    CImGui.AddFontDefault(fonts)

    # setup Platform/Renderer bindings
    glfw_ctx = ImGuiGLFWBackend.create_context(window.handle, install_callbacks = true)
    ImGuiGLFWBackend.init(glfw_ctx)
    opengl_ctx = OpenGLBackend.create_context(glsl_version)
    OpenGLBackend.init(opengl_ctx)

    # Control state
    control_state = NamedTuple()

    try
        while GLFW.WindowShouldClose(window) == 0
            GLFW.PollEvents()

            OpenGLBackend.new_frame(opengl_ctx) #ImGui_ImplOpenGL3_NewFrame()
            ImGuiGLFWBackend.new_frame(glfw_ctx) #ImGui_ImplGlfw_NewFrame()
            CImGui.NewFrame()

            # Draw map tiles
            CImGui.SetNextWindowPos((0, 0), CImGui.ImGuiCond_FirstUseEver)
            (;width, height) = GLFW.GetWindowSize(window)
            CImGui.SetNextWindowSize((width, height))
            window_flags = LibCImGui.ImGuiWindowFlags(0)
            window_flags |= LibCImGui.ImGuiWindowFlags_NoTitleBar
            window_flags |= LibCImGui.ImGuiWindowFlags_NoMove
            window_flags |= LibCImGui.ImGuiWindowFlags_NoResize
            window_flags |= LibCImGui.ImGuiWindowFlags_NoCollapse
            window_flags |= LibCImGui.ImGuiWindowFlags_NoScrollbar
            window_flags |= LibCImGui.ImGuiWindowFlags_NoBringToFrontOnFocus
            if CImGui.Begin("OSM Map", C_NULL, window_flags)
                draw_map(map_ctx, control_state)
                CImGui.End()
            end

            # Draw map controls
            CImGui.SetNextWindowPos((0, 0))
            CImGui.SetNextWindowBgAlpha(0.3)
            window_flags = LibCImGui.ImGuiWindowFlags(0)
            window_flags |= LibCImGui.ImGuiWindowFlags_NoMove
            if CImGui.Begin("Map Controls", C_NULL, window_flags)
                control_state = draw_controls(map_ctx)
                CImGui.End()
            end

            CImGui.Render()
            GLFW.MakeContextCurrent(window)

            display_w, display_h = GLFW.GetFramebufferSize(window)

            glViewport(0, 0, display_w, display_h)
            glClearColor(CLEAR_COLOR...)
            glClear(GL_COLOR_BUFFER_BIT)
            OpenGLBackend.render(opengl_ctx) #ImGui_ImplOpenGL3_RenderDrawData(CImGui.GetDrawData())

            GLFW.MakeContextCurrent(window)
            GLFW.SwapBuffers(window)

            # Let background tasks run
            sleep(1/10)
        end
    catch e
        @error "Error in renderloop!" exception=e
        Base.show_backtrace(stderr, catch_backtrace())
    finally
        OpenGLBackend.shutdown(opengl_ctx) #ImGui_ImplOpenGL3_Shutdown()
        ImGuiGLFWBackend.shutdown(glfw_ctx) #ImGui_ImplGlfw_Shutdown()
        CImGui.DestroyContext(ctx)
        GLFW.DestroyWindow(window)
    end
end # function run_gui
function run_gui(ctx::MapContext)
    wait(Threads.@spawn :interactive _run_gui(ctx))
end
run_gui(;kwargs...) = run_gui(create_map_context(; kwargs...))
