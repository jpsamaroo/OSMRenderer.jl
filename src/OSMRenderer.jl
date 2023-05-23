module OSMRenderer

import Dagger
import Dagger: Chunk
import MemPool
import MemPool: CPURAMDevice, SimpleRecencyAllocator, GenericFileDevice, FileRef

import OpenStreetMapX
import OpenStreetMapX: OSMData, LLA, Bounds, parseOSM

import SpatialIndexing as SI

import ImageMagick

import OSMToolset
import DataFrames: DataFrame

function writeOSM(path::AbstractString, xml)
    # FIXME: Write the data
end

OSMDevice = GenericFileDevice{writeOSM,parseOSM,false,false}
PNGDevice = GenericFileDevice{ImageMagick.save,ImageMagick.load,false,false}

struct MapContext
    active_center::Base.RefValue{LLA}
    view_size::Base.RefValue{Tuple{Float64,Float64}}

    xmls::Dict{Bounds{LLA}, Chunk}
    xml_downloads::Set{Bounds{LLA}}
    xml_tree::SI.RTree{Float64,2,SI.SpatialElem{Float64,2,Nothing,Bounds{LLA}}}
    xml_storage_device::SimpleRecencyAllocator
    xml_storage_leaf_device::OSMDevice

    tiled_xmls::Dict{Tuple{Int,Int,Int}, Dagger.EagerThunk}

    images::Dict{Tuple{Int,Int,Int}, Chunk}
    image_downloads::Set{Tuple{Int,Int,Int}}
    image_tree::Vector{SI.RTree{Int,2,SI.SpatialElem{Int,2,Nothing,Tuple{Int,Int}}}}
    image_textures::Dict{Tuple{Int,Int,Int},Ptr{Cvoid}}
    image_storage_device::SimpleRecencyAllocator
    image_storage_leaf_device::PNGDevice

    attract_config
    attract_metrics::Dict{Tuple{Int,Int,Int}, Dagger.EagerThunk}

    lock::Threads.ReentrantLock
end
function MapContext(center::LLA, view_size::Tuple{Float64,Float64},
                    xml_storage_device, xml_storage_leaf_device,
                    image_storage_device, image_storage_leaf_device)
    MapContext(Ref(center),
               Ref(view_size),

               # XML
               Dict{Bounds{LLA}, Chunk}(),
               Set{Bounds{LLA}}(),
               SI.RTree{Float64,2}(Nothing, Bounds{LLA}),
               xml_storage_device,
               xml_storage_leaf_device,

               # Tiled XML
               Dict{Tuple{Int,Int,Int}, Dagger.EagerThunk}(),

               # Image
               Dict{Tuple{Int,Int,Int}, Chunk}(),
               Set{Tuple{Int,Int,Int}}(),
               [SI.RTree{Int,2}(Nothing, Tuple{Int,Int}) for zoom in 1:19],
               Dict{Tuple{Int,Int,Int},Ptr{Cvoid}}(),
               image_storage_device,
               image_storage_leaf_device,

               # Attractiveness
               OSMToolset.load_attr_config(),
               Dict{Tuple{Int,Int,Int}, Dagger.EagerThunk}(),

               Threads.ReentrantLock())
end
function create_map_context(;
        center::LLA = LLA(36.162222, -86.774444), # Nashville, TN
        view_size::Tuple = (0.007, 0.003),
        cache_path=joinpath(first(DEPOT_PATH), "osmcache"),
        mem_limit=1024^3)
    mkpath(cache_path)

    xml_sfd = OSMDevice(cache_path)
    xml_sra = SimpleRecencyAllocator(mem_limit, xml_sfd, typemax(Int), :LRU; retain=true)

    image_sfd = PNGDevice(cache_path)
    image_sra = SimpleRecencyAllocator(mem_limit, image_sfd, typemax(Int), :LRU; retain=true)

    ctx = MapContext(center, view_size, xml_sra, xml_sfd, image_sra, image_sfd)

    load_cache!(ctx, cache_path)

    return ctx
end

Base.lock(f::Base.Callable, ctx::MapContext) = lock(f, ctx.lock)

function Base.show(io::IO, ctx::MapContext)
    lock(ctx) do
        print(io, "MapContext($(length(ctx.xmls)) cached XML tiles, $(length(ctx.images)) cached PNG tiles, center: $(ctx.active_center[]))")
    end
end

include("download.jl")
include("metrics.jl")
include("render.jl")

function shift_center!(ctx::MapContext, x::Real, y::Real)
    lock(ctx.lock) do
        loc = ctx.active_center[]
        view_size = ctx.view_size[]
        new_loc = LLA(loc.lat + y * view_size[2],
                      loc.lon + x * view_size[1])
        ctx.active_center[] = new_loc
    end
end

function scale_view!(ctx::MapContext, z::Real)
    lock(ctx.lock) do
        view_size = ctx.view_size[]
        new_view_size = view_size .* z
        ctx.view_size[] = new_view_size
    end
end

bounds_size(bounds::Bounds{LLA}) =
    (bounds.max_x - bounds.min_x,
     bounds.max_y - bounds.min_y)
loc_to_bounds(loc::LLA, lat_off, lon_off) =
    Bounds{LLA}(loc.lat - lat_off,
                loc.lat + lat_off,
                loc.lon - lon_off,
                loc.lon + lon_off)
loc_to_bounds(loc::LLA, view::Tuple{Float64,Float64}) =
    loc_to_bounds(loc, view[2], view[1])

function viewable_bounds(ctx::MapContext)
    center = ctx.active_center[]
    view_size = ctx.view_size[]

    view_lon_west = center.lon - view_size[1]
    view_lon_east = center.lon + view_size[1]
    view_lat_north = center.lat + view_size[2]
    view_lat_south = center.lat - view_size[2]

    return Bounds{LLA}(view_lat_south, view_lat_north, view_lon_west, view_lon_east)
end
function closest_tile(lon::Float64, lat::Float64)
    # FIXME
    TILE_STEP=16
    lon = round(lon * (1 / (TILE_STEP/2))) * (TILE_STEP/2)
    lat = round(lat * (1 / (TILE_STEP/2))) * (TILE_STEP/2)
    return (lon, lat)
end
closest_tile(loc::LLA) = closest_tile(loc.lon, loc.lat)

function lla_to_slippy(loc::LLA, zoom::Integer)
    n = 2 ^ zoom
    tile_x_raw = n * ((loc.lon + 180) / 360)
    lat_rad = deg2rad(loc.lat)
    tile_y_raw = n * (1 - (log(tan(lat_rad) + sec(lat_rad)) / π)) / 2
    return tile_x_raw, tile_y_raw
end
function slippy_to_lla(loc::Tuple, zoom::Integer)
    n = 2 ^ zoom
    tile_x, tile_y = loc
    lon = tile_x / n * 360.0 - 180.0
    lat = rad2deg(atan(sinh(π * (1 - 2 * tile_y / n))))
    return LLA(lat, lon)
end
function slippy_to_bounds(zoom, tile_x, tile_y)
    loc_real = slippy_to_lla((tile_x, tile_y) .+ (0.5, 0.5), zoom)
    tile_w, tile_h = tile_width(zoom), tile_height(zoom, loc_real.lat)
    return Bounds{LLA}(loc_real.lat - (tile_h/2), loc_real.lat + (tile_h/2),
                       loc_real.lon - (tile_w/2), loc_real.lon + (tile_w/2))
end

tile_width(zoom::Integer) = 360 / (2^zoom)
tile_height(zoom::Integer, lat::Float64) = tile_width(zoom) * cos(deg2rad(lat))

function best_zoom(bounds::Bounds{LLA})
    width = bounds.max_x - bounds.min_x
    for zoom in 1:18
        if tile_width(zoom) * 4 < width
            return zoom
        end
    end
    return 19
end

function maybe_fetch(x::Dagger.EagerThunk)
    if isready(x)
        return fetch(x)
    end
    return nothing
end
function maybe_fetch(x::Chunk)
    if MemPool.isinmemory(x.handle)
        # Should return quickly
        return fetch(x)
    end
    # Make sure we push it into memory for future fetches
    Threads.@spawn fetch(x)
    return nothing
end
maybe_fetch(::Nothing) = nothing
function viewable_maps(ctx::MapContext, kind::Symbol; zoom=nothing)
    lock(ctx) do
        view_bounds = viewable_bounds(ctx)

        if kind == :xml
            tiles = collect(SI.intersects_with(ctx.xml_tree,
                                               SI.Rect((view_bounds.min_x, view_bounds.min_y),
                                                       (view_bounds.max_x, view_bounds.max_y))))
            # FIXME: foreach(tile->download_xml!(ctx, tile.val), tiles)
            return [tile.val => maybe_fetch(get(ctx.xmls, tile.val, nothing))
                    for tile in tiles]
        elseif kind == :image
            if zoom === nothing
                zoom = best_zoom(view_bounds)
            end
            tile_northwest = floor.(Int, lla_to_slippy(LLA(view_bounds.max_y, view_bounds.min_x), zoom))
            tile_southeast = ceil.(Int, lla_to_slippy(LLA(view_bounds.min_y, view_bounds.max_x), zoom))
            tiles = collect(SI.intersects_with(ctx.image_tree[zoom],
                                               SI.Rect((tile_northwest...,),
                                                       (tile_southeast...,))))
            # Download any missing tiles
            for tile_x in tile_northwest[1]:tile_southeast[1], tile_y in tile_northwest[2]:tile_southeast[2]
                download_image!(ctx, (tile_x, tile_y), zoom)
            end
            return [(zoom, tile.val...) => maybe_fetch(get(ctx.images, (zoom, tile.val...), nothing))
                    for tile in tiles]
        elseif kind == :attractiveness
            if zoom === nothing
                zoom = best_zoom(view_bounds)
            end
            tile_northwest = floor.(Int, lla_to_slippy(LLA(view_bounds.max_y, view_bounds.min_x), zoom))
            tile_southeast = ceil.(Int, lla_to_slippy(LLA(view_bounds.min_y, view_bounds.max_x), zoom))
            tiles = collect(SI.intersects_with(ctx.image_tree[zoom],
                                               SI.Rect((tile_northwest...,),
                                                       (tile_southeast...,))))
            # Compute any missing tiles
            foreach(tile->compute_attractiveness!(ctx, (zoom, tile.val...)), tiles)
            return Dict((zoom, tile.val...) =>
                        maybe_fetch(get(ctx.attract_metrics, (zoom, tile.val...), nothing))
                        for tile in tiles)
        end
    end
end
function viewable_maps(ctx::MapContext; zoom=nothing)
    return (;xmls = viewable_maps(ctx, :xml; zoom),
             images = viewable_maps(ctx, :image; zoom))
end

end # module OSMRenderer
