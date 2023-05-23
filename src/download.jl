import Downloads

function download_xml!(ctx::MapContext, bounds::Bounds{LLA})
    should_download = false
    lock(ctx) do
        should_download = !haskey(ctx.xmls, bounds) && !(bounds in ctx.xml_downloads)
        if should_download
            push!(ctx.xml_downloads, bounds)
        end
    end
    if !should_download
        return
    end

    # Download the XML from Overpass
    t = Threads.@spawn begin
        (;min_x, max_x, min_y, max_y) = bounds
        url = "https://overpass-api.de/api/map?bbox=$min_x,$min_y,$max_x,$max_y"
        #url = "https://overpass.kumi.systems/api/map?bbox=$min_x,$min_y,$max_x,$max_y"
        xml = try
            @debug "Downloading XML for $bounds at $url"
            path = Downloads.download(url)
            @debug "Downloaded XML for $bounds"
            parseOSM(path)
        catch
            lock(ctx) do
                pop!(ctx.xml_downloads, bounds)
            end
            rethrow()
        end
        tile_name = "$(bounds.min_x)_$(bounds.max_x)_$(bounds.min_y)_$(bounds.max_y).osm"
        leaf_tag = MemPool.Tag(OSMDevice=>tile_name)
        chunk = Dagger.tochunk(xml; device=ctx.xml_storage_device, retain=true, leaf_tag)
        lock(ctx) do
            ctx.xmls[bounds] = chunk
            insert!(ctx.xml_tree, SI.Rect((bounds.min_x, bounds.min_y),
                                          (bounds.max_x, bounds.max_y)),
                    nothing, bounds)
            pop!(ctx.xml_downloads, bounds)
        end
    end
    errormonitor(t)

    return t
end

function download_image!(ctx::MapContext, tile_xy::Tuple{Int,Int}, zoom::Int)
    tile_x, tile_y = tile_xy
    tile_key = (zoom, tile_x, tile_y)
    should_download = false
    lock(ctx) do
        should_download = !haskey(ctx.images, tile_key) && !(tile_key in ctx.image_downloads)
        if should_download
            push!(ctx.image_downloads, tile_key)
        end
    end
    if !should_download
        return
    end

    # Download the PNG from OSM
    t = Threads.@spawn begin
        url = "https://tile.openstreetmap.org/$zoom/$tile_x/$tile_y.png"
        image = try
            @debug "Downloading PNG for $tile_key at $url"
            path = Downloads.download(url)
            @debug "Downloaded PNG for $tile_key"
            ImageMagick.load(path)
        catch
            lock(ctx) do
                pop!(ctx.image_downloads, tile_key)
            end
            rethrow()
        end
        tile_name = "$(zoom)_$(tile_x)_$(tile_y).png"
        leaf_tag = MemPool.Tag(PNGDevice=>tile_name)
        chunk = Dagger.tochunk(image; device=ctx.image_storage_device, retain=true, leaf_tag)
        lock(ctx) do
            ctx.images[tile_key] = chunk
            insert!(ctx.image_tree[zoom], SI.Rect((tile_x, tile_y),
                                                  (tile_x+1, tile_y+1)),
                    nothing, tile_xy)
            pop!(ctx.image_downloads, tile_key)
        end
    end
    errormonitor(t)

    return t
end
function download_image!(ctx::MapContext, loc::LLA, zoom::Int; kwargs...)
    # Calculate the Slippy tile xy
    tile_x_raw, tile_y_raw = lla_to_slippy(loc, zoom)
    tile_x = floor(Int, tile_x_raw)
    tile_y = floor(Int, tile_y_raw)
    download_image!(ctx, (tile_x, tile_y), zoom; kwargs...)
end

function load_cache!(ctx::MapContext, cache_path::String)
    # Register all cached XML and PNG tiles
    xml_rgx = r"([\-\.0-9]*)_([\-\.0-9]*)_([\-\.0-9]*)_([\-\.0-9]*)\.osm"
    image_rgx = r"([0-9]*)_([\-\.0-9]*)_([\-\.0-9]*)\.png"
    found_xmls = 0
    found_images = 0
    for tile_name in readdir(cache_path)
        tile_path = joinpath(cache_path, tile_name)

        # Try to parse XML
        tile_bounds = try
            tile_bounds_raw = something(match(xml_rgx, tile_name)).captures
            Bounds{LLA}(map(x->parse(Float64, x), tile_bounds_raw)...)
        catch
            nothing
        end
        if tile_bounds !== nothing
            leaf_tag = MemPool.Tag(OSMDevice=>tile_name)
            chunk = Dagger.tochunk(FileRef(tile_path); restore=true, retain=true,
                                   device=ctx.xml_storage_device,
                                   leaf_device=ctx.xml_storage_leaf_device, leaf_tag)
            ctx.xmls[tile_bounds] = chunk
            insert!(ctx.xml_tree, SI.Rect((tile_bounds.min_x, tile_bounds.min_y),
                                          (tile_bounds.max_x, tile_bounds.max_y)),
                    nothing, tile_bounds)
            found_xmls += 1
            continue
        end

        # Try to parse PNG
        tile_slippy = try
            tile_slippy_raw = something(match(image_rgx, tile_name)).captures
            (map(x->parse(Int, x), tile_slippy_raw)...,)
        catch
            nothing
        end
        if tile_slippy !== nothing
            leaf_tag = MemPool.Tag(PNGDevice=>tile_name)
            chunk = Dagger.tochunk(FileRef(tile_path); restore=true, retain=true,
                                   device=ctx.image_storage_device,
                                   leaf_device=ctx.image_storage_leaf_device, leaf_tag)
            ctx.images[tile_slippy] = chunk
            zoom, tile_x, tile_y = tile_slippy
            insert!(ctx.image_tree[zoom], SI.Rect((tile_x, tile_y),
                                                  (tile_x+1, tile_y+1)),
                    nothing, (tile_x, tile_y))
            found_images += 1
            continue
        end

        @error "Failed to load tile at $tile_name" exception=err
    end
    @debug "Located $found_xmls XMLs and $found_images PNGs"
end
