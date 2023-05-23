function slice_xml!(ctx::MapContext, tile_key)
    lock(ctx.lock) do
        if !haskey(ctx.tiled_xmls, tile_key)
            bounds = slippy_to_bounds(tile_key...)
            tiles = collect(SI.intersects_with(ctx.xml_tree,
                                               SI.Rect((bounds.min_x, bounds.min_y),
                                                       (bounds.max_x, bounds.max_y))))
            xmls = map(tile->get(ctx.xmls, tile.val, nothing), tiles)
            if all(xml->!isnothing(xml), xmls)
                ctx.tiled_xmls[tile_key] = Dagger.@spawn slice_xml!(tile_key, xmls...)
            end
        end
    end
end
function slice_xml!(tile_key, xmls...)
    zoom, tile_x, tile_y = tile_key
    bounds = slippy_to_bounds(zoom, tile_x, tile_y)

    # Grab all nodes in bounds
    nodes = Dict{Int, LLA}()
    for xml in xmls
        for (node, lla) in xml.nodes
            if (bounds.min_y <= lla.lat <= bounds.max_y) && (bounds.min_x <= lla.lon <= bounds.max_x)
                nodes[node] = lla
            end
        end
    end

    # Grab all ways, relations, and features with an in-bounds node
    # Grab all features and way and relation tags FIXME: with in-bounds nodes
    ways = Vector{OpenStreetMapX.Way}()
    relations = Vector{OpenStreetMapX.Relation}()
    features = Dict{Int, Tuple{String,String}}()
    way_tags = Set{String}()
    relation_tags = Set{String}()
    for xml in xmls
        for way in xml.ways
            if any(node->haskey(nodes, node), way.nodes)
                push!(ways, deepcopy(way))
            end
        end
        for relation in xml.relations
            if haskey(nodes, relation.id)
                push!(relations, deepcopy(relation))
            end
        end
        for (id, tags) in xml.features
            if haskey(nodes, id)
                features[id] = tags
            end
        end
        for tag in xml.way_tags
            push!(way_tags, tag)
        end
        for tag in xml.relation_tags
            push!(relation_tags, tag)
        end
    end
    unique!(way->way.id, ways)
    unique!(relation->relation.id, relations)

    return OSMData(nodes, ways, relations, features, bounds,
                   way_tags, relation_tags)
end

function compute_attractiveness!(ctx::MapContext, tile_key)
    lock(ctx.lock) do
        if !haskey(ctx.attract_metrics, tile_key)
            zoom, tile_x, tile_y = tile_key
            if !haskey(ctx.tiled_xmls, tile_key)
                slice_xml!(ctx, tile_key)
            end
            xml = get(ctx.tiled_xmls, tile_key, nothing)
            if xml !== nothing
                config = ctx.attract_config
                ctx.attract_metrics[tile_key] = Dagger.@spawn compute_attractiveness!(xml, config)
            end
        end
    end
end
function compute_attractiveness!(xml::OSMData, attract_config)
    df = OSMToolset.find_poi(xml; attract_config)
    if !isempty(df)
        sindex = OSMToolset.SpatIndex(df)
        lat = xml.bounds.min_y + ((xml.bounds.max_y - xml.bounds.min_y) / 2)
        lon = xml.bounds.min_x + ((xml.bounds.max_x - xml.bounds.min_x) / 2)
        return OSMToolset.attractiveness(sindex, lat, lon)
    else
        return nothing
    end
end
