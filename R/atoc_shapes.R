#' Export trips as GTFS shapes.txt
#'
#' @details
#' Export trips as GTFS shapes.txt
#'
#' @param trips trips data.frame
#'
#'
trips2shapes = function(trips, routes, stops, stop_times, ncores = 1){
  # remove bus and boat routes
  routes = routes[routes$route_type == 2, ]
  trips = trips[trips$route_id %in% routes$route_id,]
  stop_times = stop_times[stop_times$trip_id %in% trips$trip_id,]
  stops_rail = stops[stops$stop_id %in% stop_times$stop_id,]

  # read in map
  rail = sf::st_read("./data/railway_dence.geojson")
  rail = st_transform(rail, 27700)


  #rail = sf::st_transform(rail, 27700)
  #rail$railway = "rail"
  #rail$id = 1:nrow(rail)
  # make graph
  # make a graph
  wts = c(1,1)
  names(wts) = as.character(unique(rail$type))
  graph = dodgr::weight_streetnet(rail, type_col = "type", wt_profile = wts, id_col = "id")

  # match stops to graph
  verts = dodgr::dodgr_vertices(graph)
  stops.bng = sf::st_as_sf(stops_rail, coords = c("stop_lon","stop_lat"), crs = 4326)
  stops.bng = st_transform(stops.bng, 27700)
  stops.bng.coords = st_coordinates(stops.bng)
  stops.bng$X = stops.bng.coords[,1]
  stops.bng$Y = stops.bng.coords[,2]
  stops.bng = as.data.frame(stops.bng)
  #stops.bng = sf::st_coordinates(stops)

  #near = RANN::nn2(data = verts[,c("x","y")], query = stops[,c("stop_lon","stop_lat")], k = 1)

  near = RANN::nn2(data = verts[,c("x","y")], query = stops.bng[,c("X","Y")], k = 1)
  near.dist = near[["nn.dists"]][,1]
  near.index = near[["nn.idx"]][,1]

  stops_rail$vert = near.index
  stops_rail$dist = round(near.dist,2)

  #stops$vert[stops$dist > 100] = NA
  foo = stops_rail[stops_rail$dist > 50,]
  #foo = sf::st_as_sf(foo, coords = c("stop_lon","stop_lat"), crs = 4326)
  foo$geometry = st_sfc(foo$geometry)
  qtm(foo[,]) +
    qtm(rail)

  # Work out all the route pairs
  pairs = stop_times[,c("trip_id","stop_id")]
  names(pairs) = c("trip_id_from","stop_id_from")
  pairs$stop_id_to = c(pairs$stop_id_from[2:length(pairs$stop_id_from)],NA)
  pairs$trip_id_to = c(pairs$trip_id_from[2:length(pairs$stop_id_from)],NA)
  pairs = pairs[pairs$trip_id_from == pairs$trip_id_to,]
  pairs = pairs[,c("stop_id_from", "stop_id_to")]
  pairs = unique(pairs)
  pairs = pairs[!is.na(pairs$stop_id_from),]
  pairs$stop_id_from = as.character(pairs$stop_id_from)
  #rm(stop_times)

  #Match Pairs with Verts IDs
  stops.match = as.data.frame(stops[,c("stop_id","vert")])
  stops.match$geometry = NULL
  names(stops.match) = c("stop_id","vert_from")
  pairs = dplyr::left_join(pairs, stops.match, by = c("stop_id_from" = "stop_id"))
  names(stops.match) = c("stop_id","vert_to")
  pairs = dplyr::left_join(pairs, stops.match, by = c("stop_id_to" = "stop_id"))

  # Make Routes in Dodgr
  # hopefully tempory hack
  #dp.list = list()
  origins = unique(pairs$vert_from)

  dodgr_paths_byelement = function(i){
    from = origins[i]
    to = pairs$vert_to[pairs$vert_from == from]
    dp = dodgr::dodgr_paths(graph, from = as.character(from), to = as.character(to))
    return(dp)
  }

  if(ncores == 1){
    dp.list = lapply(1:length(origins),dodgr_paths_byelement)
  }else{
    CL <- parallel::makeCluster(ncores) #make clusert and set number of core
    parallel::clusterExport(cl = CL, varlist=c("origins", "pairs","graph"), envir = environment())
    parallel::clusterEvalQ(cl = CL, {library(dodgr)})
    dp.list = parallel::parLapply(cl = CL,1:length(origins),dodgr_paths_byelement)
    parallel::stopCluster(CL)
  }


  # for(i in 1:length(origins)){
  #   #message(i)
  #   from = origins[i]
  #   to = pairs$vert_to[pairs$vert_from == from]
  #   dp = dodgr::dodgr_paths(graph, from = as.character(from), to = as.character(to))
  #   dp.list[[i]] = dp
  # }

  dp.all = unlist(unlist(dp.list, recursive = FALSE), recursive = FALSE)
  dp.all.names = names(dp.sf)



  path_to_sf <- function(dp, verts){
    # Check for emplyr paths
    if(length(dp) > 0){
      path = verts[match(dp, verts$id), ]
      path = matrix(c(path$x, path$y), ncol = 2)
      path = sf::st_linestring(path)
      return(path)
    }else{
      return(NA)
    }

  }

  dp.sf = lapply(dp.all, path_to_sf, verts = verts)
  dp.nulls = dp.sf[is.na(dp.sf)]
  dp.sf = dp.sf[!is.na(dp.sf)]
  dp.names = names(dp.sf)


  dp.sfdf = data.frame(id = dp.names, geometry = sf::st_as_sfc(dp.sf), stringsAsFactors = FALSE)
  dp.sfdf = sf::st_sf(dp.sfdf)
  sf::st_crs(dp.sfdf) = 27700

  #Clean Match names
  clean_names = strsplit(dp.sfdf$id, "\\.")
  clean_names = sapply(clean_names, "[[", 2)
  clean_names = strsplit(clean_names, "-")
  dp.sfdf$vert_from = as.integer(sapply(clean_names, "[[", 1))
  dp.sfdf$vert_to = as.integer(sapply(clean_names, "[[", 2))

  pairs.sf = dplyr::left_join(pairs, dp.sfdf, by = c("vert_from" = "vert_from", "vert_to" = "vert_to"))
  pairs.na = pairs.sf[is.na(pairs.sf$id),] #was 590 with bus and boat #520 without# down to 18 with split lines

  # problems causes with routes that only have one line between start and end (hence cluster of nearby problems)

  stops.problem = stops[stops$stop_id %in% c(pairs.na$stop_id_from, pairs.na$stop_id_to),]
  tab = as.data.frame(table(c(pairs.na$stop_id_from,pairs.na$stop_id_to )))
  tab2 = as.data.frame(table(c(pairs.na$vert_from,pairs.na$vert_to )))

  head(clean_names)


}




