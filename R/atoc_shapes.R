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
  routes_rail = routes[routes$route_type == 2, ]
  trips_rail = trips[trips$route_id %in% routes_rail$route_id,]
  stop_times_rail = stop_times[stop_times$trip_id %in% trips_rail$trip_id,]
  stops_rail = stops[stops$stop_id %in% stop_times_rail$stop_id,]

  # read in map
  rail = sf::st_read("./data/railway.geojson")
  rail = sf::st_transform(rail, 27700)
  rail = rail[rail$type == "rail",]

  #rail = sf::st_transform(rail, 27700)
  #rail$railway = "rail"
  #rail$id = 1:nrow(rail)
  # make graph
  # make a graph
  wts = c(1)
  names(wts) = as.character(unique(rail$type))
  graph = dodgr::weight_streetnet(rail, type_col = "type", wt_profile = wts, id_col = "id")

  # match stops to graph
  verts = dodgr::dodgr_vertices(graph)
  stops.bng = sf::st_as_sf(stops_rail, coords = c("stop_lon","stop_lat"), crs = 4326)
  stops.bng = sf::st_transform(stops.bng, 27700)
  stops.bng.coords = sf::st_coordinates(stops.bng)
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

  stops.bng$vert = near.index
  stops.bng$dist = round(near.dist,2)

  #stops$vert[stops$dist > 100] = NA
  foo = stops.bng[stops.bng$dist > 40,]
  #foo = sf::st_as_sf(foo, coords = c("stop_lon","stop_lat"), crs = 4326)
  foo = sf::st_sf(foo)
  qtm(foo[,])# +
    #qtm(rail)

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
  stops.match = as.data.frame(stops_rail[,c("stop_id","vert")])
  #stops.match$geometry = NULL
  names(stops.match) = c("stop_id","vert_from")
  pairs = dplyr::left_join(pairs, stops.match, by = c("stop_id_from" = "stop_id"))
  names(stops.match) = c("stop_id","vert_to")
  pairs = dplyr::left_join(pairs, stops.match, by = c("stop_id_to" = "stop_id"))

  # To save time assume A-B and B-A are the same
  od_id_order <- function(x, id1 = names(x)[1], id2 = names(x)[2]) {
    dplyr::transmute_(x,
                      stplanr.id1 = as.name(id1),
                      stplanr.id2 = as.name(id2),
                      stplanr.key = ~paste(pmin(stplanr.id1, stplanr.id2), pmax(stplanr.id1, stplanr.id2))
    )
  }


  pair_nodup = pairs
  key = od_id_order(pair_nodup, "vert_from", "vert_to")
  pair_nodup$key = key$stplanr.key
  pair_nodup = dplyr::group_by(pair_nodup, key)
  pair_nodup = dplyr::summarise(pair_nodup,
                                vert_from = vert_from[1],
                                vert_to = vert_to[1],
                                count = n())


  # Make Routes in Dodgr
  # hopefully tempory hack
  #dp.list = list()
  #origins = unique(pairs$vert_from)
  print(Sys.time())
  dp.list = dodgr::dodgr_paths(graph, from = as.character(pair_nodup$vert_from), to = as.character(pair_nodup$vert_to), pairwise = T, quiet = F)
  print(Sys.time())

  # dodgr_paths_byelement = function(i){
  #   from = origins[i]
  #   to = pairs$vert_to[pairs$vert_from == from]
  #   dp = dodgr::dodgr_paths(graph, from = as.character(from), to = as.character(to))
  #   return(dp)
  # }
  #
  # if(ncores == 1){
  #   dp.list = lapply(1:length(origins),dodgr_paths_byelement)
  # }else{
  #   CL <- parallel::makeCluster(ncores) #make clusert and set number of core
  #   parallel::clusterExport(cl = CL, varlist=c("origins", "pairs","graph"), envir = environment())
  #   parallel::clusterEvalQ(cl = CL, {library(dodgr)})
  #   dp.list = parallel::parLapply(cl = CL,1:length(origins),dodgr_paths_byelement)
  #   parallel::stopCluster(CL)
  # }


  # for(i in 1:length(origins)){
  #   #message(i)
  #   from = origins[i]
  #   to = pairs$vert_to[pairs$vert_from == from]
  #   dp = dodgr::dodgr_paths(graph, from = as.character(from), to = as.character(to))
  #   dp.list[[i]] = dp
  # }

  #dp.all = unlist(unlist(dp.list, recursive = FALSE), recursive = FALSE)
  #dp.all.names = names(dp.sf)
  dp.all = unlist(dp.list, recursive = FALSE)


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
  names(dp.sf) = 1:length(dp.sf)
  #dp.nulls = dp.sf[is.na(dp.sf)]
  dp.sf = dp.sf[!is.na(dp.sf)]
  dp.names = names(dp.sf)


  dp.sfdf = data.frame(id = dp.names, geometry = sf::st_as_sfc(dp.sf), stringsAsFactors = FALSE)
  dp.sfdf = sf::st_sf(dp.sfdf)
  sf::st_crs(dp.sfdf) = 27700

  #Clean Match names
  #clean_names = strsplit(dp.sfdf$id, "\\.")
  #clean_names = sapply(clean_names, "[[", 2)
  #clean_names = strsplit(clean_names, "-")
  #dp.sfdf$vert_from = as.integer(sapply(clean_names, "[[", 1))
  #dp.sfdf$vert_to = as.integer(sapply(clean_names, "[[", 2))
  pair_nodup$id = as.character(1:nrow(pair_nodup))


  pairs.sf = dplyr::left_join(pair_nodup, dp.sfdf, by = c("id"))
  #pairs.na = pairs.sf[is.na(pairs.sf$id),] #was 590 with bus and boat #520 without# down to 18 with split lines
  pairs.sf = sf::st_sf(pairs.sf, crs = 27700)
  #qtm(pairs.sf[1:100,])
  # problems causes with routes that only have one line between start and end (hence cluster of nearby problems)
  foo = pairs.sf[sf::st_is_empty(pairs.sf),]
  foo$geometry = NULL

  foo = dplyr::left_join(foo, stops_rail[,c("stop_id","vert")], by =c("vert_from" = "vert"))
  names(foo) = c("key","vert_from", "vert_to","count","id","stop_id_from")
  foo = dplyr::left_join(foo, stops_rail[,c("stop_id","vert")], by =c("vert_to" = "vert"))
  names(foo) = c("key","vert_from", "vert_to","count","id","stop_id_from","stop_id_to")
  bar = as.data.frame(table(c(foo$stop_id_from, foo$stop_id_to)))
  foobar = st_as_sf(stops[stops$stop_id %in% bar$Var1,], coords = c("stop_lon","stop_lat"), crs = 4326)
  qtm(foobar)
  # stops.problem = stops[stops$stop_id %in% c(pairs.na$stop_id_from, pairs.na$stop_id_to),]
  # tab = as.data.frame(table(c(pairs.na$stop_id_from,pairs.na$stop_id_to )))
  # tab2 = as.data.frame(table(c(pairs.na$vert_from,pairs.na$vert_to )))
  #
  # head(clean_names)

  pairs.sf.reverse = pairs.sf

  reverse_lines = function(geom){
    if(sf::st_is_empty(geom)){
      return(geom)
    }else{
      geom.matrix = as.matrix(geom)
      geom.reverse = geom.matrix[seq(nrow(geom.matrix),1),]
      geom.reverse = sf::st_linestring(geom.reverse)
      return(geom.reverse)
    }
  }

  #pairs.sf.reverse$geometry =
  lapply(pairs.sf.reverse$geometry[1:1000], reverse_lines)


}




