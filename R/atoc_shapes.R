#' Make shapes.txt for train GTFS
#'
#' @details
#' Uses a internal map of the UK rail network to build shapes.txt
#'
#' @param gtfs a gtfs object
#' @export
#' @return
#' Returns a gtfs object
#'
ATOC_shapes <- function(gtfs) {
  trips <- gtfs$trips
  routes <- gtfs$routes
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times

  # Make Graph of Railway
  # rail <- rail
  rail_heavy <- rail_heavy
  rail_heavy$type <- "rail"
  # rail_light <- rail[rail$type == "light_rail", ]
  wts <- c(1)
  names(wts) <- as.character(unique(rail_heavy$type))
  #gc(verbose = TRUE)
  graph <- dodgr::weight_streetnet(rail_heavy, type_col = "type", wt_profile = wts, id_col = "id")


  # remove bus and boat routes
  routes_rail <- routes[routes$route_type == 2, ]
  trips_rail <- trips[trips$route_id %in% routes_rail$route_id, ]
  rm(routes_rail, routes, trips)
  stop_times_rail <- stop_times[stop_times$trip_id %in% trips_rail$trip_id, ]
  rm(stop_times)
  stops_rail <- stops[stops$stop_id %in% stop_times_rail$stop_id, ]
  rm(stops)

  # Cleaning check, should be in earlier
  stop_times_rail <- stop_times_rail[stop_times_rail$stop_id %in% stops_rail$stop_id, ]


  # Make a unique set of stop pairs
  pairs <- stop_times_rail[, c("trip_id", "stop_id")]
  names(pairs) <- c("trip_id_from", "stop_id_from")
  pairs$stop_id_to <- c(pairs$stop_id_from[2:length(pairs$stop_id_from)], NA)
  pairs$trip_id_to <- c(pairs$trip_id_from[2:length(pairs$stop_id_from)], NA)
  pairs <- pairs[pairs$trip_id_from == pairs$trip_id_to, ]
  pairs <- pairs[, c("stop_id_from", "stop_id_to")]
  pairs <- pairs[!is.na(pairs$stop_id_from), ]
  pairs <- unique(pairs)
  pairs$id <- od_id_szudzik(pairs$stop_id_from, pairs$stop_id_to)
  pairs <- pairs[!duplicated(pairs$id), ]

  # Add Coordinates
  stops_from <- stops_rail[match(pairs$stop_id_from, stops_rail$stop_id),]
  stops_to <- stops_rail[match(pairs$stop_id_to, stops_rail$stop_id),]
  stops_from <- stops_from[,c("stop_lat","stop_lon")]
  stops_to <- stops_to[,c("stop_lat","stop_lon")]
  names(stops_from) = c("from_lat","from_lon")
  names(stops_to) = c("to_lat","to_lon")

  # match stops to graph
  verts <- dodgr::dodgr_vertices(graph)

  # Route between pairs
  message(paste0(Sys.time()," Starting routing"))
  dp.list <- dodgr::dodgr_paths(graph,
    from = stops_from,
    to = stops_to,
    pairwise = TRUE, quiet = TRUE
  )
  message(paste0(Sys.time()," converting routes to GTFS format"))

  # Convert to Linestrings
  dp.list <- unlist(dp.list, recursive = FALSE)

  path_to_sf <- function(dp, verts, simplify = FALSE) {
    # Check for emplyr paths
    if (length(dp) > 0) {
      path <- verts[match(dp, verts$id), ]
      path <- matrix(c(path$x, path$y), ncol = 2)
      path <- sf::st_linestring(path)

      if (simplify) {
        path <- sf::st_as_sfc(list(path), crs = 4326)
        path <- sf::st_transform(path, 27700)
        path <- sf::st_simplify(path, 5, preserveTopology = TRUE)
        path <- sf::st_transform(path, 4326)
        path <- path[[1]]
      }
      return(path)
    } else {
      return(NA)
    }
  }

  dp.list <- pbapply::pblapply(dp.list, path_to_sf, verts = verts)
  dp.list <- unname(dp.list)
  pairs$geometry <- sf::st_sfc(dp.list, crs = 4326)
  rm(dp.list, verts)
  pairs <- sf::st_as_sf(pairs)

  # Make pairs in opposite direction
  pairs_opp <- pairs
  names(pairs_opp) <- c("stop_id_to", "stop_id_from", "id", "geometry")

  invert_linestring <- function(x) {
    x <- sf::st_coordinates(x)
    x <- x[seq(nrow(x), 1), 1:2]
    x <- sf::st_linestring(x)
  }

  message(paste0(Sys.time()," Invert routes"))
  pairs_opp$geometry <- pbapply::pblapply(pairs_opp$geometry, invert_linestring)
  pairs_opp$geometry <- sf::st_as_sfc(pairs_opp$geometry, crs = 4326)
  pairs_opp <- sf::st_as_sf(pairs_opp)
  pairs_opp <- pairs_opp[, names(pairs)]
  pairs <- rbind(pairs, pairs_opp)
  rm(pairs_opp)

  #Simplify the lines
  pairs <- sf::st_transform(pairs, 27700)
  pairs <- sf::st_simplify(pairs, dTolerance = 10)
  pairs <- sf::st_transform(pairs, 4326)

  # Identify unique trip chains
  str <- dplyr::group_by(stop_times_rail, trip_id)
  str <- dplyr::summarise(str, stop_chain = paste(stop_id, collapse = ","))
  str_uni <- str[!duplicated(str$stop_chain),]

  # Match station pairs back to
  st_split <- stop_times_rail[stop_times_rail$trip_id %in% str_uni$trip_id,]
  st_split$from <- c("foo", st_split$stop_id[seq(1, nrow(st_split) - 1)])
  st_split <- dplyr::left_join(st_split, pairs, by = c("from" = "stop_id_from", "stop_id" = "stop_id_to"))
  st_split <- dplyr::group_split(st_split, st_split$trip_id, .keep = FALSE)

  message(paste0(Sys.time()," final formatting"))
  rm(graph, pairs)
  shape_res <- pbapply::pblapply(st_split, match_lines)

  str5 <- lapply(shape_res, `[[`, 2)
  shapes <- lapply(shape_res, `[[`, 1)
  rm(shape_res, st_split, trips_rail, stops_rail)
  gc()
  str5 <- dplyr::bind_rows(str5)
  shapes <- dplyr::bind_rows(shapes)
  names(shapes)[3] = "shape_id"

  # Match up with original stop_times
  names(str_uni) <- c("shape_id","stop_chain")
  str <- dplyr::left_join(str, str_uni, by = "stop_chain")
  str$stop_chain <- NULL

  str5 <- str5[,c("trip_id","stop_id","stop_sequence","shape_dist_traveled")]
  str5 <- dplyr::left_join(str, str5, by = c("shape_id" = "trip_id"))

  stop_times = dplyr::left_join(gtfs$stop_times, str5,
                         by = c("trip_id","stop_id","stop_sequence"))
  gtfs$stop_times <- stop_times
  gtfs$trips <- dplyr::left_join(gtfs$trips, str, by = "trip_id")

  gtfs$shapes <- shapes

  return(gtfs)
}

# From stplanr
od_id_szudzik = function (x, y, ordermatters = FALSE)
{
  if (length(x) != length(y)) {
    stop("x and y are not of equal length")
  }
  if (is(x, "factor")) {
    x <- as.character(x)
  }
  if (is(y, "factor")) {
    y <- as.character(y)
  }
  lvls <- unique(c(x, y))
  x <- as.integer(factor(x, levels = lvls))
  y <- as.integer(factor(y, levels = lvls))
  if (ordermatters) {
    ismax <- x > y
    stplanr.key <- (ismax * 1) * (x^2 + x + y) + ((!ismax) *
                                                    1) * (y^2 + x)
  }
  else {
    a <- ifelse(x > y, y, x)
    b <- ifelse(x > y, x, y)
    stplanr.key <- b^2 + a
  }
  return(stplanr.key)
}

match_lines <- function(x) {
  x <- sf::st_as_sf(x, crs = 4326)
  x$length <- st_length_cheap(x)
  x$shape_dist_traveled <- round(cumsum(x$length))

  if (!sf::st_is_empty(x$geometry[1])) {
    x$geometry[1] <- NULL
  }

  if (nrow(x) > 2) {
    #shapes <- sf::st_line_merge(sf::st_union(x$geometry))
    shapes <- as.data.frame(sf::st_coordinates(x))
  } else if (nrow(x) == 2) {
    #shapes <- x$geometry[2]
    shapes <- as.data.frame(sf::st_coordinates(x))
  } else {
    stop("For trip ", x$trip_id[1], " unknown number of geometries")
  }

  # if (length(shapes) > 1) {
  #   stop("For trip ", x$trip_id[1], " unable to construct a single line")
  # }
  #shapes <- as.data.frame(sf::st_coordinates(shapes))
  shapes <- shapes[!duplicated(shapes[,1:2]),]

  names(shapes) <- c("shape_pt_lon", "shape_pt_lat", "L1")
  shapes <- shapes[, c("shape_pt_lon", "shape_pt_lat")]
  shapes$trip_id <- x$trip_id[1]
  shapes$shape_pt_sequence <- seq(1, nrow(shapes))
  shapes$dist <- geodist::geodist(shapes[,c("shape_pt_lon","shape_pt_lat")],
                                    sequential = TRUE, pad = TRUE)
  shapes$dist[1] <- 0
  shapes$shape_dist_traveled <- round(cumsum(shapes$dist),0)
  shapes$dist <- NULL

  x <- sf::st_drop_geometry(x)
  x <- x[, c(
    "trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type",
    "drop_off_type", "shape_dist_traveled"
  )]

  res <- list(shapes, x)
  names(res) <- c("shapes", "stop_times")
  return(res)
}

st_length_cheap <- function(x){
  coords <- sf::st_coordinates(x)
  coords <- coords[,c(1,2,ncol(coords))]
  coords <- split(coords[,c(1,2)], coords[,3])

  int_func <- function(y){
    y <- matrix(y, ncol = 2)
    colnames(y) <- c("lon","lat")
    dist <- geodist::geodist(y, sequential = TRUE)
    return(sum(dist))
  }

  lgths <- vapply(coords, int_func, FUN.VALUE = 1)
  if(length(lgths) != nrow(x)){
    lgths <- lgths[match(seq(1, nrow(x)),names(lgths))]
  }
  lgths <- unname(lgths)
  lgths[is.na(lgths)] <- 0
  return(lgths)
}


## Testing Scripts
# check that all stations are near the rail network
#gtfs <- gtfs_read("C:/Users/malco/OneDrive - University of Leeds/Data/UK2GTFS/GTFS/gtfs_20200502/atoc_ttis627.zip")

# stops <- gtfs$stops
# routes <- gtfs$routes
# trips <- gtfs$trips
# stop_times <- gtfs$stop_times
# routes <- routes[routes$route_type == 2, ]
# trips <- trips[trips$route_id %in% routes$route_id, ]
# stop_times <- stop_times[stop_times$trip_id %in% trips$trip_id, ]
# stops <- stops[stops$stop_id %in% unique(stop_times$stop_id),]
#
# track <- UK2GTFS::rail_heavy
# stops <- st_as_sf(stops, coords = c("stop_lon","stop_lat"), crs = 4326)
# track$type <- "rail"
# wts <- c(1)
# names(wts) <- as.character("rail")
# graph <- dodgr::weight_streetnet(track, type_col = "type", wt_profile = wts, id_col = "id")
# verts <- dodgr::dodgr_vertices(graph)
# stops <- cbind(st_drop_geometry(stops), st_coordinates(stops))
# near <- RANN::nn2(data = verts[, c("x", "y")], query = stops[, c("X", "Y")], k = 1)
# stops$vert <- verts$id[near$nn.idx]
# stops$dist <- as.numeric(near$nn.dists)
# stops$vert[stops$dist > 1e-7] <- NA
#
# graph <- dodgr::dodgr_contract_graph(graph, verts = unique(stops$vert))
#
# verts <- dodgr::dodgr_vertices(graph)
# near <- RANN::nn2(data = verts[, c("x", "y")], query = stops[, c("X", "Y")], k = 1)
# stops$vert <- verts$id[near$nn.idx]
# stops$dist <- as.numeric(near$nn.dists)
# stops$vert[stops$dist > 1e-7] <- NA
#
#
# # Make a unique set of stop pairs
# pairs <- stop_times[, c("trip_id", "stop_id")]
# names(pairs) <- c("trip_id_from", "stop_id_from")
# pairs$stop_id_to <- c(pairs$stop_id_from[2:length(pairs$stop_id_from)], NA)
# pairs$trip_id_to <- c(pairs$trip_id_from[2:length(pairs$stop_id_from)], NA)
# pairs <- pairs[pairs$trip_id_from == pairs$trip_id_to, ]
# pairs <- pairs[, c("stop_id_from", "stop_id_to")]
# pairs <- pairs[!is.na(pairs$stop_id_from), ]
# pairs <- unique(pairs)
# pairs$id <- od_id_szudzik(pairs$stop_id_from, pairs$stop_id_to)
# pairs <- pairs[!duplicated(pairs$id), ]
#
# # match stops to graph
# pairs <- dplyr::left_join(pairs[, c("stop_id_from", "stop_id_to")],
#                           stops[, c("stop_id", "vert")],
#                           by = c("stop_id_from" = "stop_id")
# )
# names(pairs) <- c("stop_id_from", "stop_id_to", "vert_from")
# pairs <- dplyr::left_join(pairs,
#                           stops[, c("stop_id", "vert")],
#                           by = c("stop_id_to" = "stop_id")
# )
# names(pairs) <- c("stop_id_from", "stop_id_to", "vert_from", "vert_to")
#
# pairs <- pairs[!is.na(pairs$vert_from),]
# pairs <- pairs[!is.na(pairs$vert_to),]
#
#
# # Route between pairs
# print(Sys.time())
# dp.list <- dodgr::dodgr_paths(graph,
#                               from = pairs$vert_from,
#                               to = pairs$vert_to,
#                               pairwise = TRUE, quiet = FALSE
# )
# print(Sys.time())
#
#
#
#
# track_points <- st_cast(track, "POINT")
# near <- st_nn(stops, track_points, returnDist = TRUE)
#
# stops$dist <- unlist(near$dist)
# stops$near <- unlist(near$nn)
# qtm(track) + qtm(stops[stops$dist > 10,])
#
# stops$geom2 <- track_points$geometry[stops$near]
# stops$geometry <- ifelse(stops$dist < 100, stops$geom2, stops$geometry)


