#' Make shapes.txt for train GTFS
#'
#' @details
#' Uses a internal map of the UK rail network to build shapes.txt
#' Done one agency at a time due to overheads.
#'
#' @param gtfs a gtfs object
#' @param ncores number of cores
#' @param agency which agency_id to build shapes for.
#' @noRd
#' @return
#' Returns a data.frame representing shapes.txt
#'
ATOC_shapes <- function(gtfs, ncores = 1, agency = gtfs$agency$agency_id[1]) {
  trips <- gtfs$trips
  routes <- gtfs$routes
  stops <- gtfs$stops
  stop_times <- gtfs$stop_times
  # rm(gtfs)
  routes <- routes[routes$agency_id == agency, ]


  # Make Graph of Railway
  # rail <- rail
  rail_heavy <- rail_heavy
  rail_heavy$type <- "rail"
  # rail_light <- rail[rail$type == "light_rail", ]
  wts <- c(1)
  names(wts) <- as.character(unique(rail_heavy$type))
  gc(verbose = TRUE)
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


  # match stops to graph
  verts <- dodgr::dodgr_vertices(graph)
  near <- RANN::nn2(data = verts[, c("x", "y")], query = stops_rail[, c("stop_lon", "stop_lat")], k = 1)
  stops_rail$vert <- verts$id[near$nn.idx]
  stops_rail$dist <- as.numeric(near$nn.dists)

  pairs <- dplyr::left_join(pairs[, c("stop_id_from", "stop_id_to")],
    stops_rail[, c("stop_id", "vert")],
    by = c("stop_id_from" = "stop_id")
  )
  names(pairs) <- c("stop_id_from", "stop_id_to", "vert_from")
  pairs <- dplyr::left_join(pairs,
    stops_rail[, c("stop_id", "vert")],
    by = c("stop_id_to" = "stop_id")
  )
  names(pairs) <- c("stop_id_from", "stop_id_to", "vert_from", "vert_to")

  # Route between pairs
  print(Sys.time())
  dp.list <- dodgr::dodgr_paths(graph,
    from = pairs$vert_from,
    to = pairs$vert_to,
    pairwise = TRUE, quiet = FALSE
  )
  print(Sys.time())

  # Convert to Linestrings
  dp.list <- unlist(dp.list, recursive = FALSE)

  path_to_sf <- function(dp, verts, simplify = TRUE) {
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
  # pairs$length <- as.numeric(st_length(pairs))
  # qtm(pairs[pairs$length > 600000,], lines.lwd = 2)

  # Make pairs in opposite direction
  pairs_opp <- pairs
  names(pairs_opp) <- c("stop_id_to", "stop_id_from", "vert_to", "vert_from", "geometry")

  invert_linestring <- function(x) {
    x <- sf::st_coordinates(x)
    x <- x[seq(nrow(x), 1), 1:2]
    x <- sf::st_linestring(x)
  }

  pairs_opp$geometry <- pbapply::pblapply(pairs_opp$geometry, invert_linestring)
  pairs_opp$geometry <- sf::st_as_sfc(pairs_opp$geometry, crs = 4326)
  pairs_opp <- sf::st_as_sf(pairs_opp)
  pairs_opp <- pairs_opp[, names(pairs)]
  pairs <- rbind(pairs, pairs_opp)
  rm(pairs_opp)

  # Match station pairs back to
  str <- stop_times_rail
  str$from <- c("foo", str$stop_id[seq(1, nrow(str) - 1)])
  str <- dplyr::left_join(str, pairs, by = c("from" = "stop_id_from", "stop_id" = "stop_id_to"))
  st_split <- split(str, str$trip_id)


  match_lines <- function(x) {
    x <- sf::st_as_sf(x, crs = 4326)
    x$length <- as.numeric(sf::st_length(x))
    x$shape_dist_traveled <- round(cumsum(x$length))

    if (!sf::st_is_empty(x$geometry[1])) {
      x$geometry[1] <- NULL
    }

    if (nrow(x) > 2) {
      shapes <- sf::st_line_merge(sf::st_union(x$geometry))
    } else if (nrow(x) == 2) {
      shapes <- x$geometry[2]
    } else {
      stop("For trip ", x$trip_id[1], " unknown number of geometries")
    }

    if (length(shapes) > 1) {
      stop("For trip ", x$trip_id[1], " unable to construct a single line")
    }
    shapes <- as.data.frame(sf::st_coordinates(shapes))
    names(shapes) <- c("shape_pt_lon", "shape_pt_lat", "L1")
    shapes <- shapes[, c("shape_pt_lon", "shape_pt_lat")]
    shapes$trip_id <- x$trip_id[1]
    shapes$shape_pt_sequence <- seq(1, nrow(shapes))

    x <- sf::st_drop_geometry(x)
    x <- x[, c(
      "trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type",
      "drop_off_type", "shape_dist_traveled"
    )]

    res <- list(shapes, x)
    names(res) <- c("shapes", "stop_times")
    return(res)
  }


  rm(graph, near, pairs, str)
  if (ncores == 1) {
    shape_res <- pbapply::pblapply(st_split, match_lines)
  } else {
    CL <- parallel::makeCluster(ncores) # make clusert and set number of core
    # parallel::clusterExport(cl = CL, varlist=c("origins", "pairs","graph"), envir = environment())
    # parallel::clusterEvalQ(cl = CL, {library(dodgr)})
    shape_res <- pbapply::pblapply(st_split, match_lines, cl = CL)
    parallel::stopCluster(CL)
    rm(CL)
  }

  stop_times_rail <- lapply(shape_res, `[[`, 2)
  shapes <- lapply(shape_res, `[[`, 1)
  rm(shape_res, st_split, trips_rail, stops_rail)
  gc()
  stop_times_rail <- dplyr::bind_rows(stop_times_rail)
  shapes <- dplyr::bind_rows(shapes, .id = "shape_id")
  return(shapes)
}
