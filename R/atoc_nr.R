#' ATOC to GTFS (Network Rail Version)
#'
#' Convert ATOC CIF files from Network Rail to GTFS
#'
#' @param path_in Character, path to Network Rail ATOC file e.g."C:/input/toc-full.CIF.gz"
#' @param silent Logical, should progress messages be surpressed (default TRUE)
#' @param ncores Numeric, When parallel processing how many cores to use
#'   (default 1)
#' @param locations where to get tiploc locations (see details)
#' @param agency where to get agency.txt (see details)
#' @param shapes Logical, should shapes.txt be generated (default FALSE)
#' @family main
#' @return A gtfs list
#'
#' @details Locations
#'
#' The .msn file contains the physical locations of stations and other TIPLOC
#' codes (e.g. junctions). However, the quality of the locations is often poor
#' only accurate to about 1km and occasionally very wrong. Therefore, the
#' UK2GTFS package contains an internal dataset of the TIPLOC locations with
#' better location accuracy, which are used by default.
#'
#' However you can also specify `locations = "file"` to use the TIPLOC locations
#' in the ATOC data or provide an SF data frame of your own.
#'
#' Agency
#'
#' The ATOC files do not contain the necessary information to build the
#' agency.txt file. Therfore this data is provided with the package. You can
#' also pass your own data frame of agency information.
#'
#'
#' @export

nr2gtfs <- function(path_in,
                      silent = TRUE,
                      ncores = 1,
                      locations = tiplocs,
                      agency = atoc_agency,
                      shapes = FALSE) {
  # checkmate
  checkmate::assert_character(path_in, len = 1)
  checkmate::assert_file_exists(path_in)
  checkmate::assert_logical(silent)
  checkmate::assert_numeric(ncores, lower = 1)
  checkmate::assert_logical(shapes)

  if (ncores == 1) {
    message(paste0(
      Sys.time(),
      " This will take some time, make sure you use 'ncores' to enable multi-core processing"
    ))
  }
  # Is input a zip or a folder
  if (!grepl(".gz", path_in)) {
    stop("path_in is not a .gz file")
  }

  # Read In each File
  mca <- importMCA(
      file = path_in,
      silent = silent, ncores = 1
  )


  # Get the Station Locations
  if ("sf" %in% class(locations)) {
    # load("data/tiplocs.RData")
    stops <- cbind(locations, sf::st_coordinates(locations))
    stops <- as.data.frame(stops)
    stops <- stops[, c(
      "stop_id", "stop_code", "stop_name",
      "Y", "X"
    )]
    names(stops) <- c(
      "stop_id", "stop_code", "stop_name",
      "stop_lat", "stop_lon"
    )
    stops$stop_lat <- round(stops$stop_lat, 5)
    stops$stop_lon <- round(stops$stop_lon, 5)
  } else {
    stops <- utils::read.csv(locations, stringsAsFactors = FALSE)
  }

  # Construct the GTFS
  stop_times <- mca[["stop_times"]]
  schedule <- mca[["schedule"]]
  rm(mca)
  gc()
  # rm(alf, flf, mca, msn)

  stop_times <- stop_times[, c(
    "Arrival Time",
    "Departure Time",
    "Location", "stop_sequence",
    "Activity", "rowID", "schedule"
  )]
  names(stop_times) <- c(
    "arrival_time", "departure_time", "stop_id",
    "stop_sequence", "Activity", "rowID", "schedule"
  )

  # remove any unused stops
  stops <- stops[stops$stop_id %in% stop_times$stop_id, ]

  # Main Timetable Build
  timetables <- schedule2routes(
    stop_times = stop_times,
    schedule = schedule,
    silent = silent,
    ncores = ncores
  )
  rm(schedule)

  # TODO: check for stop_times that are not valid stops

  timetables$agency <- agency
  timetables$stops <- stops

  # Build Shapes
  if (shapes) {
    message("Shapes are not yet supported")
  }

  return(timetables)
}
