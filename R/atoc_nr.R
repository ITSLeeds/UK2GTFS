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
#' @param working_timetable Logical, should WTT times be used instead of public times (default FALSE)
#' @param public_only Logical, only return calls/services that are for public passenger pickup/set down (default TRUE)
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
#' agency.txt file. Therefore this data is provided with the package. You can
#' also pass your own data frame of agency information.
#'
#'
#' @export

nr2gtfs <- function(path_in,
                      silent = TRUE,
                      ncores = 1,
                      locations = "tiplocs",
                      agency = "atoc_agency",
                      shapes = FALSE,
                      working_timetable = FALSE,
                      public_only = TRUE) {
  # checkmate
  checkmate::assert_character(path_in, len = 1)
  checkmate::assert_file_exists(path_in)
  checkmate::assert_logical(silent)
  checkmate::assert_numeric(ncores, lower = 1)
  checkmate::assert_logical(shapes)

  if (ncores == 1) {
    message(paste0(Sys.time(), " This will take some time, make sure you use 'ncores' to enable multi-core processing"))
  }

  agency = getCachedAgencyData( agency )

  stops = getCachedLocationData( locations )

  # Is input a zip or a folder
  if (!grepl(".gz", path_in)) {
    stop("path_in is not a .gz file")
  }

  # Read In each File
  mca <- importMCA(
      file = path_in,
      silent = silent,
      ncores = 1,
      working_timetable = working_timetable,
      public_only = public_only
  )


  # Construct the GTFS
  stop_times <- mca[["stop_times"]]
  schedule <- mca[["schedule"]]
  rm(mca)
  gc()


  stop_times <- stop_times[, c(
    "Arrival Time", "Departure Time", "Location", "stop_sequence", "Activity", "rowID", "schedule")]
  names(stop_times) <- c(
    "arrival_time", "departure_time", "stop_id", "stop_sequence", "Activity", "rowID", "schedule")

  # remove any unused stops
  stops <- stops[stops$stop_id %in% stop_times$stop_id, ]

  if ( nrow(stops)<=0 )
  {
    stop("Could not match any stops in input data to stop database.")
  }


  # Main Timetable Build
  timetables <- schedule2routes(
    stop_times = stop_times,
    stops = stops,
    schedule = schedule,
    silent = silent,
    ncores = ncores,
    public_only = public_only
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




getCachedAgencyData <- function(agency = "atoc_agency")
{
  if(inherits(agency,"character"))
  {
    if(agency == "atoc_agency")
    {
      load_data("atoc_agency")
      agency = atoc_agency
    }
    else #TODO test column names
    {
      checkmate::check_file_exists(agency)
      agency <- utils::read.csv(agency, stringsAsFactors = FALSE)
    }

    if ( !inherits(agency, "data.frame") || 0==nrow(agency) ){ stop("failed to load atoc_agency data.") }
  }

  return (agency)
}


getCachedLocationData <- function(locations = "tiplocs")
{
  if(inherits(locations,"character"))
  {
    if(locations == "tiplocs")
    {
      load_data("tiplocs")
      locations = tiplocs
    }
    else
    {
      checkmate::check_file_exists(locations)
      locations <- utils::read.csv(locations, stringsAsFactors = FALSE)
    }

    if ( !inherits(locations, "data.frame") || 0==nrow(locations) ){ stop("failed to tiploc data.") }
  }

  # Get the Station Locations
  if (inherits(locations, "sf"))
  {
    stops <- cbind(locations, sf::st_coordinates(locations))
    stops <- sf::st_drop_geometry(stops)
    stops <- as.data.table(stops)
    setnames(stops, old = c("Y", "X"), new = c("stop_lat", "stop_lon"))
  }
  else #TODO test column names
  {
    stops = locations
  }

  stops$stop_lat <- round(stops$stop_lat, 5)
  stops$stop_lon <- round(stops$stop_lon, 5)

  return (stops)
}
