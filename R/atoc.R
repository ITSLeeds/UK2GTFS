#' ATOC to GTFS
#'
#' @details
#' Convert ATOC Files to GTFS
#'
#' @param path_in Path to ATOC File
#' @param path_out Path to where GTFS files should be saved
#' @param name name that should be given to the gtfs file, without the .zip extension
#' @param silent Logical, should progress be shown
#' @param ncores Numeric, When parallel processing how many cores to use
#' @param locations deafult tiplocs object inncluded with package, or "file" or path to stops.txt GTFS file
#' @param agency the GTFS agency file, default is taken from the package
#'
#' @details
#' Locations
#' The .msn file contains the physical locations of stations and other TIPLOC codes (e.g. junctions).
#' However, the quality of the locations is often poor only accurate to about 1km and occasionally very wrong.
#' Therefore, the UK2GTFS package contains an internal dataset of the TIPLOC locations with better location accuracy.
#'
#' Agency
#' The ATOC files do not contain the necessary information to build the agency.txt file. Therfore this data is provided with the package.
#'
#' @export

atoc2gtfs <- function(path_in, path_out, name = "gtfs", silent = TRUE, ncores = 1, locations = tiplocs, agency = atoc_agency) {
  if (ncores == 1) {
    message(paste0(Sys.time(), " This will take some time, make sure you use 'ncores' to enable multi-core processing"))
  }
  # Is input a zip or a folder
  if (grepl(".zip", path_in)) {
    # Unzip
    files <- utils::unzip(path_in, exdir = "tmp")
    cleanup <- TRUE
  } else {
    # folder
    cleanup <- FALSE
    files <- list.files(path_in, full.names = T)
  }

  # Are all the files we would expect there?
  files.ext <- substr(files, nchar(files) - 3, nchar(files))
  files.ext.need <- c(".alf", ".dat", ".flf", ".mca", ".msn", ".set", ".tsi", ".ztr")
  if (!all(files.ext.need %in% files.ext)) {
    # Missing Some files
    files.ext.missing <- files.ext.need[!files.ext.need %in% files.ext]
    warning(paste0("Missing files with the extension(s) ", paste(files.ext.missing, collapse = " ")))
    stop()
  }

  # Read In each File
  # alf <- importALF(files[grepl(".alf", files)])
  # flf <- importFLF(files[grepl(".flf", files)])
  if ("sf" %in% class(locations)) {
    mca <- importMCA(file = files[grepl(".mca", files)], silent = silent, ncores = 1)
  } else if (locations == "file") {
    mca <- importMCA(file = files[grepl(".mca", files)], silent = silent, ncores = 1, full_import = TRUE)
  } else {
    mca <- importMCA(file = files[grepl(".mca", files)], silent = silent, ncores = 1)
  }
  # ztr = importMCA(files[grepl(".ztr",files)], silent = silent)

  # Get the Station Locations
  if ("sf" %in% class(locations)) {
    # load("data/tiplocs.RData")
    stops <- cbind(locations, sf::st_coordinates(locations))
    stops <- as.data.frame(stops)
    stops <- stops[, c("stop_id", "stop_code", "stop_name", "Y", "X", "valid")]
    names(stops) <- c("stop_id", "stop_code", "stop_name", "stop_lat", "stop_lon", "valid")
    stops$stop_lat <- round(stops$stop_lat, 5)
    stops$stop_lon <- round(stops$stop_lon, 5)
    stops$valid <- NULL
  } else if (locations == "file") {
    msn <- importMSN(files[grepl(".msn", files)], silent = silent)
    station <- msn[[1]]
    TI <- mca[["TI"]]
    stops.list <- station2stops(station = station, TI = TI)
    stops <- stops.list[["stops"]]
  } else {
    stops <- utils::read.csv(locations, stringsAsFactors = F)
  }

  # Construct the GTFS
  stop_times <- mca[["stop_times"]]
  schedule <- mca[["schedule"]]
  rm(mca)
  gc()
  #rm(alf, flf, mca, msn)

  stop_times <- stop_times[, c("Scheduled Arrival Time", "Scheduled Departure Time", "Location", "stop_sequence", "Activity", "rowID", "schedule")]
  names(stop_times) <- c("arrival_time", "departure_time", "stop_id", "stop_sequence", "Activity", "rowID", "schedule")

  # remove any unused stops
  stops <- stops[stops$stop_id %in% stop_times$stop_id, ]

  # Main Timetable Build
  timetables <- schedule2routes(stop_times = stop_times, schedule = schedule, silent = silent, ncores = ncores)
  rm(schedule)
  gc()
  # load("data/atoc_agency.RData")

  #TODO: check for stop_times that are not valid stops

  timetables$agency <- agency
  timetables$stops <- stops

  # Build Shapes
  if(TRUE){
    return(timetables)
  } else {
    write_gtfs(timetables, folder = path_out, name = name)
  }
}
