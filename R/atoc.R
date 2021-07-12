#' ATOC to GTFS
#'
#' Convert ATOC CIF files to GTFS
#'
#' @param path_in Character, path to ATOC file e.g."C:/input/ttis123.zip"
#' @param path_out Not used
#' @param name Not used
#' @param silent Logical, should progress messages be surpressed (default TRUE)
#' @param ncores Numeric, When parallel processing how many cores to use
#'   (default 1)
#' @param locations where to get tiploc locations (see details)
#' @param agency where to get agency.txt (see details)
#' @param shapes Logical, should shapes.txt be generated (default FALSE)
#' @param transfers Logical, should transfers.txt be generated (default TRUE)
#' @family main
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

atoc2gtfs <- function(path_in,
                      path_out = NULL,
                      name = NULL,
                      silent = TRUE,
                      ncores = 1,
                      locations = tiplocs,
                      agency = atoc_agency,
                      shapes = FALSE,
                      transfers = TRUE) {
  # Checkmates
  checkmate::assert_character(path_in, len = 1)
  checkmate::assert_file_exists(path_in)
  checkmate::assert_logical(silent)
  checkmate::assert_numeric(ncores, lower = 1)
  checkmate::assert_logical(shapes)

  # Warn depreciated features
  if(!is.null(path_out)){
    stop("writing gtfs files is now depreciated, this function now returns a gtfs object")
  }

  if(!is.null(name)){
    stop("writing gtfs files is now depreciated, this function now returns a gtfs object")
  }

  if (ncores == 1) {
    message(paste0(
      Sys.time(),
      " This will take some time, make sure you use 'ncores' to enable multi-core processing"
    ))
  }
  # Is input a zip or a folder
  if (grepl(".zip", path_in)) {
    # Unzip
    files <- utils::unzip(path_in, exdir = "tmp")
    cleanup <- TRUE
  } else {
    # folder
    cleanup <- FALSE
    files <- list.files(path_in, full.names = TRUE)
  }

  # Are all the files we would expect there?
  files.ext <- substr(files, nchar(files) - 3, nchar(files))
  # ".alf", ".dat", ".set", ".ztr", ".tsi" Not used
  files.ext.need <- c(".flf", ".mca", ".msn")

  if (!all(files.ext.need %in% files.ext)) {
    # Missing Some files
    files.ext.missing <- files.ext.need[!files.ext.need %in% files.ext]
    stop(paste0(
      "Missing files with the extension(s) ",
      paste(files.ext.missing, collapse = " ")
    ))
  }

  # Read In each File
  # alf <- importALF(files[grepl(".alf", files)])
  if(transfers){
    flf <- importFLF(files[grepl(".flf", files)])
  }

  if ("sf" %in% class(locations)) {
    mca <- importMCA(
      file = files[grepl(".mca", files)],
      silent = silent, ncores = 1
    )
  } else if (locations == "file") {
    mca <- importMCA(
      file = files[grepl(".mca", files)],
      silent = silent, ncores = 1, full_import = TRUE
    )
  } else {
    mca <- importMCA(
      file = files[grepl(".mca", files)],
      silent = silent, ncores = 1
    )
  }
  # ztr = importMCA(files[grepl(".ztr",files)], silent = silent)

  # Get the Station Locations
  if ("sf" %in% class(locations)) {
    # load("data/tiplocs.RData")
    stops <- cbind(locations, sf::st_coordinates(locations))
    stops <- as.data.frame(stops)
    stops <- stops[, c(
      "stop_id", "stop_code", "stop_name",
      "Y", "X", "valid"
    )]
    names(stops) <- c(
      "stop_id", "stop_code", "stop_name",
      "stop_lat", "stop_lon", "valid"
    )
    stops$stop_lat <- round(stops$stop_lat, 5)
    stops$stop_lon <- round(stops$stop_lon, 5)
    stops$valid <- NULL
  } else if (locations == "file") {
    msn <- importMSN(files[grepl(".msn", files)], silent = silent)
    station <- msn[[1]]
    TI <- mca[["TI"]]
    stops.list <- station2stops(station = station, TI = TI)
    stops <- stops.list[["stops"]]
    rm(msn,TI,stops.list)
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
  gc()
  # load("data/atoc_agency.RData")

  # TODO: check for stop_times that are not valid stops

  timetables$agency <- agency
  timetables$stops <- stops

  if (transfers) {
    if(!exists("station")){
      msn <- importMSN(files[grepl(".msn", files)], silent = silent)
      station <- msn[[1]]
    }
    timetables$transfers <- station2transfers(station = station, flf = flf)
  }


  # Build Shapes
  if (shapes) {
    message("Shapes are not yet supported")
  }

  return(timetables)

}
