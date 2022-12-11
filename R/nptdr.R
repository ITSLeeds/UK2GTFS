#' NPTDR to GTFS
#'
#' Convert NTPDR CIF files to GTFS
#'
#' @param path_in Character, path to ATOC file e.g."C:/input/ttis123.zip"
#' @param silent Logical, should progress messages be surpressed (default TRUE)
#' @param ncores Numeric, When parallel processing how many cores to use
#'   (default 1)
#' @param locations where to get tiploc locations (see details)
#' @param agency where to get agency.txt (see details)
#' @param shapes Logical, should shapes.txt be generated (default FALSE)
#' @param transfers Logical, should transfers.txt be generated (default TRUE)
#' @param missing_tiplocs Logical, if locations = tiplocs, then will check for
#'   any missing tiplocs agains the main file and add them.(default TRUE)
#' @family main
#'
#' @details Locations
#'
#'   The .msn file contains the physical locations of stations and other TIPLOC
#'   codes (e.g. junctions). However, the quality of the locations is often poor
#'   only accurate to about 1km and occasionally very wrong. Therefore, the
#'   UK2GTFS package contains an internal dataset of the TIPLOC locations with
#'   better location accuracy, which are used by default.
#'
#'   However you can also specify `locations = "file"` to use the TIPLOC
#'   locations in the ATOC data or provide an SF data frame of your own.
#'
#'   Or you can provide your own sf data frame of points in the same format as
#'   `tiplocs` or a path to a csv file formatted like a GTFS stops.txt
#'
#'   Agency
#'
#'   The ATOC files do not contain the necessary information to build the
#'   agency.txt file. Therfore this data is provided with the package. You can
#'   also pass your own data frame of agency information.
#'
#'
#' @export

nptdr2gtfs <- function(path_in,
                      silent = TRUE,
                      ncores = 1,
                      locations = tiplocs,
                      agency = atoc_agency,
                      shapes = FALSE,
                      transfers = TRUE,
                      missing_tiplocs = TRUE) {
  # Checkmates
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
  if (grepl(".zip", path_in)) {
    # Unzip
    files <- utils::unzip(path_in, exdir = "tmp")
    cleanup <- TRUE
  } else {
    # folder
    cleanup <- FALSE
    files <- list.files(path_in, full.names = TRUE)
  }

  folder <- list.dirs("tmp", recursive = FALSE)
  folder <- folder[!grepl("MACOSX",folder)]

  zips_txc <- list.files(file.path(folder,"Timetable Data"), pattern = ".zip",
                         full.names = TRUE, recursive = TRUE)

  dir.create("tmp/unzip_txc")
  for(i in seq_len(length(zips_txc))){
    message(i)
    utils::unzip(zips_txc[i], exdir = "tmp/unzip_txc")
  }


  files_txc <-list.files("tmp/unzip_txc", pattern = ".txc",
                           full.names = TRUE)

  cal = get_bank_holidays()
  naptan = get_naptan()

  gtfs <- transxchange2gtfs(files_txc,
                            silent = silent,
                            ncores = ncores,
                            cal = cal,
                            naptan = naptan,
                            extension = "txc")


  return(gtfs)

}
