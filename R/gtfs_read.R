#' Read GTFS
#'
#' Read in a GTFS zip  file
#'
#' @param path character, path to GTFS zip folder
#' @param stringsAsFactors logical, should character be converted to factors, default FALSE
#' @export
#'

gtfs_read <- function(path, stringsAsFactors = FALSE){
  checkmate::assert_file_exists(path)
  checkmate::check_logical(stringsAsFactors, len = 1)

  tmp_folder <- file.path(tempdir(),"gtfsread")
  dir.create(tmp_folder)
  utils::unzip(path, exdir = tmp_folder)

  files <- list.files(tmp_folder, pattern = ".txt")

  gtfs <- list()
  message_log <- c("Unable to find optional files: ")

  if(checkmate::test_file_exists(file.path(tmp_folder,"agency.txt"))){
    gtfs$agency <- utils::read.csv(file.path(tmp_folder,"agency.txt"), stringsAsFactors = stringsAsFactors)
  } else {
    warning("Unable to find required file: agency.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stops.txt"))){
    gtfs$stops <- utils::read.csv(file.path(tmp_folder,"stops.txt"), stringsAsFactors = stringsAsFactors)
  } else {
    warning("Unable to find required file: stops.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"routes.txt"))){
    gtfs$routes <- utils::read.csv(file.path(tmp_folder,"routes.txt"), stringsAsFactors = stringsAsFactors)
    gtfs$routes$route_id <- as.character(gtfs$routes$route_id)
  } else {
    warning("Unable to find required file: routes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"trips.txt"))){
    gtfs$trips <- utils::read.csv(file.path(tmp_folder,"trips.txt"), stringsAsFactors = stringsAsFactors)
    gtfs$trips$route_id <- as.character(gtfs$trips$route_id)
    gtfs$trips$trip_id <- as.character(gtfs$trips$trip_id)
  } else {
    warning("Unable to find required file: trips.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stop_times.txt"))){
    gtfs$stop_times <- utils::read.csv(file.path(tmp_folder,"stop_times.txt"), stringsAsFactors = stringsAsFactors)
    gtfs$stop_times$trip_id <- as.character(gtfs$stop_times$trip_id)
  } else {
    warning("Unable to find required file: stop_times.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar.txt"))){
    gtfs$calendar <- utils::read.csv(file.path(tmp_folder,"calendar.txt"), stringsAsFactors = stringsAsFactors)
    gtfs$calendar$start_date <- lubridate::ymd(gtfs$calendar$start_date)
    gtfs$calendar$end_date <- lubridate::ymd(gtfs$calendar$end_date)
  } else {
    message("Unable to find conditionally required file: calendar.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar_dates.txt"))){
    gtfs$calendar_dates <- utils::read.csv(file.path(tmp_folder,"calendar_dates.txt"), stringsAsFactors = stringsAsFactors)
    gtfs$calendar_dates$date <- lubridate::ymd(gtfs$calendar_dates$date)
  } else {
    message("Unable to find conditionally required file: calendar_dates.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"fare_attributes.txt"))){
    gtfs$fare_attributes <- utils::read.csv(file.path(tmp_folder,"fare_attributes.txt"), stringsAsFactors = stringsAsFactors)
  } else {
    message_log <- c(message_log, "fare_attributes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"fare_rules.txt"))){
    gtfs$fare_rules <- utils::read.csv(file.path(tmp_folder,"fare_rules.txt"), stringsAsFactors = stringsAsFactors)
  } else {
    message_log <- c(message_log, "fare_rules.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"shapes.txt"))){
    gtfs$shapes <- utils::read.csv(file.path(tmp_folder,"shapes.txt"), stringsAsFactors = stringsAsFactors)
  } else {
    message_log <- c(message_log, "shapes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"transfers.txt"))){
    gtfs$transfers <- utils::read.csv(file.path(tmp_folder,"transfers.txt"), stringsAsFactors = stringsAsFactors)
  } else {
    message_log <- c(message_log, "transfers.txt")
  }
  unlink(tmp_folder, recursive = TRUE)

  if(length(message_log) > 0){
    message(paste(message_log, collapse = " "))
  }

  return(gtfs)
}
