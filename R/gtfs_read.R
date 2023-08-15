#' Read GTFS
#'
#' Read in a GTFS zip  file
#'
#' @param path character, path to GTFS zip folder
#' @export
#'

gtfs_read <- function(path){
  checkmate::assert_file_exists(path)

  tmp_folder <- file.path(tempdir(),"gtfsread")
  dir.create(tmp_folder)
  utils::unzip(path, exdir = tmp_folder)

  files <- list.files(tmp_folder, pattern = ".txt")

  gtfs <- list()
  message_log <- c("Unable to find optional files: ")

  if(checkmate::test_file_exists(file.path(tmp_folder,"agency.txt"))){
    gtfs$agency <- readr::read_csv(file.path(tmp_folder,"agency.txt"),
                                   col_types = readr::cols(agency_id = readr::col_character(),
                                                           agency_noc = readr::col_character()),
                                   show_col_types = FALSE,
                                   lazy = FALSE)
  } else {
    warning("Unable to find required file: agency.txt")
  }
  if(checkmate::test_file_exists(file.path(tmp_folder,"stops.txt"))){
    gtfs$stops <- readr::read_csv(file.path(tmp_folder,"stops.txt"),
                                  col_types = readr::cols(stop_id = readr::col_character(),
                                                          stop_code = readr::col_character(),
                                                          stop_name = readr::col_character(),
                                                          stop_lat = readr::col_number(),
                                                          stop_lon = readr::col_number(),
                                                          wheelchair_boarding = readr::col_logical(),
                                                          location_type = readr::col_integer(),
                                                          parent_station = readr::col_character(),
                                                          platform_code = readr::col_character()),


                                  lazy = FALSE, show_col_types = FALSE)
  } else {
    warning("Unable to find required file: stops.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"routes.txt"))){
    gtfs$routes <- readr::read_csv(file.path(tmp_folder,"routes.txt"),
                                   col_types = readr::cols(route_id = readr::col_character(),
                                                           agency_id = readr::col_character(),
                                                           route_short_name = readr::col_character(),
                                                           route_long_name = readr::col_character(),
                                                           route_type = readr::col_integer()),
                                   show_col_types = FALSE,
                                   lazy = FALSE)
  } else {
    warning("Unable to find required file: routes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"trips.txt"))){
    gtfs$trips <- readr::read_csv(file.path(tmp_folder,"trips.txt"),
                                  col_types = readr::cols(trip_id = readr::col_character(),
                                                          route_id = readr::col_character(),
                                                          service_id = readr::col_character(),
                                                          block_id = readr::col_character(),
                                                          shape_id = readr::col_character(),
                                                          wheelchair_accessible = readr::col_logical()
                                                          ),
                                  show_col_types = FALSE,
                                  lazy = FALSE)
  } else {
    warning("Unable to find required file: trips.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stop_times.txt"))){
    gtfs$stop_times <- readr::read_csv(file.path(tmp_folder,"stop_times.txt"),
                                       col_types = readr::cols(trip_id = readr::col_character(),
                                                               stop_id = readr::col_character(),
                                                               stop_sequence = readr::col_integer(),
                                                               departure_time = readr::col_character(),
                                                               arrival_time = readr::col_character(),
                                                               shape_dist_traveled = readr::col_number(),
                                                               timepoint = readr::col_logical(),
                                                               pickup_type = readr::col_integer(),
                                                               drop_off_type = readr::col_integer()),
                                       show_col_types = FALSE,
                                       lazy = FALSE)
    gtfs$stop_times$arrival_time <- lubridate::hms(gtfs$stop_times$arrival_time)
    gtfs$stop_times$departure_time <- lubridate::hms(gtfs$stop_times$departure_time)

  } else {
    warning("Unable to find required file: stop_times.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar.txt"))){
    gtfs$calendar <- readr::read_csv(file.path(tmp_folder,"calendar.txt"),
                                     col_types = readr::cols(service_id = readr::col_character(),
                                                             monday = readr::col_logical(),
                                                             tuesday = readr::col_logical(),
                                                             wednesday = readr::col_logical(),
                                                             thursday = readr::col_logical(),
                                                             friday = readr::col_logical(),
                                                             saturday = readr::col_logical(),
                                                             sunday = readr::col_logical(),
                                                             start_date = readr::col_date(format = "%Y%m%d"),
                                                             end_date = readr::col_date(format = "%Y%m%d")),
                                     show_col_types = FALSE,
                                     lazy = FALSE)

  } else {
    message("Unable to find conditionally required file: calendar.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar_dates.txt"))){
    gtfs$calendar_dates <- readr::read_csv(file.path(tmp_folder,"calendar_dates.txt"),
                                           col_types = readr::cols(service_id = readr::col_character(),
                                                                   date = readr::col_date(format = "%Y%m%d"),
                                                                   exception_type = readr::col_integer()),
                                           show_col_types = FALSE,
                                           lazy = FALSE)
  } else {
    message("Unable to find conditionally required file: calendar_dates.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"fare_attributes.txt"))){
    gtfs$fare_attributes <- readr::read_csv(file.path(tmp_folder,"fare_attributes.txt"),
                                            show_col_types = FALSE,
                                            lazy = FALSE)
  } else {
    message_log <- c(message_log, "fare_attributes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"fare_rules.txt"))){
    gtfs$fare_rules <- readr::read_csv(file.path(tmp_folder,"fare_rules.txt"),
                                       show_col_types = FALSE,
                                       lazy = FALSE)
  } else {
    message_log <- c(message_log, "fare_rules.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"shapes.txt"))){
    gtfs$shapes <- readr::read_csv(file.path(tmp_folder,"shapes.txt"),
                                   col_types = readr::cols(shape_id = readr::col_character(),
                                                           shape_pt_lat = readr::col_number(),
                                                           shape_pt_lon = readr::col_number(),
                                                           shape_pt_sequence = readr::col_integer(),
                                                           shape_dist_traveled = readr::col_number()),
                                   show_col_types = FALSE,
                                   lazy = FALSE)
  } else {
    message_log <- c(message_log, "shapes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"transfers.txt"))){
    gtfs$transfers <- readr::read_csv(file.path(tmp_folder,"transfers.txt"),
                                      show_col_types = FALSE,
                                      lazy = FALSE)
  } else {
    message_log <- c(message_log, "transfers.txt")
  }

  unlink(tmp_folder, recursive = TRUE)


  if(length(message_log) > 0){
    message(paste(message_log, collapse = " "))
  }

  return(gtfs)
}

