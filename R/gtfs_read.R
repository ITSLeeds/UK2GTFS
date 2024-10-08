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

  if(checkmate::test_file_exists(file.path(tmp_folder,"agency.txt"))){

    gtfs$agency <- fread(
      file.path(tmp_folder, "agency.txt"),
      colClasses = c(
        agency_id = "character",
        agency_noc = "character"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: agency.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stops.txt"))){

    gtfs$stops <- fread(
      file.path(tmp_folder, "stops.txt"),
      colClasses = c(
        stop_id = "character",
        stop_code = "character",
        stop_name = "character",
        stop_lat = "numeric",
        stop_lon = "numeric",
        wheelchair_boarding = "integer", #enum value 2 is valid but rarely seen outside the spec document
        location_type = "integer",
        parent_station = "character",
        platform_code = "character"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: stops.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"routes.txt"))){

    gtfs$routes <- fread(
      file.path(tmp_folder, "routes.txt"),
      colClasses = c(
        route_id = "character",
        agency_id = "character",
        route_short_name = "character",
        route_long_name = "character",
        route_type = "integer"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: routes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"trips.txt"))){

    gtfs$trips <- fread(
      file.path(tmp_folder, "trips.txt"),
      colClasses = c(
        trip_id = "character",
        route_id = "character",
        service_id = "character",
        block_id = "character",
        shape_id = "character",
        wheelchair_accessible = "integer" #enum value 2 is valid but rarely seen outside the spec document
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: trips.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stop_times.txt"))){

    gtfs$stop_times <- fread(
      file.path(tmp_folder, "stop_times.txt"),
      colClasses = c(
        trip_id = "character",
        stop_id = "character",
        stop_sequence = "integer",
        departure_time = "character",
        arrival_time = "character",
        shape_dist_traveled = "numeric",
        timepoint = "integer",
        pickup_type = "integer",
        drop_off_type = "integer"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

    gtfs$stop_times$arrival_time <- lubridate::hms(gtfs$stop_times$arrival_time)
    gtfs$stop_times$departure_time <- lubridate::hms(gtfs$stop_times$departure_time)

  } else {
    warning("Unable to find required file: stop_times.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar.txt"))){

    gtfs$calendar <- fread(
      file.path(tmp_folder, "calendar.txt"),
      colClasses = c(
        service_id = "character",
        monday = "integer",
        tuesday = "integer",
        wednesday = "integer",
        thursday = "integer",
        friday = "integer",
        saturday = "integer",
        sunday = "integer",
        start_date = "character",
        end_date = "character"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

    gtfs$calendar[, start_date := as.IDate(start_date, "%Y%m%d")]
    gtfs$calendar[, end_date := as.IDate(end_date, "%Y%m%d")]

  } else {
    message("Unable to find conditionally required file: calendar.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar_dates.txt"))){

    gtfs$calendar_dates <- fread(
      file.path(tmp_folder, "calendar_dates.txt"),
      colClasses = c(
        service_id = "character",
        date = "character",
        exception_type = "integer"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )
    gtfs$calendar_dates[, date := as.IDate(date, "%Y%m%d")]

  } else {
    message("Unable to find conditionally required file: calendar_dates.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"shapes.txt"))){

    gtfs$shapes <- data.table::fread(
      file.path(tmp_folder, "shapes.txt"),
      colClasses = c(
        shape_id = "character",
        shape_pt_lat = "numeric",
        shape_pt_lon = "numeric",
        shape_pt_sequence = "integer",
        shape_dist_traveled = "numeric"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  }


  #load any other tables in the .zip file
  filenamesOnly <- tools::file_path_sans_ext(basename(files))
  notLoadedFiles = setdiff(  filenamesOnly, names(gtfs) )

  for (fileName in notLoadedFiles)
  {
    table <- fread(
      file.path(tmp_folder, paste0(fileName, ".txt")),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

    gtfs[[fileName]] <- table
  }

  #remove temp directory
  unlink(tmp_folder, recursive = TRUE)

  return(gtfs)
}

