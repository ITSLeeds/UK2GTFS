#' NI to GTFS
#'
#' @details Convert Northern Ireland CIF files to GTFS
#'
#' @param path_in Path to CIF file
#' @param silent Logical, should progress be shown
#' @return A GTFS named list
#' @details
#'
#'


ni2gtfs <- function(path_in,
                    silent = FALSE){

  stop("This function does not work yet")
  # Checkmates
  checkmate::assert_character(path_in, len = 1)
  checkmate::assert_file_exists(path_in)

  mca <- importMCA(path_in, silent = silent)

  stop_times <- mca[["stop_times"]]
  schedule <- mca[["schedule"]]
  rm(mca)

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


}
