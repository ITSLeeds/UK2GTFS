#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param stop_times stop-times
#' @param schedule list of dataframes
#' @param silent logical
#' @param ncores number of cores to use
#' @noRd
#'
schedule2routes <- function(stop_times, schedule, silent = TRUE, ncores = 1) {


  ### SECTION 1: ###############################################################################
  # make stop_times.txt
  if (!silent) {
    message(paste0(Sys.time(), " Building stop_times"))
  }

  # Convert Activity to pickup_type and drop_off_type
  stop_times$Activity[is.na(stop_times$Activity) & stop_times$stop_sequence == 1] <- "TB" # No activity specified at start

  # Fix arrival_time / departure_time being 0000 for pick up only or drop off only trains
  stop_times$departure_time <- dplyr::if_else(stop_times$departure_time == "0000" & stop_times$Activity == "D",
                                              stop_times$arrival_time,
                                              stop_times$departure_time)
  stop_times$arrival_time <- dplyr::if_else(stop_times$arrival_time == "0000" & stop_times$Activity == "U",
                                            stop_times$departure_time,
                                            stop_times$arrival_time)

  upoffs <- clean_activities2(stop_times$Activity)
  stop_times <- cbind(stop_times, upoffs)

  stop_times$arrival_time[is.na(stop_times$arrival_time)] <- stop_times$departure_time[is.na(stop_times$arrival_time)]
  stop_times$departure_time[is.na(stop_times$departure_time)] <- stop_times$arrival_time[is.na(stop_times$departure_time)]
  stop_times <- stop_times[, c("arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "rowID", "schedule")]

  stop_times <- stop_times[!(stop_times$pickup_type == 1 & stop_times$drop_off_type == 1), ]




  ### SECTION 2: ###############################################################################
  # make make the calendar.txt and calendar_dates.txt file from the schedule
  if (!silent) {
    message(paste0(Sys.time(), " Building calendar and calendar_dates"))
  }


  schedule <- schedule[, c(
    "Train UID", "Date Runs From", "Date Runs To", "Days Run", "Bank Holiday Running", "Train Status", "Train Category",
    "Headcode", "STP indicator", "rowID", "ATOC Code", "Retail Train ID"
  )]

  # build the calendar file
  res <- makeCalendar(schedule = schedule, ncores = ncores)
  calendar <- res[[1]]
  calendar_dates <- res[[2]]
  # rm(res)

  calendar$trip_id <- 1:nrow(calendar) # not sure why this was here, but used in duplicate.stop_times
  # calendar$service_id = 1:nrow(calendar) # For this purpose the serive and the trip are always the same

  # clean calednars
  # calendar = calendar[,c("UID","monday","tuesday","wednesday","thursday","friday","saturday","sunday",
  #                       "start_date","end_date","rowID","trip_id")]
  names(calendar)[names(calendar) == "UID"] <- "service_id"
  calendar$start_date <- as.character(calendar$start_date)
  calendar$start_date <- gsub("-", "", calendar$start_date)
  calendar$end_date <- as.character(calendar$end_date)
  calendar$end_date <- gsub("-", "", calendar$end_date)

  calendar_dates <- calendar_dates[, c("UID", "start_date")]
  names(calendar_dates) <- c("service_id", "date")
  calendar_dates$date <- as.character(calendar_dates$date)
  calendar_dates$date <- gsub("-", "", calendar_dates$date)
  calendar_dates$exception_type <- 2 # all events passed to calendar_dates are single day cancellations



  ### SECTION 3: ###############################################################################
  # When splitting the calendar roWIDs are duplicated
  # so create new system of trip_ids and duplicate the relevant stop_times
  if (!silent) {
    message(paste0(Sys.time(), " Duplicating necessary stop times"))
  }


  stop_times <- duplicate.stop_times_alt(calendar = calendar, stop_times = stop_times, ncores = 1)

  ### SECTION 5: ###############################################################################
  # make make the trips.txt  file by matching the calnedar to the stop_times

  trips <- calendar[, c("service_id", "trip_id", "rowID", "ATOC Code", "Train Status")]
  trips <- longnames(routes = trips, stop_times = stop_times)

  ### SECTION 4: ###############################################################################
  # make make the routes.txt
  # a route is all the trips with a common start and end
  # i.e. scheduels original UID
  if (!silent) {
    message(paste0(Sys.time(), " Building routes.txt"))
  }

  routes <- trips
  routes <- dplyr::group_by(routes, `ATOC Code`, route_long_name, `Train Status`)
  routes <- dplyr::summarise(routes)
  routes$route_id <- 1:nrow(routes)

  trips <- dplyr::left_join(trips, routes, by = c("ATOC Code" = "ATOC Code", "route_long_name" = "route_long_name", "Train Status" = "Train Status"))

  train_status <- data.frame(
    train_status = c("B", "F", "P", "S", "T", "1", "2", "3", "4", "5"),
    route_type = c(3, NA, 2, 4, NA, 2, NA, NA, 4, 3),
    stringsAsFactors = FALSE
  )

  routes$`Train Status` <- as.character(routes$`Train Status`)
  routes <- dplyr::left_join(routes, train_status, by = c("Train Status" = "train_status"))
  rm(train_status)

  routes <- routes[, c("route_id", "route_type", "ATOC Code", "route_long_name")]
  names(routes) <- c("route_id", "route_type", "agency_id", "route_long_name")

  # IDs are not meaningful, just leave out
  routes$route_short_name <- "" # was: routes$route_id

  routes$route_type[routes$agency_id == "LT"] <- 1 # London Underground is Metro

  ### Section 6: #######################################################
  # Final Checks

  # Fix Times
  stop_times <- afterMidnight(stop_times)

  # Ditch unneeded columns
  routes <- routes[, c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type")]
  trips <- trips[, c("trip_id", "route_id", "service_id")]
  stop_times <- stop_times[, c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type")]
  calendar <- calendar[, c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")]


  # end of function
  timetables <- list(calendar, calendar_dates, routes, stop_times, trips)
  names(timetables) <- c("calendar", "calendar_dates", "routes", "stop_times", "trips")
  return(timetables)
}
