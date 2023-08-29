#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param stop_times stop-times
#' @param schedule list of dataframes
#' @param silent logical
#' @param ncores number of cores to use
#' @param public_only filters to services / calls that are public pickup/set down only
#' @noRd
#'
schedule2routes <- function(stop_times, schedule, silent = TRUE, ncores = 1, public_only=TRUE) {


  ### SECTION 1: ###############################################################################
  # make stop_times.txt
  if (!silent) {
    message(paste0(Sys.time(), " Building stop_times"))
  }

  # Fix arrival_time / departure_time being 0000 for pick up only or drop off only trains
  stop_times$departure_time <- dplyr::if_else(stop_times$departure_time == "0000" & stop_times$Activity == "D",
                                              stop_times$arrival_time,
                                              stop_times$departure_time)
  stop_times$arrival_time <- dplyr::if_else(stop_times$arrival_time == "0000" & stop_times$Activity == "U",
                                            stop_times$departure_time,
                                            stop_times$arrival_time)

  # Convert Activity to pickup_type and drop_off_type
  upoffs <- clean_activities2(stop_times$Activity, public_only=public_only)
  stop_times <- cbind(stop_times, upoffs)

  #fix missing arrival / departure times by copying from the other time.
  stop_times$arrival_time[is.na(stop_times$arrival_time)] <- stop_times$departure_time[is.na(stop_times$arrival_time)]
  stop_times$departure_time[is.na(stop_times$departure_time)] <- stop_times$arrival_time[is.na(stop_times$departure_time)]

  stop_times <- stop_times[, c("arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "rowID", "schedule")]

  if (public_only)
  {
    stop_times <- stop_times[!(stop_times$pickup_type == 1 & stop_times$drop_off_type == 1), ]
  }


  ### SECTION 2: ###############################################################################
  # make make the calendar.txt and calendar_dates.txt file from the schedule
  if (!silent) {
    message(paste0(Sys.time(), " Building calendar and calendar_dates"))
  }

  # build the calendar file
  res <- makeCalendar(schedule = schedule, ncores = ncores)
  calendar <- res[[1]]
  calendar_dates <- res[[2]]
  rm(res)

  #remove columns we don't need any more
  schedule <- schedule[, c(
    "Train UID", "Train Status", "Train Category",
    "Headcode", "rowID", "ATOC Code", "Retail Train ID", "Power Type", "Train Identity"
  )]

  #making the calendar will duplicate rows where the base timetable has been layered over with cancellations etc.
  #join back to the original to get the extra columns not returned from makeCalendar
  calendar <- dplyr::left_join(calendar, schedule, by = c("rowID"))

  gc()

  calendar$trip_id <- 1:nrow(calendar) # not sure why this was here, but used in duplicate.stop_times
  # calendar$service_id = 1:nrow(calendar) # For this purpose the service and the trip are always the same

  # clean calendars
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

                                  #TODO find out why this hangs if ncores > 1
  stop_times <- duplicate.stop_times_alt(calendar = calendar, stop_times = stop_times, ncores = 1)

  ### SECTION 5: ###############################################################################
  # make make the trips.txt  file by matching the calendar to the stop_times

  trips <- calendar[, c("service_id", "trip_id", "rowID", "ATOC Code", "Train Status", "Train Category", "Power Type", "Train Identity")]
  trips <- longnames(routes = trips, stop_times = stop_times)

  ### SECTION 4: ###############################################################################
  # make make the routes.txt
  # a route is all the trips with a common start and end
  # i.e. schedules original UID
  if (!silent) {
    message(paste0(Sys.time(), " Building routes.txt"))
  }

  #do the conversion to route_type before grouping because several status map to the same route_type and we get 'duplicate' routes that look the same.
  train_status <- data.table(
    train_status = c("B", "F", "P", "S", "T", "1", "2", "3", "4", "5"),
    route_type = c(   3,   NA,  2,   4,   NA,  2,   NA,  NA,  4,   3),
    stringsAsFactors = FALSE
  )

  trips$`Train Status` <- as.character(trips$`Train Status`)
  trips <- dplyr::left_join(trips, train_status, by = c("Train Status" = "train_status"))
  rm(train_status)

  routes <- trips

  routes <- dplyr::group_by(routes, `ATOC Code`, route_long_name, `Train Category`, route_type )
  routes <- dplyr::summarise(routes)
  routes$route_id <- 1:nrow(routes)

  trips <- dplyr::left_join(trips, routes, by = c("ATOC Code", "route_long_name", "Train Category", "route_type"))

  routes <- routes[, c("route_id", "route_type", "ATOC Code", "route_long_name", "Train Category" )]
  names(routes) <- c("route_id", "route_type", "agency_id", "route_long_name", "train_category" )
  routes$route_short_name <- routes$route_id

  routes$route_type[routes$agency_id == "LT" & routes$route_type == 2 ] <- 1
      # London Underground is Metro (unless already identified as a bus/ship etc)

  ### Section 6: #######################################################
  # Final Checks

  # Fix Times
  stop_times <- afterMidnight(stop_times)


  #gtfs systems tend to be tolerant of additional fields, so expose the train_category and power_type so that the consumer can do more analysis on them if they wish.
  #e.g. filter out ECS moves
  # Ditch unneeded columns
  routes <- routes[, c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type", "train_category")]
  trips <- trips[, c("trip_id", "route_id", "service_id", "Train Identity", "Power Type")]
  names(trips) <- c("trip_id", "route_id", "service_id", "train_identity", "power_type")
  stop_times <- stop_times[, c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type")]
  calendar <- calendar[, c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")]


  # end of function
  timetables <- list(calendar, calendar_dates, routes, stop_times, trips)
  names(timetables) <- c("calendar", "calendar_dates", "routes", "stop_times", "trips")
  return(timetables)
}
