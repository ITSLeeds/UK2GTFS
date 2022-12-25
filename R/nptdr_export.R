#' make calendar nptdr
#'
#' @details
#' split overlapping start and end dates
#'
#' @param schedule scheduel data.frame
#' @noRd
#'
nptdr_makeCalendar <- function(schedule, exceptions, historic_bank_holidays = historic_bank_holidays) {
  # prep the inputs
  calendar <- schedule[, c("uid", "start_date", "end_date",
                           "days_operation","school_term_time","bank_holiday", "rowID", "running_board",
                           "schedule","route_direction")]
  names(calendar) <- c("UID", "start_date", "end_date", "Days", "school_term_time","bank_holiday",
                       "rowID", "Headcode", "schedule","route_direction")
  calendar$duration <- calendar$end_date - calendar$start_date + 1

  message(paste0(Sys.time(), " Constructing calendar and calendar_dates"))

  # Process exclusions
  if(!all(exceptions$exception_type == 0)){
    stop("Other types of exception")
  }


  cal_noexc <- calendar[!calendar$schedule %in% exceptions$schedule,]
  cal_exc <- calendar[calendar$schedule %in% exceptions$schedule,]
  cal_exc <- dplyr::group_by(cal_exc, UID)
  cal_exc <- dplyr::group_split(cal_exc)
  cal_exc <- purrr::map(cal_exc, .f = exclude_trips_nptdr, trip_exc = exceptions)
  cal_exc <- dplyr::bind_rows(cal_exc)

  cal_dates <- data.frame(UID = rep(cal_exc$UID, times = lengths(cal_exc$exclude_days)),
                          date = unlist(cal_exc$exclude_days))
  cal_dates$date <- as.Date(cal_dates$date, origin = "1970-01-01")
  cal_dates$exception_type <- 2
    cal_exc$exclude_days <- NULL

  calendar <- rbind(cal_noexc, cal_exc)

  calendar_dates <- calendar[,c("UID","start_date","end_date","school_term_time","bank_holiday")]
  calendar$school_term_time <- NULL
  calendar$bank_holiday <- NULL

  # TODO: Detect Scotland and NI
  # TODO: Historical term times
  message("Unique Scotland and NI bank holidays not correctly handeled")
  message("School term dates not supported")

  bh <- historic_bank_holidays[historic_bank_holidays$date >= min(calendar$start_date, na.rm = TRUE),]
  bh <- bh[bh$date <= min(calendar$end_date, na.rm = TRUE), ]
  bh <- bh[bh$England,]

  # TODO: Check how to handle bank_holiday = B as may need to exclude all other dates
  calendar_dates <- nptdr_parse_bank_holidays(calendar_dates, bh)
  calendar_dates <- rbind(calendar_dates, cal_dates)

  days <- lapply(calendar$Days, function(x) {
    as.integer(substring(x, 1:7, 1:7))
  })
  days <- matrix(unlist(days), ncol = 7, byrow = TRUE)
  days <- as.data.frame(days)
  names(days) <- c(
    "monday", "tuesday", "wednesday", "thursday",
    "friday", "saturday", "sunday"
  )

  calendar <- cbind(calendar, days)
  calendar$Days <- NULL

  return(list(calendar, calendar_dates))
}



#' exclude trips
#' remove trips
#' @param trip_sub desc
#' @param trip_exc desc
#' @noRd
#'
exclude_trips_nptdr <- function(trip_sub, trip_exc) {
  trip_exc_sub <- trip_exc[trip_exc$schedule == trip_sub$schedule,]
  if (!is.null(trip_exc_sub)) {
    trip_exc_sub$duration <- as.integer(trip_exc_sub$end_date - trip_exc_sub$start_date + 1)
    if(any(trip_exc_sub$duration > 3650)){
      # Sometimes extremely long exclusions e.g. 2004 to 2900 then exclude after 2005.
      message("Trip ",trip_sub$UID," trunkated from ",trip_sub$end_date," to 2020-12-31")
      trip_sub$end_date <- lubridate::ymd("2020-12-31")
    }

    # Exclusions
    # Classify Exclusions
    trip_exc_sub$type <- mapply(classify_exclusions,
                                ExStartTime = trip_exc_sub$start_date,
                                ExEndTime = trip_exc_sub$end_date,
                                StartDate = trip_sub$start_date,
                                EndDate = trip_sub$end_date
    )


    if ("total" %in% trip_exc_sub$type) {
      # Remove all
      return(NULL)
    } else {
      if ("start" %in% trip_exc_sub$type) {
        trip_sub$start_date <- max(trip_exc_sub$end_date[
          trip_exc_sub$type == "start"]) + 1
      }
      if ("end" %in% trip_exc_sub$type) {
        trip_sub$end_date <- min(trip_exc_sub$start_date[
          trip_exc_sub$type == "end"]) - 1
      }
      if ("middle" %in% trip_exc_sub$type) {
        exclude_days <- trip_exc_sub[trip_exc_sub$type == "middle", ]
        trip_sub$exclude_days <- list(list_exclude_days_nptdr(exclude_days))
      } else {
        trip_sub$exclude_days <- NA
      }
    }
  } else {
    # No Exclusions
    trip_sub$exclude_days <- NA
  }
  return(trip_sub)
}


#' list exclude days
#' ????
#' @param exclude_days desc
#' @noRd
list_exclude_days_nptdr <- function(exclude_days) {
  res <- mapply(
    function(ExStartTime, ExEndTime) {
      seq(ExStartTime, ExEndTime, by = "days")
    },
    exclude_days$start_date,
    exclude_days$end_date
  )
  res <- as.Date(unlist(res), origin = "1970-01-01")
  res <- unique(res)
  return(res)
}

#' Build calendar dates using historic bank holidays
#' ????
#' @param calendar_dates2 desc
#' @param bh desc
#' @noRd
nptdr_parse_bank_holidays <- function(calendar_dates2, bh){

  calendar_dates2$end_date[is.na(calendar_dates2$end_date)] <- max(bh$date, na.rm = TRUE) + 1

  calendar_dates2$school_term_time <- NULL

  # Blank = Operates days defined above
  # A = Operates additionally on bank holidays - GTFS 1
  # B = Operates on bank holidays only - GTFS 1
  # X = Operates except on bank holidays - GTFS 2
  calendar_dates2 <- calendar_dates2[!is.na(calendar_dates2$bank_holiday),]
  calendar_dates2$exception_type <- dplyr::if_else(calendar_dates2$bank_holiday == "X",
                                                   2,1)
  calendar_dates2$bank_holiday <- NULL

  n = nrow(calendar_dates2)
  calendar_dates2 <- calendar_dates2[rep(seq_len(n), each = nrow(bh)),]
  calendar_dates2$date <- rep(bh$date, times = n)
  calendar_dates2 <- calendar_dates2[calendar_dates2$date >= calendar_dates2$start_date,]
  calendar_dates2 <- calendar_dates2[calendar_dates2$date <= calendar_dates2$end_date,]

  calendar_dates2 <- calendar_dates2[,c("UID","date","exception_type")]
  return(calendar_dates2)
}
