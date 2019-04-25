#' exclude trips
#' remove trips
#' @param trip_sub
#' @param trip_exc
#' @noRd
#'
exclude_trips <- function(trip_sub, trip_exc){
  trip_exc_sub <- trip_exc[[trip_sub$trip_id[1]]]
  if(!is.null(trip_exc_sub)){
    # Exclusions
    # Classify Exclusions
    trip_exc_sub$type <- mapply(classify_exclusions,
                                ExStartTime = trip_exc_sub$StartDate,
                                ExEndTime = trip_exc_sub$EndDate,
                                StartDate = trip_sub$StartDate,
                                EndDate = trip_sub$EndDate)
    if("total" %in% trip_exc_sub$type){
      # Remove all
      trip_sub$exclude_days <- NA
      trip_sub <- trip_sub[NULL,]
    }else{
      if("start" %in% trip_exc_sub$type){
        trip_sub$StartDate <- max(trip_exc_sub$ExEndTime[trip_exc_sub$type == "start"])
      }
      if("end" %in% trip_exc_sub$type){
        trip_sub$EndDate <- min(trip_exc_sub$ExStartTime[trip_exc_sub$type == "end"])
      }
      if("middle" %in% trip_exc_sub$type){
        exclude_days <- trip_exc_sub[trip_exc_sub$type == "middle",]
        trip_sub$exclude_days <- list(list_exclude_days(exclude_days))
      }else{
        trip_sub$exclude_days <- NA
      }

    }

  }else{
    # No Exclusions
    trip_sub$exclude_days <- NA
  }
  return(trip_sub)
}

#' list exclude days
#' ????
#' @param exclude_days
#' @noRd
list_exclude_days <- function(exclude_days){
  res <- mapply(function(ExStartTime, ExEndTime){seq(ExStartTime, ExEndTime, by = "days")},
         exclude_days$StartDate,
         exclude_days$EndDate)
  res <- as.Date(unlist(res),origin = "1970-01-01")
  res <- unique(res)
  return(res)
}

#' list exclude days
#' break up star and end include days into list of days
#' @param include_days
#' @noRd
list_include_days <- function(include_days){
  res <- mapply(function(ExStartTime, ExEndTime){seq(ExStartTime, ExEndTime, by = "days")},
                include_days$StartDate,
                include_days$EndDate)
  res <- as.Date(unlist(res),origin = "1970-01-01")
  res <- unique(res)
  return(res)
}


#' Classify Excusions
#' Takes start and end dates of exclusion to work out if they cover the start or end etc
#' @param ExStartTime
#' @param ExEndTime
#' @param StartDate
#' @param EndDate
#' @noRd
classify_exclusions <- function(ExStartTime, ExEndTime, StartDate, EndDate){
  if(ExStartTime <= StartDate){
    if(ExEndTime >= EndDate){
      # Total Exclusion
      return("total")
    }else{
      # Trim Start
      return("start")
    }
  }else if(ExStartTime > StartDate){
    if(ExEndTime >= EndDate){
      # Total Exclusion
      return("end")
    }else{
      # Trim Start
      return("middle")
    }
  }else{
    return("no overlap")
  }
}

#' clean time
#' ????
#' @param x timepoints
#' @noRd
#'
clean_times <- function(x){
  x <- as.character(x)
  x <- gsub("PT","",x)
  x <- strsplit(x,"M")
  #mins <- grepl("M",x)
  #secs <- grepl("S",x)


  help_times2 <- function(x_sub){
    if(length(x_sub) == 2){
      if(!grepl("S",x_sub[2])){stop("Unknwown Time Structure")}
      time <- (as.integer(x_sub[1]) * 60) +  as.integer(gsub("S","",x_sub[2]))
    }else if(length(x_sub) == 1){
      if(grepl("S",x_sub)){
        time <- as.integer(gsub("S","",x_sub))
      }else if(is.na(x_sub)){
        time <- 0
      }else{
        time <- (as.integer(x_sub) * 60)
      }
    }else{
      stop("Terrible error")
    }
  }

  # help_times <- function(x_sub, min_sub, secs_sub){
  #   if(min_sub & secs_sub){
  #     # Mins and Seconds
  #     message("Mins and Secs")
  #     stop()
  #   }else if(min_sub & !secs_sub){
  #     # Mins only
  #     time <- as.numeric(gsub("M","",x_sub)) * 60
  #   }else if(!min_sub & secs_sub){
  #     # Secs only
  #     time <- as.numeric(gsub("S","",x_sub))
  #   }else if(!min_sub & !secs_sub){
  #     # Neither, due to NAs
  #     time <- 0
  #   }else{
  #     message("Terrible error")
  #     stop()
  #   }
  #   #time <- unname(time)
  #   return(time)
  # }
  #times <- unname(mapply(help_times, x_sub = x, min_sub = mins, secs_sub = secs, SIMPLIFY = T))
  times <- sapply(x,help_times2)
  return(times)
}

#' clean route type
#' Change rout types from charater to gtfs code
#' @param rt character route type
#' @noRd
clean_route_type <- function(rt){
  if(rt == "bus"){
    return(3)
  }else if(rt == "ferry"){
    return(4)
  }else{
    stop(paste0("Unknown route_type ",rt))
  }
}

#' Clean days
#' Change named days into GTFS fromat
#' @param days character of days
#' @noRd
clean_days <- function(days){
  days_ul <- unlist(strsplit(days," "))
  if(all(days_ul %in% c("Monday","Tuesday", "Wednesday", "Thursday", "Friday","Saturday","Sunday"))){
    res <- c(0,0,0,0,0,0,0)
    if("Monday" %in% days_ul){
      res[1] <- 1
    }
    if("Tuesday" %in% days_ul){
      res[2] <- 1
    }
    if("Wednesday" %in% days_ul){
      res[3] <- 1
    }
    if("Thursday" %in% days_ul){
      res[4] <- 1
    }
    if("Friday" %in% days_ul){
      res[5] <- 1
    }
    if("Saturday" %in% days_ul){
      res[6] <- 1
    }
    if("Sunday" %in% days_ul){
      res[7] <- 1
    }

  }else if(days == "MondayToFriday"){
    res <- c(1,1,1,1,1,0,0)
  }else if(days == "HolidaysOnly"){
    res <- c(0,0,0,0,0,0,0)
  }else if(days == "SaturdaySundayHolidaysOnly"){
    res <- c(0,0,0,0,0,1,1)
  }else if(days %in% c("","MondayToSunday","MondayToFridaySaturdaySundayHolidaysOnly")){
    res <- c(1,1,1,1,1,1,1)
  }else{
    stop(paste0("Unknown day pattern: ",days))
  }
  names(res) <- NULL
  res
}

#' break up holidays
#' ????
#' @param cal_data
#' @param cl
#' @noRd
break_up_holidays <- function(cal_dat, cl){
  cal_dat <- cal_dat[cal_dat[[cl]] != "",]
  if(nrow(cal_dat) == 0){
    return(NULL)
  }else{
    cal_dat_holidays <- lapply(strsplit(cal_dat[[cl]], " "),function(x){x[x != ""]})
    cal_dat <- cal_dat[rep(1:nrow(cal_dat), times = lengths(cal_dat_holidays)),]
    cal_dat$hols <- unlist(cal_dat_holidays)
    if(cl == "BankHolidaysOperate"){
      cal_dat$exception_type <- 1L
    }else{
      cal_dat$exception_type <- 2L
    }
    cal_dat <- cal_dat[,c("JourneyPatternRef","hols","exception_type")]
    return(cal_dat)
  }

}

#' break up holidays2
#' ????
#' @param cal_data
#' @param cl
#' @param cal
#' @noRd
break_up_holidays2 <- function(cal_dat, cl, cal){
  cal_dat <- cal_dat[cal_dat[[cl]] != "",]
  if(nrow(cal_dat) == 0){
    return(NULL)
  }else{
    cal_dat_holidays <- lapply(strsplit(cal_dat[[cl]], " "),function(x){x[x != ""]})
    cal_dat <- cal_dat[rep(1:nrow(cal_dat), times = lengths(cal_dat_holidays)),]
    cal_dat$hols <- unlist(cal_dat_holidays)
    if(cl == "BankHolidaysOperate"){
      cal_dat$exception_type <- 1L
    }else{
      cal_dat$exception_type <- 2L
    }
    cal_dat <- cal_dat[,c("trip_id","hols","exception_type")]
    return(cal_dat)
  }

}

#' check duplicated holidays
#' ????
#' @param i
#' @noRd
#'
check_duplicate_holidays <- function(i){
  cal_dat <- calendar_dates[i,]
  if(cal_dat$exception_type == 2){
    jpr <- calendar_dates$JourneyPatternRef[1]
    hols <- calendar_dates$hols[1]
    cal_sub <- calendar_dates[calendar_dates$JourneyPatternRef == jpr,]
    cal_sub <- cal_sub[cal_sub$hols == hols,]
    if(nrow(cal_sub) == 2){
      return(FALSE)
    }else if(nrow(cal_sub) == 1){
      return(TRUE)
    }else{
      stop(paste0("Invalid number of rows ",i))
    }
  }else{
    return(TRUE)
  }

}


# to do, need to repete stops times for each departure time
#' clean activities
#' ????
#' @param x
#' @param type
#' @noRd
clean_activity <- function(x, type){
  if(type == "pickup"){
    if(x == "pickUp"){
      x <- 0L
    }else if(x =="pickUpAndSetDown"){
      x <- 0L
    }else if(x =="setDown"){
      x <- 1L
    }else{
      stop(paste0(x," Invalid pickup type"))
    }
  }
  if(type == "drop_off"){
    if(x == "pickUp"){
      x <- 1L
    }else if(x =="pickUpAndSetDown"){
      x <- 0L
    }else if(x =="setDown"){
      x <- 0L
    }else{
      stop(paste0(x," Invalid drop off type"))
    }
  }
  x
}

#' Expan stop_times
#' ????
#' @param i
#' @param jps
#' @noRd
#'
expand_stop_times <- function(i, jps){
  jps_sub <- jps[[i]]
  trips_sub <- trips[trips$service_id == jps_sub$JourneyPatternID[1],]

  st_sub = jps_sub[,c("To.StopPointRef","To.Activity","To.SequenceNumber","JourneyPatternID","To.WaitTime","To.TimingStatus","RunTime")]
  names(st_sub) <- c("stop_id","To.Activity","stop_sequence","service_id","To.WaitTime","timepoint","RunTime")
  st_top = data.frame(stop_id       = jps_sub$From.StopPointRef[1],
                      To.Activity   = jps_sub$From.Activity[1],
                      stop_sequence = "1",
                      service_id    = jps_sub$JourneyPatternID[1],
                      To.WaitTime   = 0,
                      timepoint     = jps_sub$From.TimingStatus[1],
                      RunTime       = 0,
                      stringsAsFactors = F)
  st_sub <- rbind(st_top, st_sub)
  st_sub$RunTime <- as.integer(st_sub$RunTime)
  st_sub$To.WaitTime <- as.integer(st_sub$To.WaitTime)
  st_sub$departure_time <- cumsum(st_sub$RunTime + st_sub$To.WaitTime)
  st_sub$arrival_time <- st_sub$departure_time - st_sub$To.WaitTime
  st_sub$pickup_type <- sapply(st_sub$To.Activity, clean_activity, type = "pickup")
  st_sub$drop_off_type <- sapply(st_sub$To.Activity, clean_activity, type = "drop_off")

  n_stops <- nrow(st_sub)
  n_trips <- nrow(trips_sub)
  st_sub <- st_sub[rep(1:n_stops, times = n_trips),]
  st_sub$trip_id <- rep(trips_sub$trip_id, each = n_stops)
  st_sub$DepartureTime <- lubridate::hms(rep(trips_sub$DepartureTime, each = n_stops))

  st_sub$arrival_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$arrival_time) + lubridate::as.duration(st_sub$DepartureTime))
  st_sub$arrival_time <- sprintf('%02d:%02d:%02d', st_sub$arrival_time@day * 24 + st_sub$arrival_time@hour, minute(st_sub$arrival_time), second(st_sub$arrival_time))

  st_sub$departure_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$departure_time) + lubridate::as.duration(st_sub$DepartureTime))
  st_sub$departure_time <- sprintf('%02d:%02d:%02d', st_sub$departure_time@day * 24 + st_sub$departure_time@hour, minute(st_sub$departure_time), second(st_sub$departure_time))

  st_sub$timepoint <- sapply(st_sub$timepoint,clean_timepoints)

  st_sub <- st_sub[,c("trip_id","arrival_time","departure_time","stop_id","stop_sequence","timepoint")]

  return(st_sub)
}

#' Expan stop_times2
#' ????
#' @param i
#' @param jps
#' @param trips
#' @noRd
#'
expand_stop_times2 <- function(i, jps, trips){
  jps_sub <- jps[[i]]
  trips_sub <- trips[trips$JourneyPatternRef == jps_sub$JourneyPatternID[1],]
  jps_sub$To.Activity[is.na(jps_sub$To.Activity)] <- "pickUpAndSetDown"
  if(all(is.na(jps_sub$To.SequenceNumber))){
    jps_sub$To.SequenceNumber <- seq(2,nrow(jps_sub) + 1)
  }


  st_sub = jps_sub[,c("To.StopPointRef","To.Activity","To.SequenceNumber","JourneyPatternID","To.WaitTime","To.TimingStatus","RunTime")]
  names(st_sub) <- c("stop_id","To.Activity","stop_sequence","JourneyPatternRef","To.WaitTime","timepoint","RunTime")
  st_top = data.frame(stop_id       = jps_sub$From.StopPointRef[1],
                      To.Activity   = jps_sub$From.Activity[1],
                      stop_sequence = "1",
                      JourneyPatternRef    = jps_sub$JourneyPatternID[1],
                      To.WaitTime   = 0,
                      timepoint     = jps_sub$From.TimingStatus[1],
                      RunTime       = 0,
                      stringsAsFactors = F)
  if(is.na(st_top$To.Activity)){
    st_top$To.Activity <- "pickUp"
  }else if(st_top$To.Activity == "pass"){
    st_top$To.Activity <- "pickUp"
  }

  st_sub <- rbind(st_top, st_sub)
  #st_sub$RunTime <- as.integer(st_sub$RunTime)
  st_sub$To.WaitTime <- as.integer(st_sub$To.WaitTime)
  st_sub$departure_time <- cumsum(st_sub$RunTime + st_sub$To.WaitTime)
  st_sub$arrival_time <- st_sub$departure_time - st_sub$To.WaitTime
  st_sub$pickup_type <- sapply(st_sub$To.Activity, clean_activity, type = "pickup")
  st_sub$drop_off_type <- sapply(st_sub$To.Activity, clean_activity, type = "drop_off")

  n_stops <- nrow(st_sub)
  n_trips <- nrow(trips_sub)
  st_sub <- st_sub[rep(1:n_stops, times = n_trips),]
  st_sub$trip_id <- rep(trips_sub$trip_id, each = n_stops)
  st_sub$DepartureTime <- lubridate::hms(rep(trips_sub$DepartureTime, each = n_stops))

  st_sub$arrival_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$arrival_time) + lubridate::as.duration(st_sub$DepartureTime))
  st_sub$arrival_time <- sprintf('%02d:%02d:%02d', st_sub$arrival_time@day * 24 + st_sub$arrival_time@hour, lubridate::minute(st_sub$arrival_time), lubridate::second(st_sub$arrival_time))

  st_sub$departure_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$departure_time) + lubridate::as.duration(st_sub$DepartureTime))
  st_sub$departure_time <- sprintf('%02d:%02d:%02d', st_sub$departure_time@day * 24 + st_sub$departure_time@hour, lubridate::minute(st_sub$departure_time), lubridate::second(st_sub$departure_time))

  st_sub$timepoint <- sapply(st_sub$timepoint,clean_timepoints)

  st_sub <- st_sub[,c("trip_id","arrival_time","departure_time","stop_id","stop_sequence","timepoint")]

  return(st_sub)
}


#' clean_timepoints
#' ????
#' @param tp
#' @noRd
#'
clean_timepoints <- function(tp){
  if(tp == "OTH"){
    return(1L)
  }else if(tp %in% c("PTP","TIP","PPT")){
    return(0L)
  }else{
    stop(paste0("Unknown timepoint type: ",tp))
  }
}

#' make stop times
#' ????
#' @param jps
#' @param trips
#' @param ss
#' @noRd
#'
make_stop_times <- function(jps, trips, ss){
  jps <- jps[,c("JPS_id","From.Activity","From.StopPointRef","From.TimingStatus","To.WaitTime","To.Activity","To.StopPointRef","To.TimingStatus","RunTime","From.SequenceNumber", "To.SequenceNumber")]
  jps[] <- lapply(jps,as.character)
  jps <- clean_pass(jps)
  #vj[] <- lapply(vj, as.character)
  #rts <- unique(vj$JourneyPatternRef)
  ss_join <- ss[,c("JourneyPatternSectionRefs","JourneyPatternID")]
  ss_join[] <- lapply(ss_join, as.character)
  jps$JPS_id <- as.character(jps$JPS_id)
  jps <- dplyr::left_join(jps,ss_join, by = c("JPS_id" = "JourneyPatternSectionRefs"))
  jps <- split(jps, jps$JourneyPatternID)

  stop_times <- lapply(1:length(jps), expand_stop_times2, jps = jps, trips = trips)
  stop_times <- dplyr::bind_rows(stop_times)
  return(stop_times)
}

#' Remove passes from journey patterns
#' JPS can have a "pass" type, remove and update runtimes
#' @param jps journeypatternsections
#' @noRd
clean_pass <- function(jps){
  if("pass" %in% jps$To.Activity){
    is_pass <- jps$To.Activity == "pass"
    pass_post <- c(FALSE, is_pass[seq(1, length(is_pass) - 1)])
    runtime1 <- as.integer(jps$RunTime)
    runtime2 <- c(0, runtime1[seq(1, length(runtime1) - 1)])
    runtime3 <- ifelse(pass_post, runtime1 + runtime2, runtime1)
    jps$RunTime <- runtime3
    jps <- jps[jps$To.Activity != "pass",]

  }else{
    jps$RunTime <- as.integer(jps$RunTime)
  }
  return(jps)
}
