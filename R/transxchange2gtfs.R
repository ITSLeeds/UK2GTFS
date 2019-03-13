obj = readRDS("example_import.Rds")

transxchange2gtfs <- function(obj, run_debug = T){
  JourneyPatternSections  <-  obj[["JourneyPatternSections"]]
  Operators               <-  obj[["Operators"]]
  Routes                  <-  obj[["Routes"]]
  RouteSections           <-  obj[["RouteSections"]]
  Services_main           <-  obj[["Services_main"]]
  StandardService         <-  obj[["StandardService"]]
  Services_NonOperation   <-  obj[["Services_NonOperation"]]
  StopPoints              <-  obj[["StopPoints"]]
  VehicleJourneys         <-  obj[["VehicleJourneys"]]
  VehicleJourneys_exclude <-  obj[["VehicleJourneys_exclude"]]
  VehicleJourneys_include <-  obj[["VehicleJourneys_include"]]
  VehicleJourneysTimingLinks <- obj[["VehicleJourneysTimingLinks"]]

  ## JourneyPatternSections #####################
  clean_times <- function(x){
    x <- as.character(x)
    x <- gsub("PT","",x)
    mins <- grepl("M",x)
    secs <- grepl("S",x)

    help_times <- function(x_sub, min_sub, secs_sub){
      if(min_sub & secs_sub){
        # Mins and Seconds
        message("Mins and Secs")
        stop()
      }else if(min_sub & !secs_sub){
        # Mins only
        time <- as.numeric(gsub("M","",x_sub)) * 60
      }else if(!min_sub & secs_sub){
        # Secs only
        time <- as.numeric(gsub("S","",x_sub))
      }else if(!min_sub & !secs_sub){
        # Neither, due to NAs
        time <- 0
      }else{
        message("Terrible error")
        stop()
      }
      #time <- unname(time)
      return(time)
    }
      times <- unname(mapply(help_times, x_sub = x, min_sub = mins, secs_sub = secs, SIMPLIFY = T))
      return(times)
  }

  if(run_debug){
    chk <- gsub("[0-9]","",JourneyPatternSections$RunTime)
    chk <- unique(chk)
    if(!all(chk %in% c("PTM","PTS"))){
      message("Unknown time formats")
      stop()
    }
    rm(chk)
  }

  JourneyPatternSections$RunTime <- clean_times(JourneyPatternSections$RunTime)
  JourneyPatternSections$To.WaitTime <- clean_times(JourneyPatternSections$To.WaitTime)
  JourneyPatternSections$total_time <- JourneyPatternSections$RunTime + JourneyPatternSections$To.WaitTime

  ## stops ######################################
  stops <- StopPoints[,c("StopPointRef","CommonName")]
  names(stops) <- c("stop_id","stop_name")

  ## routes #####################################
  # route_id, agency_id, route_short_name, route_long_name, route_desc, route_type

  routes <- Services_main[c("ServiceCode","RegisteredOperatorRef","LineName","Description","Mode","Origin","Destination")]
  routes$route_long_name <- paste0(routes$Origin," - ",routes$Destination)
  names(routes) <- c("route_id","agency_id","route_short_name","route_desc","route_type","Origin","Destination","route_long_name")
  routes <- routes[,c("route_id","agency_id","route_short_name","route_long_name","route_desc","route_type")]

  ## Trips, Calendar, Calendar Dates #####################################
  # route_id, service_id, trip_id, trip_headsign, trip_short_name, direction_id,
  trips <- VehicleJourneys[,c("ServiceRef","JourneyPatternRef","VehicleJourneyCode","DepartureTime")]
  names(trips) <- c("route_id","service_id","trip_id","DepartureTime")

  # Calendar
  #service_id , mon, tues etc, start_date, end_date
  calendar <- VehicleJourneys[,c("JourneyPatternRef","days")]
  names(calendar) <- c("service_id","days")
  calendar <- calendar[!duplicated(calendar$service_id),]
  calendar$start_date <- Services_main$StartDate
  calendar$end_date <- Services_main$EndDate
  calendar$days <- as.character(calendar$days)

  clean_days <- function(days){
    if(days == "MondayToFriday"){
      days <- c(1,1,1,1,1,0,0)
    }else if(days == "Saturday"){
      days <- c(0,0,0,0,0,1,0)
    }else if(days == "Sunday"){
      days <- c(0,0,0,0,0,0,1)
    }else if(days == "HolidaysOnly"){
      days <- c(0,0,0,0,0,0,0)
    }else{
      stop(paste0("Unknown day pattern: ",days))
    }
    names(days) <- NULL
    days
  }

  calendar_days <- as.data.frame(t(sapply(as.character(calendar$days), clean_days, USE.NAMES = F)))
  names(calendar_days) <- c("Monday","Tuesday","wednesday","Thursday","Friday","Saturday","Sunday")

  calendar <- cbind(calendar, calendar_days)
  calendar <- calendar[,c("service_id","Monday","Tuesday","wednesday","Thursday","Friday","Saturday","Sunday","start_date", "end_date")]
  rm(calendar_days)

  #calendar_dates
  # to do convert names holidays to dates
  # ideitfy duplicated and overlapping rules in shcdeuels
  #service_id, date, exception_type
  calendar_dates <- VehicleJourneys[,c("JourneyPatternRef","BankHolidaysOperate","BankHolidaysNoOperate")]
  calendar_dates <- dplyr::group_by(calendar_dates, JourneyPatternRef)
  calendar_dates <- dplyr::summarise(calendar_dates, BankHolidaysOperate = paste(unique(BankHolidaysOperate), collapse = " "),
                                     BankHolidaysNoOperate = paste(unique(BankHolidaysNoOperate), collapse = " "))

  break_up_holidays <- function(cal_dat, cl){
    cal_dat <- cal_dat[cal_dat[[cl]] != "",]
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

  calendar_dates_inc <- break_up_holidays(calendar_dates, "BankHolidaysOperate")
  calendar_dates_exc <- break_up_holidays(calendar_dates, "BankHolidaysNoOperate")
  calendar_dates <- rbind(calendar_dates_inc,calendar_dates_exc)
  rm(calendar_dates_inc,calendar_dates_exc)

  check_duplicate_holidays <- function(i){
    cal_dat <- calendar_dates[i,]
    if(cal_dat$exception_type == 2){
      jpr <- calendar_dates$JourneyPatternRef[1]
      hols <- calendar_dates$hols[1]
      cal_sub <- calendar_dates[calendar_dates$JourneyPatternRef == jpr,]
      cal_sub <- cal_sub[cal_sub$hols == hols,]
      if(nrow(cal_sub) == 2){
        return(FALSE)
      }else if(nrow(cal_sub) == 2){
        return(TRUE)
      }else{
        stop(paste0("Invalid number of rows ",i))
      }
    }else{
      return(TRUE)
    }

  }

  calendar_dates <- calendar_dates[ sapply(1:nrow(calendar_dates), check_duplicate_holidays), ]
  cal <- get_bank_holidays()
  cal <- cal[cal$date >= as.Date(Services_main$StartDate) & cal$date <= as.Date(Services_main$EndDate), ]
  calendar_dates <- dplyr::left_join(calendar_dates, cal, by = c("hols" = "name"))
  calendar_dates <- calendar_dates[,c("JourneyPatternRef","date","exception_type")]
  names(calendar_dates) <- c("service_id","date","exception_type")
  rm(cal)
  #calendar_dates <- unique(calendar_dates)

  ## stop_times #################################
  # to do, need to repete stops times for each departure time
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

  make_stop_times <- function(jps, vj, ss){
    jps[] <- lapply(jps, as.character)
    vj[] <- lapply(vj, as.character)
    rts <- unique(vj$JourneyPatternRef)
    ss_join <- ss[,c("JourneyPatternSectionRefs","JourneyPatternID")]
    ss_join[] <- lapply(ss_join, as.character)
    jps$JPS_id <- as.character(jps$JPS_id)
    jps <- dplyr::left_join(jps,ss_join, by = c("JPS_id" = "JourneyPatternSectionRefs"))
    jps <- split(jps, jps$JourneyPatternID)
    vj <- split(vj, vj$JourneyPatternRef)
    if(!identical(names(jps),names(vj))){message("Different Journey Patterns in jps and vj");stop()}
    stop_times_all <- list()
    for(i in seq(1,length(jps))){
      jps_sub = jps[[i]]
      vj_sub = vj[[i]]
      stop_times_sub = jps_sub[,c("To.StopPointRef","To.Activity","total_time","To.SequenceNumber","JourneyPatternID")]
      stop_times_top = data.frame(To.StopPointRef = jps_sub$From.StopPointRef[1],
                                  To.Activity = jps_sub$From.Activity[1],
                                  total_time = "0",
                                  To.SequenceNumber = "1",
                                  JourneyPatternID = jps_sub$JourneyPatternID[1],
                                  stringsAsFactors = F)
      stop_times_sub = dplyr::bind_rows(list(stop_times_top,stop_times_sub))
      stop_times_sub$cum_time = cumsum(stop_times_sub$total_time)
      stop_times_sub$cum_time <- lubridate::seconds_to_period(stop_times_sub$cum_time)
      stop_times_sub$arrival_time <- stop_times_sub$cum_time + lubridate::hms(vj_sub$DepartureTime[1])
      stop_times_sub$arrival_time <- sprintf('%02d:%02d:%02d', stop_times_sub$arrival_time@hour, stop_times_sub$arrival_time@minute, stop_times_sub$arrival_time@.Data)
      stop_times_sub$departure_time <- stop_times_sub$arrival_time
      stop_times_sub$pickup_type <- sapply(stop_times_sub$To.Activity, clean_activity, type = "pickup")
      stop_times_sub$drop_off_type <- sapply(stop_times_sub$To.Activity, clean_activity, type = "drop_off")
      stop_times_sub <- stop_times_sub[,c("JourneyPatternID","arrival_time","departure_time","To.StopPointRef","To.SequenceNumber","pickup_type","drop_off_type")]
      names(stop_times_sub) <- c("trip_id","arrival_time","departure_time","stop_id","stop_sequence","pickup_type","drop_off_type")
      stop_times_all[[i]] <- stop_times_sub
    }
    stop_times_all <- dplyr::bind_rows(stop_times_all)

    return(stop_times_all)
  }

  # trip_id, arrival_time, departure_time, stop_id, stop_sequence,
  # stop_headsign, pickup_type, drop_off_type, shape_dist_traveled, timepoint

  expand_stop_times <- function(i){
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

    st_sub$arrival_time <-lubridate::seconds_to_period(st_sub$arrival_time) + st_sub$DepartureTime
    #st_sub$arrival_time <- st_sub$arrival_time + st_sub$DepartureTime
    st_sub$arrival_time <- sprintf('%02d:%02d', st_sub$arrival_time@hour, st_sub$arrival_time@minute)
    st_sub$arrival_time <- paste0(st_sub$arrival_time,":00")

    st_sub$departure_time <-lubridate::seconds_to_period(st_sub$departure_time) + st_sub$DepartureTime
    #st_sub$departure_time <- st_sub$departure_time + st_sub$DepartureTime
    st_sub$departure_time <- sprintf('%02d:%02d', st_sub$departure_time@hour, st_sub$departure_time@minute)
    st_sub$departure_time <- paste0(st_sub$departure_time,":00")

    st_sub$timepoint <- sapply(st_sub$timepoint,clean_timepoints)

    st_sub <- st_sub[,c("trip_id","arrival_time","departure_time","stop_id","stop_sequence","timepoint")]

    return(st_sub)
  }

  clean_timepoints <- function(tp){
    if(tp == "OTH"){
      return(1L)
    }else if(tp %in% c("PTP","TIP")){
      return(0L)
    }else{
      stop(paste0("Unknown timepoint type",tp))
    }
  }

  make_stop_times2 <- function(jps, trips, ss){
    jps <- jps[,c("JPS_id","From.Activity","From.StopPointRef","From.TimingStatus","To.WaitTime","To.Activity","To.StopPointRef","To.TimingStatus","RunTime","From.SequenceNumber", "To.SequenceNumber")]
    jps[] <- lapply(jps,as.character)
    #vj[] <- lapply(vj, as.character)
    #rts <- unique(vj$JourneyPatternRef)
    ss_join <- ss[,c("JourneyPatternSectionRefs","JourneyPatternID")]
    ss_join[] <- lapply(ss_join, as.character)
    jps$JPS_id <- as.character(jps$JPS_id)
    jps <- dplyr::left_join(jps,ss_join, by = c("JPS_id" = "JourneyPatternSectionRefs"))
    jps <- split(jps, jps$JourneyPatternID)

    stop_times <- lapply(1:length(jps), expand_stop_times)
    stop_times <- dplyr::bind_rows(stop_times)


    vj <- split(vj, vj$JourneyPatternRef)
    if(!identical(names(jps),names(vj))){message("Different Journey Patterns in jps and vj");stop()}
    stop_times_all <- list()
    for(i in seq(1,length(jps))){
      jps_sub = jps[[i]]
      vj_sub = vj[[i]]
      stop_times_sub = jps_sub[,c("To.StopPointRef","To.Activity","total_time","To.SequenceNumber","JourneyPatternID")]
      stop_times_top = data.frame(To.StopPointRef = jps_sub$From.StopPointRef[1],
                                  To.Activity = jps_sub$From.Activity[1],
                                  total_time = "0",
                                  To.SequenceNumber = "1",
                                  JourneyPatternID = jps_sub$JourneyPatternID[1],
                                  stringsAsFactors = F)
      stop_times_sub = dplyr::bind_rows(list(stop_times_top,stop_times_sub))
      stop_times_sub$cum_time = cumsum(stop_times_sub$total_time)
      stop_times_sub$cum_time <- lubridate::seconds_to_period(stop_times_sub$cum_time)
      stop_times_sub$arrival_time <- stop_times_sub$cum_time + lubridate::hms(vj_sub$DepartureTime[1])
      stop_times_sub$arrival_time <- sprintf('%02d:%02d:%02d', stop_times_sub$arrival_time@hour, stop_times_sub$arrival_time@minute, stop_times_sub$arrival_time@.Data)
      stop_times_sub$departure_time <- stop_times_sub$arrival_time
      stop_times_sub$pickup_type <- sapply(stop_times_sub$To.Activity, clean_activity, type = "pickup")
      stop_times_sub$drop_off_type <- sapply(stop_times_sub$To.Activity, clean_activity, type = "drop_off")
      stop_times_sub <- stop_times_sub[,c("JourneyPatternID","arrival_time","departure_time","To.StopPointRef","To.SequenceNumber","pickup_type","drop_off_type")]
      names(stop_times_sub) <- c("trip_id","arrival_time","departure_time","stop_id","stop_sequence","pickup_type","drop_off_type")
      stop_times_all[[i]] <- stop_times_sub
    }
    stop_times_all <- dplyr::bind_rows(stop_times_all)

    return(stop_times_all)
  }

  stop_times <-  make_stop_times(jps = JourneyPatternSections, vj = VehicleJourneys, ss = StandardService)






  Routes1 <- Routes[1,]
  RouteSections1 <- RouteSections[RouteSections$SectionID == Routes1$RouteSectionRef,]
  JourneyPatternSections1 <- JourneyPatternSections[JourneyPatternSections$RouteLinkRef %in% RouteSections1$LinkID, ]
  StandardService1 <- StandardService[StandardService$RouteRef == Routes1$PrivateCode, ]
  VehicleJourneys1 = VehicleJourneys[as.character(VehicleJourneys$JourneyPatternRef) %in% as.character(StandardService1$JourneyPatternID),]

}
