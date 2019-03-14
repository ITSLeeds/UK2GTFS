#obj = readRDS("example_import.Rds")

transxchange2gtfs <- function(obj, run_debug = T, cal = get_bank_holidays(), naptan = get_naptan()){
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

  if(nrow(VehicleJourneys_exclude) != 0 | nrow(VehicleJourneys_include) != 0 | nrow(VehicleJourneysTimingLinks) != 0){
    stop("Must consider VehicleJourneys")
  }

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
  #JourneyPatternSections$total_time <- JourneyPatternSections$RunTime + JourneyPatternSections$To.WaitTime

  ## stops ######################################
  stops <- StopPoints[,"StopPointRef", drop = FALSE]
  names(stops) <- c("stop_id")
  stops <- dplyr::left_join(stops, naptan, by = "stop_id")

  ## routes #####################################
  # route_id, agency_id, route_short_name, route_long_name, route_desc, route_type

  routes <- Services_main[c("ServiceCode","RegisteredOperatorRef","LineName","Description","Mode","Origin","Destination")]
  routes$route_long_name <- paste0(routes$Origin," - ",routes$Destination)
  names(routes) <- c("route_id","agency_id","route_short_name","route_desc","route_type","Origin","Destination","route_long_name")
  routes <- routes[,c("route_id","agency_id","route_short_name","route_long_name","route_desc","route_type")]
  routes$agency_id <- gsub("OId_","",routes$agency_id)

  clean_route_type <- function(rt){
    if(rt == "bus"){
      return(3)
    }else if(rt == "ferry"){
      return(4)
    }else{
      stop(paste0("Unknown route_type ",rt))
    }
  }

  routes$route_type <- sapply(routes$route_type, clean_route_type)

  ## Trips, Calendar, Calendar Dates #####################################
  # route_id, service_id, trip_id, trip_headsign, trip_short_name, direction_id,
  trips <- VehicleJourneys[,c("ServiceRef","JourneyPatternRef","VehicleJourneyCode","DepartureTime")]
  names(trips) <- c("route_id","service_id","trip_id","DepartureTime")

  # Calendar
  #service_id , mon, tues etc, start_date, end_date
  calendar <- VehicleJourneys[,c("JourneyPatternRef","days")]
  calendar <- unique(calendar)
  names(calendar) <- c("service_id","days")
  calendar <- calendar[!duplicated(calendar$service_id),]
  #calendar$start_date <- as.integer(gsub("-","",Services_main$StartDate))
  #calendar$end_date <- as.integer(gsub("-","",Services_main$EndDate))
  calendar$start_date <- gsub("-","",Services_main$StartDate)
  calendar$end_date <- gsub("-","",Services_main$EndDate)
  calendar$days <- as.character(calendar$days)

  clean_days <- function(days){
    if(days == "MondayToFriday"){
      days <- c(1,1,1,1,1,0,0)
    }else if(days == "Monday"){
      days <- c(1,0,0,0,0,0,0)
    }else if(days == "Tuesday"){
      days <- c(0,1,0,0,0,0,0)
    }else if(days == "Wednesday"){
      days <- c(0,0,1,0,0,0,0)
    }else if(days == "Thursday"){
      days <- c(0,0,0,1,0,0,0)
    }else if(days == "Friday"){
      days <- c(0,0,0,0,1,0,0)
    }else if(days == "Saturday"){
      days <- c(0,0,0,0,0,1,0)
    }else if(days == "Sunday"){
      days <- c(0,0,0,0,0,0,1)
    }else if(days == "HolidaysOnly"){
      days <- c(0,0,0,0,0,0,0)
    }else if(days %in% c("","MondayToSunday")){
      days <- c(1,1,1,1,1,1,1)
    }else if(days == "Monday Tuesday Wednesday Friday"){
      days <- c(1,1,1,0,1,0,0)
    }else{
      stop(paste0("Unknown day pattern: ",days))
    }
    names(days) <- NULL
    days
  }

  calendar_days <- as.data.frame(t(sapply(as.character(calendar$days), clean_days, USE.NAMES = F)))
  names(calendar_days) <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")

  calendar <- cbind(calendar, calendar_days)
  calendar <- calendar[,c("service_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday","start_date", "end_date")]
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

  if(all(calendar_dates$BankHolidaysOperate == "") & all(calendar_dates$BankHolidaysNoOperate == "")){
    calendar_dates <- data.frame(service_id = "", date = "", exception_type = "")
  }else{
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
        }else if(nrow(cal_sub) == 1){
          return(TRUE)
        }else{
          stop(paste0("Invalid number of rows ",i))
        }
      }else{
        return(TRUE)
      }

    }

    calendar_dates <- calendar_dates[ sapply(1:nrow(calendar_dates), check_duplicate_holidays), ]
    #cal <- get_bank_holidays()
    cal <- cal[cal$date >= as.Date(Services_main$StartDate) & cal$date <= as.Date(Services_main$EndDate), ]
    calendar_dates <- dplyr::left_join(calendar_dates, cal, by = c("hols" = "name"))
    calendar_dates <- calendar_dates[,c("JourneyPatternRef","date","exception_type")]
    names(calendar_dates) <- c("service_id","date","exception_type")
    calendar_dates$date <- gsub("-","",calendar_dates$date)
  }


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


  # trip_id, arrival_time, departure_time, stop_id, stop_sequence,
  # stop_headsign, pickup_type, drop_off_type, shape_dist_traveled, timepoint

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

  clean_timepoints <- function(tp){
    if(tp == "OTH"){
      return(1L)
    }else if(tp %in% c("PTP","TIP","PPT")){
      return(0L)
    }else{
      stop(paste0("Unknown timepoint type: ",tp))
    }
  }

  make_stop_times <- function(jps, trips, ss){
    jps <- jps[,c("JPS_id","From.Activity","From.StopPointRef","From.TimingStatus","To.WaitTime","To.Activity","To.StopPointRef","To.TimingStatus","RunTime","From.SequenceNumber", "To.SequenceNumber")]
    jps[] <- lapply(jps,as.character)
    #vj[] <- lapply(vj, as.character)
    #rts <- unique(vj$JourneyPatternRef)
    ss_join <- ss[,c("JourneyPatternSectionRefs","JourneyPatternID")]
    ss_join[] <- lapply(ss_join, as.character)
    jps$JPS_id <- as.character(jps$JPS_id)
    jps <- dplyr::left_join(jps,ss_join, by = c("JPS_id" = "JourneyPatternSectionRefs"))
    jps <- split(jps, jps$JourneyPatternID)

    stop_times <- lapply(1:length(jps), expand_stop_times, jps = jps)
    stop_times <- dplyr::bind_rows(stop_times)
    return(stop_times)
  }

  stop_times <-  make_stop_times(jps = JourneyPatternSections, trips = trips, ss = StandardService)

  trips <- trips[,c("route_id","service_id","trip_id")]

  #agency
  # agency_id, agency_name, agency_url, agency_timezone

  agency <- data.frame(agency_id = Operators$NationalOperatorCode,
                       agency_name = Operators$TradingName,
                       agency_url = "http://www.unknown.com",
                       agency_timezone = "Europe/London",
                       agency_lang = "en")


  # rebuild ids
  trips$service_id <- gsub("[[:punct:]]","",trips$service_id)
  trips$trip_id <- gsub("[[:punct:]]","",trips$trip_id)
  trips$route_id <- gsub("[[:punct:]]","",trips$route_id)

  routes$route_id <- gsub("[[:punct:]]","",routes$route_id)

  calendar$service_id <- gsub("[[:punct:]]","",calendar$service_id)
  calendar_dates$service_id <- gsub("[[:punct:]]","",calendar_dates$service_id)

  stop_times$trip_id <- gsub("[[:punct:]]","",stop_times$trip_id)

  # trips$trip_id <- seq(1L:nrow(trips))
  # trips$route_id <- as.integer(as.factor(trips$route_id))
  # #trips$service_id <- as.integer(as.factor(trips$service_id))
  #
  #
  # join_trips   <- trips[,c("trip_id","trip_id")]
  # join_routes  <- trips[,c("route_id","route_id")]
  # join_service <- trips[,c("service_id","service_id")]
  #
  # trips <- trips[,c("route_id","service_id","trip_id")]
  #
  # routes <- dplyr::left_join(routes, join_routes, by = "route_id")
  # routes <- routes[,c("route_id","agency_id","route_short_name", "route_long_name","route_desc","route_type")]
  #
  # calendar <- dplyr::left_join(calendar, join_service, by = "service_id")
  # calendar <- calendar[,c("service_id", "start_date", "end_date","monday","tuesday","wednesday","thursday","friday","saturday","sunday")]
  #
  # calendar_dates <- dplyr::left_join(calendar_dates, join_service, by = "service_id")
  # calendar_dates <- calendar_dates[,c("service_id","date","exception_type")]



  res_final <- list(agency,stops,routes,trips,stop_times,calendar,calendar_dates)
  names(res_final) <- c("agency","stops","routes","trips","stop_times","calendar","calendar_dates")
  return(res_final)

}
