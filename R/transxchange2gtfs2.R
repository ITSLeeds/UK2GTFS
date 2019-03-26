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

  # Early Subsets - move to import code
  VehicleJourneys <- VehicleJourneys[,c("VehicleJourneyCode","ServiceRef","JourneyPatternRef","DepartureTime","days","BankHolidaysOperate","BankHolidaysNoOperate")]



  # Check on exclusions
  if(nrow(VehicleJourneys_include) != 0  # | nrow(VehicleJourneys_exclude) != 0
     ){
    stop("Must consider VehicleJourneys")
  }

  # if(nrow(VehicleJourneysTimingLinks) != 0 ){
  #   stop("Must consider TimingLinks")
  # }

  if(nrow(VehicleJourneys_exclude) != 0){
    names(VehicleJourneys_exclude) <- c("ExStartTime","ExEndTime","VehicleJourneyCode")
    VehicleJourneys_exclude[] <- lapply(VehicleJourneys_exclude, as.character)
    VehicleJourneys_exclude$ExStartTime <- as.Date(VehicleJourneys_exclude$ExStartTime)
    VehicleJourneys_exclude$ExEndTime <- as.Date(VehicleJourneys_exclude$ExEndTime)
  }


  ## JourneyPatternSections #####################
  if(run_debug){
    chk <- gsub("[0-9]","",JourneyPatternSections$RunTime)
    chk <- unique(chk)
    if(!all(chk %in% c("PTM","PTS","PTMS"))){
      stop("Unknown time formats")
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
  routes$route_type <- sapply(routes$route_type, clean_route_type)


  ######


  #####





  # New Stratergy creat unique service for each trip then group togther at the end
  if(nrow(VehicleJourneys_exclude) != 0){
    if(any(duplicated(VehicleJourneys_exclude$VehicleJourneyCode))){
      chk <- VehicleJourneys_exclude$ExStartTime <= as.Date(Services_main$StartDate) | VehicleJourneys_exclude$ExEndTime >= as.Date(Services_main$EndDate)
      VehicleJourneys_exclude_middle <- VehicleJourneys_exclude[!chk,]
      VehicleJourneys_exclude <- VehicleJourneys_exclude[chk,]
      if(any(duplicated(VehicleJourneys_exclude$VehicleJourneyCode))){stop("Duplicated Exclusions")}
      exclude_dates <- lapply(seq(1,nrow(VehicleJourneys_exclude_middle)),
                              function(x){seq(VehicleJourneys_exclude_middle$ExStartTime[x],
                                              VehicleJourneys_exclude_middle$ExEndTime[x],
                                              by = "days")})
      calendar_exclusions <- data.frame(VehicleJourneyCode = rep(VehicleJourneys_exclude_middle$VehicleJourneyCode, times = lengths(exclude_dates)),
                                                    date = as.Date(unlist(exclude_dates), origin = "1970-01-01"), exception_type = 2)


    }
    VehicleJourneys <- dplyr::left_join(VehicleJourneys, VehicleJourneys_exclude, by = "VehicleJourneyCode")
    # Remove Any Vehicle Jounrey cancled for the whole period
    VehicleJourneys$ExStartTime <- as.Date(as.character(VehicleJourneys$ExStartTime))
    VehicleJourneys$ExEndTime <- as.Date(as.character(VehicleJourneys$ExEndTime))
    chk_start <- VehicleJourneys$ExStartTime <= as.Date(Services_main$StartDate)
    chk_start[is.na(chk_start)] <- FALSE
    chk_end <- VehicleJourneys$ExEndTime >= as.Date(Services_main$EndDate)
    chk_end[is.na(chk_end)] <- FALSE
    VehicleJourneys <- VehicleJourneys[!(chk_start & chk_end),]
    # If any left break up the Vehicle Jounreys
    if(!all(is.na(VehicleJourneys$ExStartTime))){
      VJ_Service <- VehicleJourneys[,c("JourneyPatternRef","VehicleJourneyCode","ExStartTime","ExEndTime")]
      VJ_Service <- dplyr::group_by(VJ_Service, JourneyPatternRef, ExStartTime, ExEndTime)
      VJ_Service$groups <- group_indices(VJ_Service)
      VJ_Service$groups <- sapply(seq(1, nrow(VJ_Service)), function(x){VJ_Service$groups[x] - min(VJ_Service$groups[VJ_Service$JourneyPatternRef == VJ_Service$JourneyPatternRef[x]]) + 1})

      patt <- c(letters, paste0("a",letters))
      if(max(VJ_Service$groups) > length(patt)){stop("Too many different groups of services")}
      VJ_Service$groups <- patt[VJ_Service$groups]
      VJ_Service$groups[VJ_Service$groups == "a"] <- ""

      VJ_Service$JourneyPatternRef2 <- paste0(VJ_Service$JourneyPatternRef, VJ_Service$groups)
      VJ_Service <- VJ_Service[,c("VehicleJourneyCode","JourneyPatternRef2")]
      VehicleJourneys <- dplyr::left_join(VehicleJourneys, VJ_Service, by = "VehicleJourneyCode")
      names(VehicleJourneys)[names(VehicleJourneys) == "JourneyPatternRef"] <- "JourneyPatternRef_orig"
      names(VehicleJourneys)[names(VehicleJourneys) == "JourneyPatternRef2"] <- "JourneyPatternRef"
    }
  }else{
    VehicleJourneys$JourneyPatternRef_orig <- VehicleJourneys$JourneyPatternRef
  }

  VehicleJourneys$service_id_temp <- seq(1, nrow(VehicleJourneys))

  trips <- VehicleJourneys[,c("ServiceRef","service_id_temp","VehicleJourneyCode","DepartureTime","JourneyPatternRef_orig")]
  names(trips) <-           c("route_id",  "service_id_temp","trip_id",           "DepartureTime","JourneyPatternRef_orig")

  # Calendar
  #service_id , mon, tues etc, start_date, end_date
  if(nrow(VehicleJourneys_exclude) != 0){
    calendar <- VehicleJourneys[,c("service_id_temp","days","ExStartTime","ExEndTime")]
    calendar$start_date <- as.Date(Services_main$StartDate)
    calendar$end_date <- as.Date(Services_main$EndDate)
    # Trim down end_date by exclusions
    chk_start <- calendar$ExStartTime <= calendar$end_date
    chk_start[is.na(chk_start)] <- FALSE
    chk_end <- calendar$ExEndTime >= calendar$end_date
    chk_end[is.na(chk_end)] <- FALSE
    calendar$end_date <- as.Date(ifelse(chk_start & chk_end, calendar$ExStartTime - 1, calendar$end_date), origin = "1970-01-01")

    # Trim down start_date by exclusions
    chk_start <- calendar$ExStartTime <= calendar$start_date
    chk_start[is.na(chk_start)] <- FALSE
    chk_end <- calendar$ExEndTime >= calendar$start_date
    chk_end[is.na(chk_end)] <- FALSE
    calendar$start_date <- as.Date(ifelse(chk_start & chk_end, calendar$ExEndTime + 1, calendar$start_date), origin = "1970-01-01")

    #calendar_exclusions <- calendar

    calendar$start_date <- gsub("-","",as.character(calendar$start_date))
    calendar$end_date <- gsub("-","",as.character(calendar$end_date))

    calendar <- calendar[,c("service_id_temp","days","start_date","end_date")]

  }else{
    calendar <- VehicleJourneys[,c("service_id_temp","days")]
    calendar$start_date <- gsub("-","",Services_main$StartDate)
    calendar$end_date <- gsub("-","",Services_main$EndDate)
  }

  # calendar_dates
  # service_id, date, exception_type
  calendar_dates <- VehicleJourneys[,c("service_id_temp","BankHolidaysOperate","BankHolidaysNoOperate")]
  calendar_dates <- dplyr::group_by(calendar_dates, service_id_temp)
  calendar_dates <- dplyr::summarise(calendar_dates, BankHolidaysOperate = paste(unique(BankHolidaysOperate), collapse = " "),
                                     BankHolidaysNoOperate = paste(unique(BankHolidaysNoOperate), collapse = " "))
  calendar_dates$BankHolidaysOperate[calendar_dates$BankHolidaysOperate == "AllBankHolidays"] <- paste(cal$name, collapse =  " ")
  calendar_dates$BankHolidaysNoOperate[calendar_dates$BankHolidaysNoOperate == "AllBankHolidays"] <- paste(cal$name, collapse =  " ")

  if(all(calendar_dates$BankHolidaysOperate == "") & all(calendar_dates$BankHolidaysNoOperate == "")){
    calendar_dates <- data.frame(service_id_temp = integer(), date = integer(), exception_type = integer())
  }else{
    calendar_dates_inc <- break_up_holidays2(calendar_dates, "BankHolidaysOperate")
    calendar_dates_exc <- break_up_holidays2(calendar_dates, "BankHolidaysNoOperate")
    calendar_dates <- rbind(calendar_dates_inc,calendar_dates_exc)
    rm(calendar_dates_inc,calendar_dates_exc)

    #calendar_dates <- calendar_dates[ sapply(1:nrow(calendar_dates), check_duplicate_holidays), ]
    #cal <- get_bank_holidays()
    cal <- cal[cal$date >= as.Date(Services_main$StartDate) & cal$date <= as.Date(Services_main$EndDate), ]
    calendar_dates <- dplyr::left_join(calendar_dates, cal, by = c("hols" = "name"))
    calendar_dates <- calendar_dates[,c("service_id_temp","date","exception_type")]
    calendar_dates$date <- gsub("-","",calendar_dates$date)
    calendar_dates <- calendar_dates[!is.na(calendar_dates$date),] #na caused by exluding holiday outside date range
  }

  if(exists("calendar_exclusions")){
    # Trim down to exlusions during the start and end period
    # chk_start <- calendar_exclusions$ExStartTime >= calendar_exclusions$start_date
    # chk_start[is.na(chk_start)] <- FALSE
    # chk_end <- calendar_exclusions$ExEndTime <= calendar_exclusions$start_date
    # chk_end[is.na(chk_end)] <- FALSE
    #
    # calendar_exclusions <- calendar_exclusions[chk_start & chk_end,]
    # if(nrow(calendar_exclusions) > 0){
    #   stop("Need to deal with mid service exclusions")
    # }
    calendar_exclusions <- dplyr::left_join(calendar_exclusions, trips[,c("trip_id","service_id_temp")], by = c("VehicleJourneyCode" = "trip_id"))
    calendar_exclusions <- calendar_exclusions[,c("service_id_temp", "date","exception_type")]
    calendar_exclusions$date <- gsub("-","",as.character(calendar_exclusions$date))
    calendar_dates <- rbind(calendar_dates, calendar_exclusions)
  }



  # Combine identical service patterns
  calendar_dates_summary <- dplyr::group_by(calendar_dates, service_id_temp)
  calendar_dates_summary <- dplyr::summarise(calendar_dates_summary, pattern = paste(c(date, exception_type), collapse = ""))
  calendar_dates_summary$service_id_temp <- as.integer(as.character(calendar_dates_summary$service_id_temp))
  calendar <- dplyr::left_join(calendar, calendar_dates_summary, by = "service_id_temp")
  calendar <- dplyr::group_by(calendar, days, start_date, end_date, pattern)
  calendar$service_id <- dplyr::group_indices(calendar)

  calendar_join <- calendar[,c("service_id_temp","service_id")]
  calendar <- calendar[,c("service_id","days","start_date","end_date")]
  calendar <- unique(calendar)

  calendar_days <- as.data.frame(t(sapply(as.character(calendar$days), clean_days, USE.NAMES = F)))
  names(calendar_days) <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")

  calendar <- cbind(calendar, calendar_days)
  calendar <- calendar[,c("service_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday","start_date", "end_date")]
  rm(calendar_days)

  calendar_dates <- dplyr::left_join(calendar_dates, calendar_join, by = "service_id_temp")
  calendar_dates <- calendar_dates[,c("service_id","date","exception_type")]
  calendar_dates <- unique(calendar_dates)

  trips <- dplyr::left_join(trips, calendar_join, by = "service_id_temp")
  trips <- trips[,c("route_id","service_id","trip_id","DepartureTime","JourneyPatternRef_orig")]


  ## stop_times #################################



  # trip_id, arrival_time, departure_time, stop_id, stop_sequence,
  # stop_headsign, pickup_type, drop_off_type, shape_dist_traveled, timepoint



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
  #trips$service_id <- gsub("[[:punct:]]","",trips$service_id)
  trips$trip_id <- gsub("[[:punct:]]","",trips$trip_id)
  trips$route_id <- gsub("[[:punct:]]","",trips$route_id)

  routes$route_id <- gsub("[[:punct:]]","",routes$route_id)

  #calendar$service_id <- gsub("[[:punct:]]","",calendar$service_id)
  #calendar_dates$service_id <- gsub("[[:punct:]]","",calendar_dates$service_id)

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
