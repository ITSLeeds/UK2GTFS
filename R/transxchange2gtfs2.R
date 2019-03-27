#obj = readRDS("example_import.Rds")

transxchange2gtfs <- function(obj, run_debug = T, cal = get_bank_holidays(), naptan = get_naptan()){
  JourneyPatternSections  <-  obj[["JourneyPatternSections"]]
  Operators               <-  obj[["Operators"]]
  Routes                  <-  obj[["Routes"]]
  #RouteSections           <-  obj[["RouteSections"]]
  Services_main           <-  obj[["Services_main"]]
  StandardService         <-  obj[["StandardService"]]
  Services_NonOperation   <-  obj[["Services_NonOperation"]]
  StopPoints              <-  obj[["StopPoints"]]
  VehicleJourneys         <-  obj[["VehicleJourneys"]]
  VehicleJourneys_exclude <-  obj[["VehicleJourneys_exclude"]]
  VehicleJourneys_include <-  obj[["VehicleJourneys_include"]]
  #VehicleJourneysTimingLinks <- obj[["VehicleJourneysTimingLinks"]]

  # Early Subsets - move to import code
  VehicleJourneys <- VehicleJourneys[,c("VehicleJourneyCode","ServiceRef","JourneyPatternRef","DepartureTime","days","BankHolidaysOperate","BankHolidaysNoOperate")]
  Services_main$StartDate <- as.Date(Services_main$StartDate)
  Services_main$EndDate <- as.Date(Services_main$EndDate)

  # Check on exclusions
  if(nrow(VehicleJourneys_include) != 0  # | nrow(VehicleJourneys_exclude) != 0
     ){
    stop("Must consider VehicleJourneys")
  }

  # Check on missing
  if(!is.null(Services_NonOperation)){
    stop("Must consider Services_NonOperation")
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


  # Journey Pattern Sections ------------------------------------------------

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


  # stops -------------------------------------------------------------------

  stops <- StopPoints[,"StopPointRef", drop = FALSE]
  names(stops) <- c("stop_id")
  stops <- dplyr::left_join(stops, naptan, by = "stop_id")


  # routes ------------------------------------------------------------------
  # route_id, agency_id, route_short_name, route_long_name, route_desc, route_type

  routes <- Services_main[c("ServiceCode","RegisteredOperatorRef","LineName","Description","Mode","Origin","Destination")]
  routes$route_long_name <- paste0(routes$Origin," - ",routes$Destination)
  names(routes) <- c("route_id","agency_id","route_short_name","route_desc","route_type","Origin","Destination","route_long_name")
  routes <- routes[,c("route_id","agency_id","route_short_name","route_long_name","route_desc","route_type")]
  routes$agency_id <- gsub("OId_","",routes$agency_id)
  routes$route_type <- sapply(routes$route_type, clean_route_type)


  # agency ------------------------------------------------------------------
  # agency_id, agency_name, agency_url, agency_timezone

  agency <- data.frame(agency_id = Operators$NationalOperatorCode,
                       agency_name = Operators$TradingName,
                       agency_url = "http://www.unknown.com",
                       agency_timezone = "Europe/London",
                       agency_lang = "en")


  # trips calendar calendar_dates -------------------------------------------------
  ###### Redo: Again
  trips <- VehicleJourneys[,c("ServiceRef","VehicleJourneyCode","DepartureTime","JourneyPatternRef","days")]
  names(trips) <-           c("route_id"  ,"trip_id",           "DepartureTime","JourneyPatternRef","days")

  trips$StartDate <- as.Date(Services_main$StartDate)
  trips$EndDate   <- as.Date(Services_main$EndDate)
  #trips$service_id_temp <- seq(1,nrow(trips))
  trips$trip_id <- as.character(trips$trip_id)


  # Step 1: Do we Have any exclusions
  if(nrow(VehicleJourneys_exclude) != 0){
    # Yes - Build Exclusions
    # Split Exclusions by Vehicle Jounrey
    trip_exc <- split(VehicleJourneys_exclude, VehicleJourneys_exclude$VehicleJourneyCode)
    trip_split <- split(trips,trips$trip_id)
    trip_split <- lapply(trip_split, exclude_trips, trip_exc = trip_exc)
    trips <- dplyr::bind_rows(trip_split)
    trips_exclude <- trips[,c("trip_id","exclude_days")]
    trips_exclude <- trips_exclude[lengths(trips_exclude$exclude_days) > 0,] #For lists
    trips_exclude <- trips_exclude[!is.na(trips_exclude$exclude_days),] #For NAs
    if(nrow(trips_exclude) > 0){
      trips_exclude <- data.frame(trip_id = rep(trips_exclude$trip_id, times = lengths(trips_exclude$exclude_days)),
                                  date = as.Date(unlist(trips_exclude$exclude_days), origin = "1970-01-01"))
      trips_exclude$exception_type <- 2
    }else{
      rm(trips_exclude)
    }

  }

  # Step 2: Prep the Bank Holidays
  cal <- cal[cal$date >= Services_main$StartDate & cal$date <= Services_main$EndDate, ]
  bank_holidays <- VehicleJourneys[,c("VehicleJourneyCode","BankHolidaysOperate","BankHolidaysNoOperate")]
  bank_holidays[] <- lapply(bank_holidays, as.character)
  bank_holidays <- unique(bank_holidays)
  names(bank_holidays) <-           c("trip_id",           "BankHolidaysOperate","BankHolidaysNoOperate")
  bank_holidays$BankHolidaysOperate[bank_holidays$BankHolidaysOperate == "AllBankHolidays"] <- paste(cal$name, collapse =  " ")
  bank_holidays$BankHolidaysNoOperate[bank_holidays$BankHolidaysNoOperate == "AllBankHolidays"] <- paste(cal$name, collapse =  " ")

  bank_holidays_inc <- break_up_holidays2(bank_holidays, "BankHolidaysOperate")
  bank_holidays_exc <- break_up_holidays2(bank_holidays, "BankHolidaysNoOperate")
  if(!is.null(bank_holidays_inc)){
    bank_holidays_inc <- dplyr::left_join(bank_holidays_inc, cal, by = c("hols" = "name"))
  }
  if(!is.null(bank_holidays_exc)){
    bank_holidays_exc <- dplyr::left_join(bank_holidays_exc, cal, by = c("hols" = "name"))
  }
  bank_holidays <- rbind(bank_holidays_inc, bank_holidays_exc)
  bank_holidays <- bank_holidays[,c("trip_id","date","exception_type")]

  # Step 3: Merge Exclusions and bank_holidays, then summarise the exclusions
  if(exists("trips_exclude")){
    calendar_dates <- rbind(trips_exclude, bank_holidays)
  }else{
    calendar_dates <- bank_holidays
  }

  # Step 4: Make the calendar
  calendar <- trips[,c("trip_id","StartDate", "EndDate","days")]
  names(calendar) <- c("trip_id","start_date","end_date","days")
  calendar$start_date <- gsub("-","",calendar$start_date)
  calendar$end_date <- gsub("-","",calendar$end_date)

  # Step 5: Make the unique service_id
  if(is.null(calendar_dates)){
    calendar_dates <- data.frame(trip_id = character(),
                                  date = character(),
                                  exception_type = character())
    calendar_summary <- dplyr::group_by(calendar, start_date, end_date, days)
  }else{
    calendar_dates_summary <- dplyr::group_by(calendar_dates, trip_id)
    calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
                                               pattern = paste(c(date, exception_type), collapse = ""))
    calendar_summary <- dplyr::left_join(calendar, calendar_dates_summary, by = "trip_id")
    calendar_summary <- dplyr::group_by(calendar_summary, start_date, end_date, days, pattern)
  }

  calendar_summary$service_id <- dplyr::group_indices(calendar_summary)
  calendar_summary <- calendar_summary[,c("trip_id","service_id")]
  calendar <- dplyr::left_join(calendar, calendar_summary, by = "trip_id")
  calendar_dates <- dplyr::left_join(calendar_dates, calendar_summary, by = "trip_id")
  trips <- dplyr::left_join(trips, calendar_summary, by = "trip_id")

  calendar <- calendar[,c("service_id","start_date","end_date","days")]
  calendar <- unique(calendar)

  calendar_dates <- calendar_dates[,c("service_id","date","exception_type")]
  calendar_dates <- unique(calendar_dates)
  calendar_dates$date <- gsub("-","",calendar_dates$date)

  if(run_debug){
    if(any(is.na(calendar_dates))){stop("NA values in calendar_dates")}
  }

  trips <- trips[,c("route_id","service_id","trip_id","DepartureTime","JourneyPatternRef")]

  # Step 6: Make calendar days of the week
  calendar_days <- as.data.frame(t(sapply(as.character(calendar$days), clean_days, USE.NAMES = F)))
  names(calendar_days) <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")

  calendar <- cbind(calendar, calendar_days)
  calendar <- calendar[,c("service_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday","start_date", "end_date")]
  rm(calendar_days)

  # Step 7: Make stop_times

  stop_times <-  make_stop_times(jps = JourneyPatternSections, trips = trips, ss = StandardService)

  # old code ----------------------------------------------------------------

  #   # New Stratergy creat unique service for each trip then group togther at the end
  # if(nrow(VehicleJourneys_exclude) != 0){
  #   #if(any(duplicated(VehicleJourneys_exclude$VehicleJourneyCode))){
  #     chk <- VehicleJourneys_exclude$ExStartTime <= as.Date(Services_main$StartDate) | VehicleJourneys_exclude$ExEndTime >= as.Date(Services_main$EndDate)
  #     VehicleJourneys_exclude_middle <- VehicleJourneys_exclude[!chk,]
  #     VehicleJourneys_exclude <- VehicleJourneys_exclude[chk,]
  #     if(any(duplicated(VehicleJourneys_exclude$VehicleJourneyCode))){stop("Duplicated Exclusions")}
  #     exclude_dates <- lapply(seq(1,nrow(VehicleJourneys_exclude_middle)),
  #                             function(x){seq(VehicleJourneys_exclude_middle$ExStartTime[x],
  #                                             VehicleJourneys_exclude_middle$ExEndTime[x],
  #                                             by = "days")})
  #     calendar_exclusions <- data.frame(VehicleJourneyCode = rep(VehicleJourneys_exclude_middle$VehicleJourneyCode, times = lengths(exclude_dates)),
  #                                                   date = as.Date(unlist(exclude_dates), origin = "1970-01-01"), exception_type = 2)
  #
  #
  #   #}
  #   VehicleJourneys <- dplyr::left_join(VehicleJourneys, VehicleJourneys_exclude, by = "VehicleJourneyCode")
  #   # Remove Any Vehicle Jounrey cancled for the whole period
  #   VehicleJourneys$ExStartTime <- as.Date(as.character(VehicleJourneys$ExStartTime))
  #   VehicleJourneys$ExEndTime <- as.Date(as.character(VehicleJourneys$ExEndTime))
  #   chk_start <- VehicleJourneys$ExStartTime <= as.Date(Services_main$StartDate)
  #   chk_start[is.na(chk_start)] <- FALSE
  #   chk_end <- VehicleJourneys$ExEndTime >= as.Date(Services_main$EndDate)
  #   chk_end[is.na(chk_end)] <- FALSE
  #   VehicleJourneys <- VehicleJourneys[!(chk_start & chk_end),]
  #   # If any left break up the Vehicle Jounreys
  #   if(!all(is.na(VehicleJourneys$ExStartTime))){
  #     VJ_Service <- VehicleJourneys[,c("JourneyPatternRef","VehicleJourneyCode","ExStartTime","ExEndTime")]
  #     VJ_Service <- dplyr::group_by(VJ_Service, JourneyPatternRef, ExStartTime, ExEndTime)
  #     VJ_Service$groups <- group_indices(VJ_Service)
  #     VJ_Service$groups <- sapply(seq(1, nrow(VJ_Service)), function(x){VJ_Service$groups[x] - min(VJ_Service$groups[VJ_Service$JourneyPatternRef == VJ_Service$JourneyPatternRef[x]]) + 1})
  #
  #     patt <- c(letters, paste0("a",letters))
  #     if(max(VJ_Service$groups) > length(patt)){stop("Too many different groups of services")}
  #     VJ_Service$groups <- patt[VJ_Service$groups]
  #     VJ_Service$groups[VJ_Service$groups == "a"] <- ""
  #
  #     VJ_Service$JourneyPatternRef2 <- paste0(VJ_Service$JourneyPatternRef, VJ_Service$groups)
  #     VJ_Service <- VJ_Service[,c("VehicleJourneyCode","JourneyPatternRef2")]
  #     VehicleJourneys <- dplyr::left_join(VehicleJourneys, VJ_Service, by = "VehicleJourneyCode")
  #     names(VehicleJourneys)[names(VehicleJourneys) == "JourneyPatternRef"] <- "JourneyPatternRef_orig"
  #     names(VehicleJourneys)[names(VehicleJourneys) == "JourneyPatternRef2"] <- "JourneyPatternRef"
  #   }
  # }else{
  #   VehicleJourneys$JourneyPatternRef_orig <- VehicleJourneys$JourneyPatternRef
  # }
  #
  # VehicleJourneys$service_id_temp <- seq(1, nrow(VehicleJourneys))
  #
  # trips <- VehicleJourneys[,c("ServiceRef","service_id_temp","VehicleJourneyCode","DepartureTime","JourneyPatternRef_orig")]
  # names(trips) <-           c("route_id",  "service_id_temp","trip_id",           "DepartureTime","JourneyPatternRef_orig")
  #
  # # Calendar
  # #service_id , mon, tues etc, start_date, end_date
  # if(nrow(VehicleJourneys_exclude) != 0){
  #   calendar <- VehicleJourneys[,c("service_id_temp","days","ExStartTime","ExEndTime")]
  #   calendar$start_date <- as.Date(Services_main$StartDate)
  #   calendar$end_date <- as.Date(Services_main$EndDate)
  #   # Trim down end_date by exclusions
  #   chk_start <- calendar$ExStartTime <= calendar$end_date
  #   chk_start[is.na(chk_start)] <- FALSE
  #   chk_end <- calendar$ExEndTime >= calendar$end_date
  #   chk_end[is.na(chk_end)] <- FALSE
  #   calendar$end_date <- as.Date(ifelse(chk_start & chk_end, calendar$ExStartTime - 1, calendar$end_date), origin = "1970-01-01")
  #
  #   # Trim down start_date by exclusions
  #   chk_start <- calendar$ExStartTime <= calendar$start_date
  #   chk_start[is.na(chk_start)] <- FALSE
  #   chk_end <- calendar$ExEndTime >= calendar$start_date
  #   chk_end[is.na(chk_end)] <- FALSE
  #   calendar$start_date <- as.Date(ifelse(chk_start & chk_end, calendar$ExEndTime + 1, calendar$start_date), origin = "1970-01-01")
  #
  #   #calendar_exclusions <- calendar
  #
  #   calendar$start_date <- gsub("-","",as.character(calendar$start_date))
  #   calendar$end_date <- gsub("-","",as.character(calendar$end_date))
  #
  #   calendar <- calendar[,c("service_id_temp","days","start_date","end_date")]
  #
  # }else{
  #   calendar <- VehicleJourneys[,c("service_id_temp","days")]
  #   calendar$start_date <- gsub("-","",Services_main$StartDate)
  #   calendar$end_date <- gsub("-","",Services_main$EndDate)
  # }
  #
  # # calendar_dates
  # # service_id, date, exception_type
  # calendar_dates <- VehicleJourneys[,c("service_id_temp","BankHolidaysOperate","BankHolidaysNoOperate")]
  # calendar_dates <- dplyr::group_by(calendar_dates, service_id_temp)
  # calendar_dates <- dplyr::summarise(calendar_dates, BankHolidaysOperate = paste(unique(BankHolidaysOperate), collapse = " "),
  #                                    BankHolidaysNoOperate = paste(unique(BankHolidaysNoOperate), collapse = " "))
  # calendar_dates$BankHolidaysOperate[calendar_dates$BankHolidaysOperate == "AllBankHolidays"] <- paste(cal$name, collapse =  " ")
  # calendar_dates$BankHolidaysNoOperate[calendar_dates$BankHolidaysNoOperate == "AllBankHolidays"] <- paste(cal$name, collapse =  " ")
  #
  # if(all(calendar_dates$BankHolidaysOperate == "") & all(calendar_dates$BankHolidaysNoOperate == "")){
  #   calendar_dates <- data.frame(service_id_temp = integer(), date = integer(), exception_type = integer())
  # }else{
  #   calendar_dates_inc <- break_up_holidays2(calendar_dates, "BankHolidaysOperate")
  #   calendar_dates_exc <- break_up_holidays2(calendar_dates, "BankHolidaysNoOperate")
  #   calendar_dates <- rbind(calendar_dates_inc,calendar_dates_exc)
  #   rm(calendar_dates_inc,calendar_dates_exc)
  #
  #   #calendar_dates <- calendar_dates[ sapply(1:nrow(calendar_dates), check_duplicate_holidays), ]
  #   #cal <- get_bank_holidays()
  #   cal <- cal[cal$date >= as.Date(Services_main$StartDate) & cal$date <= as.Date(Services_main$EndDate), ]
  #   calendar_dates <- dplyr::left_join(calendar_dates, cal, by = c("hols" = "name"))
  #   calendar_dates <- calendar_dates[,c("service_id_temp","date","exception_type")]
  #   calendar_dates$date <- gsub("-","",calendar_dates$date)
  #   calendar_dates <- calendar_dates[!is.na(calendar_dates$date),] #na caused by exluding holiday outside date range
  # }
  #
  # if(exists("calendar_exclusions")){
  #   # Trim down to exlusions during the start and end period
  #   # chk_start <- calendar_exclusions$ExStartTime >= calendar_exclusions$start_date
  #   # chk_start[is.na(chk_start)] <- FALSE
  #   # chk_end <- calendar_exclusions$ExEndTime <= calendar_exclusions$start_date
  #   # chk_end[is.na(chk_end)] <- FALSE
  #   #
  #   # calendar_exclusions <- calendar_exclusions[chk_start & chk_end,]
  #   # if(nrow(calendar_exclusions) > 0){
  #   #   stop("Need to deal with mid service exclusions")
  #   # }
  #   calendar_exclusions <- dplyr::left_join(calendar_exclusions, trips[,c("trip_id","service_id_temp")], by = c("VehicleJourneyCode" = "trip_id"))
  #   calendar_exclusions <- calendar_exclusions[,c("service_id_temp", "date","exception_type")]
  #   calendar_exclusions$date <- gsub("-","",as.character(calendar_exclusions$date))
  #   calendar_dates <- rbind(calendar_dates, calendar_exclusions)
  # }
  #
  #
  #
  # # Combine identical service patterns
  # calendar_dates_summary <- dplyr::group_by(calendar_dates, service_id_temp)
  # calendar_dates_summary <- dplyr::summarise(calendar_dates_summary, pattern = paste(c(date, exception_type), collapse = ""))
  # calendar_dates_summary$service_id_temp <- as.integer(as.character(calendar_dates_summary$service_id_temp))
  # calendar <- dplyr::left_join(calendar, calendar_dates_summary, by = "service_id_temp")
  # calendar <- dplyr::group_by(calendar, days, start_date, end_date, pattern)
  # calendar$service_id <- dplyr::group_indices(calendar)
  #
  # calendar_join <- calendar[,c("service_id_temp","service_id")]
  # calendar <- calendar[,c("service_id","days","start_date","end_date")]
  # calendar <- unique(calendar)
  #
  # calendar_days <- as.data.frame(t(sapply(as.character(calendar$days), clean_days, USE.NAMES = F)))
  # names(calendar_days) <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
  #
  # calendar <- cbind(calendar, calendar_days)
  # calendar <- calendar[,c("service_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday","start_date", "end_date")]
  # rm(calendar_days)
  #
  # calendar_dates <- dplyr::left_join(calendar_dates, calendar_join, by = "service_id_temp")
  # calendar_dates <- calendar_dates[,c("service_id","date","exception_type")]
  # calendar_dates <- unique(calendar_dates)
  #
  # trips <- dplyr::left_join(trips, calendar_join, by = "service_id_temp")
  # trips <- trips[,c("route_id","service_id","trip_id","DepartureTime","JourneyPatternRef_orig")]
  #
  #
  #
  #
  # trips <- trips[,c("route_id","service_id","trip_id")]
  #
  #
  #
  #
  # # stop_times --------------------------------------------------------------
  # # trip_id, arrival_time, departure_time, stop_id, stop_sequence,
  # # stop_headsign, pickup_type, drop_off_type, shape_dist_traveled, timepoint
  #
  # stop_times <-  make_stop_times(jps = JourneyPatternSections, trips = trips, ss = StandardService)


  # finish up ---------------------------------------------------------------



  # rebuild ids
  #trips$service_id <- gsub("[[:punct:]]","",trips$service_id)
  trips$trip_id <- gsub("[[:punct:]]","",trips$trip_id)
  trips$route_id <- gsub("[[:punct:]]","",trips$route_id)
  trips <- trips[,c("route_id","service_id","trip_id")]

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
