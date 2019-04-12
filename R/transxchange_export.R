#' transxchange2gtfs
#'
#' @param obj transxchange object
#' @param run_debug logical, should debugs be done?
#' @param cal calendar
#' @param naptan naptan
#' @param quiet logical should messages be displayed
#'
#' @noRd
#'
transxchange_export <- function(obj, run_debug = TRUE, cal = get_bank_holidays(), naptan = get_naptan(), quiet = TRUE){
  JourneyPatternSections  <-  obj[["JourneyPatternSections"]]
  Operators               <-  obj[["Operators"]]
  Routes                  <-  obj[["Routes"]]
  #RouteSections           <-  obj[["RouteSections"]]
  Services_main           <-  obj[["Services_main"]]
  StandardService         <-  obj[["StandardService"]]
  #Services_NonOperation   <-  obj[["Services_NonOperation"]]
  StopPoints              <-  obj[["StopPoints"]]
  VehicleJourneys         <-  obj[["VehicleJourneys"]]
  VehicleJourneys_exclude <-  obj[["VehicleJourneys_exclude"]]
  VehicleJourneys_include <-  obj[["VehicleJourneys_include"]]
  SpecialDaysOperation <-  obj[["SpecialDaysOperation"]]
  VehicleJourneys_notes <- obj[["VehicleJourneys_notes"]]
  #VehicleJourneysTimingLinks <- obj[["VehicleJourneysTimingLinks"]]

  # Check Notes
  # notecodes_flex <- "DRT1"
  # notecodes_school <- c("SchO","LRC","LRO")
  # if(nrow(VehicleJourneys_notes) > 0){
  #   if(any(notecodes_flex %in% VehicleJourneys_notes$NoteCode)){
  #     stop("Booked Services cannont be converted to GTFS")
  #   }else if(any(notecodes_school %in% VehicleJourneys_notes$NoteCode)){
  #     if(!quiet){
  #       message("Special school running patterns are not supported")
  #     }
  #   }else if(!all(VehicleJourneys_notes$NoteCode %in% c(notecodes_flex, notecodes_school))){
  #     notecodes_unknown <- unique(VehicleJourneys_notes$NoteCode)
  #     notecodes_unknown <- notecodes_unknown[!notecodes_unknown %in% c(notecodes_flex, notecodes_school)]
  #     stop("unknown Note Type", notecodes_unknown)
  #   }
  # }

  # Remove Bookeable Services
  if(nrow(VehicleJourneys_notes) > 0){
    VehicleJourneys_notes <- VehicleJourneys_notes[grepl("book", VehicleJourneys_notes$NoteText),]
    vjc_remove <- unique(VehicleJourneys_notes$VehicleJourneyCode)
    VehicleJourneys <- VehicleJourneys[!VehicleJourneys$VehicleJourneyCode %in% vjc_remove,]
    if(nrow(VehicleJourneys) == 0){
      return(NULL)
    }
  }


  # Early Subsets - move to import code
  VehicleJourneys <- VehicleJourneys[,c("VehicleJourneyCode","ServiceRef","JourneyPatternRef","DepartureTime","days","BankHolidaysOperate","BankHolidaysNoOperate")]
  Services_main$StartDate <- as.Date(Services_main$StartDate)
  Services_main$EndDate <- as.Date(Services_main$EndDate)

  # Check on exclusions
  if(nrow(VehicleJourneys_include) != 0  # | nrow(VehicleJourneys_exclude) != 0
     ){
    stop("Must consider VehicleJourneys")
  }

  if(!is.null(SpecialDaysOperation)){
    stop("Must consider SpecialDaysOperation")
  }

  # Check on missing
  # if(!is.null(Services_NonOperation)){
  #   stop("Must consider Services_NonOperation")
  # }

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
  stops$stop_id <- as.character(stops$stop_id)
  stops <- dplyr::left_join(stops, naptan, by = "stop_id")


  # routes ------------------------------------------------------------------
  # route_id, agency_id, route_short_name, route_long_name, route_desc, route_type

  routes <- Services_main[c("ServiceCode","RegisteredOperatorRef","LineName","Description","Mode","Origin","Destination")]
  routes$route_long_name <- paste0(routes$Origin," - ",routes$Destination)
  names(routes) <- c("route_id","agency_id","route_short_name","route_desc","route_type","Origin","Destination","route_long_name")
  routes <- routes[,c("route_id","agency_id","route_short_name","route_long_name","route_desc","route_type")]
  routes$agency_id <- gsub("OId_","",routes$agency_id)
  routes$route_type <- sapply(routes$route_type, clean_route_type)

  # Shorten route_short_name
  routes$route_short_name <- gsub("Park & Ride","P&R", routes$route_short_name)
  routes$route_short_name <- gsub("Road","Rd", routes$route_short_name)
  routes$route_short_name <- gsub("Connecting Communities ","", routes$route_short_name)
  routes$route_short_name <- gsub("|the busway","", routes$route_short_name)
  routes$route_short_name[nchar(routes$route_short_name) > 10] <- "" #Remove long names to pass validation check


  # agency ------------------------------------------------------------------
  # agency_id, agency_name, agency_url, agency_timezone

  # Check which code is used
  if(all(routes$agency_id %in% Operators$NationalOperatorCode)){
    agency_id <- Operators$NationalOperatorCode
  }else if(all(routes$agency_id %in% Operators$OperatorCode)){
    agency_id <- Operators$OperatorCode
  }else{
    stop("Unable to match OperatorCode between Services_main and Operators")
  }

  agency <- data.frame(agency_id = agency_id,
                       agency_name = Operators$TradingName,
                       agency_url = "http://www.unknown.com",
                       agency_timezone = "Europe/London",
                       agency_lang = "en")


  # trips calendar calendar_dates -------------------------------------------------
  ###### Redo: Again
  trips <- VehicleJourneys[,c("ServiceRef","VehicleJourneyCode","DepartureTime","JourneyPatternRef","days")]
  names(trips) <-           c("route_id"  ,"trip_id",           "DepartureTime","JourneyPatternRef","days")
  trips[] <- lapply(trips, as.character)


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

  bank_holidays <- bank_holidays[!is.na(bank_holidays$date),]


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

  gtfs_validate_internal(res_final)


  return(res_final)

}
