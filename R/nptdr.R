#' Import the .CIF file
#'
#' @details
#' Imports the CIF file and returns data.frame
#'
#' @param path Path to zipped folder for NPTDR data
#' @param silent Logical, should messages be returned
#' @param n_files debug option numerical vector for files to be passed e.g. 1:10
#'
#' @export
nptdr2gtfs <- function(path = "C:/Users/malco/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/October-2004.zip",
                       silent = FALSE,
                       n_files = NULL){
  checkmate::assert_file_exists(path, extension = "zip")
  dir.create(file.path(tempdir(),"nptdr_temp"))

  utils::unzip(path, exdir = file.path(tempdir(),"nptdr_temp"))

  # List the files

  fls <- list.files(file.path(tempdir(),"nptdr_temp"), recursive = TRUE, full.names = TRUE)
  fls <- fls[!grepl("MACOSX",fls)]

  fls_admin <- fls[grepl("Admin_Area",fls)]
  fls_naptan <- fls[grepl("naptan",fls, ignore.case = TRUE)]
  fls_ng <- fls[grepl("ng",fls, ignore.case = TRUE)]

  # Check all present
  if(length(fls_naptan) != 1){
    stop(length(fls_naptan)," NaPTAN files found")
  }

  if(length(fls_ng) != 1){
    stop(length(fls_ng)," NG files found")
  }

  if(!length(fls_admin) > 1){
    stop(length(fls_ng)," Admin Area files found")
  }

  # Unzip the Admin Area files
  dir.create(file.path(tempdir(),"nptdr_temp","areas"))

  for(i in seq_len(length(fls_admin))){
    utils::unzip(fls_admin[i], exdir = file.path(tempdir(),"nptdr_temp","areas"))
  }

  fls_cif <- list.files(file.path(tempdir(),"nptdr_temp","areas"),
                        recursive = TRUE, full.names = TRUE,
                        pattern = ".CIF")

  if(!length(fls_cif) >= 1){
    stop(length(fls_cif)," No CIF files found")
  } else {
    message(length(fls_cif)," CIF files found")
  }

  # Import the NapTAN
  stops <- nptdr_naptan_import(fls_naptan)

  if(!silent){
    message(Sys.time()," Importing timetables")
  }

  # Import each CIF file
  if(is.null(n_files)){
    res <- purrr::map(fls_cif, importCIF, .progress = "Reading files ")
  } else {
    res <- purrr::map(fls_cif[n_files], importCIF, .progress = "Reading files ")
  }

  unlink(file.path(tempdir(),"nptdr_temp"), recursive = TRUE)


  if(!silent){
    message(Sys.time()," Processing results")
  }

  # TODO: In order(stop_times$schedule, as.numeric(stop_times$departure_time))
  res <- purrr::map(res, `[[`, "stop_times")
  location <- purrr::map(res, `[[`, "locations")
  schedule <- purrr::map(res, `[[`, "schedule")
  exceptions <- purrr::map(res, `[[`, "Exceptions")

  names(stop_times) <- as.character(seq_len(length(stop_times)))
  stop_times <- dplyr::bind_rows(stop_times, .id = "file_id")
  stop_times$schedule <- paste0(stop_times$file_id,"_",stop_times$schedule)
  stop_times$rowID <- paste0(stop_times$file_id,"_",stop_times$rowID)

  names(schedule) <- as.character(seq_len(length(schedule)))
  schedule <- dplyr::bind_rows(schedule, .id = "file_id")
  schedule$schedule <- paste0(schedule$file_id,"_",schedule$rowID)
  schedule$uid <- paste0(schedule$file_id,"_",schedule$uid)

  names(exceptions) <- as.character(seq_len(length(exceptions)))
  exceptions <- dplyr::bind_rows(exceptions, .id = "file_id")
  exceptions$schedule <- paste0(exceptions$file_id,"_",exceptions$schedule)


  timetables <- nptdr_schedule2routes(
    stop_times = stop_times,
    schedule = schedule,
    exceptions = exceptions
  )

  stops <- stops[stops$stop_id %in% unique(timetables$stop_times$stop_id),]
  location <- dplyr::bind_rows(location, .id = "file_id")
  location <- location[,c("stop_id","stop_name","easting","northing")]
  location <- sf::st_as_sf(location, coords = c("easting","northing"), crs = 27700)

  summary(location$stop_id %in% timetables$stop_times$stop_id)
  summary(timetables$stop_times$stop_id %in% location$stop_id)
  summary(location$stop_id %in% stops$stop_id)

  timetables$stops <- stops

  # foo = timetables$stop_times[!timetables$stop_times$stop_id %in% stops$stop_id,]
  # foo = foo[!duplicated(foo$stop_id),]

  return(timetables)



}


#' Import CIF naptan
#'
#' @details
#' Imports the CIF file and returns data.frame
#'
#' @param path_naptan Path to naptan file
#' @noRd
nptdr_naptan_import <- function(path_naptan){

  dir.create(file.path(tempdir(),"nptdr_temp","naptan"))

  utils::unzip(path_naptan, exdir = file.path(tempdir(),"nptdr_temp","naptan"))

  naptan_stops <- utils::read.csv(file.path(tempdir(),"nptdr_temp","naptan","stops.csv"))
  naptan_stops <- naptan_stops[,c("ATCOCode","Lon","Lat","CommonName")]
  names(naptan_stops) <- c("stop_id","stop_lon","stop_lat","stop_name")

  unlink(file.path(tempdir(),"nptdr_temp","naptan"), recursive = TRUE)

  return(naptan_stops)

}


#' Import the .CIF file
#'
#' @details
#' Imports the CIF file and returns data.frame
#'
#' @param file Path to .CIF file
#' @noRd
importCIF <- function(file) {

  # see https://slideplayer.com/slide/14931535/
  raw <- readLines(
    con = file,
    n = -1
  )
  types <- substr(raw, 1, 2)

  # break out each part of the file
  # Header Record
  # Not Needed

  # AT     QB     QI     QL     QO     QS     QT     VS
  # 1   4410 478789   4410  11692  11692  11692      1
  known_types <- c("QS","QO","QI","QT","QB","QL","AT","VS","QV","QE","QR")
  types_have <- unique(types)

  if(any(!types_have %in% known_types)){
    stop("Unknown types in types ",paste(types_have[!types_have %in% known_types], collapse = ", "))
  }

  # QS - Service Header
  # QO - Origin
  # QT - Terminating
  # QI - Intermediate
  # QN - Note
  # QE - Running date (exceptions)
  # QR - Bus Repetitions
  # QP - Bus Operator
  # QD - Route Description
  # QV - Vehicle Type
  # QH - Bank Holiday
  # QL, QA, QB, QC, QG, QJ, QW - Location


  QS <- raw[types == "QS"] # Service
  QO <- raw[types == "QO"] # Origin
  QI <- raw[types == "QI"] # Intermediate
  QT <- raw[types == "QT"] # Terminate

  QB <- raw[types == "QB"] # Location
  QL <- raw[types == "QL"] # Location
  #QV <- raw[types == "QV"] # Vehicle Type
  QE <- raw[types == "QE"] # Running date (exceptions)
  QR <- raw[types == "QR"] # Bus Repetitions


  hd <- raw[1] # Some info not sure what
  tl <- raw[length(raw)]


  # Journey Header, Service
  QS <- iotools::dstrfw(
    x = QS,
    col_types = rep("character", 14),
    widths = c(
      2,1,4,6,8,8,7,1,1,4,6,8,8,1
    )
  )
  names(QS) <- c(
    "recordID","transaction_type","operator_code", "uid", "start_date",
    "end_date", "days_operation", "school_term_time", "bank_holiday",
    "service_number", "running_board", "vehicle_type", "registation_number",
    "route_direction" )

  QS$recordID <- NULL
  QS <- strip_whitespace(QS)

  # clean data
  QS$start_date <- as.Date(QS$start_date, format = "%Y%m%d")
  QS$end_date <- as.Date(QS$end_date, format = "%Y%m%d")

  # Add the rowid
  QS$rowID <- seq(from = 1, to = length(types))[types == "QS"]

  # Add vehicle type if missing
  vt <- substr(hd, 41, 46)
  vt <- trimws(vt)
  QS$vehicle_type[is.na(QS$vehicle_type)] <- vt


  # Origin Stop
  QO <- iotools::dstrfw(
    x = QO,
    col_types = rep("character", 6),
    widths = c(2,12,4,3,2,2)
  )
  names(QO) <- c(
    "recordID", "stop_id", "departure_time", "bay_number",
    "timing_point", "fare_stage"
  )
  QO$recordID <- NULL
  QO <- strip_whitespace(QO)

  # Add the rowid
  QO$rowID <- seq(from = 1, to = length(types))[types == "QO"]

  # Terminating Stop
  QT <- iotools::dstrfw(
    x = QT,
    col_types = rep("character", 6),
    widths = c(2,12,4,3,2,2)
  )
  names(QT) <- c(
    "recordID", "stop_id", "arrival_time", "bay_number",
    "timing_point", "fare_stage"
  )
  QT$recordID <- NULL
  QT <- strip_whitespace(QT)

  # Add the rowid
  QT$rowID <- seq(from = 1, to = length(types))[types == "QT"]

  # Intermediate Stop
  QI <- iotools::dstrfw(
    x = QI,
    col_types = rep("character", 8),
    widths = c(2,12,4,4,1,3,2,1)
  )
  names(QI) <- c(
    "recordID", "stop_id", "arrival_time","departure_time",
    "activity","bay_number","timing_point", "fare_stage"
  )
  QI$recordID <- NULL

  QI <- strip_whitespace(QI)

  # Add the rowid
  QI$rowID <- seq(from = 1, to = length(types))[types == "QI"]

  # Location
  QL <- iotools::dstrfw(
    x = QL,
    col_types = rep("character", 7),
    widths = c(2,1,12,48,1,1,8)
  )
  names(QL) <- c(
    "recordID","transaction_type", "stop_id","stop_name","gazetteer_code",
    "point_type","NatGazID"
  )
  QL$recordID <- NULL
  QL <- strip_whitespace(QL)

  # Additional Location
  QB <- iotools::dstrfw(
    x = QB,
    col_types = rep("character", 7),
    widths = c(2,1,12,8,8,24,24)
  )
  names(QB) <- c(
    "recordID","transaction_type","stop_id","easting","northing","district","town"
  )
  QB$recordID <- NULL
  QB$unknown <- NULL
  QB <- strip_whitespace(QB)

  # Exceptions
  if(length(QE) > 0){
    QE <- iotools::dstrfw(
      x = QE,
      col_types = rep("character", 4),
      widths = c(2,8,8,1)
    )
    names(QE) <- c("recordID", "start_date", "end_date", "exception_type")
    QE$recordID <- NULL
    QE$start_date <- as.Date(QE$start_date, format = "%Y%m%d")
    QE$end_date <- as.Date(QE$end_date, format = "%Y%m%d")
    QE$rowID <- seq(from = 1, to = length(types))[types == "QE"]
    QE$schedule <- as.integer(as.character(cut(QE$rowID,
                                               c(QS$rowID, length(raw)),
                                               labels = QS$rowID
    )))
  } else {
    QE <- NULL
  }

  # Repetitions
  if(length(QR) > 0){
    QR <- iotools::dstrfw(
      x = QR,
      col_types = rep("character", 6),
      widths = c(2,12,4,6,6,8)
    )
    names(QR) <- c("recordID", "Location", "departure_time", "uid","running_board","vehicle_type")
    QR$recordID <- NULL
    QR$rowID <- seq(from = 1, to = length(types))[types == "QR"]
    QR$schedule <- as.integer(as.character(cut(QR$rowID,
                                               c(QS$rowID, length(raw)),
                                               labels = QS$rowID
    )))
    QR = strip_whitespace(QR)

  } else {
    QR <- NULL
  }


  # Header
  hd <- iotools::dstrfw(
    x = hd,
    col_types = rep("character", 7),
    widths = c(8,2,2,32,16,8,6)
  )
  names(hd) <- c("file_type", "version_major", "version_minor",
                 "originator","source","production_date","production_time")
  hd <- strip_whitespace(hd)


  stop_times <- dplyr::bind_rows(list(QO, QI, QT))
  stop_times <- stop_times[order(stop_times$rowID), ]
  stop_times$schedule <- as.integer(as.character(cut(stop_times$rowID,
                                                     c(QS$rowID, length(raw)),
                                                     labels = QS$rowID
  )))
  stop_times$arrival_time <- dplyr::if_else(is.na(stop_times$arrival_time),
                                            stop_times$departure_time,
                                            stop_times$arrival_time)

  stop_times$departure_time <- dplyr::if_else(is.na(stop_times$departure_time),
                                            stop_times$arrival_time,
                                            stop_times$departure_time)

  # Fix bug where times are 0000 when no pickup
  stop_times$arrival_time <- dplyr::if_else(stop_times$arrival_time == "0000" &
                                              stop_times$activity %in% "U",
                                            stop_times$departure_time,
                                            stop_times$arrival_time)


  stop_times <- stop_times[order(stop_times$schedule,as.numeric(stop_times$departure_time)), ]
  stop_times$stop_sequence <- sequence(rle(stop_times$schedule)$lengths)

  # Add in repetitions
  if(!is.null(QR)){
    QR$schedule <- as.integer(QR$schedule)
    st_res <- list()
    sch_res <- list()
    for(i in seq_len(nrow(QR))){
      qr_sub <- QR[i,]
      st_sub <- stop_times[stop_times$schedule == qr_sub$schedule,]
      qs_sub <- QS[QS$rowID == qr_sub$schedule,]
      qr_sub$departure_time <- lubridate::hm(sub("(\\d{2})(\\d{2})", "\\1:\\2", qr_sub$departure_time))
      st_sub$departure_time <- lubridate::hm(sub("(\\d{2})(\\d{2})", "\\1:\\2", st_sub$departure_time))
      st_sub$arrival_time <- lubridate::hm(sub("(\\d{2})(\\d{2})", "\\1:\\2", st_sub$arrival_time))

      offset <- (qr_sub$departure_time - st_sub$departure_time[1])
      st_sub$departure_time <- st_sub$departure_time + offset
      st_sub$departure_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$departure_time))
      st_sub$arrival_time <- st_sub$arrival_time + offset
      st_sub$arrival_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$arrival_time))

      st_sub$departure_time <- sprintf("%02d%02d", st_sub$departure_time@day *
                                       24 + st_sub$departure_time@hour,
                                     lubridate::minute(st_sub$departure_time))

      st_sub$arrival_time <- sprintf("%02d%02d", st_sub$arrival_time@day *
                                       24 + st_sub$arrival_time@hour,
                                     lubridate::minute(st_sub$arrival_time))
      st_sub$schedule <- qr_sub$schedule
      qs_sub$rowID <- as.integer(qr_sub$rowID)
      qs_sub$uid <- qr_sub$uid
      qs_sub$running_board  <- ifelse(is.na(qr_sub$running_board),qs_sub$running_board,qr_sub$running_board)
      qs_sub$vehicle_type   <- ifelse(is.na(qr_sub$vehicle_type),qs_sub$vehicle_type,qr_sub$vehicle_type)

      st_res[[i]] <- st_sub
      sch_res[[i]] <- qs_sub

    }

    st_res <- dplyr::bind_rows(st_res)
    sch_res <- dplyr::bind_rows(sch_res)

    stop_times <- rbind(stop_times, st_res)
    QS <- rbind(QS, sch_res)

  }


  locs <- dplyr::left_join(QL, QB, by = "stop_id")


  results <- list(stop_times, locs, QS, hd, tl, QE)
  names(results) <- c("stop_times", "locations","schedule","Header", "Footer", "Exceptions")

  return(results)
}

#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param stop_times stop-times
#' @param schedule list of dataframes
#' @param exceptions exceptions df
#' @param silent logical
#' @param ncores number of cores to use
#' @noRd
#'
nptdr_schedule2routes <- function(stop_times, schedule, exceptions, silent = TRUE, ncores = 1) {


  ### SECTION 1: ###############################################################################
  # make stop_times.txt
  if (!silent) {
    message(paste0(Sys.time(), " Building stop_times"))
  }

  # Convert Activity to pickup_type and drop_off_type
  stop_times$activity[is.na(stop_times$activity) & stop_times$stop_sequence == 1] <- "TB" # No activity specified at start

  upoffs <- clean_activities2(stop_times$activity)
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

  # Fill in missing data
  schedule$end_date <- dplyr::if_else(is.na(schedule$end_date),
                                      lubridate::ymd("2011-12-31"),
                                      schedule$end_date)

  utils::data(historic_bank_holidays)

  # build the calendar file
  res <- nptdr_makeCalendar(schedule = schedule, exceptions = exceptions,
                            historic_bank_holidays = historic_bank_holidays)
  calendar <- res[[1]]
  calendar_dates <- res[[2]]
  # rm(res)


  # clean calednars
  calendar_dates_hash <- calendar_dates
  calendar_dates_hash$hash <- as.numeric(calendar_dates_hash$date) + calendar_dates_hash$exception_type / 10
  calendar_dates_hash <- calendar_dates_hash[,c("UID","hash")]

  calendar_dates_hash <- dplyr::group_by(calendar_dates_hash, UID)
  calendar_dates_hash <- dplyr::summarise(calendar_dates_hash,
                                          hash = digest::digest(hash, algo = "xxhash32"))

  calendar <- dplyr::left_join(calendar, calendar_dates_hash, by = "UID")

  names(calendar)[names(calendar) == "UID"] <- "trip_id"
  calendar$start_date <- as.character(calendar$start_date)
  calendar$start_date <- gsub("-", "", calendar$start_date)
  calendar$end_date <- as.character(calendar$end_date)
  calendar$end_date <- gsub("-", "", calendar$end_date)


  calendar$service_id <- seq_len(nrow(calendar))
  calendar <- calendar[,c("trip_id","service_id","start_date","end_date",
                          "monday","tuesday","wednesday","thursday","friday",
                          "saturday","sunday","schedule","route_direction","hash")]
  calendar$is_dup <- duplicated(calendar[,c("start_date","end_date",
                                            "monday","tuesday","wednesday",
                                            "thursday","friday",
                                            "saturday","sunday","hash")])
  breaks <- calendar$service_id[!calendar$is_dup]
  breaks <- breaks -1
  calendar$service_id <- as.integer(as.character(cut(calendar$service_id,
                                                      c(breaks, nrow(calendar)),
                                                      labels = breaks
  ))) + 1

  trips <- calendar
  trips <- trips[,c("trip_id","service_id","schedule","route_direction")]

  calendar_dates_hash <- calendar[,c("trip_id","service_id")]
  calendar_dates <- dplyr::left_join(calendar_dates, calendar_dates_hash, by = c("UID" = "trip_id"))
  calendar_dates <- calendar_dates[,c("service_id","date","exception_type")]
  calendar_dates <- unique(calendar_dates)

  calendar <- calendar[!calendar$is_dup,]
  calendar <- calendar[,c("service_id","start_date","end_date",
                       "monday","tuesday","wednesday","thursday","friday",
                       "saturday","sunday")]

  trips$direction_id <- 0
  trips$direction_id[trips$route_direction == "O"] <- 1
  trips$route_direction <- NULL

  ### SECTION 4: ###############################################################################
  # make make the routes.txt
  # find stop_tims with common start and end

  routes <- dplyr::group_by(stop_times, schedule)
  routes <- dplyr::summarise(routes,
                             from = stop_id[stop_sequence == min(stop_sequence)],
                             to = stop_id[stop_sequence == max(stop_sequence)],
                             n_stops = dplyr::n())

  routes <- dplyr::left_join(routes, schedule, by = "schedule")

  routes <- routes[,c("uid","from","to","n_stops","operator_code","service_number","running_board","vehicle_type")]
  routes <- routes[order(routes$from, routes$to, routes$operator_code, routes$service_number, routes$running_board, routes$vehicle_type),]
  routes$is_dup <- duplicated(routes[,c("from","to","n_stops","operator_code","service_number","running_board","vehicle_type")])

  routes$route_id <- seq_len(nrow(routes))
  breaks <- routes$route_id[!routes$is_dup]
  breaks <- breaks -1
  routes$route_id <- as.integer(as.character(cut(routes$route_id,
                                                 c(breaks, nrow(routes)),
                                                 labels = breaks
  ))) + 1

  routes_join <- routes[,c("uid","route_id")]
  routes <- dplyr::group_by(routes, route_id)
  routes <- dplyr::summarise(routes,
                             agency_id = unique(operator_code),
                             route_short_name = unique(service_number),
                             route_long_name = unique(running_board),
                             route_type = unique(vehicle_type)
                             )


  routes$route_type[routes$route_type == ""] <- NA
  routes$route_type <- sapply(routes$route_type, clean_route_type, guess_bus = TRUE)


  trips <- dplyr::left_join(trips, routes_join, by = c("trip_id" = "uid"))

  stop_times <- dplyr::left_join(stop_times, trips[,c("trip_id","schedule")], by = "schedule")


  agency <- data.frame(agency_id = unique(routes$agency_id))
  agency$agency_name <- agency$agency_id
  agency$agency_url <- "http://www.unknownsite.com"
  agency$agency_timezone <- "Europe/London"



  # Ditch unneeded columns
  routes <- routes[, c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type")]
  trips <- trips[, c("trip_id", "route_id", "service_id","direction_id")]
  stop_times <- stop_times[, c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type")]
  calendar <- calendar[, c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")]


  # Fix Times
  stop_times <- afterMidnight(stop_times)


  # end of function
  timetables <- list(calendar, calendar_dates, routes, stop_times, trips, agency)
  names(timetables) <- c("calendar", "calendar_dates", "routes", "stop_times", "trips","agency")
  return(timetables)
}



