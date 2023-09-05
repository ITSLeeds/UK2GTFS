#' Export ATOC stations as GTFS stops.txt
#'
#' @details
#' Export ATOC stations as GTFS stops.txt
#'
#' @param station station SF data frame from the importMSN function
#' @param TI TI object from the importMCA function
#' @family atoc
#' @export
#'
station2stops <- function(station, TI) {

  # Discard Unneded Columns
  TI <- TI[, c("TIPLOC code", "NALCO", "TPS Description", "CRS Code")]
  station <- station[, c(
    "Station Name", "CATE Interchange status", "TIPLOC Code",
    "CRS Code", "geometry"
  )]

  jnd <- dplyr::left_join(TI, station, by = c("TIPLOC code" = "TIPLOC Code"))
  station.extra <- station[!station$`TIPLOC Code` %in% jnd$`TIPLOC code`, ]
  station.extra$`TIPLOC code` <- station.extra$`TIPLOC Code`
  station.extra$NALCO <- NA
  station.extra$`CRS Code.y` <- station.extra$`CRS Code`
  station.extra$`TPS Description` <- NA
  station.extra$`CRS Code.x` <- NA
  station.extra <- station.extra[, names(jnd)]
  jnd <- suppressWarnings(dplyr::bind_rows(jnd, station.extra))

  jnd$geometry <- sf::st_sfc(jnd$geometry)
  jnd <- sf::st_sf(jnd)
  sf::st_crs(jnd) <- 4326

  jnd$CRS <- ifelse(is.na(jnd$`CRS Code.y`), jnd$`CRS Code.x`,
    jnd$`CRS Code.y`
  )
  jnd$name <- ifelse(is.na(jnd$`TPS Description`), jnd$`Station Name`,
    jnd$`TPS Description`
  )

  stops <- jnd[, c("CRS", "TIPLOC code", "name")]
  stops <- stops[!sf::st_is_empty(stops), ]

  stops.final <- stops

  stops.final <- as.data.frame(stops.final)
  stops.final$geometry <- sf::st_sfc(stops.final$geometry)
  stops.final <- sf::st_sf(stops.final)
  sf::st_crs(stops.final) <- 4326
  stops.final <- stops.final[, c("TIPLOC code", "CRS", "name", "geometry")]

  # recorder the match the GTFS stops.txt
  names(stops.final) <- c("stop_id", "stop_code", "stop_name", "geometry")
  coords <- sf::st_coordinates(stops.final)
  stops.final$stop_lat <- coords[, 2]
  stops.final$stop_lon <- coords[, 1]
  # sub metre precison is sufficent
  stops.final$stop_lat <- round(stops.final$stop_lat, 5)
  stops.final$stop_lon <- round(stops.final$stop_lon, 5)
  stops.final <- as.data.frame(stops.final)
  stops.final$geometry <- NULL

  # Built tiploc to CRS lookup
  lookup <- as.data.frame(jnd)
  lookup <- lookup[, c("TIPLOC code", "CRS")]
  lookup$match <- ifelse(is.na(lookup$CRS), lookup$`TIPLOC code`, lookup$CRS)
  lookup <- lookup[, c("TIPLOC code", "match")]
  names(lookup) <- c("TIPLOC", "match")



  results <- list(stops.final, lookup)
  names(results) <- c("stops", "lookup")
  return(results)
}


#' Export ATOC stations and FLF file as transfers.txt
#'
#' @details
#' Export ATOC FLF file as transfers.txt
#'
#' @param station station SF data frame from the importMSN function
#' @param flf imported flf file from importFLF
#' @noRd
#'
station2transfers <- function(station, flf, path_out) {

  ### SECTION 4: ############################################################
  # make make the transfers.txt
  # transfer between stations are in the FLF file
  transfers1 <- flf[, c("from", "to", "time")]
  transfers1$time <- transfers1$time * 60
  transfers1$transfer_type <- 2

  # transfer within stations are in the stations file
  transfers2 <- station[, c("TIPLOC Code", "CRS Code", "Minimum Change Time")]
  transfers2 <- as.data.frame(transfers2)
  transfers2$geometry <- NULL

  transfers3 <- transfers2[, c("TIPLOC Code", "CRS Code")]
  names(transfers3) <- c("from_stop_id", "CRS Code")
  transfers1 <- dplyr::left_join(transfers1, transfers3,
    by = c("from" = "CRS Code")
  )
  names(transfers3) <- c("to_stop_id", "CRS Code")
  transfers1 <- dplyr::left_join(transfers1, transfers3,
    by = c("to" = "CRS Code")
  )
  transfers1 <- transfers1[, c(
    "from_stop_id", "to_stop_id",
    "transfer_type", "time"
  )]
  names(transfers1) <- c(
    "from_stop_id", "to_stop_id",
    "transfer_type", "min_transfer_time"
  )

  transfers2$min_transfer_time <- as.integer(transfers2$`Minimum Change Time`) * 60
  transfers2$to_stop_id <- transfers2$`TIPLOC Code`
  transfers2$transfer_type <- 2
  names(transfers2) <- c(
    "from_stop_id", "CRS Code", "Minimum Change Time",
    "min_transfer_time", "to_stop_id", "transfer_type"
  )
  transfers2 <- transfers2[, c(
    "from_stop_id", "to_stop_id", "transfer_type",
    "min_transfer_time"
  )]

  transfers <- rbind(transfers1, transfers2)
  return(transfers)
}




#' internal function for constructing longnames of routes
#'
#' @details
#' creates the long name of a route from appropriate variables
#'
#' @param routes routes data.frame
#' @param stop_times stop_times data.frame
#' @noRd
#'
longnames <- function(routes, stop_times) {

  stop_times_sub <- dplyr::group_by(stop_times, trip_id)
  stop_times_sub <- dplyr::summarise(stop_times_sub,
    schedule = unique(schedule),
    stop_a = stop_id[stop_sequence == 1],
    stop_b = stop_id[stop_sequence == max(stop_sequence)]
  )

  stop_times_sub$route_long_name <- paste0(stop_times_sub$stop_a,
                                           " to ",
                                           stop_times_sub$stop_b)
  stop_times_sub <- stop_times_sub[!duplicated(stop_times_sub$schedule), ]
  stop_times_sub <- stop_times_sub[, c("schedule", "route_long_name")]

  routes <- dplyr::left_join(routes, stop_times_sub,
                             by = c("rowID" = "schedule"))

  routes[`Train Category` == "SS", route_long_name := paste("Ship from",route_long_name)]
  routes[`Train Category` %in% c("BS", "BR"), route_long_name := paste("Bus from",route_long_name)]
  routes[!(`Train Category` %in% c("SS", "BS", "BR")), route_long_name := paste("Train from",route_long_name)]
  #TODO reflect the London Transport services being set to metro/underground in this naming code

  return(routes)
}



#' make calendar
#'
#' @details
#' split overlapping start and end dates
#'
#' @param schedule schedule data.frame
#' @param ncores number of processes for parallel processing (default = 1)
#' @noRd
#'
makeCalendar <- function(schedule, ncores = 1) {
  # prep the inputs
  calendar <- schedule[, c("Train UID", "Date Runs From", "Date Runs To", "Days Run", "STP indicator", "rowID" )]
  names(calendar) <- c("UID", "start_date", "end_date", "Days", "STP", "rowID" )

  calendar$STP <- as.character(calendar$STP)
  calendar$duration <- calendar$end_date - calendar$start_date + 1

  if ( !all(validateCalendarDates( calendar ) ) )
  {
    warning(paste0(Sys.time(), " Some calendar dates had incorrect start or end dates that did not align with operating day bitmask"))
    #TODO be more verbose about which ones
  }


  #we're going to be splitting and replicating calendar entries - stash the original UID so we can join back on it later
  calendar$originalUID <- calendar$UID

  #brutal, but makes code later on simpler, make all cancellations one day cancellations by splitting
  #TODO don't split up into one day cancellations if all the operating day patterns on a service are identical
  cancellations <- makeAllOneDay( calendar[calendar$STP == "C", ] )
  calendar <- calendar[calendar$STP != "C", ]

  #calendar$start_date = as.integer( calendar$start_date )
  #calendar$end_date = as.integer( calendar$end_date )
#test treating date as int: seem to be about twice as fast on the critical line when selecting base timetable
#TODO add package option to switch between processing as date/int otherwise debugging is too hard

  #debugging option
  set_STOP_PROCESSING_UID( getOption("UK2GTFS_opt_stopProcessingAtUid") )

  message(paste0(Sys.time(), " Constructing calendar and calendar_dates"))
  calendar$`__TEMP__` <- calendar$UID
  calendar_split <- calendar[, .(list(.SD)), by = `__TEMP__`][,V1]

  if (ncores > 1) {
    cl <- parallel::makeCluster(ncores)
    parallel::clusterEvalQ(cl, {
      loadNamespace("UK2GTFS")
    })
    pbapply::pboptions(use_lb = TRUE)
    res <- pbapply::pblapply(calendar_split,
      makeCalendarInner,
      cl = cl
    )
    parallel::stopCluster(cl)
    rm(cl)
  } else {
    res <- pbapply::pblapply(
      calendar_split,
      makeCalendarInner
    )
  }

  res.calendar <- lapply(res, `[[`, 1)
  res.calendar <- data.table::rbindlist(res.calendar, use.names=FALSE) #performance, takes 10 minutes to execute bind_rows on full GB daily timetable

  res.cancellation_dates <- lapply(res, `[[`, 2)
  res.cancellation_dates <- res.cancellation_dates[!is.na(res.cancellation_dates)]
  res.cancellation_dates <- data.table::rbindlist(res.cancellation_dates, use.names=FALSE)
  stopifnot( 0==nrow(res.cancellation_dates) )
  rm(res.cancellation_dates)
  #since we didn't pass in any cancellations we should no longer get any back

  res.calendar = splitAndRebindBitmask( res.calendar )
  cancellations = splitAndRebindBitmask( cancellations )

  #associate the split up cancellations with the (new) calendar they are associated with
  #(only works because cancellations are all one day duration)
  cancellations = allocateCancellationsAcrossCalendars( res.calendar, cancellations )

  #no longer need the field that was used to associate the original and replicated calendars together
  cancellations$originalUID <- NULL
  res.calendar$originalUID <- NULL

  #error checking
  dups = duplicated( res.calendar$UID )
  if( any(TRUE==dups) )
  {
    dups = unique( res.calendar$UID[ dups ] )

    warning(paste(Sys.time(), "Duplicate UIDs were created by the makeCalendar() process, this is likely to cause downstream proceessing errors. ",
                                "Please capture the data and raise a bug / create a test case. ", dups))
  }

  return(list(res.calendar, cancellations))
}




#' duplicateItem
#'
#' @details
#' Function that duplicates a data.table, adding a "index" column to all rows in the output indicating which
#' instance of the duplication the row is associated with
#'
#' @param dt data.table
#' @param reps number of duplicates to be created
#' @param indexStart starting number for the "index" value added to the item
#' @noRd
#'
duplicateItem <- function( dt, reps, indexStart=1 )
{
  if ( is.na(reps) | reps<1 ) return (NULL)

  #replicate all the rows in dt       times=reps
  duplicates <- dt[rep(seq(1, nrow(dt)), reps), ]

  #create and apply indexes to the created rows- each group of replicated rows gets the same index number.
  index <- rep(seq( indexStart, indexStart-1+reps ), nrow(dt))

  duplicates$index <- index[order(index)]

  return(duplicates)
}




#' duplicateItems
#'
#' @details
#' Function that duplicates a large data.table, adding a "index" column to all rows in the output indicating which
#' instance of the duplication the row is associated with
#'
#' requires a column called "_reps" on the object to determine how many times it is to be duplicated
#'
#' @param dt data.table
#' @param split_attribute name of attribute to split the items between worker tasks
#' @param indexStart starting number for the "index" value added to the item
#' @noRd
#'
duplicateItems <- function( dt, split_attribute, ncores=1, indexStart=1 )
{
  #add an additional column before splitting on it - so that the value we're really splitting on still appears in the output.
  dt[, `_TEMP_` := get(split_attribute) ]
  dt_split <- dt[, .(list(.SD)), by = `_TEMP_`][,V1]
  dt$`_TEMP_` <- NULL


  duplicate_int <- function(dta) {
    rep <- dta$`_reps`[1]
    return ( duplicateItem( dta, rep, indexStart ) )
  }


  if (ncores == 1) {
    duplicates <- pbapply::pblapply(dt_split, duplicate_int)
  } else {
    cl <- parallel::makeCluster(ncores)
    parallel::clusterEvalQ(cl, {
      loadNamespace("UK2GTFS")
    })

    duplicates <- pbapply::pblapply(dt_split,
                                        duplicate_int,
                                        cl = cl)
    parallel::stopCluster(cl)
    rm(cl)
  }

  duplicates <- data.table::rbindlist(duplicates, use.names=FALSE)

  duplicates$`_reps` <- NULL #performance, putting this inside duplicate_int roughly doubles the execution time

  return (duplicates)
}





#' Duplicate stop_times
#'
#' @details
#' Function that duplicates stop times for trips that have been split into
#'     multiple trips and sets the new trip id on the duplicated stop_times
#'
#' @param calendar calendar data.frame
#' @param stop_times stop_times data.frame
#' @param ncores number of processes for parallel processing (default = 1)
#' @noRd
#'
duplicate_stop_times <- function(calendar, stop_times, ncores = 1) {

  outputColumnNames = c(
    "trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence",
    "pickup_type", "drop_off_type", "schedule"
  )

  #it's pretty marginal doing this on multiple threads. With a typical sized day all GB file,
  #doing the split takes 2.4s and the duplication 7.8s (on one thread)
  #TODO look at avoiding the split if threads=1

  return ( duplicate_related_items( calendar, stop_times,
                                    original_join_field = "schedule",
                                    new_join_field = "trip_id",
                                    outputColumnNames = outputColumnNames,
                                    ncores=ncores ) )
}



#' Duplicate related items
#'
#' @details
#' Function that duplicates items that are related to calendar
#'     expected input are calendar items have been duplicated but retain the same (now duplicate) 'rowID'
#'     this tells us which objects to duplicate and how many are required
#'     the related_items have an attribute <original_join_field>, which joins back to 'rowID' on the calendar items
#'
#'     After duplication the duplicated items are joined back onto the input calendar items
#'     to create an additional attribute on the output objects
#'
#'     The calendar item attribute <new_join_field> forms the new relation between the calendar items and
#'     related items, so must be unique.
#'
#' @param calendar calendar data.frame
#' @param related_items data.frame of items to be replicated
#' @param ncores number of processes for parallel processing (default = 1) (currently hangs/crashes if >1)
#' @noRd
#'
duplicate_related_items <- function(calendar, related_items, original_join_field, new_join_field, outputColumnNames, ncores = 1) {

  calendar.dup <- calendar[duplicated(calendar$rowID), ]

  if( nrow(calendar.dup) <= 0 )
  {
    #no duplicating to do
    warning("duplicate_related_items: there were no duplicates detected. In real data this may indicate there has been an error earlier in the processing.")
    related_items_dup = data.table()
  }
  else
  {
    #create a count of the number of each duplicate of rowID
    rowID.unique <- as.data.frame(table(calendar.dup$rowID))
    rowID.unique$Var1 <- as.integer(as.character(rowID.unique$Var1))

    #join the count of number of duplicates required to the stop times (so we can retrieve it later when doing the duplication)
    related_items <- dplyr::left_join(related_items, rowID.unique,
                                   by = setNames("Var1",original_join_field)  )

    #set the number of duplications required
    related_items$`_reps` <- related_items$Freq

    # TODO: The could handle cases of non duplicated stoptimes within duplicate.stop_times.int
    # rather than splitting and rejoining, would bring code tidyness and speed improvements
    related_items_dup <- duplicateItems( related_items, original_join_field, ncores=ncores, indexStart=1 )


    # join via rowID+index to get new de-duplicated trip_id

    #create index on the table we want to join to - group by the rowId, index runs from 0..count()-1 of group size
    #we start at zero so we don't effect the original stop_times rows and just join in the duplicated rows
    new_join_ids <- dplyr::group_by(calendar, rowID)
    new_join_ids <- dplyr::mutate(new_join_ids, Index = seq(0, dplyr::n()-1))
    new_join_ids <- as.data.frame( new_join_ids[, c("rowID", new_join_field, "Index")] )

    related_items_dup <- dplyr::left_join(related_items_dup, new_join_ids, by = setNames(c("rowID","Index"),c(original_join_field,"index")) )

    #select columns required
    related_items_dup <- related_items_dup[, outputColumnNames, with=FALSE]
  }

  calendar.nodup <- calendar[!duplicated(calendar$rowID), ]

  # when routes are cancelled their stop times are left without valid trip_ids - remove those rows
  # this only applies to the non-duplicated rows
  # Join via rowID to determine the trip_id
  related_ids_nodup <- calendar.nodup[, c("rowID", new_join_field), with=FALSE]
  related_items_no_dup <- dplyr::left_join(related_items, related_ids_nodup, by = setNames("rowID",original_join_field))
  related_items_no_dup <- related_items_no_dup[!is.na(related_items_no_dup[[new_join_field]]), ]


  #select columns required, join output results together
  related_items_no_dup <- related_items_no_dup[, outputColumnNames, with=FALSE]

  related_items_comb <- data.table::rbindlist(list(related_items_no_dup, related_items_dup), use.names=FALSE)

  return(related_items_comb)
}







#' fix times for journeys that run past midnight
#'
#' @details
#' When train runs over midnight GTFS requires the stop times to be in
#'    24h+ e.g. 26:30:00
#'
#' @param stop_times stop_times data.frame
#' @param safe logical (default = TRUE) should the check for trains
#'    running more than 24h be performed?
#'
#' @details
#' Not running the 24 check is faster, if the check is run a warning
#'    is returned, but the error is not fixed. As the longest train
#'    journey in the UK is 13 hours (Aberdeen to Penzance) this is
#'    unlikely to be a problem.
#' @noRd
#'
afterMidnight <- function(stop_times, safe = TRUE) {

  stop_times$arv <- as.integer(stop_times$arrival_time)
  stop_times$dept <- as.integer(stop_times$departure_time)

  stop_times.summary <- dplyr::group_by(stop_times, trip_id)
  stop_times.summary <- dplyr::summarise(stop_times.summary,
    dept_first = dept[stop_sequence == min(stop_sequence)]
  )

  stop_times <- dplyr::left_join(stop_times, stop_times.summary, by = "trip_id")
  stop_times$arvfinal <- ifelse(stop_times$arv < stop_times$dept_first, stop_times$arv + 2400, stop_times$arv)
  stop_times$depfinal <- ifelse(stop_times$dept < stop_times$dept_first, stop_times$dept + 2400, stop_times$dept)


  if (safe) {
    # check if any train more than 24 hours
    stop_times.summary2 <- dplyr::group_by(stop_times, trip_id)
    stop_times.summary2 <- dplyr::summarise(stop_times.summary2,
      arv_last = arvfinal[stop_sequence == max(stop_sequence)],
      arv_max = max(arvfinal, na.rm = TRUE)
    )

    check <- stop_times.summary2$arv_last < stop_times.summary2$arv_max
    if (any(check)) {
      warning("24 hour clock correction will return false results for any trip where total travel time exceeds 24 hours")
    }
  }

  numb2time2 <- function(numb){
    #performance, substr is relatively expensive
    numb <- sprintf("%02d:%02d:00", numb %/% 100, numb %% 100)
  }

  stop_times$arrival_time <- numb2time2(stop_times$arvfinal)
  stop_times$departure_time <- numb2time2(stop_times$depfinal)

  stop_times <- stop_times[, c("trip_id", "arrival_time", "departure_time",
                               "stop_id", "stop_sequence", "pickup_type",
                               "drop_off_type")]
  return(stop_times)
}




fixStopTimeData <- function(stop_times)
{
  # Fix arrival_time / departure_time being 0000 for pick up only or drop off only trains
  stop_times$departure_time <- dplyr::if_else(stop_times$departure_time == "0000" & stop_times$Activity == "D",
                                              stop_times$arrival_time,
                                              stop_times$departure_time)
  stop_times$arrival_time <- dplyr::if_else(stop_times$arrival_time == "0000" & stop_times$Activity == "U",
                                            stop_times$departure_time,
                                            stop_times$arrival_time)

  #fix missing arrival / departure times by copying from the other time.
  stop_times$arrival_time[is.na(stop_times$arrival_time)] <- stop_times$departure_time[is.na(stop_times$arrival_time)]
  stop_times$departure_time[is.na(stop_times$departure_time)] <- stop_times$arrival_time[is.na(stop_times$departure_time)]

  return (stop_times)
}



#' Clean Activities
#' @param x character activities
#' @details
#' Change Activities code to pickup and drop_off
#' https://wiki.openraildata.com//index.php?title=Activity_codes
#'
#' @noRd
#'
clean_activities2 <- function(x, public_only = TRUE) {

  x <- data.frame(activity = x, stringsAsFactors = FALSE)

  if (public_only)
  {
    x <- dplyr::left_join(x, activity_codes, by = c("activity"))
    if (anyNA(x$pickup_type)) {
      mss <- unique(x$activity[is.na(x$pickup_type)])
      warning("Unknown Activity codes '", paste(unique(mss), collapse = "' '"), "' please report these codes as a GitHub Issue")
      x$pickup_type[is.na(x$pickup_type)] <- 0
      x$drop_off_type[is.na(x$drop_off_type)] <- 0
    }
    x <- x[, c("pickup_type", "drop_off_type")]
  }
  else #set all of the stops on a route to be valid for passenger boarding / alighting from a GTFS perspective
  {
    x$pickup_type <- 0
    x$drop_off_type <- 0
    x <- x[, c("pickup_type", "drop_off_type", "activity")]
  }

  return(x)
}
