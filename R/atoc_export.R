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



NOT_NEEDED <- c("__NOT_NEEDED_MARKER__~@$$%&*((")


#this function is massively performance critical - profile any changes to it.makes up 30% of the whole makeCalendar process
selectOverlayTimeableAndCopyAttributes <- function(cal, calNew, rowIndex)
{
  #if we have two adjacent complete items e.g. ....end 13th Jan      start 14th jan.....
  #then it's not a real gap and just an artefact of the algorithm use to generate the dates
  if( rowIndex>1 && rowIndex<nrow(calNew)
      && !is.na(calNew$UID[rowIndex-1]) && !is.na(calNew$UID[rowIndex+1])
      && calNew$end_date[rowIndex-1]+1 == calNew$start_date[rowIndex+1])
  {
    calNew$UID[rowIndex] <- NOT_NEEDED
    return (calNew)
  }

  #get candidate base timetable(s) that the new period sits within
  baseTimetableIndexes = cal[ cal$start_date <= calNew$start_date[rowIndex]     #performance - this is an expensive line
                        & cal$end_date >= calNew$end_date[rowIndex],,which=TRUE]

  #are we in a gap between two base timetables with no overlays
  if ( length(baseTimetableIndexes)<=0 )
  {
    calNew$UID[rowIndex] <- NOT_NEEDED
    return (calNew)
  }


  # apply timetable overlay selection logic - pick highest priority timetable type
  # as per https://wiki.openraildata.com/index.php/SCHEDULE
  # "Conveniently, it also means that the lowest alphabetical STP indicator wins - 'C' and 'O' are both lower in the alphabet than 'P'."

  #pick the lowest alphabetic STP (highest priority), and just in case there is more than one, the shortest duration one.

  #priorityTimetable <- baseTimetables[order(STP, duration), head(.SD, 1)]
  #performance we pre-sort all the entries by the priority & duration
  #this speeds things up when we look up the required priority overlay **SEE_NOTE**
  #so we don't need to sort again here, just pick the top filtered result

  #stash the generated start & end dates
  #performance - copying to separate variables seems to be fastest
  start_date = calNew$start_date[rowIndex]
  end_date = calNew$end_date[rowIndex]

  calNew[rowIndex,] <- cal[baseTimetableIndexes[1],] #this is the most time consuming line in this fn. takes about 10x longer than the
                                                     #single variable copy below
  calNew$start_date[rowIndex] = start_date
  calNew$end_date[rowIndex] = end_date

  return (calNew)
}



#' split overlapping start and end dates
#' duplicated items have the same rowId as the original but a new UID with an alpha character appended to it.
#'
#' this function is performance critical - profile any changes
#'
#' THIS ONLY WORKS ON ITEMS WHERE THE DAY PATTERNS ARE ALL THE SAME
#' (or are only 1 day DURATION)
#'
#' @param cal calendar object
#' @details split overlapping start and end dates
#' @noRd

splitDates <- function(cal) {

  # get a vector of all the start and end dates together from all base & overlay timetables and sort them
  dates <- c(cal$start_date, cal$end_date)
  dates <- dates[order(dates)]

  # create all unique pairs so we know how to chop the dates up into non-overlapping periods
  dates.dt <- unique( data.table(
    start_date = dates[seq(1, length(dates) - 1)],
    end_date = dates[seq(2, length(dates))]
  ) )

  #left join back to the source data so we can see which (if any) date segments we have already covered, and which we need to replicate
  calNew <- cal[dates.dt, on = c("start_date", "end_date")]

  #some dates may already be overlapping
  calNew <- fixOverlappingDates( calNew )

  # fill in the missing schedule parts from the original
  # the filled in parts should (if the data is correctly layered) be the highest priority part of the timetable

  # we make multiple passes over the timetable working our way outwards from completed items to NA items

  rowCount = nrow(calNew)

  for (i in seq(1,10)) #should really be a max of 3 passes
  {
    #forwards
    for (j in seq(1, rowCount)) {

      #if we are not valid & the next item is already valid, fill in our details and adjust our end date
      if (j<rowCount && is.na(calNew$UID[j]) && !is.na(calNew$UID[j+1]) )
      {
        calNew <- selectOverlayTimeableAndCopyAttributes(cal, calNew, j)

        if ( NOT_NEEDED != calNew$UID[j+1])
        {
          calNew$end_date[j] <- calNew$start_date[j+1] -1
        }

        #if previous item valid adjust our start date
        if(j>1 && !is.na(calNew$UID[j-1]) && NOT_NEEDED != calNew$UID[j-1] )
        {
          calNew$start_date[j] <- calNew$end_date[j-1] +1
        }
      }
    }

    #backwards
    for (j in seq(rowCount, 1)) {

      #if we are not valid & the previous item is already valid, fill in our details and adjust our start date
      if (j>1 && is.na(calNew$UID[j]) && !is.na(calNew$UID[j-1]) )
      {
        calNew <- selectOverlayTimeableAndCopyAttributes(cal, calNew, j)

        if ( NOT_NEEDED != calNew$UID[j-1])
        {
          calNew$start_date[j] <- calNew$end_date[j-1] +1
        }

        #if next item valid adjust our start date
        if(j<rowCount && !is.na(calNew$UID[j+1]) && NOT_NEEDED != calNew$UID[j+1]  )
        {
          calNew$end_date[j] <- calNew$start_date[j+1] -1
        }
      }
    }

    if ( !any( is.na(calNew$UID) ) ) break #if all done jump out of loop
  }

  # fix duration
  calNew$duration <- calNew$end_date - calNew$start_date + 1

  #remove the items we know are not needed
#  calNew <- calNew[ NOT_NEEDED != calNew$UID, ]

  # remove any gaps. this can occur when we have multiple base timetables over a period of time
  # e.g. a March timetable and a May timetable, with a gap in April where there is no base timetable
#  calNew <- calNew[!is.na(calNew$UID), ]

  # remove cancelled trips (we just leave a gap in the calendar with nothing running)
#  calNew <- calNew[calNew$STP != "C", ]

  # remove any zero or negative day schedules
#  calNew <- calNew[calNew$duration > 0, ]

  #performance, do all subsets in one go
  #calNew <- calNew[!is.na(UID) & UID != NOT_NEEDED & STP != "C" & duration > 0]
  calNew <- calNew[ (!is.na(UID)) & (get("NOT_NEEDED") != UID) & (STP != "C") & (duration > 0), ]

  # Append UID to note the changes
  if (nrow(calNew) > 0) {
    if (nrow(calNew) <= 26) {
      calNew$UID <- paste0(calNew$UID, " ", letters[1:nrow(calNew)])
    } else {
      # Cases where we need extra letters, gives upto 676 ids
      lett <- paste0(rep(letters, each = 26), rep(letters, times = 26))
      calNew$UID <- paste0(calNew$UID, " ", lett[1:nrow(calNew)])
    }
  } else {
    calNew <- NA
  }

  return(calNew)
}


# triggered by test case "10:test makeCalendarInner"
# when we have a 1 day overlay sitting on the start/end data of a base timetable
# the dates overlap - fix it
fixOverlappingDates <- function( cal )
{
  rowCount = nrow(cal)

  #forwards
  for (j in seq(1, rowCount)) {

    #adjust our end date if next item a higher priority overlay
    if (j<rowCount && !is.na(cal$UID[j]) && !is.na(cal$UID[j+1]) )
    {
      if ( cal$STP[j+1] < cal$STP[j] )
      {
        cal$end_date[j] <- cal$start_date[j+1] -1
      }

      if(j>1 && !is.na(cal$UID[j-1]) && cal$STP[j-1] < cal$STP[j] )
      {
        cal$start_date[j] <- cal$end_date[j-1] +1
      }
    }
  }

  #backwards
  for (j in seq(rowCount, 1)) {

    #adjust our end date if previous item a higher priority overlay
    if (j>1 && !is.na(cal$UID[j]) && !is.na(cal$UID[j-1]) )
    {
      if ( cal$STP[j-1] < cal$STP[j] )
      {
        cal$start_date[j] <- cal$end_date[j-1] +1
      }

      if(j<rowCount && !is.na(cal$UID[j+1]) && cal$STP[j+1] < cal$STP[j] )
      {
        cal$end_date[j] <- cal$start_date[j+1] -1
      }
    }
  }

  return (cal)
}




DATE_EPOC <- as.Date(lubridate::origin) #as.Date("01/01/1970", format = "%d/%m/%Y")
WEEKDAY_NAME_VECTOR <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
CHECKROWS_NAME_VECTOR <- c(WEEKDAY_NAME_VECTOR, "duration", "start_date", "end_date")

DURATION_INDEX <- match("duration", CHECKROWS_NAME_VECTOR)
START_DATE_INDEX <- match("start_date", CHECKROWS_NAME_VECTOR)
END_DATE_INDEX <- match("end_date", CHECKROWS_NAME_VECTOR)
MONDAY_INDEX <- match("monday", CHECKROWS_NAME_VECTOR)
SUNDAY_INDEX <- match("sunday", CHECKROWS_NAME_VECTOR)

# TODO: Does not work within functions, rejig to work in package.
#
#' internal function for cleaning calendar
#'
#' @details
#' check for schedules that don't overlay with the days they run i.e.
#'     Mon - Sat schedules for a sunday only service
#' return a logical vector of if the calendar is valid
#'
#' @param tmp 1 row dataframe
#' @noRd
#'
checkrows <- function(tmp) {

  if (tmp[DURATION_INDEX] >= 7)
  {
    return (TRUE)
  }

  days.valid <- weekdays(seq.POSIXt(
    from = as.POSIXct.Date( as.Date(tmp[START_DATE_INDEX], DATE_EPOC) ),
    to = as.POSIXct.Date( as.Date(tmp[END_DATE_INDEX], DATE_EPOC) ),
    by = "DSTday"
  ))
  days.valid <- tolower(days.valid)

  #get a vector of names of days of week that the timetable is valid on
  days.match <- tmp[MONDAY_INDEX:SUNDAY_INDEX]
  days.match <- WEEKDAY_NAME_VECTOR[ 1==days.match ]

  return (any(days.valid %in% days.match))
}


checkOperatingDayActive <- function(calendar) {

  if (all(calendar$duration >= 7))
  {
    return (calendar$Days!="0000000")
  }

  #get a list of days of week that the timetable is valid on
  opDays <- splitBitmaskMat( calendar$Days, asInteger=FALSE )
  opDays <- split(opDays, row(opDays))

  checkValid <- function(dur, sd, ed, od ){

    if (dur >= 7)
    {
      return (any(od))
    }

    dayNumbers <- lubridate::wday( seq.Date(from = sd, to = ed, by = "day"), label = FALSE, week_start=1 )

    return ( any(od[dayNumbers]) )
  }

  validCalendars <- mapply( checkValid, calendar$duration,
                            calendar$start_date, calendar$end_date,
                            opDays, SIMPLIFY = TRUE )
  return (validCalendars)
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

  return(routes)
}


START_PATTERN_VECTOR = c("1","01","001","0001","00001","000001","0000001")
END_PATTERN_VECTOR = c("1000000","100000","10000","1000","100","10","1")

#calendars should start on the first day they are effective, and end on the last day.
#i.e. if the first day in the day bitmask is Tuesday - then the start date should be Tuesday, not some other day.
validateCalendarDates <- function( calendar )
{
  start_day_number = lubridate::wday( calendar$start_date, label = FALSE, week_start=1 )
  end_day_number = lubridate::wday( calendar$end_date, label = FALSE, week_start=1 )

  startOk <- START_PATTERN_VECTOR[ start_day_number ] == stringi::stri_sub(calendar$Days, 1, start_day_number)
  endOk <- END_PATTERN_VECTOR[ end_day_number ] == stringr::str_sub(calendar$Days, end_day_number, 7)

  return (startOk & endOk)
}




#' split and rebind bitmask
#'
#' @details
#' splits 'Days' bitmask into individual logical fields called monday, tuesday, etc...
#'
#' @param calendar data.table of calendar items
#' @noRd
#'
splitAndRebindBitmask <- function( calendar )
{
  return (cbind( calendar, splitBitmaskDt( calendar$Days, FALSE ) ) )
}

#this function gets expensive if you call it a lot, creating data.table takes a while
splitBitmaskDt <- function( bitmaskVector, asInteger=FALSE )
{
  return (as.data.table(splitBitmaskMat( bitmaskVector, asInteger=asInteger )))
}

splitBitmaskMat <- function( bitmaskVector, asInteger=FALSE )
{
  splitDays = splitBitmask( bitmaskVector, asInteger=asInteger )

  return (matrix(splitDays, ncol=7, byrow=TRUE, dimnames=list(NULL,WEEKDAY_NAME_VECTOR)))
}

splitBitmask <- function( bitmask, asInteger=FALSE )
{
  duff = which( nchar(bitmask) != 7 )

  bitmask[duff] = "       "

  splitDays = strsplit(bitmask, "")

  splitDays = as.integer(unlist(splitDays))

  if (!asInteger)
  {
    splitDays = as.logical(splitDays)
  }

  return (splitDays)
}



#' allocate Cancellations Across Calendars
#'
#' @details
#' expects input calendar items to have been separated out into non-overlapping dates
#' and 'Days' bitmask unpacked into separate int or logical attributes
#'
#' "originalUID" is used to identify where the cancellations originally came from
#' after allocating across the split calender items the cancellations will have an updated
#' "UID" that says which calender they are now associated with
#'
#' @param calendar data.table of calendar items that are NOT cancellations (that has 'Days' bitmask unpacked )
#' @param cancellations data.table of calender items that ARE cancellations (that has 'Days' bitmask unpacked )
#' @noRd
#'
allocateCancellationsAcrossCalendars <- function( calendar, cancellations )
{
  tempNames = names(calendar)

  #stash some join fields away because we want to keep the data from the cancellations rather than the calendar table
  #which otherwise get over-written by the join process
  cancellations$start_date2 <- cancellations$start_date
  cancellations$end_date2 <- cancellations$end_date
  cancellations$UID <- NULL #we want the new UID from the calendar entries

  #left join cancellations to calendar by the original service ID
  #and the date of the cancellation lying inside the period of the calendar
  #and the day of the cancellation is an operating day of the calendar item
  joined = cancellations[calendar, on = .(originalUID==originalUID,
                                          start_date>=start_date,
                                          end_date<=end_date)][
                                            ((i.monday&monday) | (i.tuesday&tuesday) | (i.wednesday&wednesday)
                                             | (i.thursday&thursday) | (i.friday&friday) | (i.saturday&saturday) | (i.sunday&sunday)), ]
  #revert the stashed (join) fields
  joined$start_date <- joined$start_date2
  joined$start_date2 <- NULL
  joined$end_date <- joined$end_date2
  joined$end_date2 <- NULL

  #remove joined fields we don't need
  joined <- joined[, .SD, .SDcols=tempNames]

  #belt and braces - fix any NA fields by reverting from the original UID
  joined[is.na(UID), UID := originalUID]

  return( joined )
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

  # UIDs = unique(calendar$UID)
  # length_todo = length(UIDs)
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

  return(list(res.calendar, cancellations))










  if (FALSE) #don't think we need any of this code any more ?
  {
  message(paste0(
    Sys.time(),
    " Removing trips that only occur on days of the week that are outside the timetable validity period"
  ))

  #unpack the days bitmask into a vector of int
  days <- lapply(res.calendar$Days, function(x) {
    as.integer(substring(x, 1:7, 1:7))
  })
  days <- matrix(unlist(days), ncol = 7, byrow = TRUE)
  days <- as.data.frame(days)
  names(days) <- WEEKDAY_NAME_VECTOR

  #attach unpacked bits back onto source calendar
  res.calendar <- cbind(res.calendar, days)
  res.calendar$Days <- NULL



  res.calendar.days <- res.calendar[, ..CHECKROWS_NAME_VECTOR]
  res.calendar.days <- data.table::transpose(res.calendar.days)
  #res.calendar.split <- split(res.calendar, seq(1, nrow(res.calendar)))
  #transpose runs in around 3s (compared to 60s for split() on a data.frame),
  #but causes named dataframe with mixed datatypes to be coerced to unnamed vector of integer.
  #TODO see if data.table performs as well but with simpler code.

  if (ncores > 1) {
    cl <- parallel::makeCluster(ncores)
    parallel::clusterEvalQ(cl, {
      loadNamespace("UK2GTFS")
    })
    keep <- pbapply::pbsapply(res.calendar.days, checkrows,
      cl = cl
    )
    parallel::stopCluster(cl)
    rm(cl)
  } else {
    keep <- pbapply::pbsapply(res.calendar.days, checkrows)
  }

  res.calendar <- res.calendar[keep, ]
  }

}






makeAllOneDay0 <- function( cal )
{
  duration <- cal$end_date - cal$start_date + 1

  if ( 0==nrow(cal) || all(1 == duration))
  {
    #nothing to do
    return (cal)
  }

  start_day_number = lubridate::wday( cal$start_date, label = FALSE, week_start=1 )

  # we want to rotate the day pattern so that the pattern aligns with the start date,
  # then we can replicate it the required number of times
  #TODO made this too complex, leave the pattern where it is and just work out an offset

  #create all possible rotations of day pattern
  allDayPatterns <- c( cal$Days,
                       (paste0(substr(cal$Days, 2, 7),substr(cal$Days, 1, 1))),
                       (paste0(substr(cal$Days, 3, 7),substr(cal$Days, 1, 2))),
                       (paste0(substr(cal$Days, 4, 7),substr(cal$Days, 1, 3))),
                       (paste0(substr(cal$Days, 5, 7),substr(cal$Days, 1, 4))),
                       (paste0(substr(cal$Days, 6, 7),substr(cal$Days, 1, 5))),
                       (paste0(substr(cal$Days, 7, 7),substr(cal$Days, 1, 6))))

  dayPatternMatrix <- matrix( allDayPatterns, ncol=7 )

  #create logical matrix with the pattern we want selected
  cols <- col(dayPatternMatrix)
  toSelect <- cols == start_day_number

  #pull out the desired pattern (need to transpose both matrices otherwise the unwind into a vector is in the wrong order)
  selectedRotation <- t(dayPatternMatrix)[ t(toSelect) ]

  numWeeks <- as.integer(ceiling(as.integer(cal$duration) / 7))


  # replicate the pattern, truncate to <duration> number of days
  # create a sequence of dates - then return the dates selected in the pattern
  makeDates <- function(rot, w, d, start, end){

    selectedDaysLogical <- as.logical(as.integer(strsplit(rot, "")[[1]]))

    selectedDays <- rep(selectedDaysLogical, times = w)

    truncated <- selectedDays[ 1:d ]

    dateSequence <- seq.Date(from = start, to = end, by = "day")

    selectedDates <- dateSequence[ truncated ]
  }

  selectedDates <- mapply(
    makeDates,
    selectedRotation, numWeeks, duration, cal$start_date, cal$end_date
  )

  #replicate the calendar rows the appropriate number of times
  repetitions <- sapply(selectedDates, length)
  replicatedcal <- cal[rep(seq_len(.N), times = repetitions)]

  #set the start and end date for each calender item to the single day identified earlier
  all_dates <- as.Date(unlist(selectedDates), origin = "1970-01-01")
  replicatedcal$end_date <- replicatedcal$start_date <- all_dates

  #tidy up the values so they are correct for the spilt items
  replicatedcal$duration <- 1
  replicatedcal$Days = SINGLE_DAY_PATTERN_VECTOR[ lubridate::wday( replicatedcal$start_date, label = FALSE, week_start=1 ) ]

  return (replicatedcal)
}








SINGLE_DAY_PATTERN_VECTOR = c("1000000","0100000","0010000","0001000","0000100","0000010","0000001")

SINGLE_DAY_PATTERN_LIST = list(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                                c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
                                c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
                                c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
                                c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                                c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                                c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))


makeReplicationDates <- function(cal, startDayNum, endDayNum){

  #make a sequences of dates, offsetting the start date so it's always monday (aligning with bitmask start day)
  #                           and the end date so it's always sunday
  firstDate = min(cal$start_date) - 7
  lastDate = max(cal$end_date) + 7
  allDates = seq.Date(from = firstDate, to = lastDate, by = "day")

  offset = as.integer(cal$start_date)-startDayNum+2-as.integer(firstDate)
  end = as.integer(cal$end_date)+8-endDayNum-as.integer(firstDate)

  dates <- Map(function(o, e) allDates[o:e], offset, end)

  return ( as.Date( unlist(dates), origin = DATE_EPOC ) )
}



#' replicates the input calendar objects into single day duration calendar objects
#' calender objects should NOT have had the 'days' bitmask field unpacked
#' (will still produce an output but the unpacked monday, tuesday etc fields will no longer be consistent with the packed 'Days' bitmask)
#'
#' @param cal data.table containing all the calendars to be split up into individual days
#' @noRd
#'
makeAllOneDay <- function( cal )
{
  duration <- cal$end_date - cal$start_date + 1

  if ( 0==nrow(cal) || all(1 == duration))
  {
    #nothing to do
    return (cal)
  }

  #make a list of dates for each object being replicated
  startDayNum = lubridate::wday( cal$start_date, label = FALSE, week_start=1 )
  endDayNum = lubridate::wday( cal$end_date, label = FALSE, week_start=1 )
  dateSequence = makeReplicationDates( cal, startDayNum, endDayNum )

  #work out how many time we need to replicate each item: number of operating days in week * num weeks
  bitmaskMat = splitBitmaskMat( cal$Days, asInteger=FALSE )
  dayCount = rowSums(bitmaskMat)
  numWeeks <- ceiling(as.integer(cal$duration) / 7)
  repetitions = dayCount * numWeeks

  #replicate the calendar rows the appropriate number of times
  replicatedcal <- cal[rep(seq_len(.N), times = repetitions)]

  #get a mask of operating days
  operatingDayLogical <- rep( split(bitmaskMat, row(bitmaskMat)), times = numWeeks)

  #set the start and end date for each calender item to the single day identified earlier
  selectedDates = dateSequence[unlist(operatingDayLogical)]
  replicatedcal$end_date <- replicatedcal$start_date <- selectedDates

  #tidy up the values so they are correct for the spilt items
  replicatedcal$duration <- 1
  replicatedcal$Days = SINGLE_DAY_PATTERN_VECTOR[ lubridate::wday( replicatedcal$start_date, label = FALSE, week_start=1 ) ]

  return (replicatedcal)
}




#' along a similar line to 'makeAllOneDay' duplicates input calendar objects into single WEEK duration calendar objects
#'
#' @param cal data.table containing all the calendars to be split up into individual weeks
#' @noRd
#'
expandAllWeeks <- function( cal )
{
  if ( 0==nrow(cal) )
  {
    #nothing to do
    return (cal)
  }

  #duration <- cal$end_date - cal$start_date + 1

  #make a list of dates for each object being replicated
  startDayNum = lubridate::wday( cal$start_date, label = FALSE, week_start=1 )
  endDayNum = lubridate::wday( cal$end_date, label = FALSE, week_start=1 )
  dateSequence = makeReplicationDates( cal, startDayNum, endDayNum )

  numWeeks <- ceiling(as.integer(cal$duration) / 7)

  #replicate a logical vector for the start date and use that to select the relevant dates from the date sequence
  startDayLogical <- SINGLE_DAY_PATTERN_LIST[startDayNum]
  startDays <- rep(startDayLogical, times = numWeeks)
  startDates <- dateSequence[ unlist(startDays) ]

  #replicate a logical vector for the end date and use that to select the relevant dates from the date sequence
  endDayLogical <- SINGLE_DAY_PATTERN_LIST[endDayNum]
  endDays <- rep(endDayLogical, times = numWeeks)
  endDates <- dateSequence[ unlist(endDays) ]

  #replicate the calendar rows the appropriate number of times
  replicatedcal <- cal[rep(seq_len(.N), times = numWeeks)]

  #set the start and end date for each calender item
  replicatedcal$start_date <- startDates
  replicatedcal$end_date <- endDates

  #tidy up the values so they are correct for the spilt items
  replicatedcal$duration <- replicatedcal$end_date - replicatedcal$start_date + 1

  return (replicatedcal)
}





#' make calendar helper function
#' this originally expected and dealt with cancellations too. This worked ok for single day duration cancellations
#' but had problems with multi-day cancellations when combined with overlays
#' code hasn't been changed to reject / avoid cancellations but results may not be predictable / tested scenarios
#' @param calendarSub data.table containing all the calendars (aka CIF operating patterns) for a single service
#' @noRd
#'
makeCalendarInner <- function(calendarSub) {

  if ( 1 == nrow(calendarSub) )
  {
    # make into an single entry
    res = list(calendarSub, NA)
  }
  else
  {
    if (length(unique(calendarSub$UID)) > 1)
    {
      stop(paste("Error: makeCalendarInner was passed more than one service to work on. service=", unique(calendarSub$UID)))
    }

    # check duration and types
    allTypes <- calendarSub$STP

    # as per https://wiki.openraildata.com/index.php/SCHEDULE
    # "Conveniently, it also means that the lowest alphabetical STP indicator wins - 'C' and 'O' are both lower in the alphabet than 'P'."
    baseType = max(allTypes) #usually we expect 'P' to be the base timetable... but it can also be STP service in which case it will be 'N'

    overlayDurations <- as.numeric(calendarSub$duration[calendarSub$STP != baseType])
    overlayTypes <- calendarSub$STP[calendarSub$STP != baseType]

    if( length(overlayDurations) <= 0 )
    {
      #assume the input data is good and the base timetables don't break any of the overlaying /operating day rules
      res = list(calendarSub, NA)
    }
    #if every overlay is a one day cancellation (and only one base timetable)
    else if (all(overlayDurations == 1) && all(overlayTypes == "C") && sum(allTypes == baseType) == 1 )
    {
      warning("Unexpected item in the makeCalendarInner-ing area, cancellations should now be handled at a higher level (1)")

      # Apply the cancellation via entries in calendar_dates.txt
      res = list( calendarSub[calendarSub$STP != "C", ],
                   calendarSub[calendarSub$STP == "C", ])
    }
    else
    {
      uniqueDayPatterns <- unique(calendarSub$Days[calendarSub$STP != "C"])

      # if the day patterns are all identical
      if (length(uniqueDayPatterns) <= 1 )
      {
        #performance pre-sort all the entries by the priority
        #this speeds things up when we look up the required priority overlay **SEE_NOTE**
        #calendarSub = calendarSub[ order(STP, duration), ]
        setkey( calendarSub, STP, duration )
        setindex( calendarSub, start_date, end_date)

        calendar_new <- makeCalendarsUnique( splitDates(calendarSub) )
        res = list(calendar_new, NA)
      }
      else # split by day pattern
      {
        #this works if the day patterns don't overlap any operating days.
        if ( any( countIntersectingDayPatterns(uniqueDayPatterns) > 1) )
        {
          #this scenario DOES exist in the downloaded ATOC test data
          #stop(paste("Scenario with overlay pattern not matching base pattern is not currently handled. service=", unique(calendarSub$UID)))
          res = makeCalendarForDifferentDayPatterns( calendarSub )
        }
        else
        {
          res = makeCalendarForDayPatterns( uniqueDayPatterns, calendarSub )
        }
      }
    }
  }

  #stopifnot( is.list(res) )
  return (res)
}




CALENDAR_UNIQUE_CHECK_COLUMN_NAMES <- c("originalUID","start_date","end_date","Days","STP","duration" )


# We have lots of complex logic, which means that when we have multiple base timetables that are separated
# in the temporal domain e.g. march, april - we end up duplicating the overlays
#
# this is a bit of a gluey hack that could be fixed by looking in the temporal domain when deciding what overlaps
# see test case no.10 ("10:test makeCalendarInner") that triggered addition of this logic
#
makeCalendarsUnique <- function ( calendar )
{
  calendar <- calendar[ !duplicated( calendar, by=CALENDAR_UNIQUE_CHECK_COLUMN_NAMES ) ]

  return( calendar )
}




countIntersectingDayPatterns <- function( dayPatterns )
{
  unpacked = splitBitmaskMat( dayPatterns, asInteger = TRUE )
  sums = colSums(unpacked) #add up number of intersections for monday etc...
  names(sums) <- NULL #makes unit test construction easier
  return ( sums )
}

intersectingDayPattern <- function( dayPattern1, dayPattern2 )
{
  return (any( countIntersectingDayPatterns( c(dayPattern1,dayPattern2) ) > 1) )
}


intersectingDayPatterns <- function( dayPatternBase, dayPatternOverlay )
{
  if (is.null(dayPatternOverlay) || is.null(dayPatternBase) ) return (NULL)

  unpackedOverlay = splitBitmaskMat( dayPatternOverlay, asInteger = FALSE )
  unpackedBase = splitBitmaskMat( dayPatternBase, asInteger = FALSE )

  #repeat the base for every Overlay
  unpackedBaseRep = rep( unpackedBase, length(dayPatternOverlay) )
  unpackedBaseRepmat = matrix(unpackedBaseRep, ncol=7, byrow=TRUE)

  intersects = unpackedBaseRepmat & unpackedOverlay

  res <- apply(intersects, 1, any)

  return ( res )
}


intersectingDayPatterns0 <- function( dayPatternBase, dayPatternOverlay )
{
  if (is.null(dayPatternOverlay) || is.null(dayPatternBase) ) return (NULL)

  intersectingDayPattern_vec <- Vectorize(intersectingDayPattern, vectorize.args = c("dayPattern2"))

  res = intersectingDayPattern_vec( dayPatternBase, dayPatternOverlay )
  names(res) <- NULL #makes unit test construction easier
stopifnot(is.logical(res))
  return ( res )
}



makeCalendarForDayPatterns <- function( dayPatterns, calendar )
{
  splits <- list()

  #performance pre-sort all the entries by the priority
  #this speeds things up when we look up the required priority overlay **SEE_NOTE**
  #calendar = calendar[ order(STP, duration), ]
  setkey( calendar, STP, duration )
  setindex( calendar, start_date, end_date)

  for (k in seq(1, length(dayPatterns))) {
    # select for each pattern but include cancellations with a
    # different day pattern
    calendarDay <- calendar[calendar$Days == dayPatterns[k] | calendar$STP == "C", ]
    # TODO cancellations now handled elsewhere - remove this once code stable

    if (all(calendarDay$STP == "C")) {
      # ignore cases of everything is cancelled
      splits[[k]] <- NULL
      warning("unexpected item in the makeCalendarForDayPatterns-ing area, cancellations should now be handled at a higher level")
    }
    else {
      calendarNewDay <- splitDates(calendarDay)

      # rejects NAs
      if (inherits(calendarNewDay, "data.frame")) {
        # further differentiate the UID by appending a number to the end for each different days pattern
        calendarNewDay$UID <- paste0(calendarNewDay$UID, k)
        splits[[k]] <- calendarNewDay
      }
    }
  }

  splits <- data.table::rbindlist(splits, use.names=FALSE)

  splits <- makeCalendarsUnique( splits )

  # after all this faffing about and splitting and joining, it's quite likely we've created some
  # small fragments of base timetable that aren't valid (e.g mon-fri service but start and end date on weekend)
  splits <- splits[ checkOperatingDayActive( splits ) ]

  return(list(splits, NA))
}


# this is a complex case where the overlays don't have the same day pattern as the base timetable
#
# e.g base is mon-sat, and we have some engineering work for 3 weeks tue-thur
#
# the approach we take is to duplicate the overlay timetables for every week they are in effect, then overlay them.
#
# aha but the complexity isn't finished. If the overlay is tue+thur then wed is the base timetable.
#
# when we get to this latter complexity we just split the overlay into individual days and apply it that way.
#
makeCalendarForDifferentDayPatterns <- function( calendar )
{
  baseType = max(calendar$STP)
  baseTimetables =  calendar[calendar$STP == baseType]
  overlayTimetables =  calendar[calendar$STP != baseType]

  gappyOverlays = overlayTimetables[ hasGapInOperatingDays(overlayTimetables$Days) ]
  continiousOverlays = overlayTimetables[ !hasGapInOperatingDays(overlayTimetables$Days) ]

  gappyOverlays = makeAllOneDay( gappyOverlays )
  continiousOverlays = expandAllWeeks( continiousOverlays )

  overlays =  data.table::rbindlist( list(continiousOverlays,gappyOverlays), use.names=FALSE)


  splits <- list()

  distinctBasePatterns = unique( baseTimetables$Days )

  for (k in seq(1, length(distinctBasePatterns))) {

    thisBase = baseTimetables[baseTimetables$Days == distinctBasePatterns[k] ]

    thisOverlay = overlays[ intersectingDayPatterns( distinctBasePatterns[k], overlays$Days ) ]

    if (nrow(thisOverlay) <= 0)
    {
      splits[[k]] <- thisBase
    }
    else
    {
      timetablesForThisPattern = data.table::rbindlist( list( thisBase, thisOverlay ), use.names=FALSE)

      #performance pre-sort all the entries by the priority
      #this speeds things up when we look up the required priority overlay **SEE_NOTE**
      #timetablesForThisPattern = timetablesForThisPattern[ order(STP, duration), ]
      setkey( timetablesForThisPattern, STP, duration )
      setindex( timetablesForThisPattern, start_date, end_date)

      thisSplit <- splitDates( timetablesForThisPattern )

      # rejects NAs
      if (inherits(thisSplit, "data.frame")) {
        # further differentiate the UID by appending a number to the end for each different days pattern
        thisSplit$UID <- paste0(thisSplit$UID, k)
        splits[[k]] <- thisSplit
      }
    }
  }

  splits <- data.table::rbindlist(splits, use.names=FALSE)

  splits <- makeCalendarsUnique( splits )

  # after all this faffing about and splitting and joining, it's quite likely we've created some
  # small fragments of base timetable that aren't valid (e.g mon-fri service but start and end date on weekend)
  splits <- splits[ checkOperatingDayActive( splits ) ]

  return(list(splits, NA))
}


# in a week bitmask, if there are non-operating days between the first and last operating day of the week - will return TRUE
# e.g.    0010000 = FALSE      0011100 = FALSE       0101000 = TRUE
hasGapInOperatingDays <- function( daysBitmask )
{
  firstDay = stringi::stri_locate_first( daysBitmask, fixed = "1" )[,1]
  lastDay = stringi::stri_locate_last( daysBitmask, fixed = "1" )[,1]

  operatingDayCount = stringi::stri_count( daysBitmask, fixed = "1" )

  res = ( lastDay-firstDay+1 != operatingDayCount )

  res[is.na(res)] <- FALSE #shouldn't really get this, probably operating days are '0000000'

  return( res )
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
#' Function that duplicates a very large data.table, adding a "index" column to all rows in the output indicating which
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

  #it's pretty marginal doing this on multiple threads. With a typical number,
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
  else #set all of the stops on a route to be passenger boarding / alighting from a GTFS perspective
  {
    x$pickup_type <- 0
    x$drop_off_type <- 0
    x <- x[, c("pickup_type", "drop_off_type", "activity")]
  }

  return(x)
}
