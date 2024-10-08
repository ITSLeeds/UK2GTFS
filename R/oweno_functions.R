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
      #put any setup required for all worker processes in here
      options( UK2GTFS_opt_updateCachedDataOnLibaryLoad = FALSE )
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
                                      by = stats::setNames("Var1",original_join_field)  )

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


fixStopTimeData <- function(stop_times)
{
  # Fix arrival_time / departure_time being 0000 for pick up only or drop off only trains
  stop_times$departure_time <- dplyr::if_else(stop_times$departure_time == "000000" & stop_times$Activity == "D",
                                              stop_times$arrival_time,
                                              stop_times$departure_time)
  stop_times$arrival_time <- dplyr::if_else(stop_times$arrival_time == "000000" & stop_times$Activity == "U",
                                            stop_times$departure_time,
                                            stop_times$arrival_time)

  #fix missing arrival / departure times by copying from the other time.
  stop_times$arrival_time[is.na(stop_times$arrival_time)] <- stop_times$departure_time[is.na(stop_times$arrival_time)]
  stop_times$departure_time[is.na(stop_times$departure_time)] <- stop_times$arrival_time[is.na(stop_times$departure_time)]

  return (stop_times)
}


#' Strip White Space
#'
#' @details
#' Input data.table is modified in-place and returned to the caller.
#'
#' Strips trailing whitespace from all char columns in a data.table
#' empty values are converted to NA
#'     returns the data.table
#'
#' @param dt data table
#' @noRd
#'
strip_whitespace <- function(dt) {

  char_cols <- sapply(dt, is.character)
  char_col_names <- names(char_cols[char_cols])

  for (col_name in char_col_names) {
    set(dt, j = col_name, value = trimws(dt[[col_name]], which = "right"))
    dt[dt[[col_name]] == "", (col_name) := NA_character_]
  }

  return (dt)
}


process_times <- function(dt, working_timetable) {

  dt = processOneTime(dt, working_timetable, "Arrival Time", "Scheduled Arrival Time", "Public Arrival Time")
  dt = processOneTime(dt, working_timetable, "Departure Time", "Scheduled Departure Time", "Public Departure Time")

  return(dt)
}


#does in place-modification of input data.table
#select the public arrive/depart times if they exist, otherwise select the wtt arrive/depart times if they exist, otherwise select the pass time
#and at the same time fill in the missing seconds values (and 30 seconds if 'H' is indicated)
processOneTime <- function(dt, working_timetable, targetField, sourceFieldWtt, sourceField)
{
  hasPass = "Scheduled Pass" %in% colnames(dt)

  if (sourceFieldWtt %in% colnames(dt))
  {
    if (working_timetable)
    {
      if(hasPass)
      {
        set(dt, j = targetField, value = gsub("^(\\d{4}) $","\\100",gsub("^(\\d{4})H$", "\\130",
                                                                         data.table::fifelse( "     "==dt[[sourceFieldWtt]],
                                                                                              dt[["Scheduled Pass"]],
                                                                                              dt[[sourceFieldWtt]])))
        )
      }
      else
      {
        set(dt, j = targetField, value = gsub("^(\\d{4}) $","\\100",gsub("^(\\d{4})H$", "\\130", dt[[sourceFieldWtt]])))
      }
    }
    else
    {
      if(hasPass)
      {
        set(dt, j = targetField, value = data.table::fifelse( "0000"==dt[[sourceField]],
                                                              gsub("^(\\d{4}) $","\\100",gsub("^(\\d{4})H$", "\\130",
                                                                                              data.table::fifelse( "     "==dt[[sourceFieldWtt]],
                                                                                                                   dt[["Scheduled Pass"]],
                                                                                                                   dt[[sourceFieldWtt]]))),
                                                              gsub("^(\\d{4})$", "\\100", dt[[sourceField]]))
        )
      }
      else
      {
        #If there is no Public Arrival time this field will default to 0000. (we will use WTT instead)
        set(dt, j = targetField, value = data.table::fifelse( "0000"==dt[[sourceField]],
                                                              gsub("^(\\d{4}) $","\\100",gsub("^(\\d{4})H$", "\\130", dt[[sourceFieldWtt]])),
                                                              gsub("^(\\d{4})$", "\\100", dt[[sourceField]]))
        )
      }
    }
  }

  return (dt)
}



# Process Activity Codes
process_activity <- function(dt, public_only) {

  #  if ( any( 12 != nchar(dt$Activity) ) )
  #  {
  #    stop("bad input data in process_activity(), all Activity fields should be 12 chars long")
  #  }
  # don't really need this test since we're reading in from fixed width files

  #performance, runs about twice as fast if we do processing outside data.table then insert it later
  splitActivity = unlist( stringi::stri_extract_all_regex(dt$Activity, ".{2}") )

  splitActivityMat = matrix(splitActivity, ncol=6, byrow=TRUE)

  # Filter to stops for passengers
  #see https://wiki.openraildata.com/index.php?title=Activity_codes for definitions
  acts <- c(
    "TB", # Train Starts
    "T " , # Stops to take up and set down passengers
    "D ", # Stops to set down passengers
    "U ", # Stops to take up passengers
    "R ", # Request stop
    "TF"  # Train Finishes
  )

  if(public_only)
  {
    allowed = ("  "!=splitActivityMat) & (splitActivityMat %in% acts)
  }
  else
  {
    allowed = ("  "!=splitActivityMat)
  }

  splitActivityMat[!allowed] <- ""

  activity = sprintf("%s,%s,%s,%s,%s,%s", splitActivityMat[,1], splitActivityMat[,2], splitActivityMat[,3], splitActivityMat[,4], splitActivityMat[,5], splitActivityMat[,6] )

  #replace multiple comma with single comma, remove whitespace, remove leading comma, remove trailing comma.
  activity = gsub(",+", ",", activity)
  set(dt, j="Activity", value = gsub("\\s+|^,|,$", "", activity))

  #remove rows with no activity we're interested in (there is no activity at 'pass' locations)
  if(public_only)
  {
    dt <- dt[ ""!=dt$Activity ]
  }

  return(dt)
}




getCachedAgencyData <- function(agency = "atoc_agency")
{
  if(inherits(agency,"character"))
  {
    if(agency == "atoc_agency")
    {
      load_data("atoc_agency")
      agency = atoc_agency
    }
    else #TODO test column names
    {
      checkmate::check_file_exists(agency)
      agency <- utils::read.csv(agency, stringsAsFactors = FALSE)
    }

    if ( !inherits(agency, "data.frame") || 0==nrow(agency) ){ stop("failed to load atoc_agency data.") }
  }

  return (agency)
}


getCachedLocationData <- function(locations = "tiplocs")
{
  if(inherits(locations,"character"))
  {
    if(locations == "tiplocs")
    {
      load_data("tiplocs")
      locations = tiplocs
    }
    else
    {
      checkmate::check_file_exists(locations)
      locations <- utils::read.csv(locations, stringsAsFactors = FALSE)
    }

    if ( !inherits(locations, "data.frame") || 0==nrow(locations) ){ stop("failed to tiploc data.") }
  }

  # Get the Station Locations
  if (inherits(locations, "sf"))
  {
    stops <- cbind(locations, sf::st_coordinates(locations))
    stops <- sf::st_drop_geometry(stops)
    stops <- as.data.table(stops)
    setnames(stops, old = c("Y", "X"), new = c("stop_lat", "stop_lon"))
  }
  else #TODO test column names
  {
    stops = locations
  }

  stops$stop_lat <- round(stops$stop_lat, 5)
  stops$stop_lon <- round(stops$stop_lon, 5)

  return (stops)
}
