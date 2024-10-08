# Fix for no variable bindings when using dplyr
utils::globalVariables(c(
  "trip_id", "dept", "stop_sequence",
  "arvfinal", "tiplocs", "atoc_agency", "calendar_dates",
  "activity_codes", "rowID", "trips", "minute",
  "second", "naptan_missing", "n", "arrival_time", "nstops",
  "stop_id", "exception_type", "start_date", "end_date",
  "monday", "tuesday", "wednesday", "thursday", "friday",
  "saturday", "sunday", "pattern", "schedule", "ATOC Code",
  "route_long_name", "Train Status", "i", "DaysOfWeek",
  'speed','agency_id','agency_name', 'agency_url','agency_timezone',
  'agency_lang','agency_id', 'Freq', 'operator_code','route_id',
  'UID','hash','vehicle_type','running_board','service_number',
  'speed_after','distance','school_terms','distance_after','historic_bank_holidays',
  'runs_monday','runs_tuesday','runs_wednesday','runs_thursday','runs_friday',
  'runs_saturday','runs_sunday', 'total_sunday',
  'runs_Mon','runs_Tue','runs_Wed','runs_Thu','runs_Fri',
  'runs_Sat','runs_Sun',
  'tot_Mon','tot_Tue','tot_Wed','tot_Thu','tot_Fri',
  'tot_Sat','tot_Sun',
  'zone_id','time_bands',
  '%>%', '.', 'Activity', 'Arrival Time', 'Departure Time', 'N', 'Public Arrival Time',
  'Public Departure Time','STP', 'Scheduled Arrival Time', 'Scheduled Departure Time',
  'Train Category', 'V1', '_TEMP_', '__TEMP__', 'duration',
  'i.friday', 'i.monday', 'i.saturday', 'i.sunday', 'i.thursday', 'i.tuesday', 'i.wednesday', 'originalUID',
  'route_id_new', 'route_type', 'service_id', 'service_id_new', 'stop_name', 'trip_id_new'
))



#' UK2GTFS option stopProcessingAtUid
#' @description sets/gets a UID value at which processing will stop - used for debugging
#' @param value option value to be set (char)
#' @details If no value passed in will return the current setting of the option. (Usually NULL)
#'   If value passed in, timetable build processing will stop in atoc_overlay.makeCalendarInner()
#'   when an exact match for that value is encountered.
#'
#'   THIS ONLY WORKS WITH ncores==1
#'
#' @export
UK2GTFS_option_stopProcessingAtUid <- function(value)
{
  if (missing(value))
  {
    return( getOption("UK2GTFS_opt_stopProcessingAtUid", default=NULL) )
  }
  else
  {
    if ( !is.null(value) && !inherits(value, "character") ){ value = as.character( value ) }

    if ( !is.null(value) && 0==nchar(value) ){ value=NULL }

    return( options(UK2GTFS_opt_stopProcessingAtUid = value ) )
  }
}




#' UK2GTFS option treatDatesAsInt
#' @description sets/gets a logical value which determines how dates are processed while building calendar - used for debugging
#' @param value option value to be set (logical)
#' @details In the critical part of timetable building, handling dates as dates is about half the speed of handling as int
#'   so we treat them as integers. However that's a complete pain for debugging, so make it configurable.
#'   if errors are encountered during the timetable build phase, try setting this value to FALSE
#'
#' @export
UK2GTFS_option_treatDatesAsInt <- function(value)
{
  if (missing(value))
  {
    return( getOption("UK2GTFS_opt_treatDatesAsInt", default=TRUE) )
  }
  else
  {
    return( options(UK2GTFS_opt_treatDatesAsInt = as.logical(value) ) )
  }
}



#' UK2GTFS option updateCachedDataOnLibaryLoad
#' @description sets/gets a logical value which determines if the data cached in the library is checked for update when loaded
#' @param value option value to be set (logical)
#' @details when child processes are initialised we want to suppress this check, so it is also used for that purpose
#'
#' @export
UK2GTFS_option_updateCachedDataOnLibaryLoad <- function(value)
{
  if (missing(value))
  {
    return( getOption("UK2GTFS_opt_updateCachedDataOnLibaryLoad", default=TRUE) )
  }
  else
  {
    return( options(UK2GTFS_opt_updateCachedDataOnLibaryLoad = as.logical(value) ) )
  }
}



