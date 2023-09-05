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
  'speed','agency_id','agency_name',
  'agency_url','agency_timezone',
  'agency_lang','agency_id',   'Freq',
  'UID','hash','vehicle_type','running_board','service_number',
  'operator_code','route_id'
))



#' UK2GTFS option stopProcessingAtUid
#' @description sets/gets a UID value at which processing will stop - used for debugging
#' @param value option value to be set
#' @details If no value passed in will return the current setting of the option. (Usually NULL)
#'   If value passed in, timetable build processing will stop in atoc_overlay.makeCalendarInner()
#'   when an exact match for that value is encountered.
#'
#'   THIS ONLY WORKS WITH ncores==1
#'   (probably some environment nonsense I don't understand)
#'
#' @export
UK2GTFS_option_stopProcessingAtUid <- function(value)
{
  if (missing(value))
  {
    return( getOption("UK2GTFS_opt_stopProcessingAtUid") )
  }
  else
  {
    return( options(UK2GTFS_opt_stopProcessingAtUid = value) )
  }
}

