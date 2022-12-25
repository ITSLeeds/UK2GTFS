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

