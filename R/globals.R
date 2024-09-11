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
  'operator_code','route_id',
  'speed_after','distance','school_terms','distance_after','historic_bank_holidays',
  'runs_monday','runs_tuesday','runs_wednesday','runs_thursday','runs_friday',
  'runs_saturday','runs_sunday', 'total_sunday',
  'runs_Mon','runs_Tue','runs_Wed','runs_Thu','runs_Fri',
  'runs_Sat','runs_Sun',
  'tot_Mon','tot_Tue','tot_Wed','tot_Thu','tot_Fri',
  'tot_Sat','tot_Sun',
  'service_id','zone_id','time_bands','stop_name'
))

