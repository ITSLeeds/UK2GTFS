#' Convert ATOC Files to GTFS
#'
#'
#' @details
#' Requires the internet and a transportapi.com API key.
#'
#' @param path_in Path to ATOC File
#' @param path_out Path to where GTFS files should be saved
#' @param silent Logical, should progress be shown
#' @param ncores Numeric, When parallel processing how many cores to use
#' @export
#' }
atoc2gtfs <- function(path_in,path_out, silent = TRUE, ncores = 1){

  if(ncores == 1){message(paste0(Sys.time()," This will take some time, make sure you use 'ncores' to enable multi-core processing"))}
  # Is input a zip or a folder
  if(grepl(".zip",path_in)){
    # Unzip
    files <- unzip(path_in, exdir = "tmp")
    cleanup <- TRUE
  }else{
    # folder
    cleanup <- FALSE
    files <- list.files(path_in, full.names = T)
  }

  # Are all the files we would expect there?
  files.ext = substr(files, nchar(files) - 3, nchar(files))
  files.ext.need = c(".alf",".dat",".flf",".mca",".msn",".set",".tsi",".ztr")
  if(!all(files.ext.need %in% files.ext)){
    # Missing Some files
    files.ext.missing = files.ext.need[!files.ext.need %in% files.ext]
    warning(paste0("Missing files with the extension(s) ", paste(files.ext.missing, collapse = " ")))
    stop()
  }

  # Read In each File
  alf = importALF(files[grepl(".alf",files)])
  flf = importFLF(files[grepl(".flf",files)])
  mca = importMCA_alt(file = files[grepl(".mca",files)], silent = silent, ncores = 1)
  msn = importMSN(files[grepl(".msn",files)], silent = silent)
  #ztr = importMCA(files[grepl(".ztr",files)], silent = silent)

  #Construct the GTFS
  station = msn[[1]]
  TI = mca[["TI"]]
  stops.list = station2stops(station = station, TI = TI)
  stops = stops.list[["stops"]]
  stops.lookup = stops.list[["lookup"]]

  stop_times = mca[["stop_times"]]
  schedule = mca[["schedule"]]

  stop_times = stop_times[,c("Public Arrival Time","Public Departure Time","Location","stop_sequence","Activity","rowID","schedule")]
  names(stop_times) = c("arrival_time","departure_time","stop_id","stop_sequence","Activity","rowID","schedule")

  # change stop_times from TIPLOCS to CRS
  stop_times = dplyr::left_join(stop_times, stops.lookup, by = c("stop_id" = "TIPLOC"))
  stop_times = stop_times[,c("arrival_time","departure_time","match","stop_sequence","Activity","rowID","schedule")]
  names(stop_times) = c("arrival_time","departure_time","stop_id","stop_sequence","Activity","rowID","schedule")


  # remove any unused stops
  stops = stops[stops$stop_id %in% stop_times$stop_id,]

  # remove any stop_times at unknown stops
  # these are junctions that have scheduels arrive and departute times
  #stop_times = stop_times[stop_times$stop_id %in% stops$stop_id,]


  timetables = schedule2routes_alt(stop_times = stop_times, schedule = schedule, silent = silent, ncores = ncores)

  calendar = timetables[["calendar"]]
  calendar_dates = timetables[["calendar_dates"]]
  routes = timetables[["routes"]]
  stop_times = timetables[["stop_times"]]
  trips = timetables[["trips"]]

  write.csv(calendar,paste0(path_out,"/calendar.txt"), row.names = FALSE )
  write.csv(calendar_dates,paste0(path_out,"/calendar_dates.txt"), row.names = FALSE )
  write.csv(routes,paste0(path_out,"/routes.txt"), row.names = FALSE )
  write.csv(stop_times,paste0(path_out,"/stop_times.txt"), row.names = FALSE )
  write.csv(trips,paste0(path_out,"/trips.txt"), row.names = FALSE )
  write.csv(stops,paste0(path_out,"/stops.txt"), row.names = FALSE )

}



