#' ATOC to GTFS
#'
#' @details
#' Convert ATOC Files to GTFS
#'
#' @param path_in Path to ATOC File
#' @param path_out Path to where GTFS files should be saved
#' @param silent Logical, should progress be shown
#' @param ncores Numeric, When parallel processing how many cores to use
#' @param locations character "file" or "package" or path to stops.txt GTFS file, where to get the location data from, default "package"
#'
#' @details
#' Locations
#' The .msn file contains the physical locations of stations and other TIPLOC codes (e.g. junctions).
#' However, the quality of the locations is often poor only accurate to about 1km and occasionally very wrong.
#' Therefore, the UK2GTFS package contains an internal dataset of the TIPLOC locations with better location accuracy.
#'
#' @export

atoc2gtfs <- function(path_in,path_out, silent = TRUE, ncores = 1, locations = "package"){

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


  stop_times = mca[["stop_times"]]
  schedule = mca[["schedule"]]

  stop_times = stop_times[,c("Scheduled Arrival Time","Scheduled Departure Time","Location","stop_sequence","Activity","rowID","schedule")]
  names(stop_times) = c("arrival_time","departure_time","stop_id","stop_sequence","Activity","rowID","schedule")


  # Get the Station Locations
  if(locations == "package"){
    stops = sf::read_sf("./data/tiplocs.geojson", stringsAsFactors = F)
    #stops = readRDS("data/tiplocs.Rds")
    stops = cbind(stops, sf::st_coordinates(stops))
    stops = as.data.frame(stops)
    stops = stops[,c("stop_id","stop_code", "stop_name","Y","X","valid")]
    names(stops) = c("stop_id","stop_code", "stop_name","stop_lat","stop_lon","valid")
    stops$stop_lat = round(stops$stop_lat, 5)
    stops$stop_lon = round(stops$stop_lon, 5)
    stops$valid = NULL
  }else if(locations == "file"){
    station = msn[[1]]
    TI = mca[["TI"]]
    stops.list = station2stops(station = station, TI = TI)
    stops = stops.list[["stops"]]
    #stops.lookup = stops.list[["lookup"]]
  }else{
    stops = read.csv(locations,stringsAsFactors = F)
  }

  # remove any unused stops
  stops = stops[stops$stop_id %in% stop_times$stop_id,]


  timetables = schedule2routes_alt(stop_times = stop_times, schedule = schedule, silent = silent, ncores = ncores)

  calendar = timetables[["calendar"]]
  calendar_dates = timetables[["calendar_dates"]]
  routes = timetables[["routes"]]
  stop_times = timetables[["stop_times"]]
  trips = timetables[["trips"]]
  agency = read.csv("data/agency.txt", stringsAsFactors = F)

  write.csv(calendar,paste0(path_out,"/calendar.txt"), row.names = FALSE )
  write.csv(calendar_dates,paste0(path_out,"/calendar_dates.txt"), row.names = FALSE )
  write.csv(routes,paste0(path_out,"/routes.txt"), row.names = FALSE )
  write.csv(stop_times,paste0(path_out,"/stop_times.txt"), row.names = FALSE )
  write.csv(trips,paste0(path_out,"/trips.txt"), row.names = FALSE )
  write.csv(stops,paste0(path_out,"/stops.txt"), row.names = FALSE )
  write.csv(agency,paste0(path_out,"/agency.txt"), row.names = FALSE )
}
