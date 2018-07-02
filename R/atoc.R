#' Convert ATOC Files to GTFS
#'
#'
#' @details
#' Requires the internet and a transportapi.com API key.
#'
#' @param path_in Path to ATOC File
#' @param path_out Path to where GTFS files should be saved
#' @inheritParams
#' @seealso
#' @export
#' @examples
#' \dontrun{
#' from = c(-0.134649,51.529258) # Euston Station
#' to = c(-0.088780,51.506383) # Bridge House
#' r1 = journey(from, to)
#' r2 = journey(from, to, apitype = "car")
#' }
path_out =  "D:/Users/earmmor/OneDrive - University of Leeds/Routing/gtfs"
path_in =  "D:/Users/earmmor/OneDrive - University of Leeds/Routing/ttis898.zip"
ncores = 7
silent = TRUE
atoc2gtfs <- function(path_in,path_out, silent = TRUE, ncores = 1){

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
  mca = importMCA(files[grepl(".mca",files)], silent = silent)
  msn = importMSN(files[grepl(".msn",files)], silent = silent)
  #ztr = importMCA(files[grepl(".ztr",files)], silent = silent)

  #Construct the GTFS
  station = msn[[1]]
  TI = mca[["TI"]]
  stops.list = station2stops(station = station, TI = TI)
  stops = stops.list[["stops"]]
  stops.lookup = stops.list[["lookup"]]
  timetables = schedule2routes(mca = mca, ncores = ncores)

  calendar = timetables[["calendar"]]
  calendar_dates = timetables[["calendar_dates"]]
  routes = timetables[["routes"]]
  stop_times = timetables[["stop_times"]]
  trips = timetables[["trips"]]

  # change stop_times from TIPLOCS to CRS
  stop_times = dplyr::left_join(stop_times, stops.lookup, by = c("stop_id" = "TIPLOC"))
  stop_times = stop_times[,c("trip_id","arrival_time","departure_time", "match","stop_sequence","pickup_type","drop_off_type")]
  names(stop_times) = c("trip_id","arrival_time","departure_time", "stop_id","stop_sequence","pickup_type","drop_off_type")

  # remove any unused stops
  stops = stops[stops$stop_id %in% stop_times$stop_id,]

  #remove any stop_times at unknown stops
  # these are junctions that have scheduels arrive and departute times
  stop_times = stop_times[stop_times$stop_id %in% stops$stop_id,]

  #foo = unique(stop_times$stop_id[!stop_times$stop_id %in% stops$stop_id])

  #an clean duplicated stop names
  # summary(unique(stop_times$stop_id) %in% stops$stop_id)
  # stops.used = stops[stops$stop_id %in% unique(stop_times$stop_id),]
  # stops.used$used = sapply(stops.used$stop_id,function(x){sum(stop_times$stop_id == x)})
  # stops.used$stop_id.new = sapply(1:nrow(stops.used),function(i){stops.used$stop_id[stops.used$stop_code == stops.used$stop_code[i] &
  #                                                                                     stops.used$used == max(stops.used$used[stops.used$stop_code == stops.used$stop_code[i]])]})
  #
  # stops.match = stops.used[,c("stop_id","stop_id.new")]
  # stop_times = dplyr::left_join(stop_times,stops.match, by = "stop_id")
  # stop_times.clean = stop_times[!is.na(stop_times$stop_id.new),]
  # stop_times.clean = stop_times.clean[,c("trip_id","arrival_time","departure_time","stop_id.new","stop_sequence", "pickup_type","drop_off_type")]
  # names(stop_times.clean) = c("trip_id","arrival_time","departure_time","stop_id","stop_sequence", "pickup_type","drop_off_type")
  #
  # stops.used = stops.used[stops.used$stop_id.new %in% unique(stop_times$stop_id),]
  # stops.used = stops.used[,c("stop_id.new","stop_code","stop_name","stop_lat","stop_lon")]
  # names(stops.used) = c("stop_id","stop_code","stop_name","stop_lat","stop_lon")
  # stops.used = stops.used[!duplicated(stops.used$stop_id),]
  #
  # #clean stops with identical locations
  # stops.used$used = sapply(stops.used$stop_id,function(x){sum(stop_times.clean$stop_id == x)})
  # stops.used$stop_id.new = sapply(1:nrow(stops.used),function(i){stops.used$stop_id[stops.used$stop_lat == stops.used$stop_lat[i] &
  #                                                                                     stops.used$stop_lon == stops.used$stop_lon[i] &
  #                                                                                     stops.used$used == max(stops.used$used[stops.used$stop_code == stops.used$stop_code[i]])]})
  #
  # stops.match = stops.used[,c("stop_id","stop_id.new")]
  # stop_times.clean2 = dplyr::left_join(stop_times.clean,stops.match, by = "stop_id")
  # stop_times.clean2 = stop_times.clean2[!is.na(stop_times.clean2$stop_id.new),]
  # stop_times.clean2 = stop_times.clean2[,c("trip_id","arrival_time","departure_time","stop_id.new","stop_sequence", "pickup_type","drop_off_type")]
  # names(stop_times.clean2) = c("trip_id","arrival_time","departure_time","stop_id","stop_sequence", "pickup_type","drop_off_type")
  #
  # stops.used = stops.used[stops.used$stop_id.new %in% unique(stop_times.clean2$stop_id),]
  # stops.used = stops.used[,c("stop_id.new","stop_code","stop_name","stop_lat","stop_lon")]
  # names(stops.used) = c("stop_id","stop_code","stop_name","stop_lat","stop_lon")
  # stops.used = stops.used[!duplicated(stops.used$stop_id),]


  write.csv(calendar,paste0(path_out,"/calendar.txt"), row.names = FALSE )
  write.csv(calendar_dates,paste0(path_out,"/calendar_dates.txt"), row.names = FALSE )
  write.csv(routes,paste0(path_out,"/routes.txt"), row.names = FALSE )
  write.csv(stop_times,paste0(path_out,"/stop_times.txt"), row.names = FALSE )
  write.csv(trips,paste0(path_out,"/trips.txt"), row.names = FALSE )
  write.csv(stops,paste0(path_out,"/stops.txt"), row.names = FALSE )

}



