#' Write GTFS
#'
#' Takes a list of data.frames represneting the GTFS fromat and saves them as GTFS
#' Zip file.
#'
#' @param gtfs named list of data.frames
#' @param folder folder to save the gtfs file to
#' @param name the name of the zip file, default "gtfs"
#'
write_gtfs <- function(gtfs, folder = getwd(), name = "gtfs"){
  #dir.create(paste0(folder,"/gtfs_temp"))
  write.csv(gtfs$calendar,       paste0(folder,"/calendar.txt"),       row.names = FALSE )
  write.csv(gtfs$calendar_dates, paste0(folder,"/calendar_dates.txt"), row.names = FALSE )
  write.csv(gtfs$routes,         paste0(folder,"/routes.txt"),         row.names = FALSE )
  write.csv(gtfs$stop_times,     paste0(folder,"/stop_times.txt"),     row.names = FALSE )
  write.csv(gtfs$trips,          paste0(folder,"/trips.txt"),          row.names = FALSE )
  write.csv(gtfs$stops,          paste0(folder,"/stops.txt"),          row.names = FALSE )
  write.csv(gtfs$agency,         paste0(folder,"/agency.txt"),         row.names = FALSE )
  #zip(zipfile = paste0(folder,"/",name,".zip"), files = dir(paste0(folder,"/gtfs_temp"), full.names = TRUE))
  #unlink(paste0(folder,"/gtfs_temp"), recursive = T)
  message(paste0(folder,"/",name,".zip"))
}


