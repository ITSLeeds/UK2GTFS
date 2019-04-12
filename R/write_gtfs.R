#' Write GTFS
#'
#' Takes a list of data.frames represneting the GTFS fromat and saves them as GTFS
#' Zip file.
#'
#' @param gtfs named list of data.frames
#' @param folder folder to save the gtfs file to
#' @param name the name of the zip file, default "gtfs"
#' @export
#'
write_gtfs <- function(gtfs, folder = getwd(), name = "gtfs"){
  dir.create(paste0(folder,"/gtfs_temp"))
  utils::write.csv(gtfs$calendar,       paste0(folder,"/gtfs_temp/calendar.txt"),       row.names = FALSE )
  utils::write.csv(gtfs$calendar_dates, paste0(folder,"/gtfs_temp/calendar_dates.txt"), row.names = FALSE )
  utils::write.csv(gtfs$routes,         paste0(folder,"/gtfs_temp/routes.txt"),         row.names = FALSE )
  utils::write.csv(gtfs$stop_times,     paste0(folder,"/gtfs_temp/stop_times.txt"),     row.names = FALSE )
  utils::write.csv(gtfs$trips,          paste0(folder,"/gtfs_temp/trips.txt"),          row.names = FALSE )
  utils::write.csv(gtfs$stops,          paste0(folder,"/gtfs_temp/stops.txt"),          row.names = FALSE )
  utils::write.csv(gtfs$agency,         paste0(folder,"/gtfs_temp/agency.txt"),         row.names = FALSE )
  zip::zipr(paste0(folder,"/",name,".zip"), list.files(paste0(folder,"/gtfs_temp"), full.names = TRUE), recurse = FALSE)
  unlink(paste0(folder,"/gtfs_temp"), recursive = T)
  message(paste0(folder,"/",name,".zip"))
}


