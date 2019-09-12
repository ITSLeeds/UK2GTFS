#' Reduce file size of a GTFS object
#'
#' @param gtfs agtfs objects
#' @export
gtfs_compress <- function(gtfs){

  agency <- gtfs$agency
  stops <- gtfs$stops
  routes <- gtfs$routes
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  # Simplify stop_ids
  stop_id <- unique(stops$stop_id)
  stops$stop_id <- as.integer(factor(stops$stop_id, levels = stop_id))
  stop_times$stop_id <- as.integer(factor(stop_times$stop_id, levels = stop_id))

  # Simplify trip_ids
  trip_id <- unique(trips$trip_id)
  trips$trip_id <- as.integer(factor(trips$trip_id, levels = trip_id))
  stop_times$trip_id <- as.integer(factor(stop_times$trip_id, levels = trip_id))

  # Simplify route_ids
  route_id <- unique(routes$route_id)
  routes$route_id <- as.integer(factor(routes$route_id, levels = route_id))
  trips$route_id <- as.integer(factor(trips$route_id, levels = route_id))

  # Find similar trips and compress
  # 1 Find routes with lots of trips
  # tab <- dplyr::group_by(trips, route_id)
  # tab <- dplyr::summarise(tab, count = length(trip_id))
  # tab <- tab[tab$count > 1000,]
  # if(nrow(tab) > 0){
  #   for(i in tab$route_id[7:length(tab$route_id)]){
  #     message(i)
  #     trips_sub <- trips[trips$route_id == i,]
  #     utid <- unique(trips_sub$service_id)
  #     if(length(utid) > 1){
  #       calendar_sub <- calendar[calendar$service_id %in% utid,]
  #       calendar_dates_sub <- calendar_dates[calendar_dates$service_id %in% utid,]
  #       cds_dup <- duplicated(calendar_dates_sub[,c("date","exception_type")])
  #       if(any(cds_dup)){
  #         # Check for duplication in calendar dates
  #         tidc <- calendar_dates_sub$service_id[cds_dup | duplicated(calendar_dates_sub[,c("date","exception_type")], fromLast = TRUE)]
  #         calendar_sub <- calendar_sub[calendar_sub$service_id %in% tidc,]
  #         calendar_sub <- split(calendar_sub, paste0(calendar_sub$start_date, calendar_sub$end_date))
  #         res <- list()
  #         # check for duplication in calendar
  #         for(j in seq(1, length(calendar_sub))){
  #           calendar_sub2 <- calendar_sub[[j]]
  #           if(nrow(calendar_sub2) > 1){
  #             sms <- colSums(calendar_sub2[,c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")])
  #             if(max(sms) == 1){
  #               # check for duplication in stop_times
  #               trips_sub2 <- trips_sub[trips_sub$service_id %in% calendar_sub2$service_id,]
  #               stop_times_sub <- stop_times[stop_times$trip_id %in% trips_sub2$trip_id,]
  #               sts_trip_ids <- stop_times_sub$trip_id
  #               stop_times_sub$trip_id <- NULL
  #               stop_times_sub <- split(stop_times_sub, sts_trip_ids)
  #               if(any(duplicated(stop_times_sub))){
  #                 stop(" Found one")
  #               }else{
  #                 res[[j]] <- NULL
  #               }
  #
  #
  #
  #             }else{
  #               res[[j]] <- NULL
  #
  #             }
  #           }else{
  #             res[[j]] <- NULL
  #           }
  #
  #         }
  #       }
  #     }
  #   }
  # }




  gtfs$stops <- stops
  gtfs$routes <- routes
  gtfs$trips <- trips
  gtfs$stop_times <- stop_times

  return(gtfs)

}
