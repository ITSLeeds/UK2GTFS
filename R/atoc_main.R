#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param mca list of dataframs from the importMCA function
#' @param path_out Path to save file to
#'
schedule2routes = function(mca,ncores = 1){
  #list(HD,TI,TA,TD,AA,BS,BX,LO,LI,LT,CR,ZZ)

  #break out the relevant parts of the mca file
  schedule = mca[[6]]
  schedule.extra = mca[[7]]
  station.origin = mca[[8]]
  station.intermediate = mca[[9]]
  station.terminal = mca[[10]]

  ### SECTION 1: ###############################################################################
  # make stop_times.txt from the three station files

  # Remove Passing Stops as GTFS is only intrested in actual stops
  station.intermediate = station.intermediate[is.na(station.intermediate$`Scheduled Pass`),]
  station.intermediate = station.intermediate[station.intermediate$`Public Departure` != "0000",]
  station.intermediate = station.intermediate[station.intermediate$`Public Arrival` != "0000",]

  # bind the station secions toghter into a single df
  station.origin = station.origin[,c("Public Departure Time","Location","rowID")]
  names(station.origin) = c("departure_time","stop_id","rowID")
  station.intermediate = station.intermediate[,c("Public Arrival","Public Departure","Location","rowID")]
  names(station.intermediate) = c("arrival_time","departure_time","stop_id","rowID")
  station.terminal = station.terminal[,c("Public Arrival Time","Location","rowID")]
  names(station.terminal) = c("arrival_time","stop_id","rowID")

  # bind together and rorder
  stop_times = dplyr::bind_rows(list(station.origin,station.intermediate,station.terminal))
  rm(station.origin,station.intermediate,station.terminal)
  #stop_times$trip_id = NA

  # match routes
  # for each station in the stop_times, match the rowID in the schdeduel
  # data must be sorted
  stop_times = stop_times[order(stop_times$rowID),]
  schedule = schedule[order(schedule$rowID),]
  trip_ids = matchRoutes(schedule.rowID = schedule$rowID, stop_times.rowID = stop_times$rowID, ncores = ncores)

  stop_times = dplyr::left_join(stop_times,trip_ids, by = c("rowID" = "stop_times.rowID"))
  names(stop_times) = c("departure_time","stop_id","rowID","arrival_time","trip_id")

  ### SECTION 2: ###############################################################################
  # make make the calendar.txt and calendar_dates.txt file from the schedule

  # build the calendar file
  res = makeCalendar(schedule = schedule, ncores = ncores)
  calendar = res[[1]]
  calendar_dates = res[[2]]
  rm(res)


  ### SECTION 4: ###############################################################################
  # make make the routes.txt
  # a route is all the trips with a common start and end i.e. scheduels original UID

  routes = schedule[!duplicated(schedule$`Train UID`),]
  routes = routes[,c("rowID","Train UID","Train Status")]
  #routes = dplyr::left_join(routes,stop_times,by = c("rowID" = "trip_id"))

  schedule.extra$rowIDm1 = schedule.extra$rowID -1
  routes = dplyr::left_join(routes,schedule.extra,by = c("rowID" = "rowIDm1"))
  routes = routes[,c("rowID","Train UID","Train Status","ATOC Code")]
  names(routes) = c("rowID","route_id","Train Status","agency_id")

  train_status = data.frame(train_status = c("B","F","P","S","T","1","2","3","4","5"),
                            route_type   = c( 3 ,NA , 2 , 4 , NA, 2 , NA, NA, 4 , 3 ),
                            stringsAsFactors = FALSE)

  routes$`Train Status` = as.character(routes$`Train Status`)
  routes = dplyr::left_join(routes,train_status,by = c("Train Status" = "train_status"))
  rm(train_status)

  routes = routes[,c("rowID","route_id","route_type","agency_id")]
  routes$route_short_name = routes$route_id

  #make the long names from the desitnation and time
  route_long_name = longnames(routes = routes, stop_times = stop_times, ncores = ncores)
  routes$route_long_name = route_long_name
  routes =  routes[,c("route_id","agency_id","route_short_name","route_long_name","route_type")]
  #head(routes)

  ### SECTION 3: ###############################################################################
  # make make the trips.txt  file by matching the calnedar to the stop_times

  trips = calendar[c("UID","rowID")]
  names(trips) = c("service_id","trip_id")

  route_id = strsplit(trips$service_id,  " ")
  route_id = lapply(route_id, `[[`, 1)
  route_id = unlist(route_id)
  trips$route_id = route_id
  trips = trips[,c("route_id","service_id","trip_id")]

  #section 5: #######################################################
  # clean calednars
  calendar = calendar[,c("UID","monday","tuesday","wednesday","thursday","friday","saturday","sunday",
                         "start_date","end_date")]
  names(calendar)[1] = "service_id"
  calendar$start_date = as.character(calendar$start_date)
  calendar$start_date = gsub("-","",calendar$start_date)
  calendar$end_date = as.character(calendar$end_date)
  calendar$end_date = gsub("-","",calendar$end_date)

  calendar_dates = calendar_dates[,c("UID","start_date")]
  names(calendar_dates) = c("service_id","date")
  calendar_dates$date = as.character(calendar_dates$date)
  calendar_dates$date = gsub("-","",calendar_dates$date)
  calendar_dates$exception_type = 2 # all events passed to calendar_dates are single day cancellations

  #clean stop times
  stop_times = stop_times[,c("trip_id","arrival_time","departure_time","stop_id")]
  stop_times = dplyr::group_by(stop_times, trip_id)
  stop_times = dplyr::mutate(stop_times, stop_sequence=1:n())
  stop_times$pickup_type = 0 # trains stop at all timetabled stops (unlike busses)
  stop_times$drop_off_type = 0
  stop_times$arrival_time[is.na(stop_times$arrival_time)] = stop_times$departure_time[is.na(stop_times$arrival_time)]
  stop_times$departure_time[is.na(stop_times$departure_time)] = stop_times$arrival_time[is.na(stop_times$departure_time)]
  stop_times$arrival_time = paste0(substr(stop_times$arrival_time,1,2),":",substr(stop_times$arrival_time,3,4))
  stop_times$departure_time = paste0(substr(stop_times$departure_time,1,2),":",substr(stop_times$departure_time,3,4))


  #end of function
  results =     list(calendar,   calendar_dates,  routes,  stop_times,  trips)
  names(results) = c("calendar","calendar_dates","routes","stop_times","trips")
  return(results)




}
