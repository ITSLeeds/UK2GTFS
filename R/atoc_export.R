#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param mca list of dataframs from the importMCA function
#' @param path_out Path to save file to
#'
schedule2routes = function(mca,path_out){
  #list(HD,TI,TA,TD,AA,BS,BX,LO,LI,LT,CR,ZZ)

  schedule = mca[[6]]
  #schedule.extra = mca[[7]]
  station.origin = mca[[8]]
  station.intermediate = mca[[9]]
  station.terminal = mca[[10]]

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

  stop_times = stop_times[order(stop_times$rowID),]

  # Join the schedule and schedule.extra into on df
  #schedule.extra$schedulerowID = schedule.extra$rowID - 1
  #schedule.extra$rowID = NULL
  #schedule = dplyr::left_join(schedule, schedule.extra, by = c("rowID" = "schedulerowID"))
  schedule = schedule[order(schedule$rowID),]

  # build the calendar file
  calendar = schedule[,c("Train UID","Date Runs From", "Date Runs To","Days Run","STP indicator","rowID")]
  calendar$`STP indicator` = as.character(calendar$`STP indicator`)
  #calendar = calendar[order(-calendar$`STP indicator`),]
  names(calendar) = c("UID","start_date", "end_date","Days","STP",  "rowID"  )
  calendar$duration = calendar$end_date - calendar$start_date + 1

  # calendar$service_id = 1:nrow(calendar)
  UIDs = unique(calendar$UID)
  res.calendar = list()
  res.calendar_dates = list()
  for(i in 1:length(UIDs)){
    UIDs.sub = UIDs[i]
    calendar.sub = calendar[calendar$UID == UIDs.sub,]
    #calendar.sub = schedule[schedule$`Train UID` == UIDs.sub,]
    if(nrow(calendar.sub)==1){
      #make into an single entry
      res.calendar[[i]] = calendar.sub
    }else if(nrow(calendar.sub)==2){
      # check duration
      dur = as.numeric(calendar.sub$duration[calendar.sub$STP != "P"])
      if(dur == 1){
        # Modify in the calendar_dates.txt
        res.calendar[[i]] = calendar.sub[calendar.sub$STP == "P", ]
        res.calendar_dates[[i]] = calendar.sub[calendar.sub$STP != "P", ]
      }else{
        message("complicated two way")
        stop()
      }
    }else{
      #check for a difference
      message("three way")
      stop()

      #Firs get all the dates that something changes
      # dates = unique(c(calendar.sub$start_date, calendar.sub$end_date))
      # dates = dates[order(dates)]
      #
      # calendar.new = data.frame(start_date = dates[1:(length(dates)-1) ],
      #                           end_date = dates[2:length(dates)])
      #
      # calendar.sub2 = calendar.sub[,c("UID","start_date","Days","STP","rowID","duration")]
      # calendar.new = dplyr::left_join(calendar.new, calendar.sub2, by = "start_date")
    }

  }




  calendar.p = calendar[calendar$`STP indicator` == "P",]
  #days = substring(calendar$`Days Run`[1:2], seq(1, 7, 1), seq(1, 7, 1) )

  days = lapply(calendar.p$`Days Run`,function(x){as.integer(substring(x, 1:7, 1:7 ))})
  days = matrix(unlist(days), ncol = 7, byrow = T)
  days = as.data.frame(days)
  names(days) = c("monday","tuesday", "wednesday","thursday", "friday", "saturday", "sunday")

  calendar.p = cbind(calendar.p,days)
  calendar.p$`Days Run` = NULL
  names(calendar.p) = c("service_id","start_date", "end_date","STP indicator","rowID","monday","tuesday","wednesday",
                        "thursday","friday","saturday","sunday")



  #Work Our which trips match which stops
  #length_todo = nrow(schedule)
  #length_stops = seq(from = 1, to = nrow(stop_times))
  #trip_ids = list()
  #microbenchmark(matchRoutes(1))

  lookup = matrix(c(schedule$rowID,
                    schedule$rowID[2:length(schedule$rowID)],
                    lookup[nrow(lookup),1]+1000),
                  ncol = 2)
  lookup <- cbind(lookup,lookup[,2]-lookup[,1])
  lookup.df = data.frame(rowID = seq(from = lookup[1,1],
                                     to = lookup[nrow(lookup),2]-1),
                         trip_id = rep(schedule$`Train UID`,times = as.integer(lookup[,3]) ))

  stop_times = dplyr::left_join(stop_times,lookup.df, by = c("rowID" = "rowID"))
  stop_times = stop_times[,c("trip_id","arrival_time","departure_time","stop_id","rowID")]

  # matchRoutes = function(i){
  #   if(i != length_todo){
  #     schedule.sub = schedule[c(i,i+1), ]
  #     sub.rowID1 = schedule.sub$rowID[1]
  #     sub.rowID2 = schedule.sub$rowID[2]
  #     sub.UID = schedule.sub$`Train UID`[1]
  #     stop_times.sub = stop_times[stop_times$rowID > sub.rowID1,]
  #     rows.todo = stop_times.sub$rowID[stop_times.sub$rowID < sub.rowID2]
  #   }else{
  #     schedule.sub = schedule[i, ]
  #     sub.rowID1 = schedule.sub$rowID[1]
  #     sub.UID = schedule.sub$`Train UID`[1]
  #     rows.todo = stop_times$rowID[stop_times$rowID > sub.rowID1]
  #   }
  #
  #   res = data.frame(rowID = rows.todo, trip_id = sub.UID)
  #   return(res)
  #
  #   #if(!silent){
  #   #  if(i %% 1000 == 0){
  #   #    message(paste0(Sys.time()," matched ",round(i/length_todo,1),"% of routes"))
  #   #  }
  #   #}
  #   #End of loop
  #   #rm(schedule.sub,sub.rowID1,sub.UID,res)
  # }


  matchRoutes = function(i){
    if(i != length_todo){
      schedule.sub = schedule[c(i,i+1), ]
      sub.rowID1 = schedule.sub$rowID[1]
      sub.rowID2 = schedule.sub$rowID[2]
      sub.UID = schedule.sub$`Train UID`[1]
      stop_times.sub = stop_times[stop_times$rowID > sub.rowID1,]
      rows.todo = stop_times.sub$rowID[stop_times.sub$rowID < sub.rowID2]
    }else{
      schedule.sub = schedule[i, ]
      sub.rowID1 = schedule.sub$rowID[1]
      sub.UID = schedule.sub$`Train UID`[1]
      rows.todo = stop_times$rowID[stop_times$rowID > sub.rowID1]
    }

    res = data.frame(rowID = rows.todo, trip_id = sub.UID)
    return(res)

    #if(!silent){
    #  if(i %% 1000 == 0){
    #    message(paste0(Sys.time()," matched ",round(i/length_todo,1),"% of routes"))
    #  }
    #}
    #End of loop
    #rm(schedule.sub,sub.rowID1,sub.UID,res)
  }


}



#' Export ATOC stations as GTFS stops.txt
#'
#' @details
#' Export ATOC stations as GTFS stops.txt
#'
#' @param station station SF data frame from the importMSN function
#' @param path_out Path to save file to
#'
station2stops = function(station,path_out){
  # recorder the match the GTFS stops.txt
  stops = station[,c("TIPLOC Code","CRS Code","Station Name")]
  names(stops) = c("stop_id","stop_code","stop_name","geometry")
  coords = st_coordinates(stops)
  stops$stop_lat = coords[,2]
  stops$stop_lon = coords[,1]
  stops = as.data.frame(stops)
  stops$geometry = NULL
  write.csv(stops,paste0(path_out,"/stops.txt"), row.names = F, header = F)
}
