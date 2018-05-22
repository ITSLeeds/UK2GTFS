#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param mca list of dataframs from the importMCA function
#' @param path_out Path to save file to
#'
schedule2routes = function(mca,path_out,ncores = 1){
  #list(HD,TI,TA,TD,AA,BS,BX,LO,LI,LT,CR,ZZ)

  #break out the relevant parts of the mca file
  schedule = mca[[6]]
  #schedule.extra = mca[[7]]
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
  calendar = schedule[,c("Train UID","Date Runs From", "Date Runs To","Days Run","STP indicator","rowID")]
  calendar$`STP indicator` = as.character(calendar$`STP indicator`)
  #calendar = calendar[order(-calendar$`STP indicator`),]
  names(calendar) = c("UID","start_date", "end_date","Days","STP",  "rowID"  )
  calendar$duration = calendar$end_date - calendar$start_date + 1

  # calendar$service_id = 1:nrow(calendar)
  UIDs = unique(calendar$UID)
  res.calendar = list()
  res.calendar_dates = list()
  length_todo = length(UIDs)
  for(i in 1:length_todo){
    UIDs.sub = UIDs[i]
    calendar.sub = calendar[calendar$UID == UIDs.sub,]
    #calendar.sub = schedule[schedule$`Train UID` == UIDs.sub,]
    if(nrow(calendar.sub)==1){
      #make into an single entry
      res.calendar[[i]] = calendar.sub
    }else{
      # check duration and types
      dur = as.numeric(calendar.sub$duration[calendar.sub$STP != "P"])
      typ = calendar.sub$STP[calendar.sub$STP != "P"]
      typ.all = calendar.sub$STP
      if(all(dur == 1) & all(typ == "C") & length(typ) > 0 & length(typ.all) == 2){
        # One Day cancelationss
        # Modify in the calendar_dates.txt
        res.calendar[[i]] = calendar.sub[calendar.sub$STP == "P", ]
        res.calendar_dates[[i]] = calendar.sub[calendar.sub$STP != "P", ]
      }else{
        # check for identical day pattern
        if(length(unique(calendar.sub$Days)) == 1 & sum(typ.all == "P") == 1){

          calendar.new = splitDates(calendar.sub)
          res.calendar[[i]] = calendar.new
        }else{
          # split by day pattern
          splits = list()
          daypatterns = unique(calendar.sub$Days)
          for(k in seq(1,length(daypatterns))){
            #slect for each patter but include cancellations with a different day pattern
            calendar.sub.day = calendar.sub[calendar.sub$Days == daypatterns[k] | calendar.sub$STP == "C", ]
            calendar.new.day = splitDates(calendar.sub.day)
            # rejects nas
            if(class(calendar.new.day) == "data.frame"){
              calendar.new.day$UID = paste0(calendar.new.day$UID,k)
              splits[[k]] = calendar.new.day
            }

          }
          splits = dplyr::bind_rows(splits)

          # message("Going From")
          # print(calendar.sub)
          # message("To")
          # print(splits)
          # readline(prompt="Press [enter] to continue")

          res.calendar[[i]] = splits

        }
      }
    }
    if(i %% 10000 == 0){
      message(paste0(Sys.time()," matched ",round(i/length_todo*100,1),"% of routes"))
    }
  }

  res.calendar = dplyr::bind_rows(res.calendar)
  res.calendar_dates = dplyr::bind_rows(res.calendar_dates)

  days = lapply(res.calendar$Days,function(x){as.integer(substring(x, 1:7, 1:7 ))})
  days = matrix(unlist(days), ncol = 7, byrow = T)
  days = as.data.frame(days)
  names(days) = c("monday","tuesday", "wednesday","thursday", "friday", "saturday", "sunday")

  calendar = cbind(res.calendar,days)
  calendar$Days = NULL


  calendar$keep = sapply(seq(1,nrow(calendar)),checkrows)
  calendar = calendar[calendar$keep,]






  # Join the schedule and schedule.extra into on df
  #schedule.extra$schedulerowID = schedule.extra$rowID - 1
  #schedule.extra$rowID = NULL
  #schedule = dplyr::left_join(schedule, schedule.extra, by = c("rowID" = "schedulerowID"))

  #}

  # lookup = matrix(c(schedule$rowID,
  #                   schedule$rowID[2:length(schedule$rowID)],
  #                   lookup[nrow(lookup),1]+1000),
  #                 ncol = 2)
  # lookup <- cbind(lookup,lookup[,2]-lookup[,1])
  # lookup.df = data.frame(rowID = seq(from = lookup[1,1],
  #                                    to = lookup[nrow(lookup),2]-1),
  #                        trip_id = rep(schedule$`Train UID`,times = as.integer(lookup[,3]) ))

  stop_times = dplyr::left_join(stop_times,lookup.df, by = c("rowID" = "rowID"))
  stop_times = stop_times[,c("trip_id","arrival_time","departure_time","stop_id","rowID")]

  ### SECTION 3: ###############################################################################
  # make make the trips.txt  file by matching the calnedar to the stop_times

  trips = calendar[c("UID","rowID")]
  names(trips) = c("service_id","trip_id")
  # trip_id = strsplit(trips$UID,  " ")
  # trip_id = lapply(trip_id, `[[`, 1)
  # trip_id = unlist(trip_id)
  #
  # trips$trip_id = trip_id


  ### SECTION 4: ###############################################################################
  # make make the routes.txt
  # a route is all the trips with a common start and end i.e. scheduels original UID

  routes = schedule[!duplicated(schedule$`Train UID`),]
  routes = routes[,c("rowID","Train UID","Train Status","Train Category")]

  routes = dplyr::left_join(routes,stop_times,by = c("rowID" = "trip_id"))
  head(routes)
  #end of function
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


#' split overlapping start and end dates
#'
#' @details
#' split overlapping start and end dates
#'
#' @param start_date station SF data frame from the importMSN function
#' @param start_date Path to save file to
#'
splitDates = function(cal){

  #get all the dates that
  dates = c(cal$start_date,cal$end_date)
  dates = dates[order(dates)]
  #create all unique pairs
  dates.df = data.frame(start_date = dates[seq(1,length(dates)-1 )],
                        end_date = dates[seq(2,length(dates))])

  cal.new = dplyr::left_join(dates.df,cal, by = c("start_date" = "start_date", "end_date" = "end_date"))

  if("P" %in% cal$STP){
    match = "P"
  }else{
    match = cal$STP[cal$STP != "C"]
    match = match[1]
  }

  #fill in the original missing schdule
  for(j in seq(1,nrow(cal.new))){
    if(is.na(cal.new$UID[j])){
      st_tmp = cal.new$start_date[j]
      ed_tmp = cal.new$end_date[j]
      new.UID = cal$UID[cal$STP == match & cal$start_date <= st_tmp &  cal$end_date >= ed_tmp ]
      new.Days = cal$Days[cal$STP == match & cal$start_date <= st_tmp &  cal$end_date >= ed_tmp ]
      new.roWID = cal$rowID[cal$STP == match & cal$start_date <= st_tmp &  cal$end_date >= ed_tmp ]
      if(length(new.UID) == 1){
        cal.new$UID[j]   =  new.UID
        cal.new$Days[j]  =  new.Days
        cal.new$rowID[j] =  new.roWID
        cal.new$STP[j] = match
      }else if(length(new.UID) > 1){
        message("Going From")
        print(cal)
        message("To")
        print(cal.new)
        stop()
        #readline(prompt="Press [enter] to continue")print()
      }

    }
  }

  # remove any gaps
  cal.new = cal.new[!is.na(cal.new$UID),]

  #remove duplicated rows
  cal.new = cal.new[!duplicated(cal.new), ]

  #modify end and start dates
  for(j in seq(1,nrow(cal.new))){
    if(cal.new$STP[j] == "P"){
      # check if end date need changing
      if(j < nrow(cal.new)){
        if(cal.new$end_date[j] == cal.new$start_date[j+1]){
          cal.new$end_date[j] = (cal.new$end_date[j] - 1)
        }
      }
      #check if start date needs changing
      if(j > 1){
        if(cal.new$start_date[j] == cal.new$end_date[j-1]){
          cal.new$start_date[j] = (cal.new$start_date[j] + 1)
        }
      }
    }
  }

  #remove cancled trips
  cal.new = cal.new[cal.new$STP != "C",]

  #fix duration
  cal.new$duration = cal.new$end_date - cal.new$start_date + 1

  # remove any zero or negative day schduels
  cal.new = cal.new[cal.new$duration > 0,]

  #Append UID to note the changes
  if(nrow(cal.new) >0){
    cal.new$UID = paste0(cal.new$UID," ",letters[1:nrow(cal.new)])
  }else{
    cal.new = NA
  }


  return(cal.new)

}



#' internal function for matching stop_times to the basic schdule
#'
#' @details
#' Takes in a row of the schdedule and then gets the next row (schedule must be sored by rowID)
#'
#' @param i interger row number from schdules
#' @param length_todo max number of rows
#'
matchRoutes = function(schedule.rowID, stop_times.rowID, ncores = 1){
  schedule_tmp = matrix(c(schedule.rowID, schedule.rowID[2:length(schedule.rowID)],max(schedule.rowID)+99999), ncol = 2)

  if(ncores == 1){
    matches = lapply(1:nrow(schedule_tmp),function(x){stop_times.rowID[ dplyr::between(stop_times.rowID,
                                                                                       schedule_tmp[x,1],
                                                                                       schedule_tmp[x,2]) ]})
  }else{
    CL <- parallel::makeCluster(ncores) #make clusert and set number of core
    parallel::clusterExport(cl = CL, varlist=c("stop_times.rowID", "schedule_tmp"), envir = environment())
    parallel::clusterEvalQ(cl = CL, {library(dplyr)})
    matches = parallel::parLapply(cl = CL,1:nrow(schedule_tmp),function(x){stop_times.rowID[ dplyr::between(stop_times.rowID,
                                                                                       schedule_tmp[x,1],
                                                                                       schedule_tmp[x,2]) ]})
    parallel::stopCluster(CL)
  }

  #names(matches) = schedule_tmp[1:10]
  result = data.frame(stop_times.rowID = unlist(matches),
                      schedule.rowID = rep(schedule.rowID, times = lengths(matches)))

  return(result)
}

# matchRoutes = function(i,length_todo){
#   if(i != length_todo){
#     schedule.sub = schedule[c(i,i+1), ]
#     #if(schedule.sub$`STP indicator`[1] != "C"){ #Cancelations don have stop pattern
#       sub.rowID1 = schedule.sub$rowID[1]
#       sub.rowID2 = schedule.sub$rowID[2]
#       #sub.UID = schedule.sub$`Train UID`[1]
#       #rows.todo = stop_times$rowID[which(stop_times$rowID > sub.rowID1)]
#       #rows.todo = rows.todo[which(rows.todo < sub.rowID2)]
#       rows.todo = stop_times$rowID[dplyr::between(stop_times$rowID, sub.rowID1, sub.rowID2)]
#     #}else{
#     #  rows.todo = NA
#     #}
#   }else{
#     schedule.sub = schedule[i, ]
#     sub.rowID1 = schedule.sub$rowID[1]
#     sub.UID = schedule.sub$`Train UID`[1]
#     rows.todo = stop_times$rowID[stop_times$rowID > sub.rowID1]
#   }
#   if(length(rows.todo) > 0){
#     res = data.frame(rowID = rows.todo, trip_id = sub.rowID1)
#   }else{
#     res = NA
#   }
#
#   if(i %% 1000 == 0){
#     message(paste0("done ",i))
#   }
#
#   return(res)
#
# }

#
#' internal function for cleaning calendar
#'
#' @details
#' check for schdules that don overlay with the day they rund i.e. Mon - Sat schduel for a sunday only service
#' return a logcal vector of if the calendar is valid
#'
#' @param i interger row number calendar
#'
checkrows = function(i){
  tmp = calendar[i,]
  if(tmp$duration < 7){
    days.valid = weekdays(seq.POSIXt(from = as.POSIXct.Date(tmp$start_date), to = as.POSIXct.Date(tmp$end_date), by = "DSTday"))
    days.valid = tolower(days.valid)
    days.match = tmp[,c("monday","tuesday", "wednesday","thursday", "friday", "saturday", "sunday")]
    days.match = sapply(days.match,function(x){x == 1})
    days.match = days.match[days.match]
    days.match = names(days.match)
    if(any(days.valid %in% days.match)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(TRUE)
  }
}
