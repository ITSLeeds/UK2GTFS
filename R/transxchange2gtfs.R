obj = readRDS("example_import.Rds")

transxchange2gtfs <- function(obj, run_debug = T){
  JourneyPatternSections  <-  obj[["JourneyPatternSections"]]
  Operators               <-  obj[["Operators"]]
  Routes                  <-  obj[["Routes"]]
  RouteSections           <-  obj[["RouteSections"]]
  Services_main           <-  obj[["Services_main"]]
  StandardService         <-  obj[["StandardService"]]
  Services_NonOperation   <-  obj[["Services_NonOperation"]]
  StopPoints              <-  obj[["StopPoints"]]
  VehicleJourneys         <-  obj[["VehicleJourneys"]]
  VehicleJourneys_exclude <-  obj[["VehicleJourneys_exclude"]]
  VehicleJourneys_include <-  obj[["VehicleJourneys_include"]]
  VehicleJourneysTimingLinks <- obj[["VehicleJourneysTimingLinks"]]

  ## JourneyPatternSections #####################
  clean_times <- function(x){
    x <- as.character(x)
    x <- gsub("PT","",x)
    mins <- grepl("M",x)
    secs <- grepl("S",x)

    help_times <- function(x_sub, min_sub, secs_sub){
      if(min_sub & secs_sub){
        # Mins and Seconds
        message("Mins and Secs")
        stop()
      }else if(min_sub & !secs_sub){
        # Mins only
        time <- as.numeric(gsub("M","",x_sub)) * 60
      }else if(!min_sub & secs_sub){
        # Secs only
        time <- as.numeric(gsub("S","",x_sub))
      }else if(!min_sub & !secs_sub){
        # Neither, due to NAs
        time <- 0
      }else{
        message("Terrible error")
        stop()
      }
      #time <- unname(time)
      return(time)
    }
      times <- unname(mapply(help_times, x_sub = x, min_sub = mins, secs_sub = secs, SIMPLIFY = T))
      return(times)
  }

  if(run_debug){
    chk <- gsub("[0-9]","",JourneyPatternSections$RunTime)
    chk <- unique(chk)
    if(!all(chk %in% c("PTM","PTS"))){
      message("Unknown time formats")
      stop()
    }
    rm(chk)
  }

  JourneyPatternSections$RunTime <- clean_times(JourneyPatternSections$RunTime)
  JourneyPatternSections$To.WaitTime <- clean_times(JourneyPatternSections$To.WaitTime)
  JourneyPatternSections$total_time <- JourneyPatternSections$RunTime + JourneyPatternSections$To.WaitTime

  ## stops ######################################
  stops <- StopPoints[,c("StopPointRef","CommonName")]
  names(stops) <- c("stop_id","stop_name")

  ## stop_times #################################
  clean_activity <- function(x, type){
    if(type == "pickup"){
      if(x == "pickUp"){
        x <- 0L
      }else if(x =="pickUpAndSetDown"){
        x <- 0L
      }else if(x =="setDown"){
        x <- 1L
      }else{
        stop(paste0(x," Invalid pickup type"))
      }
    }
    if(type == "drop_off"){
      if(x == "pickUp"){
        x <- 1L
      }else if(x =="pickUpAndSetDown"){
        x <- 0L
      }else if(x =="setDown"){
        x <- 0L
      }else{
        stop(paste0(x," Invalid drop off type"))
      }
    }
    x
  }

  make_stop_times <- function(jps, vj, ss){
    jps[] <- lapply(jps, as.character)
    vj[] <- lapply(vj, as.character)
    rts <- unique(vj$JourneyPatternRef)
    ss_join <- ss[,c("JourneyPatternSectionRefs","JourneyPatternID")]
    ss_join[] <- lapply(ss_join, as.character)
    jps$JPS_id <- as.character(jps$JPS_id)
    jps <- dplyr::left_join(jps,ss_join, by = c("JPS_id" = "JourneyPatternSectionRefs"))
    jps <- split(jps, jps$JourneyPatternID)
    vj <- split(vj, vj$JourneyPatternRef)
    if(!identical(names(jps),names(vj))){message("Different Journey Patterns in jps and vj");stop()}
    stop_times_all <- list()
    for(i in seq(1,length(jps))){
      jps_sub = jps[[i]]
      vj_sub = vj[[i]]
      stop_times_sub = jps_sub[,c("To.StopPointRef","To.Activity","total_time","To.SequenceNumber","JourneyPatternID")]
      stop_times_top = data.frame(To.StopPointRef = jps_sub$From.StopPointRef[1],
                                  To.Activity = jps_sub$From.Activity[1],
                                  total_time = "0",
                                  To.SequenceNumber = "1",
                                  JourneyPatternID = jps_sub$JourneyPatternID[1],
                                  stringsAsFactors = F)
      stop_times_sub = dplyr::bind_rows(list(stop_times_top,stop_times_sub))
      stop_times_sub$cum_time = cumsum(stop_times_sub$total_time)
      stop_times_sub$cum_time <- lubridate::seconds_to_period(stop_times_sub$cum_time)
      stop_times_sub$arrival_time <- stop_times_sub$cum_time + lubridate::hms(vj_sub$DepartureTime[1])
      stop_times_sub$arrival_time <- sprintf('%02d:%02d:%02d', stop_times_sub$arrival_time@hour, stop_times_sub$arrival_time@minute, stop_times_sub$arrival_time@.Data)
      stop_times_sub$departure_time <- stop_times_sub$arrival_time
      stop_times_sub$pickup_type <- sapply(stop_times_sub$To.Activity, clean_activity, type = "pickup")
      stop_times_sub$drop_off_type <- sapply(stop_times_sub$To.Activity, clean_activity, type = "drop_off")
      stop_times_sub <- stop_times_sub[,c("JourneyPatternID","arrival_time","departure_time","To.StopPointRef","To.SequenceNumber","pickup_type","drop_off_type")]
      names(stop_times_sub) <- c("trip_id","arrival_time","departure_time","stop_id","stop_sequence","pickup_type","drop_off_type")
      stop_times_all[[i]] <- stop_times_sub
    }
    stop_times_all <- dplyr::bind_rows(stop_times_all)

    return(stop_times_all)
  }



  stop_times <-  make_stop_times(jps = JourneyPatternSections, vj = VehicleJourneys, ss = StandardService)


  ## routes #####################################
  #routes <- Routes
  Routes1 <- Routes[1,]
  RouteSections1 <- RouteSections[RouteSections$SectionID == Routes1$RouteSectionRef,]
  JourneyPatternSections1 <- JourneyPatternSections[JourneyPatternSections$RouteLinkRef %in% RouteSections1$LinkID, ]
  StandardService1 <- StandardService[StandardService$RouteRef == Routes1$PrivateCode, ]
  VehicleJourneys1 = VehicleJourneys[as.character(VehicleJourneys$JourneyPatternRef) %in% as.character(StandardService1$JourneyPatternID),]

}
