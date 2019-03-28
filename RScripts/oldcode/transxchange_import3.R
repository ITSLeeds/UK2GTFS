#' Import a TransXchange XML file
#'
#' @param file character, path to an XML file e.g. "C:/data/file.xml"
#' @param export character, path to folder to save results, or NULL to return results
#' @param run_debug logical, if TRUE extra checks are performed, default FALSE
#'
#' @export
#' If export is NULL returns a list of data.frames else saves results to the `export` folder as a RDS file
#'
#' @details
#' This function imports the raw transXchange XML files and converts them to a R readable format.
#'

transxchange_import3 <- function(file, export = NULL, run_debug = FALSE){
  if(run_debug){
    message(paste0(Sys.time()," doing file ",file))
  }

  xml = xml2::read_xml(file)
  if(!is.null(export)){
    if(!dir.exists(export)){
      message("Export directory does not exist")
      stop()
    }
  }
  ## StopPoints ##########################################
  StopPoints = xml2::xml_child(xml,1)
  StopPoints = xml2::as_list(StopPoints)
  # Sometimes the Indicator variaible is missing
  if(!all(lengths(StopPoints) == 5)){
    sp_clean = function(sp){
      if(is.null(sp$Indicator)){
        sp$Indicator <- NA
      }
      sp <- sp[c("StopPointRef","CommonName","Indicator","LocalityName","LocalityQualifier")]
      return(sp)
    }
    StopPoints = lapply(StopPoints,sp_clean)
  }

  StopPoints <- data.frame(matrix(unlist(StopPoints), nrow=length(StopPoints), byrow=T),stringsAsFactors=T)
  names(StopPoints) <- c("StopPointRef","CommonName","Indicator","LocalityName","LocalityQualifier")

  ## RouteSections ##########################################
  RouteSections = xml2::xml_child(xml,2)
  RouteSections = xml2::as_list(RouteSections)

  rs_clean <- function(rs){
    rs_attr = attributes(rs)$id
    rs = rs[names(rs) == "RouteLink"]
    rs <- lapply(rs, function(x){tmp <- x$Distance
    ids <- attributes(x)$id
    if(is.null(tmp)){tmp <- NA}
    x$LinkID <- ids
    x$Distance <- tmp
    x <- x[c("From","To","Distance","Direction","LinkID")]
    return(x)})
    rs =  data.frame(matrix(unlist(rs), nrow=length(rs), byrow=T),stringsAsFactors=FALSE)
    names(rs) = c("From","To","Distance","Direction","LinkID")
    rs$SectionID = rs_attr
    return(rs)
  }
  RouteSections = lapply(RouteSections, rs_clean)
  RouteSections = dplyr::bind_rows(RouteSections)
  RouteSections[] <- lapply( RouteSections, factor)

  ## Routes ##########################################
  Routes = xml2::xml_child(xml,3)
  Routes = xml2::as_list(Routes)

  Routes <- data.frame(matrix(unlist(Routes), nrow=length(Routes), byrow=T),stringsAsFactors=FALSE)
  names(Routes) <- c("PrivateCode","Description","RouteSectionRef")
  Routes[] <- lapply(Routes, factor)

  ## JourneyPatternSections ##########################################
  JourneyPatternSections = xml2::xml_child(xml,4)
  JourneyPatternSections = xml2::as_list(JourneyPatternSections)

  jps_clean <- function(jps){
    jptl_clean <- function(jptl){
      jptl_id <- attributes(jptl)$id
      jptl_from_seq <- attributes(jptl$From)$SequenceNumber
      jptl_to_seq <- attributes(jptl$To)$SequenceNumber
      if(is.null(jptl_from_seq)){jptl_from_seq <- NA}
      if(is.null(jptl_to_seq)){jptl_to_seq <- NA}
      jptl = unlist(jptl)
      nms = names(jptl)
      if(!"To.WaitTime" %in% nms){
        jptl = c(jptl,NA)
        names(jptl) = c(nms,"To.WaitTime")
        nms = c("From.Activity","From.StopPointRef", "From.TimingStatus","To.WaitTime",
                "To.Activity","To.StopPointRef","To.TimingStatus","RouteLinkRef","RunTime")
        jptl = jptl[nms]
      }
      jptl = c(jptl_id,jptl,jptl_from_seq,jptl_to_seq)
      names(jptl) = c("JPTL_ID",nms,"From.SequenceNumber","To.SequenceNumber")

      return(jptl)
    }

    jps_id = attributes(jps)$id
    jps = lapply(jps,jptl_clean)
    jps = data.frame(jps ,stringsAsFactors=FALSE)
    jps = t(jps)
    jps = as.data.frame(jps, stringsAsFactors=FALSE)
    jps$JPS_id = jps_id
    row.names(jps) = seq(1,nrow(jps))
    return(jps)
  }

  JourneyPatternSections = lapply(JourneyPatternSections,jps_clean)
  JourneyPatternSections = dplyr::bind_rows(JourneyPatternSections)
  JourneyPatternSections[] <- lapply(JourneyPatternSections, factor)

  ## Operators ##########################################
  Operators <- xml2::xml_child(xml,5)
  if(run_debug){
    if(xml2::xml_length(Operators) > 1){
      message("More than one Operators")
      stop()
    }
  }
  Operators <- xml2::xml_child(Operators,1)
  Operators <- xml2::as_list(Operators)
  Operators <- as.data.frame(t(Operators))

  ## Services ##########################################
  Services = xml2::xml_child(xml,6)
  if(run_debug){
    if(xml2::xml_length(Services) > 1){
      message("More than one service")
      stop()
    }
  }
  Services <- xml2::xml_child(Services,1)
  Services <- xml2::as_list(Services)

  Services_main <- Services[c("ServiceCode","PrivateCode","Mode","Description","RegisteredOperatorRef")]
  Services_main <- as.data.frame(t(Services_main))
  Services_main$StartDate <- Services$OperatingPeriod$StartDate[[1]]
  Services_main$EndDate <- Services$OperatingPeriod$EndDate[[1]]
  Services_main$DaysOfWeek <- paste(names(Services$OperatingProfile$RegularDayType$DaysOfWeek), collapse = " ")
  Services_main$StopRequirements <- names(Services$StopRequirements)
  Services_main$Origin <- Services$StandardService$Origin[[1]]
  Services_main$Destination <- Services$StandardService$Destination[[1]]
  Services_main$LineName <- Services$Lines$Line$LineName[[1]]
  Services_main$BankHolidayNonOperation <- paste(names(Services$OperatingProfile$BankHolidayOperation$DaysOfNonOperation), collapse = " ")
  Services_main$BankHolidayOperation <- paste(names(Services$OperatingProfile$BankHolidayOperation$DaysOfOperation), collapse = " ")

  StandardService <- Services$StandardService
  StandardService$Origin <- NULL
  StandardService$Destination <- NULL

  jp_clean <- function(jp){
    jp_id = attributes(jp)$id[[1]]
    VehicleType = jp$Operational$VehicleType$Description[[1]]
    if(is.null(VehicleType)){VehicleType <- NA}
    jp_data = c(jp$Direction[[1]],
                VehicleType,
                jp$RouteRef[[1]],
                jp$JourneyPatternSectionRefs[[1]],
                jp_id)

    names(jp_data) = c("Direction","VehicleType","RouteRef","JourneyPatternSectionRefs","JourneyPatternID")
    return(jp_data)
  }

  if(run_debug){
    jp_chk <- function(jp){
      #jp2 = jp
      jp$Direction = NULL
      jp$Operational$VehicleType$VehicleTypeCode = NULL
      jp$Operational$VehicleType$Description = NULL
      jp$RouteRef = NULL
      jp$JourneyPatternSectionRefs = NULL
      if(length(jp$Operational$VehicleType) == 0){
        jp$Operational$VehicleType <- NULL
      }
      if(length(jp$Operational) == 0){
        jp$Operational<- NULL
      }
      #jp = unlist(jp)
      #if(!is.factor(jp)){
      if(length(jp) != 0){
        message("Unexpected strucutre in Jounrey Patterns")
        print(jp)
        stop()
      }
      return(NULL)

    }
    chk = lapply(StandardService,jp_chk)
    rm(chk)
  }

  StandardService <- lapply(StandardService, jp_clean)
  StandardService <- as.data.frame(t(data.frame(StandardService)))
  row.names(StandardService) <- seq(1,nrow(StandardService))
  #StandardService <- lapply(StandardService, factor)

  Services_NonOperation <- Services$OperatingProfile$SpecialDaysOperation$DaysOfNonOperation
  if(!is.null(Services_NonOperation)){
    Services_NonOperation <- as.data.frame(matrix(unlist(Services_NonOperation), nrow = length(Services_NonOperation), byrow = T), stringsAsFactors = F)
    names(Services_NonOperation) <- c("Start","End")
  }

  if(run_debug){
    if(length(Services$Lines) > 1){
      message("more than one line")
      stop()
    }

    chk = Services[!names(Services) %in% c("ServiceCode","PrivateCode","Mode","Description","RegisteredOperatorRef","StandardService")]
    chk$OperatingPeriod$StartDate = NULL
    chk$OperatingPeriod$EndDate = NULL
    chk$OperatingProfile$RegularDayType$DaysOfWeek = NULL
    chk$StopRequirements = NULL
    chk$StandardService$Origin = NULL
    chk$StandardService$Destination = NULL
    chk$Lines$Line$LineName = NULL
    chk$OperatingProfile$BankHolidayOperation$DaysOfOperation = NULL
    chk$OperatingProfile$BankHolidayOperation$DaysOfNonOperation = NULL
    if(length(chk$OperatingProfile$BankHolidayOperation) == 0){
      chk$OperatingProfile$BankHolidayOperation = NULL
    }
    chk$OperatingProfile$SpecialDaysOperation = NULL
    chk <- unlist(sapply(chk, names))
    if(!identical(unname(chk),c("Line","RegularDayType"))){
      message("Unexpected strucutre in Services")
      print(str(Services))
    }

  }
  rm(Services)

  ## VehicleJourneys ##########################################
  VehicleJourneys = xml2::xml_child(xml,7)
  VehicleJourneys = xml2::as_list(VehicleJourneys)

  vj_clean <- function(vj,run_debug){
    # break simple and complex parts
    vj_simple = vj[c("PrivateCode","VehicleJourneyCode","ServiceRef","LineRef",
                     "JourneyPatternRef","DepartureTime")]
    vj_simple = data.frame(t(unlist(vj_simple)), stringsAsFactors = F)
    vj_op = vj[["OperatingProfile"]]


    op_rdw = names(vj_op$RegularDayType$DaysOfWeek) # Regular Day of the Week
    op_rdho = names(vj_op$RegularDayType) # Holidays Only
    op_rdho = op_rdho[op_rdho != "DaysOfWeek"]
    op_dno = vj_op$SpecialDaysOperation$DaysOfNonOperation # Days of non operation
    op_do = vj_op$SpecialDaysOperation$DaysOfOperation # Day of operation
    if(!is.null(op_dno)){
      op_dno = data.frame(matrix(unlist(op_dno), nrow=length(op_dno), byrow=T),stringsAsFactors=FALSE)
      op_dno$VehicleJourneyCode = vj_simple$VehicleJourneyCode
      names(op_dno) = c("StartTime","EndTime","VehicleJourneyCode")
    }
    if(!is.null(op_do)){
      op_do = data.frame(matrix(unlist(op_do), nrow=length(op_do), byrow=T),stringsAsFactors=FALSE)
      op_do$VehicleJourneyCode = vj_simple$VehicleJourneyCode
      names(op_do) = c("StartTime","EndTime","VehicleJourneyCode")
    }
    op_bh_dno = names(vj_op$BankHolidayOperation$DaysOfNonOperation) # Bank Holidays non operation
    op_bh_do = names(vj_op$BankHolidayOperation$DaysOfOperation) # Bank Holidays operation

    vj_simple$days = paste0(c(op_rdw,op_rdho), collapse = " ")
    vj_simple$BankHolidaysOperate = paste0(op_bh_do, collapse = " ")
    vj_simple$BankHolidaysNoOperate = paste0(op_bh_dno, collapse = " ")


    #### Check disable in working code
    if(run_debug){
      chk = vj_op

      chk$RegularDayType$DaysOfWeek = NULL
      chk$RegularDayType$HolidaysOnly = NULL
      chk$SpecialDaysOperation$DaysOfNonOperation = NULL
      chk$SpecialDaysOperation$DaysOfOperation = NULL
      chk$BankHolidayOperation$DaysOfNonOperation = NULL
      chk$BankHolidayOperation$DaysOfOperation = NULL
      chk <- unlist(sapply(chk, names))
      if(length(chk) != 0){
        message("Unexpected Structure in Operating Profile")
        print(str(vj_op))
        stop()
      }

      if(!all(c(op_rdw,op_rdho) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday",
                                       "Saturday","Sunday","MondayToFriday","HolidaysOnly",
                                       "MondayToSunday","Weekend"))){
        message("Unexpected Week Structure")
        print(str(op_rdw))
        stop()
      }
      if(!is.null(op_dno)){
        if(ncol(op_dno) > 3){
          message("Unexpected Days of Non Operation Structure")
          print(str(op_dno))
          stop()
        }
      }
      if(!is.null(op_do)){
        if(ncol(op_do) > 3){
          message("Unexpected Days of Operation Structure")
          print(str(op_do))
          stop()
        }
      }

    }
    result <- list(vj_simple,op_dno,op_do)
    return(result)
  }

  VehicleJourneys <- lapply(VehicleJourneys, vj_clean, run_debug = run_debug)
  VehicleJourneys_exclude <- lapply(VehicleJourneys,`[[`,2)
  VehicleJourneys_exclude <- dplyr::bind_rows(VehicleJourneys_exclude)
  VehicleJourneys_include <- lapply(VehicleJourneys,`[[`,3)
  VehicleJourneys_include <- dplyr::bind_rows(VehicleJourneys_include)
  VehicleJourneys <- lapply(VehicleJourneys,`[[`,1)
  VehicleJourneys <- dplyr::bind_rows(VehicleJourneys)

  VehicleJourneys[] <- lapply(VehicleJourneys, factor)
  VehicleJourneys_exclude[] <- lapply(VehicleJourneys_exclude, factor)
  VehicleJourneys_include[] <- lapply(VehicleJourneys_include, factor)

  ## Final Steps ##########################################

  finalres <- list(JourneyPatternSections, Operators, Routes,
                   RouteSections, Services_main, StandardService,
                   Services_NonOperation, StopPoints, VehicleJourneys,
                   VehicleJourneys_exclude,VehicleJourneys_include)
  names(finalres) = c("JourneyPatternSections", "Operators", "Routes",
                      "RouteSections", "Services_main","StandardService",
                      "Services_NonOperation", "StopPoints", "VehicleJourneys",
                      "VehicleJourneys_exclude","VehicleJourneys_include")

  if(!is.null(export)){
    filename <- unlist(strsplit(file,"/"))
    filename <- filename[length(filename)]
    filename <- gsub(".xml","",filename)
    saveRDS(finalres,paste0(export,"/",filename,".Rds"))
    return(NULL)
  }else{
    return(finalres)
  }


}
