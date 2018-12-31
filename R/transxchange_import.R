file = "C:/Users/Malcolm/Downloads/Traveline/EA/ea_20-2-A-y08-1.xml"


transxchange_import <- function(file, run_debug = FALSE){
  xml_data = XML::xmlToList(file)
  #xml_data = xml2::as_list(xml2::read_xml(file))
  #xml_data = xml_data[[1]]

  StopPoints = xml_data["StopPoints"]
  StopPoints = StopPoints[[1]]

  # Sometimes the Indicator variaible is missing
  if(!all(lengths(StopPoints) == 5)){
    sp_clean = function(sp){
      if(is.null(sp$Indicator)){
        sp$Indicator <- NA
      }
      return(sp)
    }
    StopPoints = lapply(StopPoints,sp_clean)
  }


  StopPoints <- data.frame(matrix(unlist(StopPoints), nrow=length(StopPoints), byrow=T),stringsAsFactors=T)
  names(StopPoints) <- c("StopPointRef","CommonName","Indicator","LocalityName","LocalityQualifier")

  RouteSections = xml_data["RouteSections"][[1]]

  rs_clean <- function(rs){
    rs_attr = rs[[".attrs"]]
    rs = rs[names(rs) == "RouteLink"]
    rs =  data.frame(matrix(unlist(rs), nrow=length(rs), byrow=T),stringsAsFactors=FALSE)
    names(rs) = c("From","To","Direction","attrs")
    rs$attrs.id = rs_attr
    return(rs)
  }
  RouteSections = lapply(RouteSections, rs_clean)
  RouteSections = dplyr::bind_rows(RouteSections)
  RouteSections[] <- lapply( RouteSections, factor)

  Routes <- xml_data["Routes"][[1]]
  Routes <- data.frame(matrix(unlist(Routes), nrow=length(Routes), byrow=T),stringsAsFactors=FALSE)
  names(Routes) <- c("PrivateCode","Description","RouteSectionRef","attrs")
  Routes[] <- lapply(Routes, factor)

  jps_clean = function(jps){
    nms = c("From.Activity","From.StopPointRef","From.TimingStatus","From..attrs.SequenceNumber",
            "To.Activity","To.StopPointRef","To.TimingStatus","To..attrs.SequenceNumber",
            "RouteLinkRef","RunTime",".attrs.id")
    jps = lapply(jps,unlist)
    jps = lapply(jps,function(x){x[nms]})
    jps = jps[names(jps) == "JourneyPatternTimingLink"]
    jps = data.frame(jps ,stringsAsFactors=FALSE)
    jps = t(jps)
    jps = jps[1:(nrow(jps)-1),]
    jps = as.data.frame(jps, stringsAsFactors=FALSE)
    row.names(jps) = seq(1,nrow(jps))
    return(jps)
  }

  JourneyPatternSections = xml_data["JourneyPatternSections"][[1]]
  JourneyPatternSections = lapply(JourneyPatternSections,jps_clean)
  JourneyPatternSections = dplyr::bind_rows(JourneyPatternSections)
  JourneyPatternSections[] <- lapply(JourneyPatternSections, factor)

  Operators = xml_data["Operators"][[1]]
  Operators <- data.frame(matrix(unlist(Operators), nrow=length(Operators), byrow=T),stringsAsFactors=FALSE)
  names(Operators) <- c("NationalOperatorCode","OperatorCode","OperatorShortName","OperatorNameOnLicence","TradingName","attrs")

  sv_clean <- function(sv,run_debug){
    sv_simple = data.frame(sv[c("ServiceCode","PrivateCode","Mode","Description","RegisteredOperatorRef")],stringsAsFactors = F)
    sv_simple$StartDate <- sv$OperatingPeriod$StartDate
    sv_simple$EndDate <- sv$OperatingPeriod$EndDate
    sv_simple$DaysOfWeek <- paste(names(sv$OperatingProfile$RegularDayType$DaysOfWeek), collapse = " ")
    sv_simple$StopRequirements <- names(sv$StopRequirements)
    sv_simple$Origin <- sv$StandardService$Origin
    sv_simple$Destination <- sv$StandardService$Destination
    sv_simple$LineName <- sv$Lines$Line$LineName
    sv_simple$BankHolidayNonOperation <- paste(names(sv$OperatingProfile$BankHolidayOperation$DaysOfNonOperation), collapse = " ")
    sv_simple$BankHolidayOperation <- paste(names(sv$OperatingProfile$BankHolidayOperation$DaysOfOperation), collapse = " ")

    sv_service <- sv$StandardService
    sv_service$Origin = NULL
    sv_service$Destination = NULL
    sv_service = as.data.frame(matrix(unlist(sv_service), nrow = length(sv_service), byrow = T), stringsAsFactors = F)
    names(sv_service) = c("Direction","RouteRef","JourneyPatternSectionRefs",".attrs")

    sv_nonoperation <- sv$OperatingProfile$SpecialDaysOperation$DaysOfNonOperation
    if(!is.null(sv_nonoperation)){
      sv_nonoperation <- as.data.frame(matrix(unlist(sv_nonoperation), nrow = length(sv_nonoperation), byrow = T), stringsAsFactors = F)
      names(sv_nonoperation) <- c("Start","End")
    }

    #sv_special$RegularDayType <- NULL

    if(run_debug){
      if(length(sv$Lines) > 1){
        message("more than one line")
        stop()
      }

      chk = sv[!names(sv) %in% c("ServiceCode","PrivateCode","Mode","Description","RegisteredOperatorRef","StandardService")]
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
        print(str(sv))
      }

    }
    result = list(sv_simple, sv_service, sv_nonoperation)
    return(result)
  }


  Services = xml_data["Services"][[1]]

  if(run_debug){
    if(length(Services) > 1){
      message("More than one service")
      stop()
    }

  }

  Services = Services[[1]]
  Services = sv_clean(Services, run_debug = run_debug)
  Services_main = Services[[1]]
  Services_Schedule = Services[[2]]
  Services_Schedule[] <- lapply(Services_Schedule, factor)
  Services_NonOperation = Services[[3]]
  rm(Services)

  VehicleJourneys = xml_data["VehicleJourneys"][[1]]

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
    if(!is.null(op_dno)){
      op_dno = data.frame(matrix(unlist(op_dno), nrow=length(op_dno), byrow=T),stringsAsFactors=FALSE)
      op_dno$VehicleJourneyCode = vj_simple$VehicleJourneyCode
      names(op_dno) = c("StartTime","EndTime","VehicleJourneyCode")
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
      chk$BankHolidayOperation$DaysOfNonOperation = NULL
      chk$BankHolidayOperation$DaysOfOperation = NULL
      chk <- unlist(sapply(chk, names))
      if(length(chk) != 0){
        message("Unexpected Structure in Operating Profile")
        print(str(vj_op))
        stop()
      }

      if(!all(c(op_rdw,op_rdho) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday",
                                   "Saturday","Sunday","MondayToFriday","HolidaysOnly"))){
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

    }
    #####

    #res_df = data.frame(t(vj_simple))

    # this is complicated need to see differetn examples
    result <- list(vj_simple,op_dno)
    return(result)
  }

  VehicleJourneys <- lapply(VehicleJourneys, vj_clean, run_debug = run_debug)
  VehicleJourneys_exclude <- lapply(VehicleJourneys,`[[`,2)
  VehicleJourneys_exclude <- dplyr::bind_rows(VehicleJourneys_exclude)
  #names(VehicleJourneys_exclude) <- c("StartTime","EndTime","VehicleJourneyCode")
  VehicleJourneys <- lapply(VehicleJourneys,`[[`,1)
  VehicleJourneys <- dplyr::bind_rows(VehicleJourneys)

  VehicleJourneys[] <- lapply(VehicleJourneys, factor)
  VehicleJourneys_exclude[] <- lapply(VehicleJourneys_exclude, factor)

  finalres <- list(JourneyPatternSections, Operators, Routes, RouteSections, Services_main,
                   Services_Schedule, Services_NonOperation, StopPoints, VehicleJourneys, VehicleJourneys_exclude)
  names(finalres) = c("JourneyPatternSections", "Operators", "Routes", "RouteSections", "Services_main",
                      "Services_Schedule", "Services_NonOperation", "StopPoints", "VehicleJourneys", "VehicleJourneys_exclude")
  return(finalres)

}
