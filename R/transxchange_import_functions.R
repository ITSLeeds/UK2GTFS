# TransXchange import fucntions

#' Import Simple
#' ????
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_simple <- function(xml1, nm){
  xml2::xml_text(xml2::xml_find_all(xml1, nm))
}


#' Import When some rows are missing
#' Checks lengths of obejct against lgth
#' @param xml1 XML object
#' @param nm character name to find
#' @param lgth numeric length check
#' @noRd
import_withmissing <- function(xml1, nm, lgth){
  xml2 <- import_simple(xml1, nm)
  ids <- xml2::xml_length(xml2::xml_children(xml1))
  ids <- ids == lgth
  ids <- cumsum(ids)
  ids[duplicated(ids)] <- NA
  xml2 <- xml2[ids]
  return(xml2)
}

#' Import When some rows are missing
#' Goes down mulitple layers and returns a value with NA for missing
#' @param xml1 XML object
#' @param nm character name to find
#' @param lgth numeric length check
#' @param idvar the id variaible in the higher tree
#' @noRd
import_withmissing2 <- function(xml1, nm, layers, idvar){
  xml_2 <- xml2::xml_find_all(xml1, nm)
  xml2_parent <- xml2::xml_parent(xml_2)
  if(layers > 1){
    for(i in seq(2,layers)){
      xml2_parent <- xml2::xml_parent(xml2_parent)
    }
  }
  xml2_parent_id <- xml2::xml_text(xml2::xml_find_all(xml2_parent, idvar))
  xml1_id <- xml2::xml_text(xml2::xml_find_all(xml1, idvar))

  res <- rep(NA, length(xml1_id))
  res[match(xml2_parent_id, xml1_id)] <- xml2::xml_text(xml_2)
  return(res)
}



#' Import stoppoints
#' ????
#' @param StopPoints stoppoints
#' @param full_import logical
#' @noRd
import_stoppoints <- function(StopPoints, full_import = TRUE){
  StopPointRef       <- import_simple(StopPoints, ".//d1:StopPointRef")


  if(full_import){
    CommonName         <- import_simple(StopPoints, ".//d1:CommonName")
    LocalityName       <- import_simple(StopPoints, ".//d1:LocalityName")
    LocalityQualifier  <- import_simple(StopPoints, ".//d1:LocalityQualifier")
    Indicator <- import_withmissing(StopPoints, ".//d1:Indicator", 5)

    StopPoints <- data.frame(StopPointRef = StopPointRef,
                             CommonName = CommonName,
                             Indicator = Indicator,
                             LocalityName = LocalityName,
                             LocalityQualifier = LocalityQualifier)

  }else{
    StopPoints <- data.frame(StopPointRef = StopPointRef)
  }
  return(StopPoints)
}

#' Import routes
#' ????
#' @param routes routes
#' @noRd
import_routes <- function(routes){
  Description       <- import_simple(routes, ".//d1:Description")
  RouteSectionRef   <- import_simple(routes, ".//d1:RouteSectionRef")
  PrivateCode       <- import_simple(routes, ".//d1:PrivateCode")
  if(length(PrivateCode) == 0){
    PrivateCode <- rep(NA, length(RouteSectionRef))
  }

  routes <- data.frame(PrivateCode = PrivateCode,
                       Description = Description,
                       RouteSectionRef = RouteSectionRef)
  return(routes)
}

#' Import journeypatternsections
#' ????
#' @param journeypatternsections journeypattern sections
#' @noRd
import_journeypatternsections <- function(journeypatternsections){
  JourneyPatternTimingLink <- xml2::xml_find_all(journeypatternsections, ".//d1:JourneyPatternTimingLink")
  JPTL_ID               <- import_simple(JourneyPatternTimingLink, "@id")
  #JPTL_ID               <- rep(JPTL_ID, times = xml2::xml_length(JourneyPatternTimingLink, only_elements = FALSE))

  RouteLinkRef          <- import_simple(JourneyPatternTimingLink, "d1:RouteLinkRef")
  RunTime               <- import_simple(JourneyPatternTimingLink, "d1:RunTime")
  From                  <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:From")
  From.StopPointRef     <- import_simple(From, "d1:StopPointRef")
  From.Activity         <- import_simple(From, "d1:Activity")
  if(length(From.Activity) == 0){
    From.Activity <- rep(NA, length(From.StopPointRef))
  }
  From.TimingStatus     <- import_simple(From, "d1:TimingStatus")
  From.SequenceNumber   <- import_simple(From, "@SequenceNumber")
  if(length(From.SequenceNumber) == 0){
    From.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }
  To                    <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:To")
  To.StopPointRef       <- import_simple(To, "d1:StopPointRef")
  To.WaitTime           <- xml2::xml_text(xml2::xml_find_first(To, "d1:WaitTime"))
  To.Activity           <- import_simple(To, "d1:Activity")
  if(length(To.Activity) == 0){
    To.Activity <- rep(NA, length(To.StopPointRef))
  }
  To.TimingStatus       <- import_simple(To, "d1:TimingStatus")
  To.SequenceNumber     <- import_simple(To, "@SequenceNumber")
  if(length(To.SequenceNumber) == 0){
    To.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }

  JPS                   <- xml2::xml_children(journeypatternsections)
  JPS_id                <- import_simple(JPS, "@id")
  JPS_id                <- rep(JPS_id, times = xml2::xml_length(JPS, only_elements = FALSE))


  journeypatternsections <- data.frame(JPTL_ID = JPTL_ID,
                                       From.Activity = From.Activity,
                                       From.StopPointRef = From.StopPointRef,
                                       From.TimingStatus = From.TimingStatus,
                                       To.WaitTime = To.WaitTime,
                                       To.Activity = To.Activity,
                                       To.StopPointRef = To.StopPointRef,
                                       To.TimingStatus = To.TimingStatus,
                                       RouteLinkRef = RouteLinkRef,
                                       RunTime = RunTime,
                                       From.SequenceNumber = From.SequenceNumber,
                                       To.SequenceNumber = To.SequenceNumber,
                                       JPS_id = JPS_id
  )
  return(journeypatternsections)
}

#' import operators
#' slower so not used
#' @param operators operators object
#' @noRd
import_operators <- function(operators){

  NationalOperatorCode       <- import_simple(operators, ".//d1:NationalOperatorCode")
  OperatorCode               <- import_simple(operators, ".//d1:OperatorCode")
  OperatorShortName          <- import_simple(operators, ".//d1:OperatorShortName")
  OperatorNameOnLicence      <- import_simple(operators, ".//d1:OperatorNameOnLicence")
  TradingName                <- import_simple(operators, ".//d1:TradingName")


  operators <- data.frame(NationalOperatorCode = NationalOperatorCode,
                          OperatorCode = OperatorCode,
                          OperatorShortName = OperatorShortName,
                          OperatorNameOnLicence = OperatorNameOnLicence,
                          TradingName = TradingName
  )
  return(operators)
}

#' import services
#' ????
#' @param service
#' @param full_import
#' @noRd

import_services <- function(service, full_import = TRUE){

  ServiceCode             <- import_simple(service, ".//d1:ServiceCode")
  Mode                    <- import_simple(service, ".//d1:Mode")
  Description             <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:Description"))
  RegisteredOperatorRef   <- import_simple(service, ".//d1:RegisteredOperatorRef")
  StartDate               <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:StartDate"))
  EndDate                 <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:EndDate"))
  DaysOfWeek              <- paste(xml2::xml_name(xml2::xml_children(xml2::xml_find_first(service, ".//d1:DaysOfWeek"))), collapse = " ")
  StopRequirements        <- import_simple(service, ".//d1:StopRequirements")
  Origin                  <- import_simple(service, ".//d1:Origin")
  Destination             <- import_simple(service, ".//d1:Destination")
  LineName                <- import_simple(service, ".//d1:LineName")
  BankHolidayNonOperation <- import_simple(service, ".//d1:BankHolidayNonOperation")
  if(length(BankHolidayNonOperation) == 0){
    BankHolidayNonOperation <- rep(NA, length(ServiceCode))
  }
  BankHolidayOperation    <- import_simple(service, ".//d1:BankHolidayOperation")
  if(length(BankHolidayOperation) == 0){
    BankHolidayOperation <- rep(NA, length(ServiceCode))
  }

  if(full_import){
    PrivateCode <- import_simple(service, ".//d1:PrivateCode")
  }

  ss <- xml2::xml_find_all(service, ".//d1:JourneyPattern")
  Direction                 <- import_simple(ss, ".//d1:Direction")
  VehicleType               <- import_withmissing2(ss, ".//d1:Description", 3, "@id")
  RouteRef                  <- import_simple(ss, ".//d1:RouteRef")
  JourneyPatternSectionRefs <- import_simple(ss, ".//d1:JourneyPatternSectionRefs")
  JourneyPatternID          <- import_simple(ss, "@id")


  SpecialDaysOperation      <- xml2::xml_find_all(service, ".//d1:SpecialDaysOperation")
  DaysOperation             <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfOperation")
  DaysNonOperation          <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfNonOperation")

  if(xml2::xml_length(DaysOperation) > 0){
    DaysOperation_StartDate   <- import_simple(DaysOperation, ".//d1:StartDate")
    DaysOperation_EndDate     <- import_simple(DaysOperation, ".//d1:EndDate")
    DaysOperation_Note        <- import_simple(DaysOperation, ".//d1:Note")
    DaysOperation <- data.frame(type =      "DaysOperation",
                                StartDate = DaysOperation_StartDate,
                                EndDate =   DaysOperation_EndDate,
                                Note =      DaysOperation_Note)
  }else{
    DaysOperation <- NULL
  }

  if(xml2::xml_length(DaysNonOperation) > 0){
    DaysNonOperation_StartDate   <- import_simple(DaysNonOperation, ".//d1:StartDate")
    DaysNonOperation_EndDate     <- import_simple(DaysNonOperation, ".//d1:EndDate")
    DaysNonOperation_Note        <- import_simple(DaysNonOperation, ".//d1:Note")
    DaysNonOperation <- data.frame(type =      "DaysNonOperation",
                                   StartDate = DaysNonOperation_StartDate,
                                   EndDate =   DaysNonOperation_EndDate,
                                   Note =      DaysNonOperation_Note)
  }else{
    DaysNonOperation <- NULL
  }

  if(!is.null(DaysOperation) & !is.null(DaysNonOperation)){
    SpecialDaysOperation <- rbind(DaysOperation, DaysNonOperation)
  }else if(!is.null(DaysOperation)){
    SpecialDaysOperation <- DaysOperation
  }else{
    SpecialDaysOperation <- DaysNonOperation
  }



  StandardService <- data.frame(Direction = Direction,
                                VehicleType = VehicleType,
                                RouteRef = RouteRef,
                                JourneyPatternSectionRefs = JourneyPatternSectionRefs,
                                JourneyPatternID = JourneyPatternID
  )

  Services_main <- data.frame(ServiceCode = ServiceCode,
                              #PrivateCode = PrivateCode,
                              Mode = Mode,
                              Description = Description,
                              RegisteredOperatorRef = RegisteredOperatorRef,
                              StartDate = StartDate,
                              EndDate = EndDate,
                              DaysOfWeek = DaysOfWeek,
                              StopRequirements = StopRequirements,
                              Origin = Origin,
                              Destination = Destination,
                              LineName = LineName,
                              BankHolidayNonOperation = BankHolidayNonOperation,
                              BankHolidayOperation = BankHolidayOperation
  )

  if(full_import){
    Services_main$PrivateCode <- PrivateCode
  }


  results <- list(StandardService, Services_main, SpecialDaysOperation)
  names(results) <- c("StandardService", "Services_main", "SpecialDaysOperation")

  return(results)
}



#' Import Vehicle Journeys
#' ????
#' @param vehiclejourneys
#' @noRd
import_vehiclejourneys <- function(vehiclejourneys){

  PrivateCode <- import_simple(vehiclejourneys, ".//d1:PrivateCode")
  VehicleJourneyCode <- import_simple(vehiclejourneys, ".//d1:VehicleJourneyCode")
  ServiceRef <- import_simple(vehiclejourneys, ".//d1:ServiceRef")
  LineRef <- import_simple(vehiclejourneys, ".//d1:LineRef")
  JourneyPatternRef <- import_simple(vehiclejourneys, ".//d1:JourneyPatternRef")
  DepartureTime <- import_simple(vehiclejourneys, ".//d1:DepartureTime")
  BankHolidaysOperate <- import_simple(vehiclejourneys, ".//d1:BankHolidaysOperate")
  #Notes <- xml2::xml_find_all(vehiclejourneys, ".//d1:Note")

  if(length(BankHolidaysOperate) == 0){
    BankHolidaysOperate <- rep(NA, length(VehicleJourneyCode))
  }
  BankHolidaysNoOperate <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:BankHolidaysNoOperate"))
  if(length(BankHolidaysNoOperate) == 0){
    BankHolidaysNoOperate <- rep(NA, length(VehicleJourneyCode))
  }


  vj_simple <- data.frame(PrivateCode = PrivateCode,
                          VehicleJourneyCode = VehicleJourneyCode,
                          ServiceRef = ServiceRef,
                          LineRef = LineRef,
                          JourneyPatternRef = JourneyPatternRef,
                          DepartureTime = DepartureTime,
                          #days = days,
                          BankHolidaysOperate = BankHolidaysOperate,
                          BankHolidaysNoOperate = BankHolidaysNoOperate
  )

  OperatingProfile <- xml2::xml_find_all(vehiclejourneys, ".//d1:OperatingProfile")
  if(length(xml_length(OperatingProfile)) != nrow(vj_simple)){
    stop("Missing operating profiles in Vehicle Journeys")
  }

  # Regular pattern
  RegularDayType <- xml2::xml_find_all(OperatingProfile, ".//d1:RegularDayType")
  DaysOfWeek <- xml2::xml_find_all(RegularDayType, ".//d1:DaysOfWeek")
  HolidaysOnly <- xml2::xml_find_all(RegularDayType, ".//d1:HolidaysOnly")
  RegularDayType_id <- xml2::xml_name(xml2::xml_children(RegularDayType))
  DaysOfWeek <- xml2::xml_name(xml2::xml_children(DaysOfWeek))
  HolidaysOnly <- xml2::xml_name(HolidaysOnly)

  RegularDayType_id <- data.frame(RegularDayType = RegularDayType_id, id = as.integer(ave(RegularDayType_id, RegularDayType_id, FUN = seq_along)))
  RegularDayType_id$DaysOfWeek <- ifelse(RegularDayType_id$RegularDayType == "DaysOfWeek", DaysOfWeek[RegularDayType_id$id], NA)
  RegularDayType_id$HolidaysOnly <- ifelse(RegularDayType_id$RegularDayType == "HolidaysOnly", HolidaysOnly[RegularDayType_id$id], NA)

  vj_simple$DaysOfWeek <- RegularDayType_id$DaysOfWeek
  vj_simple$HolidaysOnly <- RegularDayType_id$HolidaysOnly

  #Special Days
  SpecialDaysOperation <- xml2::xml_find_all(vehiclejourneys, ".//d1:SpecialDaysOperation")
  DaysOfNonOperation <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfNonOperation")




  if(xml2::xml_length(DaysOfNonOperation) > 0){
    DaysOfNonOperation_StartDate <- xml2::xml_text(xml2::xml_find_all(DaysOfNonOperation, ".//d1:StartDate"))
    DaysOfNonOperation_EndDate <- xml2::xml_text(xml2::xml_find_all(DaysOfNonOperation, ".//d1:EndDate"))
    DaysOfNonOperation_id <-  xml2::xml_length(xml2::xml_children(vehiclejourneys), only_elements = FALSE)
    DaysOfNonOperation_id <- DaysOfNonOperation_id > 0
    DaysOfNonOperation_id <- as.character(vj_simple$VehicleJourneyCode)[DaysOfNonOperation_id]
    DaysOfNonOperation <- data.frame(id = DaysOfNonOperation_id,
                                     StartDate = DaysOfNonOperation_StartDate,
                                     EndDate = DaysOfNonOperation_EndDate)
  }else{

  }

  DaysOfOperation <- xml2::xml_find_all(vehiclejourneys, ".//d1:DaysOfOperation")




  JPS                   <- xml_children(journeypatternsections)
  JPS_id                <- xml2::xml_text(xml2::xml_find_all(JPS, "@id"))
  JPS_id                <- rep(JPS_id, times = xml2::xml_length(JPS, only_elements = FALSE))


}


#' Imports when Multiple Values
#' Returns a dataframe with appopiate lookup id
#' @param Notes
#' @noRd
import_notes <- function(Notes){
  xml2::xml_parent(Notes)
  Notes_ids <- xml2::xml_text(xml2::xml_find_all(xml2_parent, idvar))
  VehicleJourneyCode <- import_simple(vehiclejourneys, ".//d1:VehicleJourneyCode")
  NoteCode <- xml2::xml_find_all(Notes, ".//d1:NoteCode")
  xml2::xml_parents(xml2::xml_parents(NoteCode))
  xml2::xml_text(xml2::xml_find_all(xml2::xml_parent(Notes), ".//d1:VehicleJourneyCode"))
}
