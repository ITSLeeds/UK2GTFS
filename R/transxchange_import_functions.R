# TransXchange import fucntions

#' Import stoppoints
#' @noRd
import_stoppoints <- function(StopPoints){
  StopPointRef       <- xml2::xml_text(xml2::xml_find_all(StopPoints, ".//d1:StopPointRef"))
  CommonName         <- xml2::xml_text(xml2::xml_find_all(StopPoints, ".//d1:CommonName"))
  Indicator          <- xml2::xml_text(xml2::xml_find_all(StopPoints, ".//d1:Indicator"))
  if(length(Indicator) == 0){
    Indicator <- rep(NA, length(StopPointRef))
  }
  LocalityName       <- xml2::xml_text(xml2::xml_find_all(StopPoints, ".//d1:LocalityName"))
  if(length(LocalityName) == 0){
    LocalityName <- rep(NA, length(StopPointRef))
  }
  LocalityQualifier  <- xml2::xml_text(xml2::xml_find_all(StopPoints, ".//d1:LocalityQualifier"))
  if(length(LocalityQualifier) == 0){
    LocalityQualifier <- rep(NA, length(StopPointRef))
  }

  StopPoints <- data.frame(StopPointRef = StopPointRef,
                           CommonName = CommonName,
                           Indicator = Indicator,
                           LocalityName = LocalityName,
                           LocalityQualifier = LocalityQualifier)
  return(StopPoints)
}

#' Import routes
#' @noRd
import_routes <- function(routes){
  Description       <- xml2::xml_text(xml2::xml_find_all(routes, ".//d1:Description"))
  RouteSectionRef   <- xml2::xml_text(xml2::xml_find_all(routes, ".//d1:RouteSectionRef"))
  PrivateCode       <- xml2::xml_text(xml2::xml_find_all(routes, ".//d1:PrivateCode"))
  if(length(PrivateCode) == 0){
    PrivateCode <- rep(NA, length(StopPointRef))
  }

  routes <- data.frame(PrivateCode = PrivateCode,
                       Description = Description,
                       RouteSectionRef = RouteSectionRef)
  return(routes)
}

#' Import journeypatternsections
#' @noRd
import_journeypatternsections <- function(journeypatternsections){
  JourneyPatternTimingLink <- xml2::xml_find_all(journeypatternsections, ".//d1:JourneyPatternTimingLink")
  JPTL_ID               <- xml2::xml_text(xml2::xml_find_all(JourneyPatternTimingLink, "@id"))
  #JPTL_ID               <- rep(JPTL_ID, times = xml2::xml_length(JourneyPatternTimingLink, only_elements = FALSE))

  RouteLinkRef          <- xml2::xml_text(xml2::xml_find_all(JourneyPatternTimingLink, "d1:RouteLinkRef"))
  RunTime               <- xml2::xml_text(xml2::xml_find_all(JourneyPatternTimingLink, "d1:RunTime"))
  From                  <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:From")
  From.Activity         <- xml2::xml_text(xml2::xml_find_all(From, "d1:Activity"))
  if(length(From.Activity) == 0){
    From.Activity <- rep(NA, length(From.StopPointRef))
  }
  From.StopPointRef     <- xml2::xml_text(xml2::xml_find_all(From, "d1:StopPointRef"))
  From.TimingStatus     <- xml2::xml_text(xml2::xml_find_all(From, "d1:TimingStatus"))
  From.SequenceNumber   <- xml2::xml_text(xml2::xml_find_all(From, "@SequenceNumber"))
  if(length(From.SequenceNumber) == 0){
    From.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }
  To                    <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:To")
  To.WaitTime           <- xml2::xml_text(xml2::xml_find_first(To, "d1:WaitTime"))
  To.Activity           <- xml2::xml_text(xml2::xml_find_all(To, "d1:Activity"))
  if(length(To.Activity) == 0){
    To.Activity <- rep(NA, length(From.StopPointRef))
  }
  To.StopPointRef       <- xml2::xml_text(xml2::xml_find_all(To, "d1:StopPointRef"))
  To.TimingStatus       <- xml2::xml_text(xml2::xml_find_all(To, "d1:TimingStatus"))
  To.SequenceNumber     <- xml2::xml_text(xml2::xml_find_all(To, "@SequenceNumber"))
  if(length(To.SequenceNumber) == 0){
    To.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }

  JPS                   <- xml_children(journeypatternsections)
  JPS_id                <- xml2::xml_text(xml2::xml_find_all(JPS, "@id"))
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
#' @noRd
import_operators <- function(operators){

  NationalOperatorCode       <- xml2::xml_text(xml2::xml_find_all(operators, ".//d1:NationalOperatorCode"))
  OperatorCode               <- xml2::xml_text(xml2::xml_find_all(operators, ".//d1:OperatorCode"))
  OperatorShortName          <- xml2::xml_text(xml2::xml_find_all(operators, ".//d1:OperatorShortName"))
  OperatorNameOnLicence      <- xml2::xml_text(xml2::xml_find_all(operators, ".//d1:OperatorNameOnLicence"))
  TradingName                <- xml2::xml_text(xml2::xml_find_all(operators, ".//d1:TradingName"))


  operators <- data.frame(NationalOperatorCode = NationalOperatorCode,
                          OperatorCode = OperatorCode,
                          OperatorShortName = OperatorShortName,
                          OperatorNameOnLicence = OperatorNameOnLicence,
                          TradingName = TradingName
  )
  return(operators)
}

#' import services
#' @noRd

import_services <- function(service){

  ServiceCode             <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:ServiceCode"))
  PrivateCode             <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:PrivateCode"))
  Mode                    <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:Mode"))
  Description             <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:Description"))
  RegisteredOperatorRef   <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:RegisteredOperatorRef"))
  StartDate               <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:StartDate"))
  EndDate                 <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:EndDate"))
  DaysOfWeek              <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:DaysOfWeek"))
  StopRequirements        <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:StopRequirements"))
  Origin                  <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:Origin"))
  Destination             <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:Destination"))
  LineName                <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:LineName"))
  BankHolidayNonOperation <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:BankHolidayNonOperation"))
  if(length(BankHolidayNonOperation) == 0){
    BankHolidayNonOperation <- rep(NA, length(ServiceCode))
  }
  BankHolidayOperation    <- xml2::xml_text(xml2::xml_find_all(service, ".//d1:BankHolidayOperation"))
  if(length(BankHolidayOperation) == 0){
    BankHolidayOperation <- rep(NA, length(ServiceCode))
  }


  ss <- xml2::xml_find_all(service, ".//d1:JourneyPattern")
  Direction                 <- xml2::xml_text(xml2::xml_find_all(ss, ".//d1:Direction"))
  VehicleType               <- xml2::xml_text(xml2::xml_find_all(ss, ".//d1:VehicleType"))
  if(length(VehicleType) == 0){
    VehicleType <- rep(NA, length(Direction))
  }
  RouteRef                  <- xml2::xml_text(xml2::xml_find_all(ss, ".//d1:RouteRef"))
  JourneyPatternSectionRefs <- xml2::xml_text(xml2::xml_find_all(ss, ".//d1:JourneyPatternSectionRefs"))
  JourneyPatternID          <- xml2::xml_text(xml2::xml_find_all(ss, "@id"))


  SpecialDaysOperation      <- xml2::xml_find_all(service, ".//d1:SpecialDaysOperation")
  DaysOperation             <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfOperation")
  DaysNonOperation          <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfNonOperation")

  if(xml2::xml_length(DaysOperation) > 0){
    DaysOperation_StartDate   <- xml2::xml_text(xml2::xml_find_all(DaysOperation, ".//d1:StartDate"))
    DaysOperation_EndDate     <- xml2::xml_text(xml2::xml_find_all(DaysOperation, ".//d1:EndDate"))
    DaysOperation_Note        <- xml2::xml_text(xml2::xml_find_all(DaysOperation, ".//d1:Note"))
    DaysOperation <- data.frame(type =      "DaysOperation",
                                StartDate = DaysOperation_StartDate,
                                EndDate =   DaysOperation_EndDate,
                                Note =      DaysOperation_Note)
  }else{
    DaysOperation <- NULL
  }

  if(xml2::xml_length(DaysNonOperation) > 0){
    DaysNonOperation_StartDate   <- xml2::xml_text(xml2::xml_find_all(DaysNonOperation, ".//d1:StartDate"))
    DaysNonOperation_EndDate     <- xml2::xml_text(xml2::xml_find_all(DaysNonOperation, ".//d1:EndDate"))
    DaysNonOperation_Note        <- xml2::xml_text(xml2::xml_find_all(DaysNonOperation, ".//d1:Note"))
    DaysNonOperation <- data.frame(type =      "DaysNonOperation",
                                   StartDate = DaysNonOperation_StartDate,
                                   EndDate =   DaysNonOperation_EndDate,
                                   Note =      DaysNonOperation_Note)
  }else{
    DaysOperation <- NULL
  }


  SpecialDaysOperation <- rbind(DaysOperation, DaysNonOperation)


  StandardService <- data.frame(Direction = Direction,
                                VehicleType = VehicleType,
                                RouteRef = RouteRef,
                                JourneyPatternSectionRefs = JourneyPatternSectionRefs,
                                JourneyPatternID = JourneyPatternID
  )

  Services_main <- data.frame(ServiceCode = ServiceCode,
                              PrivateCode = PrivateCode,
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


  results <- list(StandardService, Services_main, SpecialDaysOperation)
  names(results) <- c("StandardService", "Services_main", "SpecialDaysOperation")

  return(results)
}
