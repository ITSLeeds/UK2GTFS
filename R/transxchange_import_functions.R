# TransXchange import fucntions

# Import stoppoints
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

#Import routes
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

#Import journeypatternsections
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

#import operators
# slower so not used
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
