# Import RouteLink
import_RouteLink = function(x){
  data.frame(From = x$From$StopPointRef,
             To = x$To$StopPointRef,
             Direction = x$Direction,
             RouteLink = x$.attrs,
             stringsAsFactors = F)
}

import_RouteSection = function(y){
  res = lapply(y[seq(1,length(y)-1)], import_RouteLink)
  res = dplyr::bind_rows(res)
  res$RouteSection = y$.attrs
  return(res)
}

import_RouteSections = function(z){
  res = lapply(z, import_RouteSection)
  res = dplyr::bind_rows(res)
  return(res)
}

# Import VehicleJourneys
import_VehicleJourney = function(x){
  res = data.frame(PrivateCode = x$PrivateCode,
             VehicleJourneyCode = x$VehicleJourneyCode,
             ServiceRef = x$ServiceRef,
             LineRef = x$LineRef,
             JourneyPatternRef = x$JourneyPatternRef,
             DepartureTime = x$DepartureTime,
             stringsAsFactors = F)
  res$RegularDayType = list(x$OperatingProfile$RegularDayType)
  res$DaysOfNonOperation = list(x$OperatingProfile$SpecialDaysOperation$DaysOfNonOperation)
  res$BankHolidayOperation = list(x$OperatingProfile$BankHolidayOperation$DaysOfNonOperation)
  #res$OperatingProfile = list(x$OperatingProfile)
  return(res)
}

import_VehicleJourneys = function(y){
  res = lapply(y, import_VehicleJourney)
  res = dplyr::bind_rows(res)
  return(res)
}

# Import Services
import_Services = function(y){
  res = lapply(y, import_Service)
  res = dplyr::bind_rows(res)
  return(res)
}

import_Service = function(x){
  res = data.frame(ServiceCode = x$ServiceCode,
                   PrivateCode = x$PrivateCode,
                   LineName = x$Lines$Line$LineName,
                   StartDate = x$OperatingPeriod$StartDate,
                   EndDate = x$OperatingPeriod$EndDate,
                   OperatingProfile_RegularDayType = names(x$OperatingProfile$RegularDayType$DaysOfWeek),
                   #OperatingProfile = list(Services$OperatingProfile),
                   RegisteredOperatorRef = x$RegisteredOperatorRef,
                   #StopRequirements = list(Services$StopRequirements),
                   Mode = x$Mode,
                   Description = x$Description,
                   #StandardService = list(Services$StandardService),
                   StandardService_Origin = x$StandardService$Origin,
                   StandardService_Destination = x$StandardService$Destination,
                   stringsAsFactors=FALSE)

  #OperatingProfile = x$OperatingProfile
  StopRequirements = x$StopRequirements #to do

  StandardService = x$StandardService
  StandardService = lapply(StandardService[names(StandardService) == "JourneyPattern"], import_JourneyPattern)
  StandardService = dplyr::bind_rows(StandardService)

  res$JourneyPattern = list(StandardService)
  return(res)
}


import_JourneyPattern = function(z){
  data.frame(Direction = z$Direction,
             RouteRef = z$RouteRef,
             JourneyPatternSectionRefs = z$JourneyPatternSectionRefs,
             id = z$.attrs,
             stringsAsFactors = F)
}

# Import JourneyPatternSections

import_JourneyPatternTimingLink = function(x){
  data.frame(RouteLinkRef = x$RouteLinkRef,
             RunTime = x$RunTime,
             From_Activity = x$From$Activity,
             From_StopPointRef = x$From$StopPointRef,
             From_TimingStatus = x$From$TimingStatus,
             From_SequenceNumber= x$From$.attrs,
             To_Activity = x$To$Activity,
             To_StopPointRef = x$To$StopPointRef,
             To_TimingStatus = x$To$TimingStatus,
             To_SequenceNumber= x$To$.attrs,
             stringsAsFactors = F
             )
}

import_JourneyPatternSection = function(y){
  res = lapply(y[names(y) == "JourneyPatternTimingLink"], import_JourneyPatternTimingLink)
  res = dplyr::bind_rows(res)
  res$id = y$.attrs
  return(res)
}

import_JourneyPatternSections = function(z){
  res = lapply(z, import_JourneyPatternSection)
  res = dplyr::bind_rows(res)
  return(res)
}

# Read in whole XML file
import_transXchange = function(xml){

  data = XML::xmlParse(xml)
  xml_data = XML::xmlToList(data)
  #names(xml_data)

  # StopPoints
  StopPoints = xml_data["StopPoints"]
  StopPoints = StopPoints[[1]]
  StopPoints <- data.frame(matrix(unlist(StopPoints), nrow=length(StopPoints), byrow=T),stringsAsFactors=FALSE)
  names(StopPoints) = c("StopPointRef","CommonName","Indicator","LocalityName","LocalityQualifier")

  # RouteSections
  RouteSections = xml_data["RouteSections"]
  RouteSections = import_RouteSections(RouteSections[[1]])

  # Routes
  Routes = xml_data["Routes"]
  Routes = Routes[[1]]
  Routes <- data.frame(matrix(unlist(Routes), nrow=length(Routes), byrow=T),stringsAsFactors=FALSE)
  names(Routes) = c("PrivateCode","Description","RouteSectionRef","id")

  # Operators
  Operators = xml_data["Operators"]
  Operators = Operators[[1]]
  Operators <- data.frame(matrix(unlist(Operators), nrow=length(Operators), byrow=T),stringsAsFactors=FALSE)
  names(Operators) = c("NationalOperatorCode","OperatorCode","OperatorShortName","OperatorNameOnLicence","TradingName","id")

  # Services
  Services = xml_data["Services"]
  Services = Services[[1]]
  Services = import_Services(Services)

  # JourneyPatternSections
  JourneyPatternSections = xml_data["JourneyPatternSections"]
  JourneyPatternSections = JourneyPatternSections[[1]]
  JourneyPatternSections = import_JourneyPatternSections(JourneyPatternSections)

  # VehicleJourneys
  VehicleJourneys = xml_data["VehicleJourneys"]
  VehicleJourneys = VehicleJourneys[[1]]
  VehicleJourneys = import_VehicleJourneys(VehicleJourneys)

  rm(data, xml, xml_data)
  all = list(StopPoints,RouteSections,Routes,JourneyPatternSections,Operators,Services,VehicleJourneys)
  names(all) = c("StopPoints","RouteSections","Routes","JourneyPatternSections", "Operators","Services","VehicleJourneys" )
  rm(StopPoints,RouteSections,Routes,JourneyPatternSections,Operators,Services,VehicleJourneys)
  return(all)
}
