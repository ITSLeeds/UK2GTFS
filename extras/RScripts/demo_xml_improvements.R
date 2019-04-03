dir = "E:/OneDrive - University of Leeds/Routing/TransitExchangeData/data_20180515"
files = list.files(dir, full.names = T, recursive = T, pattern = ".xml")
file = files[1]
xml = xml2::read_xml(file)

Services_raw = xml2::xml_child(xml,"d1:Services")

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


  ss <- xml2::xml_find_all(ss, ".//d1:JourneyPattern")
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
  DaysOperation_StartDate   <- xml2::xml_text(xml2::xml_find_all(DaysOperation, ".//d1:StartDate"))
  DaysOperation_EndDate     <- xml2::xml_text(xml2::xml_find_all(DaysOperation, ".//d1:EndDate"))
  DaysOperation_Note        <- xml2::xml_text(xml2::xml_find_all(DaysOperation, ".//d1:Note"))

  DaysNonOperation             <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfNonOperation")
  DaysNonOperation_StartDate   <- xml2::xml_text(xml2::xml_find_all(DaysNonOperation, ".//d1:StartDate"))
  DaysNonOperation_EndDate     <- xml2::xml_text(xml2::xml_find_all(DaysNonOperation, ".//d1:EndDate"))
  DaysNonOperation_Note        <- xml2::xml_text(xml2::xml_find_all(DaysNonOperation, ".//d1:Note"))

  DaysOperation <- data.frame(type =      "DaysOperation",
                              StartDate = DaysOperation_StartDate,
                              EndDate =   DaysOperation_EndDate,
                              Note =      DaysOperation_Note)

  DaysNonOperation <- data.frame(type =      "DaysNonOperation",
                              StartDate = DaysNonOperation_StartDate,
                              EndDate =   DaysNonOperation_EndDate,
                              Note =      DaysNonOperation_Note)

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

  Services_NonOperation <- data.frame()

  results <- list(StandardService, Services_main, Services_NonOperation, SpecialDaysOperation)
  names(results) <- c("StandardService", "Services_main", "Services_NonOperation", "SpecialDaysOperation")

  return(results)
}



t1 <- Sys.time()



Services <- xml2::xml_child(Services_raw,1)
Services <- xml2::as_list(Services)

Services_main <- Services[c("ServiceCode","PrivateCode","Mode","Description","RegisteredOperatorRef")]
Services_main <- lapply(Services_main, unlist, recursive = TRUE)
Services_main <- as.data.frame(Services_main, stringsAsFactors = FALSE)
#Services_main[] <- lapply(Services_main, unlist, recursive = TRUE)
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


t2 <- Sys.time()

Operators2 <- import_operators(Operators_raw)

t3 <- Sys.time()

d1 <- as.numeric(difftime(t2,t1))
d2 <- as.numeric(difftime(t3,t2))
Operators[] <- lapply(Operators, as.character)
Operators2[] <- lapply(Operators2, as.character)
message(paste0("New method is ",round(d1/d2,3)," times faster and identical check is ",identical(Operators, Operators2)))



