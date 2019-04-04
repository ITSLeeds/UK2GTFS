dir = "E:/OneDrive - University of Leeds/Routing/TransitExchangeData/data_20180515"
files = list.files(dir, full.names = T, recursive = T, pattern = ".xml")
file = "E:/OneDrive - University of Leeds/Routing/TransitExchangeData/data_20180515/EA/ea_20-12-A-y08-1.xml"
xml = xml2::read_xml(file)
run_debug = TRUE

VehicleJourneys_raw = xml2::xml_child(xml,"d1:VehicleJourneys")


import_vehiclejourneys <- function(vehiclejourneys){

  PrivateCode <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:PrivateCode"))
  VehicleJourneyCode <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:VehicleJourneyCode"))
  ServiceRef <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:ServiceRef"))
  LineRef <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:LineRef"))
  JourneyPatternRef <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:JourneyPatternRef"))
  DepartureTime <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:DepartureTime"))
  BankHolidaysOperate <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:BankHolidaysOperate"))
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



t1 <- Sys.time()




VehicleJourneys = xml2::as_list(VehicleJourneys_raw)

vj_clean <- function(vj,run_debug){
  # break simple and complex parts
  vj_simple = vj[c("PrivateCode","VehicleJourneyCode","ServiceRef","LineRef",
                   "JourneyPatternRef","DepartureTime")]
  vj_simple = data.frame(t(unlist(vj_simple)), stringsAsFactors = F)
  vj_op = vj[["OperatingProfile"]]
  vjtls = vj[names(vj) == "VehicleJourneyTimingLink"]
  if(length(vjtls) < 1){
    #message("VehicleJourneyTimingLinks are missing")
    vjtls = NULL
  }else{
    vjtl_clean <- function(vjtl){
      vjtl_attr <- attributes(vjtl)$id
      vjtl_ref <- vjtl$JourneyPatternTimingLinkRef[[1]]
      vjtl_from <- vjtl$From
      if(length(vjtl_from) == 0){vjtl_from <- NA}else{message("data in VJTL from");stop()}
      vjtl_to <- vjtl$To
      if(length(vjtl_to) == 0){vjtl_to <- NA}else{message("data in VJTL to");stop()}
      if(!all(names(vjtl) %in% c("JourneyPatternTimingLinkRef", "From","To"))){message("extra data in VJTL");stop()}
      vjtl = c(vjtl_attr, vjtl_ref, vjtl_from, vjtl_to)
      names(vjtl) = c("id","JourneyPatternTimingLinkRef","From","To")
      return(vjtl)
    }
    vjtls <- lapply(vjtls,vjtl_clean)
    vjtls <- data.frame(t(as.data.frame(vjtls)))
    row.names(vjtls) <- seq(1,nrow(vjtls))
  }

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
  result <- list(vj_simple,op_dno,op_do, vjtls)
  return(result)
}

VehicleJourneys <- lapply(VehicleJourneys, vj_clean, run_debug = run_debug)
VehicleJourneys_exclude <- lapply(VehicleJourneys,`[[`,2)
VehicleJourneys_exclude <- dplyr::bind_rows(VehicleJourneys_exclude)
VehicleJourneys_include <- lapply(VehicleJourneys,`[[`,3)
VehicleJourneys_include <- dplyr::bind_rows(VehicleJourneys_include)

if(full_import){
  VehicleJourneysTimingLinks <- lapply(VehicleJourneys,`[[`,4)
  VehicleJourneysTimingLinks <- dplyr::bind_rows(VehicleJourneysTimingLinks)
  VehicleJourneysTimingLinks[] <- lapply(VehicleJourneysTimingLinks, factor)
}else{
  VehicleJourneysTimingLinks <- NA
}

VehicleJourneys <- lapply(VehicleJourneys,`[[`,1)
VehicleJourneys <- dplyr::bind_rows(VehicleJourneys)

VehicleJourneys[] <- lapply(VehicleJourneys, factor)
VehicleJourneys_exclude[] <- lapply(VehicleJourneys_exclude, factor)
VehicleJourneys_include[] <- lapply(VehicleJourneys_include, factor)


t2 <- Sys.time()

Services2 <- import_services(Services_raw)

t3 <- Sys.time()

d1 <- as.numeric(difftime(t2,t1))
d2 <- as.numeric(difftime(t3,t2))
#Operators[] <- lapply(Operators, as.character)
#Operators2[] <- lapply(Operators2, as.character)
message(paste0("New method is ",round(d1/d2,3)," times faster and identical check is ",identical(Services, Services2)))

identical(Services$StandardService, Services2$StandardService)
f1 <- Services$StandardService
f2 <- Services2$StandardService
identical(f1$Direction,f2$Direction)
