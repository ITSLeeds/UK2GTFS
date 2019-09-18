#' Import Vehicle Journeys
#' ????
#' @param vehiclejourneys desc
#' @param Services_main desc
#' @param cal cal object
#' @noRd

import_vehiclejourneys2 <- function(vehiclejourneys, Services_main, cal) {

  # PrivateCode <- import_simple(vehiclejourneys, ".//d1:PrivateCode")
  VehicleJourneyCode <- import_simple(vehiclejourneys, ".//d1:VehicleJourneyCode")
  ServiceRef <- import_simple(vehiclejourneys, ".//d1:ServiceRef")
  LineRef <- import_simple(vehiclejourneys, ".//d1:LineRef")
  JourneyPatternRef <- import_simple(vehiclejourneys, ".//d1:JourneyPatternRef")
  DepartureTime <- import_simple(vehiclejourneys, ".//d1:DepartureTime")
  BankHolidaysOperate <- import_simple(vehiclejourneys, ".//d1:BankHolidaysOperate")
  Notes <- xml2::xml_find_all(vehiclejourneys, ".//d1:Note")

  if (any(xml2::xml_length(Notes) > 0)) {
    Notes <- import_notes2(vehiclejourneys)
  } else {
    Notes <- NA
  }

  if (length(BankHolidaysOperate) == 0) {
    BankHolidaysOperate <- rep(NA, length(VehicleJourneyCode))
  }
  BankHolidaysNoOperate <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:BankHolidaysNoOperate"))
  if (length(BankHolidaysNoOperate) == 0) {
    BankHolidaysNoOperate <- rep(NA, length(VehicleJourneyCode))
  }

  if(length(JourneyPatternRef) != length(VehicleJourneyCode)){
    JourneyPatternRef <- import_withmissing(vehiclejourneys, ".//d1:JourneyPatternRef", 8)
    VehicleJourneyRef <- import_withmissing(vehiclejourneys, ".//d1:VehicleJourneyRef", 8)
    # JourneyPatternRef <- ifelse(is.na(JourneyPatternRef),
    #                             VehicleJourneyRef,
    #                             JourneyPatternRef)

    stop("JourneyPatternRef and VehicleJourneyRefs not same length")
  }


  vj_simple <- data.frame( # PrivateCode = PrivateCode,
    VehicleJourneyCode = VehicleJourneyCode,
    ServiceRef = ServiceRef,
    LineRef = LineRef,
    JourneyPatternRef = JourneyPatternRef,
    #VehicleJourneyRef = VehicleJourneyRef,
    DepartureTime = DepartureTime,
    # days = days,
    BankHolidaysOperate = BankHolidaysOperate,
    BankHolidaysNoOperate = BankHolidaysNoOperate,
    stringsAsFactors = FALSE
  )

  OperatingProfile <- xml2::xml_find_all(vehiclejourneys, ".//d1:OperatingProfile")
  if(length(OperatingProfile) > 0){
    OperatingProfile <- import_OperatingProfile(OperatingProfile)
    SpecialDays <- OperatingProfile$SpecialDays
    OperatingProfile <- OperatingProfile$OperatingProfile
  } else {
    OperatingProfile <- NULL
    SpecialDays <- NULL
  }






  result <- list(vj_simple, OperatingProfile, SpecialDays, Notes)
  names(result) <- c("VehicleJourneys", "OperatingProfile", "SpecialDays", "VJ_Notes")
  # JPS                   <- xml_children(journeypatternsections)
  # JPS_id                <- xml2::xml_text(xml2::xml_find_all(JPS, "@id"))
  # JPS_id                <- rep(JPS_id, times = xml2::xml_length(JPS, only_elements = FALSE))
  return(result)
}
