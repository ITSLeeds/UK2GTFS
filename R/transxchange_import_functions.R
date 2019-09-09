# TransXchange import fucntions

#' Import Simple
#' ????
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_simple <- function(xml1, nm) {
  xml2::xml_text(xml2::xml_find_all(xml1, nm))
}


#' Import When some rows are missing
#' Checks lengths of obejct against lgth
#' @param xml1 XML object
#' @param nm character name to find
#' @param lgth numeric length check
#' @noRd
import_withmissing <- function(xml1, nm, lgth) {
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
#' @param layers how many layers down
#' @param idvar the id variaible in the higher tree
#' @noRd
import_withmissing2 <- function(xml1, nm, layers, idvar) {
  xml_2 <- xml2::xml_find_all(xml1, nm)
  xml2_parent <- xml2::xml_parent(xml_2)
  if (layers > 1) {
    for (i in seq(2, layers)) {
      xml2_parent <- xml2::xml_parent(xml2_parent)
    }
  }
  xml2_parent_id <- xml2::xml_text(xml2::xml_find_all(xml2_parent, idvar))
  xml1_id <- xml2::xml_text(xml2::xml_find_all(xml1, idvar))

  res <- rep(NA, length(xml1_id))
  res[match(xml2_parent_id, xml1_id)] <- xml2::xml_text(xml_2)
  return(res)
}


#' Import FromTo
#' To work with missing cases in childnre names
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
import_FromTo <- function(xml1, nm) {
  res <- xml2::xml_text(xml2::xml_find_all(xml1, nm))
  lth <- length(xml2::xml_length(xml1))
  if (length(res) == lth) {
    return(res)
  } else {
    # There are missing values
    res <- list()
    for (i in seq(1:lth)) {
      sub <- xml2::xml_text(xml2::xml_find_all(xml1[i], nm))
      if (length(sub) == 0) {
        sub <- NA
      }
      res[[i]] <- sub
    }
    res <- unlist(res)
    return(res)
  }
}

#' Clean NA from sequence
#' @param x sequency of numbers
#'

clean_sequence <- function(x) {
  if (anyNA(x)) {
    x <- as.integer(x)
    lth <- length(x)
    for (i in seq(1, lth)) {
      val <- x[i]
      if (is.na(val)) {
        if (i == 1) {
          # First in sequence
          if (x[2] > 1) {
            x[i] <- 1
          } else {
            stop("Can't clean NA from sequence")
          }
        } else if (i == lth) {
          # Last value just add one
          x[i] <- x[i - 1] + 1
        } else {
          # Middle Value
          if (is.na(x[i + 1])) {
            # Next value also NA
            x[i] <- x[i - 1] + 1
          } else {
            if (x[i + 1] - x[i - 1] >= 2) {
              x[i] <- x[i - 1] + 1
            } else {
              stop("Can't clean NA from sequence")
            }
          }
        }
      }
    }
    x <- as.character(x)
  }
  return(x)
}




#' Import stoppoints
#' ????
#' @param StopPoints stoppoints
#' @param full_import logical
#' @noRd
import_stoppoints <- function(StopPoints, full_import = TRUE) {
  StopPointRef <- import_simple(StopPoints, ".//d1:StopPointRef")


  if (full_import) {
    CommonName <- import_simple(StopPoints, ".//d1:CommonName")
    LocalityName <- import_simple(StopPoints, ".//d1:LocalityName")
    LocalityQualifier <- import_simple(StopPoints, ".//d1:LocalityQualifier")
    Indicator <- import_withmissing(StopPoints, ".//d1:Indicator", 5)

    StopPoints <- data.frame(
      StopPointRef = StopPointRef,
      CommonName = CommonName,
      Indicator = Indicator,
      LocalityName = LocalityName,
      LocalityQualifier = LocalityQualifier
    )
  } else {
    StopPoints <- data.frame(StopPointRef = StopPointRef)
  }
  return(StopPoints)
}

#' Import routes
#' ????
#' @param routes routes
#' @noRd
import_routes <- function(routes) {
  Description <- import_simple(routes, ".//d1:Description")
  RouteSectionRef <- import_simple(routes, ".//d1:RouteSectionRef")
  PrivateCode <- import_simple(routes, ".//d1:PrivateCode")
  if (length(PrivateCode) == 0) {
    PrivateCode <- rep(NA, length(RouteSectionRef))
  }

  routes <- data.frame(
    PrivateCode = PrivateCode,
    Description = Description,
    RouteSectionRef = RouteSectionRef
  )
  return(routes)
}

#' Import journeypatternsections
#' ????
#' @param journeypatternsections journeypattern sections
#' @noRd
import_journeypatternsections <- function(journeypatternsections) {
  JourneyPatternTimingLink <- xml2::xml_find_all(journeypatternsections, ".//d1:JourneyPatternTimingLink")
  JPTL_ID <- import_simple(JourneyPatternTimingLink, "@id")
  # JPTL_ID               <- rep(JPTL_ID, times = xml2::xml_length(JourneyPatternTimingLink, only_elements = FALSE))

  RouteLinkRef <- import_simple(JourneyPatternTimingLink, "d1:RouteLinkRef")
  RunTime <- import_simple(JourneyPatternTimingLink, "d1:RunTime")
  From <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:From")
  From.StopPointRef <- import_simple(From, "d1:StopPointRef")
  From.Activity <- import_simple(From, "d1:Activity")
  if (length(From.Activity) == 0) {
    From.Activity <- rep(NA, length(From.StopPointRef))
  }
  From.TimingStatus <- import_simple(From, "d1:TimingStatus")
  From.SequenceNumber <- import_FromTo(From, "@SequenceNumber")
  From.SequenceNumber <- clean_sequence(From.SequenceNumber)

  if (length(From.SequenceNumber) == 0) {
    From.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }
  To <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:To")
  To.StopPointRef <- import_simple(To, "d1:StopPointRef")
  To.WaitTime <- xml2::xml_text(xml2::xml_find_first(To, "d1:WaitTime"))
  To.Activity <- import_simple(To, "d1:Activity")
  if (length(To.Activity) == 0) {
    To.Activity <- rep(NA, length(To.StopPointRef))
  }
  To.TimingStatus <- import_simple(To, "d1:TimingStatus")
  To.SequenceNumber <- import_FromTo(To, "@SequenceNumber")
  To.SequenceNumber <- clean_sequence(To.SequenceNumber)
  if (length(To.SequenceNumber) == 0) {
    To.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }

  JPS <- xml2::xml_children(journeypatternsections)
  JPS_id <- import_simple(JPS, "@id")
  JPS_id <- rep(JPS_id, times = xml2::xml_length(JPS, only_elements = FALSE))


  journeypatternsections <- data.frame(
    JPTL_ID = JPTL_ID,
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
import_operators <- function(operators) {
  NationalOperatorCode <- import_simple(operators, ".//d1:NationalOperatorCode")
  OperatorCode <- import_simple(operators, ".//d1:OperatorCode")
  OperatorShortName <- import_simple(operators, ".//d1:OperatorShortName")
  OperatorNameOnLicence <- import_simple(operators, ".//d1:OperatorNameOnLicence")
  TradingName <- import_simple(operators, ".//d1:TradingName")

  if (length(OperatorNameOnLicence) == 0) {
    OperatorNameOnLicence <- rep(NA, length(NationalOperatorCode))
  }
  if (length(TradingName) == 0) {
    TradingName <- rep(NA, length(NationalOperatorCode))
  }

  operators <- data.frame(
    NationalOperatorCode = NationalOperatorCode,
    OperatorCode = OperatorCode,
    OperatorShortName = OperatorShortName,
    OperatorNameOnLicence = OperatorNameOnLicence,
    TradingName = TradingName
  )
  return(operators)
}

#' import services
#' ????
#' @param service desc
#' @param full_import desc
#' @noRd

import_services <- function(service, full_import = TRUE) {
  ServiceCode <- import_simple(service, ".//d1:ServiceCode")
  Mode <- import_simple(service, ".//d1:Mode")
  Description <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:Description"))
  RegisteredOperatorRef <- import_simple(service, ".//d1:RegisteredOperatorRef")
  StartDate <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:StartDate"))
  EndDate <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:EndDate"))
  DaysOfWeek <- paste(xml2::xml_name(xml2::xml_children(xml2::xml_find_first(service, ".//d1:DaysOfWeek"))), collapse = " ")
  StopRequirements <- import_simple(service, ".//d1:StopRequirements")
  Origin <- import_simple(service, ".//d1:Origin")
  Destination <- import_simple(service, ".//d1:Destination")
  LineName <- import_simple(service, ".//d1:LineName")
  BankHolidayNonOperation <- import_simple(service, ".//d1:BankHolidayNonOperation")
  if (length(BankHolidayNonOperation) == 0) {
    BankHolidayNonOperation <- rep(NA, length(ServiceCode))
  }
  BankHolidayOperation <- import_simple(service, ".//d1:BankHolidayOperation")
  if (length(BankHolidayOperation) == 0) {
    BankHolidayOperation <- rep(NA, length(ServiceCode))
  }

  if (full_import) {
    PrivateCode <- import_simple(service, ".//d1:PrivateCode")
    if (length(PrivateCode) == 0) {
      PrivateCode <- rep(NA, length(ServiceCode))
    }
  }

  ss <- xml2::xml_find_all(service, ".//d1:JourneyPattern")
  Direction <- import_simple(ss, ".//d1:Direction")
  VehicleType <- import_withmissing2(ss, ".//d1:Description", 3, "@id")
  RouteRef <- import_simple(ss, ".//d1:RouteRef")
  if (length(RouteRef) == 0) {
    RouteRef <- rep(NA, length(Direction))
  }
  JourneyPatternSectionRefs <- import_simple(ss, ".//d1:JourneyPatternSectionRefs")
  JourneyPatternID <- import_simple(ss, "@id")


  SpecialDaysOperation <- xml2::xml_find_all(service, ".//d1:SpecialDaysOperation")
  DaysOperation <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfOperation")
  DaysNonOperation <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfNonOperation")

  if (xml2::xml_length(DaysOperation) > 0) {
    DaysOperation_StartDate <- import_simple(DaysOperation, ".//d1:StartDate")
    DaysOperation_EndDate <- import_simple(DaysOperation, ".//d1:EndDate")
    DaysOperation_Note <- import_simple(DaysOperation, ".//d1:Note")
    DaysOperation <- data.frame(
      type = "DaysOperation",
      StartDate = DaysOperation_StartDate,
      EndDate = DaysOperation_EndDate,
      Note = DaysOperation_Note
    )
  } else {
    DaysOperation <- NULL
  }

  if (xml2::xml_length(DaysNonOperation) > 0) {
    DaysNonOperation_StartDate <- import_simple(DaysNonOperation, ".//d1:StartDate")
    DaysNonOperation_EndDate <- import_simple(DaysNonOperation, ".//d1:EndDate")
    # DaysNonOperation_Note        <- import_simple(DaysNonOperation, ".//d1:Note")
    DaysNonOperation <- data.frame(
      type = "DaysNonOperation",
      StartDate = DaysNonOperation_StartDate,
      EndDate = DaysNonOperation_EndDate
    ) # ,
    # Note =      DaysNonOperation_Note)
  } else {
    DaysNonOperation <- NULL
  }

  if (!is.null(DaysOperation) & !is.null(DaysNonOperation)) {
    SpecialDaysOperation <- rbind(DaysOperation, DaysNonOperation)
  } else if (!is.null(DaysOperation)) {
    SpecialDaysOperation <- DaysOperation
  } else {
    SpecialDaysOperation <- DaysNonOperation
  }



  StandardService <- data.frame(
    Direction = Direction,
    VehicleType = VehicleType,
    RouteRef = RouteRef,
    JourneyPatternSectionRefs = JourneyPatternSectionRefs,
    JourneyPatternID = JourneyPatternID
  )

  Services_main <- data.frame(
    ServiceCode = ServiceCode,
    # PrivateCode = PrivateCode,
    Mode = Mode,
    Description = Description,
    RegisteredOperatorRef = RegisteredOperatorRef,
    StartDate = as.Date(StartDate),
    EndDate = as.Date(EndDate),
    DaysOfWeek = DaysOfWeek,
    StopRequirements = StopRequirements,
    Origin = Origin,
    Destination = Destination,
    LineName = LineName,
    BankHolidayNonOperation = BankHolidayNonOperation,
    BankHolidayOperation = BankHolidayOperation,
    stringsAsFactors = FALSE
  )

  if (full_import) {
    Services_main$PrivateCode <- PrivateCode
  }


  results <- list(StandardService, Services_main, SpecialDaysOperation)
  names(results) <- c("StandardService", "Services_main", "SpecialDaysOperation")

  return(results)
}



#' Import Vehicle Journeys
#' ????
#' @param vehiclejourneys desc
#' @noRd
import_vehiclejourneys <- function(vehiclejourneys, Services_main, cal) {

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


  vj_simple <- data.frame( # PrivateCode = PrivateCode,
    VehicleJourneyCode = VehicleJourneyCode,
    ServiceRef = ServiceRef,
    LineRef = LineRef,
    JourneyPatternRef = JourneyPatternRef,
    DepartureTime = DepartureTime,
    # days = days,
    BankHolidaysOperate = BankHolidaysOperate,
    BankHolidaysNoOperate = BankHolidaysNoOperate,
    stringsAsFactors = FALSE
  )

  OperatingProfile <- xml2::xml_find_all(vehiclejourneys, ".//d1:OperatingProfile")
  if (length(xml2::xml_length(OperatingProfile)) != nrow(vj_simple) | sum(xml2::xml_length(OperatingProfile)) == 0) {
    warning("Missing operating profiles in Vehicle Journeys")
    vj_simple$DaysOfWeek <- Services_main$DaysOfWeek
    vj_simple$HolidaysOnly <- NA
  } else {
    # Regular pattern
    RegularDayType <- xml2::xml_find_all(OperatingProfile, ".//d1:RegularDayType")
    DaysOfWeek <- xml2::xml_find_all(RegularDayType, ".//d1:DaysOfWeek")
    HolidaysOnly <- xml2::xml_find_all(RegularDayType, ".//d1:HolidaysOnly")
    RegularDayType_id <- xml2::xml_name(xml2::xml_children(RegularDayType))
    DaysOfWeek <- xml2::xml_name(xml2::xml_children(DaysOfWeek))
    HolidaysOnly <- xml2::xml_name(HolidaysOnly)

    RegularDayType_id <- data.frame(RegularDayType = RegularDayType_id, id = as.integer(stats::ave(RegularDayType_id, RegularDayType_id, FUN = seq_along)))
    RegularDayType_id$DaysOfWeek <- ifelse(RegularDayType_id$RegularDayType == "DaysOfWeek", DaysOfWeek[RegularDayType_id$id], NA)
    RegularDayType_id$HolidaysOnly <- ifelse(RegularDayType_id$RegularDayType == "HolidaysOnly", HolidaysOnly[RegularDayType_id$id], NA)

    vj_simple$DaysOfWeek <- RegularDayType_id$DaysOfWeek
    vj_simple$HolidaysOnly <- RegularDayType_id$HolidaysOnly
  }


  # ServicedOrganisations
  ServicedOrganisationDayType <- xml2::xml_find_all(vehiclejourneys, ".//d1:ServicedOrganisationDayType")

  if (any(xml2::xml_length(ServicedOrganisationDayType) > 0)) {
    ServicedOrganisationDayType <- import_ServicedOrganisationsDayType(ServicedOrganisationDayType)
    ServicedDaysOfOperation <- ServicedOrganisationDayType$ServicedDaysOfOperation
    ServicedDaysOfNonOperation <- ServicedOrganisationDayType$ServicedDaysOfNonOperation
    if (nrow(ServicedDaysOfOperation) > 0) {
      vj_simple <- dplyr::left_join(vj_simple, ServicedDaysOfOperation, by = "VehicleJourneyCode")
    } else {
      vj_simple$ServicedDaysOfOperation <- NA
    }
    if (nrow(ServicedDaysOfNonOperation) > 0) {
      vj_simple <- dplyr::left_join(vj_simple, ServicedDaysOfNonOperation, by = "VehicleJourneyCode")
    } else {
      vj_simple$ServicedDaysOfNonOperation <- NA
    }
  } else {
    vj_simple$ServicedDaysOfOperation <- NA
    vj_simple$ServicedDaysOfNonOperation <- NA
  }

  # Special Days
  SpecialDaysOperation <- xml2::xml_find_all(vehiclejourneys, ".//d1:SpecialDaysOperation")
  DaysOfNonOperation <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfNonOperation")
  DaysOfOperation <- xml2::xml_find_all(vehiclejourneys, ".//d1:DaysOfOperation")

  # Probelm days non oprationa re different for each vehicle jounrey
  # Need to get the right vehicle jounrey code for each day non operation

  if (any(xml2::xml_length(DaysOfNonOperation) > 0)) {
    DaysOfNonOperation_StartDate <- xml2::xml_text(xml2::xml_find_all(DaysOfNonOperation, ".//d1:StartDate"))
    DaysOfNonOperation_EndDate <- xml2::xml_text(xml2::xml_find_all(DaysOfNonOperation, ".//d1:EndDate"))
    DaysOfNonOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(DaysOfNonOperation)))
    DaysOfNonOperation_id <- import_simple(DaysOfNonOperation_id, ".//d1:VehicleJourneyCode")
    DaysOfNonOperation_id <- rep(DaysOfNonOperation_id, times = xml2::xml_length(DaysOfNonOperation))
    DaysOfNonOperation <- data.frame(
      VehicleJourneyCode = DaysOfNonOperation_id,
      StartDate = as.Date(DaysOfNonOperation_StartDate),
      EndDate = as.Date(DaysOfNonOperation_EndDate),
      stringsAsFactors = FALSE
    )
  } else {
    DaysOfNonOperation <- NA
  }

  if (any(xml2::xml_length(DaysOfOperation) > 0)) {
    DaysOfOperation <- import_DaysOfOperation(DaysOfOperation, Services_main = Services_main, cal = cal)
  } else {
    DaysOfOperation <- NA
  }


  result <- list(vj_simple, DaysOfOperation, DaysOfNonOperation, Notes)
  names(result) <- c("VehicleJourneys", "DaysOfOperation", "DaysOfNonOperation", "VJ_Notes")
  # JPS                   <- xml_children(journeypatternsections)
  # JPS_id                <- xml2::xml_text(xml2::xml_find_all(JPS, "@id"))
  # JPS_id                <- rep(JPS_id, times = xml2::xml_length(JPS, only_elements = FALSE))
  return(result)
}

#' Imports day of operation
#' to deal with date range and serviceorganisation working days
#' @param Notes desc
#' @noRd
import_DaysOfOperation <- function(DaysOfOperation, cal, Services_main) {
  result <- list()
  for (i in seq(1, length(xml2::xml_length(DaysOfOperation)))) {
    chld <- DaysOfOperation[i]
    if (xml2::xml_length(xml2::xml_child(chld)) == 0) {
      # Text based rather than date based
      if (xml2::xml_name(xml2::xml_child(chld)) == "AllBankHolidays") {
        DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(chld)))
        DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
        cal2 <- cal[cal$date >= Services_main$StartDate, ]
        cal2 <- cal2[cal2$date >= Services_main$EndDate, ]

        res <- data.frame(
          VehicleJourneyCode = DaysOfOperation_id,
          StartDate = cal2$date,
          EndDate = cal2$date,
          ServicedOrganisationRef = NA,
          stringsAsFactors = FALSE
        )
      } else if (all(xml2::xml_name(xml2::xml_children(chld)) %in% c(unique(cal$name)))) {
        # Named Holidays We can match to
        DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(chld)))
        DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
        cal2 <- cal[cal$date >= Services_main$StartDate, ]
        cal2 <- cal2[cal2$date >= Services_main$EndDate, ]
        cal2 <- cal2[cal2$name %in% unique(xml2::xml_name(xml2::xml_children(chld))), ]
        res <- data.frame(
          VehicleJourneyCode = DaysOfOperation_id,
          StartDate = cal2$date,
          EndDate = cal2$date,
          ServicedOrganisationRef = NA,
          stringsAsFactors = FALSE
        )
      } else {
        stop("Unknown Days of Operation")
      }
    } else {
      DaysOfOperation_StartDate <- xml2::xml_text(xml2::xml_find_all(chld, ".//d1:StartDate"))
      DaysOfOperation_EndDate <- xml2::xml_text(xml2::xml_find_all(chld, ".//d1:EndDate"))
      DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(chld)))
      DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
      DaysOfOperation_ServicedOrganisationRef <- xml2::xml_text(xml2::xml_find_all(chld, ".//d1:ServicedOrganisationRef"))
      if (length(DaysOfOperation_StartDate) == 0) {
        DaysOfOperation_StartDate <- NA
      }
      if (length(DaysOfOperation_EndDate) == 0) {
        DaysOfOperation_EndDate <- NA
      }
      if (length(DaysOfOperation_ServicedOrganisationRef) == 0) {
        DaysOfOperation_ServicedOrganisationRef <- NA
      }
      res <- data.frame(
        VehicleJourneyCode = DaysOfOperation_id,
        StartDate = as.Date(DaysOfOperation_StartDate),
        EndDate = as.Date(DaysOfOperation_EndDate),
        ServicedOrganisationRef = DaysOfOperation_ServicedOrganisationRef,
        stringsAsFactors = FALSE
      )
    }

    result[[i]] <- res
  }
  result <- dplyr::bind_rows(result)

  return(result)
}



#' Imports when Multiple Values
#' Returns a dataframe with appopiate lookup id
#' @param Notes desc
#' @noRd
import_notes <- function(Notes) {
  parent <- xml2::xml_parent(Notes)

  VehicleJourneyCode <- import_simple(parent, ".//d1:VehicleJourneyCode")
  NoteCode <- import_simple(Notes, ".//d1:NoteCode")
  NoteText <- import_simple(Notes, ".//d1:NoteText")
  result <- data.frame(
    VehicleJourneyCode = VehicleJourneyCode,
    NoteCode = NoteCode,
    NoteText = NoteText
  )
  return(result)
}

#' Imports when Multiple Values
#' Returns a dataframe with appopiate lookup id
#' @param vehiclejourneys desc
#' @noRd
import_notes2 <- function(vehiclejourneys) {
  VehicleJourneyCode <- import_simple(vehiclejourneys, ".//d1:VehicleJourneyCode")
  result <- list()
  for (i in seq(1, xml2::xml_length(vehiclejourneys))) {
    # message(i)
    chld <- xml2::xml_child(vehiclejourneys, i)
    NoteCode <- import_simple(chld, ".//d1:NoteCode")
    NoteText <- import_simple(chld, ".//d1:NoteText")
    if (length(NoteCode) == 0) {
      NoteCode <- NA
    }
    if (length(NoteText) == 0) {
      NoteText <- NA
    }
    res <- data.frame(
      VehicleJourneyCode = VehicleJourneyCode[i],
      NoteCode = NoteCode,
      NoteText = NoteText,
      stringsAsFactors = FALSE
    )
    result[[i]] <- res
  }
  result <- dplyr::bind_rows(result)
  result <- result[!is.na(result$NoteCode), ]

  return(result)
}

#' Import ServicedOrganisations Internal
#' Imports ServicedOrganisations Internal Loop
#' @param ServicedOrganisations ServicedOrganisations object
#' @noRd
#'
import_ServicedOrganisations_internal <- function(ServicedOrganisations, full_import = FALSE) {
  nmchk <- unique(xml2::xml_name(xml2::xml_children(xml2::xml_children(ServicedOrganisations))))
  if (!all(nmchk %in% c("OrganisationCode", "Name", "WorkingDays", "Holidays"))) {
    stop("Unknown Structure in ServicedOrganisations")
  }
  OrganisationCode <- import_simple(ServicedOrganisations, ".//d1:OrganisationCode")

  WorkingDays <- xml2::xml_find_all(ServicedOrganisations, ".//d1:WorkingDays")
  WorkingDays.StartDate <- import_simple(WorkingDays, ".//d1:StartDate")
  WorkingDays.EndDate <- import_simple(WorkingDays, ".//d1:EndDate")

  Holidays <- xml2::xml_find_all(ServicedOrganisations, ".//d1:Holidays")
  Holidays.StartDate <- import_simple(Holidays, ".//d1:StartDate")
  Holidays.EndDate <- import_simple(Holidays, ".//d1:EndDate")
  Holidays.Description <- import_simple(Holidays, ".//d1:Description")

  rep_lengths_work <- sum(xml2::xml_length(WorkingDays))
  rep_lengths_holiday <- sum(xml2::xml_length(Holidays))

  if (rep_lengths_work > 0 & rep_lengths_holiday == 0) {
    rep_lengths <- rep_lengths_work
    Holidays.StartDate <- rep(NA, times = rep_lengths)
    Holidays.EndDate <- rep(NA, times = rep_lengths)
    Holidays.Description <- rep(NA, times = rep_lengths)
  } else if (rep_lengths_work == 0 & rep_lengths_holiday > 0) {
    rep_lengths <- rep_lengths_holiday
    WorkingDays.StartDate <- rep(NA, times = rep_lengths)
    WorkingDays.EndDate <- rep(NA, times = rep_lengths)
  } else {
    stop("Lengths of Holidays and working days do not match in ServicedOrganisations")
  }

  OrganisationCode <- rep(OrganisationCode, times = rep_lengths)


  result <- data.frame(
    OrganisationCode = OrganisationCode,
    WorkingDays.StartDate = as.Date(WorkingDays.StartDate),
    WorkingDays.EndDate = as.Date(WorkingDays.EndDate),
    Holidays.StartDate = as.Date(Holidays.StartDate),
    Holidays.EndDate = as.Date(Holidays.EndDate),
    Holidays.Description = Holidays.Description,
    stringsAsFactors = FALSE
  )

  if (full_import) {
    Name <- import_simple(ServicedOrganisations, ".//d1:Name")
    Name <- rep(Name, times = rep_lengths)
    result$Name <- Name
  }


  return(result)
}




#' Import ServicedOrganisations
#' Imports ServicedOrganisations
#' @param ServicedOrganisations ServicedOrganisations object
#' @noRd
#'
import_ServicedOrganisations <- function(ServicedOrganisations, full_import = FALSE) {
  nmchk <- unique(xml2::xml_name(xml2::xml_children(xml2::xml_children(ServicedOrganisations))))
  if (!all(nmchk %in% c("OrganisationCode", "Name", "WorkingDays", "Holidays"))) {
    stop("Unknown Structure in ServicedOrganisations")
  }
  result <- list()
  for (i in seq(1, xml2::xml_length(ServicedOrganisations))) {
    sub <- xml2::xml_child(ServicedOrganisations, i)
    OrganisationCode <- import_simple(sub, ".//d1:OrganisationCode")

    WorkingDays <- xml2::xml_find_all(sub, ".//d1:WorkingDays")
    WorkingDays.StartDate <- import_simple(WorkingDays, ".//d1:StartDate")
    WorkingDays.EndDate <- import_simple(WorkingDays, ".//d1:EndDate")

    Holidays <- xml2::xml_find_all(sub, ".//d1:Holidays")
    Holidays.StartDate <- import_simple(Holidays, ".//d1:StartDate")
    Holidays.EndDate <- import_simple(Holidays, ".//d1:EndDate")
    Holidays.Description <- import_simple(Holidays, ".//d1:Description")

    rep_lengths_work <- sum(xml2::xml_length(WorkingDays))
    rep_lengths_holiday <- sum(xml2::xml_length(Holidays))

    if (rep_lengths_work > 0 & rep_lengths_holiday == 0) {
      rep_lengths <- rep_lengths_work
      Holidays.StartDate <- rep(NA, times = rep_lengths)
      Holidays.EndDate <- rep(NA, times = rep_lengths)
      Holidays.Description <- rep(NA, times = rep_lengths)
    } else if (rep_lengths_work == 0 & rep_lengths_holiday > 0) {
      rep_lengths <- rep_lengths_holiday
      WorkingDays.StartDate <- rep(NA, times = rep_lengths)
      WorkingDays.EndDate <- rep(NA, times = rep_lengths)
    } else {
      stop("Lengths of Holidays and working days do not match in ServicedOrganisations")
    }

    OrganisationCode <- rep(OrganisationCode, times = rep_lengths)


    res <- data.frame(
      OrganisationCode = OrganisationCode,
      WorkingDays.StartDate = as.Date(WorkingDays.StartDate),
      WorkingDays.EndDate = as.Date(WorkingDays.EndDate),
      Holidays.StartDate = as.Date(Holidays.StartDate),
      Holidays.EndDate = as.Date(Holidays.EndDate),
      Holidays.Description = Holidays.Description,
      stringsAsFactors = FALSE
    )

    if (full_import) {
      Name <- import_simple(ServicedOrganisations, ".//d1:Name")
      Name <- rep(Name, times = rep_lengths)
      res$Name <- Name
    }
    result[[i]] <- res
  }
  result <- dplyr::bind_rows(result)


  return(result)
}

#' Import ServicedOrganisationsDay
#' Imports ServicedOrganisations within VehicleJounrney
#' @param ServicedOrganisations ServicedOrganisations object
#' @noRd
#'
import_ServicedOrganisationsDayType <- function(ServicedOrganisationDayType) {

  # ServicedOrganisationDayType <- xml2::xml_find_all(vehiclejourneys, ".//d1:ServicedOrganisationDayType")
  ServicedDaysOfNonOperation <- xml2::xml_find_all(ServicedOrganisationDayType, ".//d1:DaysOfNonOperation")
  DaysOfNonOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(ServicedDaysOfNonOperation)))
  DaysOfNonOperation_id <- import_simple(DaysOfNonOperation_id, ".//d1:VehicleJourneyCode")
  DaysOfNonOperation_id <- rep(DaysOfNonOperation_id, times = xml2::xml_length(ServicedDaysOfNonOperation))
  ServicedDaysOfNonOperation <- import_simple(ServicedDaysOfNonOperation, ".//d1:ServicedOrganisationRef")

  ServicedDaysOfNonOperation <- data.frame(
    VehicleJourneyCode = DaysOfNonOperation_id,
    ServicedDaysOfNonOperation = ServicedDaysOfNonOperation,
    stringsAsFactors = FALSE
  )

  ServicedDaysOfOperation <- xml2::xml_find_all(ServicedOrganisationDayType, ".//d1:DaysOperation")
  DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(ServicedDaysOfOperation)))
  DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
  DaysOfOperation_id <- rep(DaysOfOperation_id, times = xml2::xml_length(ServicedDaysOfOperation))
  ServicedDaysOfOperation <- import_simple(ServicedDaysOfOperation, ".//d1:ServicedOrganisationRef")

  ServicedDaysOfOperation <- data.frame(
    VehicleJourneyCode = DaysOfOperation_id,
    ServicedDaysOfOperation = ServicedDaysOfOperation,
    stringsAsFactors = FALSE
  )

  result <- list(ServicedDaysOfOperation, ServicedDaysOfNonOperation)
  names(result) <- c("ServicedDaysOfOperation", "ServicedDaysOfNonOperation")

  return(result)
}
