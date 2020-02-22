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

  if (length(WorkingDays.StartDate) != length(WorkingDays.EndDate)) {
    WorkingDays.StartDate <- import_vialoop(WorkingDays, ".//d1:StartDate")
    WorkingDays.EndDate <- import_vialoop(WorkingDays, ".//d1:EndDate")
    WorkingDays.StartDate <- ifelse(is.na(WorkingDays.StartDate),
      WorkingDays.EndDate,
      WorkingDays.StartDate
    )
    WorkingDays.EndDate <- ifelse(is.na(WorkingDays.EndDate),
      WorkingDays.StartDate,
      WorkingDays.EndDate
    )
  }

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

    if (length(WorkingDays.StartDate) != length(WorkingDays.EndDate)) {
      WorkingDays.StartDate <- import_vialoop(WorkingDays, ".//d1:StartDate")
      WorkingDays.EndDate <- import_vialoop(WorkingDays, ".//d1:EndDate")
      WorkingDays.StartDate <- ifelse(is.na(WorkingDays.StartDate),
        WorkingDays.EndDate,
        WorkingDays.StartDate
      )
      WorkingDays.EndDate <- ifelse(is.na(WorkingDays.EndDate),
        WorkingDays.StartDate,
        WorkingDays.EndDate
      )
    }


    Holidays <- xml2::xml_find_all(sub, ".//d1:Holidays")
    Holidays.StartDate <- import_simple(Holidays, ".//d1:StartDate")
    Holidays.EndDate <- import_simple(Holidays, ".//d1:EndDate")
    Holidays.Description <- import_simple(Holidays, ".//d1:Description")

    rep_lengths_work <- sum(xml2::xml_length(WorkingDays))
    rep_lengths_holiday <- sum(xml2::xml_length(Holidays))

    if (length(Holidays.Description) == 0) {
      Holidays.Description <- rep(NA, times = rep_lengths_holiday)
    }

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
      # stop("Lengths of Holidays and working days do not match in ServicedOrganisations")
      if (rep_lengths_work > rep_lengths_holiday) {
        rep_lengths <- rep_lengths_work
        rep_part <- rep_lengths_work - rep_lengths_holiday
        Holidays.StartDate <- c(Holidays.StartDate, rep(NA, times = rep_part))
        Holidays.EndDate <- c(Holidays.EndDate, rep(NA, times = rep_part))

        Holidays.Description <- c(Holidays.Description, rep(NA, times = rep_part))
      } else {
        rep_lengths <- rep_lengths_holiday
        rep_part <- rep_lengths_holiday - rep_lengths_work
        WorkingDays.StartDate <- c(WorkingDays.StartDate, rep(NA, times = rep_part))
        WorkingDays.EndDate <- c(WorkingDays.EndDate, rep(NA, times = rep_part))
      }
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
