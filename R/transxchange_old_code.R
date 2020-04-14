#' Expan stop_times
#' ????
#' @param i desc
#' @param jps desc
#' @noRd
#'
# expand_stop_times <- function(i, jps) {
#   jps_sub <- jps[[i]]
#   trips_sub <- trips[trips$service_id == jps_sub$JourneyPatternID[1], ]
#
#   st_sub <- jps_sub[, c("To.StopPointRef", "To.Activity", "To.SequenceNumber",
#                         "JourneyPatternID", "To.WaitTime", "To.TimingStatus",
#                         "RunTime")]
#   names(st_sub) <- c("stop_id", "To.Activity", "stop_sequence", "service_id",
#                      "To.WaitTime", "timepoint", "RunTime")
#   st_top <- data.frame(
#     stop_id = jps_sub$From.StopPointRef[1],
#     To.Activity = jps_sub$From.Activity[1],
#     stop_sequence = "1",
#     service_id = jps_sub$JourneyPatternID[1],
#     To.WaitTime = 0,
#     timepoint = jps_sub$From.TimingStatus[1],
#     RunTime = 0,
#     stringsAsFactors = FALSE
#   )
#   st_sub <- rbind(st_top, st_sub)
#   st_sub$RunTime <- as.integer(st_sub$RunTime)
#   st_sub$To.WaitTime <- as.integer(st_sub$To.WaitTime)
#   st_sub$departure_time <- cumsum(st_sub$RunTime + st_sub$To.WaitTime)
#   st_sub$arrival_time <- st_sub$departure_time - st_sub$To.WaitTime
#   st_sub$pickup_type <- sapply(st_sub$To.Activity,
#                                clean_activity, type = "pickup")
#   st_sub$drop_off_type <- sapply(st_sub$To.Activity,
#                                  clean_activity, type = "drop_off")
#
#   n_stops <- nrow(st_sub)
#   n_trips <- nrow(trips_sub)
#   st_sub <- st_sub[rep(1:n_stops, times = n_trips), ]
#   st_sub$trip_id <- rep(trips_sub$trip_id, each = n_stops)
#   st_sub$DepartureTime <- lubridate::hms(rep(trips_sub$DepartureTime,
#                                              each = n_stops))
#
#   st_sub$arrival_time <- lubridate::seconds_to_period(lubridate::as.duration(
#     st_sub$arrival_time) + lubridate::as.duration(st_sub$DepartureTime))
#   st_sub$arrival_time <- sprintf("%02d:%02d:%02d",
#                                  st_sub$arrival_time@day * 24 +
#                                    st_sub$arrival_time@hour,
#                                  minute(st_sub$arrival_time),
#                                  second(st_sub$arrival_time))
#
#   st_sub$departure_time <- lubridate::seconds_to_period(lubridate::as.duration(
#     st_sub$departure_time) + lubridate::as.duration(st_sub$DepartureTime))
#   st_sub$departure_time <- sprintf("%02d:%02d:%02d", st_sub$departure_time@day *
#                                      24 + st_sub$departure_time@hour,
#                                    minute(st_sub$departure_time),
#                                    second(st_sub$departure_time))
#
#   st_sub$timepoint <- sapply(st_sub$timepoint, clean_timepoints)
#
#   st_sub <- st_sub[, c("trip_id", "arrival_time", "departure_time",
#                        "stop_id", "stop_sequence", "timepoint")]
#
#   return(st_sub)
# }



#' check duplicated holidays
#' ????
#' @param i desc
#' @noRd
#'
# check_duplicate_holidays <- function(i) {
#   cal_dat <- calendar_dates[i, ]
#   if (cal_dat$exception_type == 2) {
#     jpr <- calendar_dates$JourneyPatternRef[1]
#     hols <- calendar_dates$hols[1]
#     cal_sub <- calendar_dates[calendar_dates$JourneyPatternRef == jpr, ]
#     cal_sub <- cal_sub[cal_sub$hols == hols, ]
#     if (nrow(cal_sub) == 2) {
#       return(FALSE)
#     } else if (nrow(cal_sub) == 1) {
#       return(TRUE)
#     } else {
#       stop(paste0("Invalid number of rows ", i))
#     }
#   } else {
#     return(TRUE)
#   }
# }


#' break up holidays
#' ????
#' @param cal_data desc
#' @param cl desc
#' @noRd
# break_up_holidays <- function(cal_dat, cl) {
#   cal_dat <- cal_dat[cal_dat[[cl]] != "", ]
#   if (nrow(cal_dat) == 0) {
#     return(NULL)
#   } else {
#     cal_dat_holidays <- lapply(strsplit(cal_dat[[cl]], " "), function(x) {
#       x[x != ""]
#     })
#     cal_dat <- cal_dat[rep(1:nrow(cal_dat),
#                            times = lengths(cal_dat_holidays)), ]
#     cal_dat$hols <- unlist(cal_dat_holidays)
#     if (cl == "BankHolidaysOperate") {
#       cal_dat$exception_type <- 1L
#     } else {
#       cal_dat$exception_type <- 2L
#     }
#     cal_dat <- cal_dat[, c("JourneyPatternRef", "hols", "exception_type")]
#     return(cal_dat)
#   }
# }


#' Check stop time sequence
#' Check that stoptimes are in order
#' @param stop_times list of data frames
#' @noRd
# check_stop_times <- function(stop_times) {
#   stop_times <- split(stop_times, stop_times$trip_id)
#
#   st_int <- function(st) {
#     st$stop_sequence <- st$stop_sequence[order(st$stop_sequence)]
#     return(st)
#   }
#
#   stop_times <- lapply(stop_times, st_int)
#   stop_times <- dplyr::bind_rows(stop_times)
#   return(stop_times)
# }






#' Import FromTo
#' To work with missing cases in childnre names
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
# import_FromTo <- function(xml1, nm) {
#   res <- xml2::xml_text(xml2::xml_find_all(xml1, nm))
#   lth <- length(xml2::xml_length(xml1))
#   if (length(res) == lth) {
#     return(res)
#   } else {
#     # There are missing values
#     res <- list()
#     for (i in seq(1:lth)) {
#       sub <- xml1[i]
#       sub <- xml2::xml_attrs(sub)
#       sub <- unlist(sub)
#       sub <- sub["SequenceNumber"]
#       sub <- unname(sub)
#       if (length(sub) == 0) {
#         sub <- NA
#       }
#       res[[i]] <- sub
#     }
#     res <- unlist(res)
#     return(res)
#   }
# }



#' Import over nodeset using loop
#' Loops over a nodeset returing a value
#' @param xml1 XML object
#' @param nm name to find
#' @noRd
# import_loop <- function(xml1, nm) {
#   res <- list()
#   for (i in seq(1, length(xml1))) {
#     chld <- xml1[i]
#     chld <- xml2::xml_text(xml2::xml_child(chld, nm))
#     if (length(chld) == 0) {
#       chld <- NA
#     }
#     res[[i]] <- chld
#   }
#   res <- unlist(res)
#   return(res)
# }


#' Import routes
#' ????
#' @param routes routes
#' @noRd
# import_routes <- function(routes) {
#   Description <- import_simple(routes, ".//d1:Description")
#   RouteSectionRef <- import_simple(routes, ".//d1:RouteSectionRef")
#   PrivateCode <- import_simple(routes, ".//d1:PrivateCode")
#   if (length(PrivateCode) == 0) {
#     PrivateCode <- rep(NA, length(RouteSectionRef))
#   }
#
#   routes <- data.frame(
#     PrivateCode = PrivateCode,
#     Description = Description,
#     RouteSectionRef = RouteSectionRef
#   )
#   return(routes)
# }

#' Clean NA from sequence
#' @param x sequency of numbers
#'
#
# clean_sequence <- function(x) {
#   if (anyNA(x)) {
#     x <- as.integer(x)
#     lth <- length(x)
#     for (i in seq(1, lth)) {
#       val <- x[i]
#       if (is.na(val)) {
#         if (i == 1) {
#           # First in sequence
#           if (x[2] > 1) {
#             x[i] <- 1
#           } else {
#             stop("Can't clean NA from sequence")
#           }
#         } else if (i == lth) {
#           # Last value just add one
#           x[i] <- x[i - 1] + 1
#         } else {
#           # Middle Value
#           if (is.na(x[i + 1])) {
#             # Next value also NA
#             x[i] <- x[i - 1] + 1
#           } else {
#             if (x[i + 1] - x[i - 1] >= 2) {
#               x[i] <- x[i - 1] + 1
#             } else {
#               stop("Can't clean NA from sequence")
#             }
#           }
#         }
#       }
#     }
#     x <- as.character(x)
#   }
#   return(x)
# }





#' Imports when Multiple Values
#' Returns a dataframe with appopiate lookup id
#' @param Notes desc
#' @noRd
# import_notes <- function(Notes) {
#   parent <- xml2::xml_parent(Notes)
#
#   VehicleJourneyCode <- import_simple(parent, ".//d1:VehicleJourneyCode")
#   NoteCode <- import_simple(Notes, ".//d1:NoteCode")
#   NoteText <- import_simple(Notes, ".//d1:NoteText")
#   result <- data.frame(
#     VehicleJourneyCode = VehicleJourneyCode,
#     NoteCode = NoteCode,
#     NoteText = NoteText
#   )
#   return(result)
# }




#' Imports day of operation
#' to deal with date range and serviceorganisation working days
#' @param Notes desc
#' @noRd
# import_DaysOfOperation <- function(DaysOfOperation, cal, Services_main) {
#   result <- list()
#   for (i in seq(1, length(xml2::xml_length(DaysOfOperation)))) {
#     # message(i)
#     chld <- DaysOfOperation[i]
#     if (xml2::xml_length(xml2::xml_child(chld)) == 0) {
#       # Text based rather than date based
#       if (xml2::xml_name(xml2::xml_child(chld)) == "AllBankHolidays") {
#         DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(chld)))
#         DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
#         cal2 <- cal[cal$date >= Services_main$StartDate, ]
#         cal2 <- cal2[cal2$date <= Services_main$EndDate, ]
#
#         res <- data.frame(
#           VehicleJourneyCode = DaysOfOperation_id,
#           StartDate = cal2$date,
#           EndDate = cal2$date,
#           ServicedOrganisationRef = NA,
#           stringsAsFactors = FALSE
#         )
#       } else if (all(xml2::xml_name(xml2::xml_children(chld)) %in% c(unique(cal$name)))) {
#         # Named Holidays We can match to
#         DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(chld)))
#         DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
#         cal2 <- cal[cal$date >= Services_main$StartDate, ]
#         cal2 <- cal2[cal2$date <= Services_main$EndDate, ]
#         cal2 <- cal2[cal2$name %in% unique(xml2::xml_name(xml2::xml_children(chld))), ]
#         res <- data.frame(
#           VehicleJourneyCode = DaysOfOperation_id,
#           StartDate = cal2$date,
#           EndDate = cal2$date,
#           ServicedOrganisationRef = NA,
#           stringsAsFactors = FALSE
#         )
#       } else if (any(xml2::xml_name(xml2::xml_children(chld)) == "HolidayMondays")) {
#         DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(chld)))
#         DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
#         cal2 <- cal[cal$date >= Services_main$StartDate, ]
#         cal2 <- cal2[cal2$date <= Services_main$EndDate, ]
#         cal2 <- cal2[cal2$name %in% unique(xml2::xml_name(xml2::xml_children(chld))) |
#                        lubridate::wday(cal2$date, TRUE) == "Mon", ]
#
#         if (nrow(cal2) > 0) {
#           res <- data.frame(
#             VehicleJourneyCode = DaysOfOperation_id,
#             StartDate = cal2$date,
#             EndDate = cal2$date,
#             ServicedOrganisationRef = NA,
#             stringsAsFactors = FALSE
#           )
#         } else {
#           res <- NULL
#         }
#       } else {
#         stop("Unknown Days of Operation")
#       }
#     } else {
#       DaysOfOperation_StartDate <- xml2::xml_text(xml2::xml_find_all(chld, ".//d1:StartDate"))
#       DaysOfOperation_EndDate <- xml2::xml_text(xml2::xml_find_all(chld, ".//d1:EndDate"))
#       DaysOfOperation_id <- xml2::xml_parent(xml2::xml_parent(xml2::xml_parent(chld)))
#       DaysOfOperation_id <- import_simple(DaysOfOperation_id, ".//d1:VehicleJourneyCode")
#       DaysOfOperation_ServicedOrganisationRef <- xml2::xml_text(xml2::xml_find_all(chld, ".//d1:ServicedOrganisationRef"))
#       if (length(DaysOfOperation_StartDate) == 0) {
#         DaysOfOperation_StartDate <- NA
#       }
#       if (length(DaysOfOperation_EndDate) == 0) {
#         DaysOfOperation_EndDate <- NA
#       }
#       if (length(DaysOfOperation_ServicedOrganisationRef) == 0) {
#         DaysOfOperation_ServicedOrganisationRef <- NA
#       }
#       res <- data.frame(
#         VehicleJourneyCode = DaysOfOperation_id,
#         StartDate = as.Date(DaysOfOperation_StartDate),
#         EndDate = as.Date(DaysOfOperation_EndDate),
#         ServicedOrganisationRef = DaysOfOperation_ServicedOrganisationRef,
#         stringsAsFactors = FALSE
#       )
#     }
#
#     result[[i]] <- res
#   }
#   result <- dplyr::bind_rows(result)
#
#   return(result)
# }


