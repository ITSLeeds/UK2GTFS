# import_OperatingProfile <- function(OperatingProfile){
#   result <- list()
#   result_special <- list()
#   #for(i in seq(1, 10)){
#   for(i in seq(1, length(OperatingProfile))){
#     chld <- OperatingProfile[i]
#     VehicleJourneyCode <- xml2::xml_text(xml2::xml_child(xml2::xml_parent(chld), "d1:VehicleJourneyCode"))
#
#     # Top Level Sections
#     RegularDayType <- xml2::xml_child(chld, "d1:RegularDayType")
#     ServicedOrganisationDayType <- xml2::xml_child(chld, "d1:ServicedOrganisationDayType")
#     BankHolidayOperation <- xml2::xml_child(chld, "d1:BankHolidayOperation")
#     SpecialDaysOperation <- xml2::xml_child(chld, "d1:SpecialDaysOperation")
#
#     # Main Section #########################
#     # RegularDayType
#     if(xml2::xml_length(RegularDayType) > 0){
#       DaysOfWeek <- xml2::xml_child(RegularDayType, "d1:DaysOfWeek")
#       DaysOfWeek <- xml2::xml_name(xml2::xml_children(DaysOfWeek))
#
#       HolidaysOnly <- xml2::xml_child(RegularDayType, "d1:HolidaysOnly")
#       HolidaysOnly <- xml2::xml_name(HolidaysOnly)
#
#       # Clean NA
#       if(length(DaysOfWeek) == 0){
#         DaysOfWeek <- NA
#       }
#
#       if(length(HolidaysOnly) == 0){
#         HolidaysOnly <- NA
#       }
#
#     } else {
#       DaysOfWeek <- NA
#       HolidaysOnly <- NA
#     }
#
#     # ServicedOrganisationDayType
#     if(xml2::xml_length(ServicedOrganisationDayType) > 0){
#       #message(str(xml2::as_list(ServicedOrganisationDayType)))
#       ServicedDaysOfOperation <- xml2::xml_child(ServicedOrganisationDayType, "d1:DaysOfOperation")
#       ServicedDaysOfNonOperation <- xml2::xml_child(ServicedOrganisationDayType, "d1:DaysOfNonOperation")
#
#       if(any(xml2::xml_length(ServicedDaysOfOperation) > 0)){
#         ServicedDaysOfOperation <- xml2::xml_find_all(ServicedDaysOfOperation, ".//d1:ServicedOrganisationRef")
#         ServicedDaysOfOperation <- xml2::xml_text(ServicedDaysOfOperation)
#       } else {
#         ServicedDaysOfOperation <- NA
#       }
#
#       if(any(xml2::xml_length(ServicedDaysOfNonOperation) > 0)){
#         ServicedDaysOfNonOperation <- xml2::xml_find_all(ServicedDaysOfNonOperation, ".//d1:ServicedOrganisationRef")
#         ServicedDaysOfNonOperation <- xml2::xml_text(ServicedDaysOfNonOperation)
#       } else {
#         ServicedDaysOfNonOperation <- NA
#       }
#
#     } else {
#       ServicedDaysOfOperation <- NA
#       ServicedDaysOfNonOperation <- NA
#     }
#
#     # BankHolidayOperation
#     if(xml2::xml_length(BankHolidayOperation) > 0){
#       BHDaysOfNonOperation <- xml2::xml_child(BankHolidayOperation, "d1:DaysOfNonOperation")
#       BHDaysOfOperation    <- xml2::xml_child(BankHolidayOperation, "d1:DaysOfOperation")
#       # Should be text based e.g. "AllBankHolidays"
#       BHDaysOfNonOperation <- xml2::xml_name(xml2::xml_children(BHDaysOfNonOperation))
#       BHDaysOfOperation <- xml2::xml_name(xml2::xml_children(BHDaysOfOperation))
#
#       # Clean NA
#       if(length(BHDaysOfNonOperation) == 0){
#         BHDaysOfNonOperation <- NA
#       }
#
#       if(length(BHDaysOfOperation) == 0){
#         BHDaysOfOperation <- NA
#       }
#
#
#     } else {
#       BHDaysOfNonOperation <- NA
#       BHDaysOfOperation <- NA
#     }
#
#     # SpecialDaysOperation
#     if(xml2::xml_length(SpecialDaysOperation) > 0){
#       SDDaysOfNonOperation <- xml2::xml_child(SpecialDaysOperation, "d1:DaysOfNonOperation")
#       SDDaysOfOperation    <- xml2::xml_child(SpecialDaysOperation, "d1:DaysOfOperation")
#
#       if(xml2::xml_length(SDDaysOfNonOperation) > 0){
#         SDDaysOfNonOperation_start <- xml2::xml_find_all(SDDaysOfNonOperation, ".//d1:StartDate")
#         SDDaysOfNonOperation_start <- xml2::xml_text(SDDaysOfNonOperation_start)
#         SDDaysOfNonOperation_end <- xml2::xml_find_all(SDDaysOfNonOperation, ".//d1:EndDate")
#         SDDaysOfNonOperation_end <- xml2::xml_text(SDDaysOfNonOperation_end)
#       }else{
#         SDDaysOfNonOperation_start <- NA
#         SDDaysOfNonOperation_end <- NA
#       }
#
#
#       if(xml2::xml_length(SDDaysOfOperation) > 0){
#         SDDaysOfOperation_start <- xml2::xml_find_all(SDDaysOfOperation, ".//d1:StartDate")
#         SDDaysOfOperation_start <- xml2::xml_text(SDDaysOfOperation_start)
#         SDDaysOfOperation_end <- xml2::xml_find_all(SDDaysOfOperation, ".//d1:EndDate")
#         SDDaysOfOperation_end <- xml2::xml_text(SDDaysOfOperation_end)
#       }else{
#         SDDaysOfOperation_start <- NA
#         SDDaysOfOperation_end <- NA
#       }
#
#       # Check for when lenghts don't match
#       lns <- length(SDDaysOfNonOperation_start)
#       lne <- length(SDDaysOfNonOperation_end)
#       los <- length(SDDaysOfOperation_start)
#       loe <- length(SDDaysOfOperation_end)
#       laa <- c(lns, lne, los, loe)
#
#
#       if(length(unique(laa)) != 1){
#         if(lns != max(laa)){
#           SDDaysOfNonOperation_start <- c(SDDaysOfNonOperation_start, rep(NA, times = max(laa) - lns))
#         }
#
#         if(lne != max(laa)){
#           SDDaysOfNonOperation_end <- c(SDDaysOfNonOperation_end, rep(NA, times = max(laa) - lne))
#         }
#
#         if(lns != max(los)){
#           SDDaysOfOperation_start <- c(SDDaysOfOperation_start, rep(NA, times = max(laa) - los))
#         }
#
#         if(lns != max(loe)){
#           SDDaysOfOperation_end <- c(SDDaysOfOperation_end, rep(NA, times = max(laa) - loe))
#         }
#       }
#
#
#       ssdf <- data.frame(VehicleJourneyCode = VehicleJourneyCode,
#                          OperateStart = as.Date(SDDaysOfOperation_start),
#                          OperateEnd = as.Date(SDDaysOfOperation_end),
#                          NoOperateStart = as.Date(SDDaysOfNonOperation_start),
#                          NoOperateEnd = as.Date(SDDaysOfNonOperation_end),
#                          stringsAsFactors = FALSE)
#
#       result_special[[i]] <- ssdf
#       rm(SDDaysOfOperation_start, SDDaysOfOperation_end,
#          SDDaysOfNonOperation_start, SDDaysOfNonOperation_end,
#          ssdf)
#
#     } else {
#       result_special[[i]] <- NULL
#     }
#
#
#     # Build Results #######################
#     res <- data.frame(VehicleJourneyCode = VehicleJourneyCode,
#                       DaysOfWeek = paste(DaysOfWeek, collapse = " "),
#                       HolidaysOnly = paste(HolidaysOnly,  collapse = " "),
#                       BHDaysOfOperation = paste(BHDaysOfOperation,  collapse = " "),
#                       BHDaysOfNonOperation = paste(BHDaysOfNonOperation,  collapse = " "),
#                       ServicedDaysOfOperation = ServicedDaysOfOperation,
#                       ServicedDaysOfNonOperation = ServicedDaysOfNonOperation,
#                       stringsAsFactors = FALSE)
#     result[[i]] <- res
#     rm(DaysOfWeek, HolidaysOnly,
#        BHDaysOfOperation, BHDaysOfNonOperation)
#
#   }
#   result <- dplyr::bind_rows(result)
#   result_special <- dplyr::bind_rows(result_special)
#
#   result_final <- list(result, result_special)
#   names(result_final) <- c("OperatingProfile", "SpecialDays")
#   return(result_final)
# }

