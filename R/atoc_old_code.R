#' internal function for matching stop_times to the basic schdule
#'
#' @details
#' Takes in a row of the schdedule and then gets the next row (schedule must
#'    be sorted by rowID)
#'
#' @param schedule.rowID rowID field from schedule object
#' @param stop_times.rowID rowID field from stop_times object
#' @param ncores number of processes for parallel processing (default = 1)
#' @noRd
#'
# matchRoutes <- function(schedule.rowID, stop_times.rowID, ncores = 1) {
#   schedule_tmp <- matrix(c(
#     schedule.rowID,
#     schedule.rowID[2:length(schedule.rowID)],
#     max(schedule.rowID) + 99999
#   ), ncol = 2)
#
#   if (ncores == 1) {
#     matches <- lapply(1:nrow(schedule_tmp), function(x) {
#       stop_times.rowID[dplyr::between(
#         stop_times.rowID,
#         schedule_tmp[x, 1],
#         schedule_tmp[x, 2]
#       )]
#     })
#   } else {
#     CL <- parallel::makeCluster(ncores) # make clusert and set number of core
#     parallel::clusterExport(
#       cl = CL, varlist = c(
#         "stop_times.rowID",
#         "schedule_tmp"
#       ),
#       envir = environment()
#     )
#     parallel::clusterEvalQ(cl = CL, {
#       library(dplyr)
#     })
#     matches <- parallel::parLapply(
#       cl = CL, 1:nrow(schedule_tmp),
#       function(x) {
#         stop_times.rowID[dplyr::between(
#           stop_times.rowID,
#           schedule_tmp[x, 1],
#           schedule_tmp[x, 2]
#         )]
#       }
#     )
#     parallel::stopCluster(CL)
#   }
#
#   # names(matches) = schedule_tmp[1:10]
#   result <- data.frame(
#     stop_times.rowID = unlist(matches),
#     schedule.rowID = rep(schedule.rowID, times = lengths(matches))
#   )
#
#   return(result)
# }
