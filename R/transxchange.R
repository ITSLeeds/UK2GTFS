#' TransXchange to GTFS
#'
#' @details Convert transxchange files to GTFS
#'
#' @param path_in Path to zipped transxchange files
#' @param silent Logical, should progress be shown
#' @param ncores Numeric, When parallel processing how many cores to use
#' @param cal Calendar object from get_bank_holidays()
#' @param naptan Naptan stop locations from get_naptan()
#' @param scotland character, should Scottish bank holidays be used? Can be
#'   "auto" (defualt), "yes", "no". If "auto" and path_in ends with "S.zip"
#'   Scottish bank holidays will be used, otherwise England and Wales bank
#'   holidays are used.
#' @param try_mode Logical, if TRUE import and conversion are wrapped in try
#'   calls thus a failure on a single file will not cause the whole process to
#'   fail. Warning this could result in a GTFS file with missing routes.
#' @param force_merge Logical, passed to gtfs_merge(force), default FALSE
#' @return A GTFS named list
#' @details
#'
#' This is a meta fucntion which aids TransXchange to GTFS conversion. It simple
#' runs transxchange_import(), transxchange_export(), gtfs_merge(), gtfs_write()
#'
#' Progress Bars
#'
#' To minimise overall processing when using mulitple cores the fucntion works
#' from largest to smallest file.This can mean the progress bar sits a 0% for
#' quite some time, before starting to move rapidly.
#'
#'
#' @export


transxchange2gtfs <- function(path_in,
                              silent = TRUE,
                              ncores = 1,
                              cal = get_bank_holidays(),
                              naptan = get_naptan(),
                              scotland = "auto",
                              try_mode = TRUE,
                              force_merge = FALSE) {
  # Check inputs
  checkmate::assert_numeric(ncores)
  checkmate::assert_logical(silent)
  checkmate::assert_character(scotland)
  checkmate::assert_file_exists(path_in)
  checkmate::assert_logical(try_mode)

  if (ncores == 1) {
    message(paste0(Sys.time(), " This will take some time, make sure you use 'ncores' to enable multi-core processing"))
  }

  # Check calendar and naptan
  if(!nrow(cal) > 0){
    stop("Calendar is missing")
  }

  if(!nrow(naptan) > 0){
    stop("Naptan is missing")
  }

  # Are we in Scotland?
  if (scotland == "yes") {
    scotland <- TRUE
  } else if (scotland == "no") {
    scotland <- FALSE
  } else if (scotland == "auto") {
    # Decide where we are
    if (length(path_in) == 1) {
      loc <- substr(path_in, nchar(path_in) - 5, nchar(path_in))
      if (loc == "/S.zip") {
        scotland <- TRUE
        message("Using Scottish Bank Holidays")
      } else {
        scotland <- TRUE
      }
    } else {
      scotland <- FALSE
    }
  } else {
    stop("Unknown value for scotland, can be 'yes' 'no' or 'auto'")
  }

  if (length(path_in) > 1) {
    message("Parsing provided xml files")
    files <- path_in[substr(path_in, nchar(path_in) - 4 + 1, nchar(path_in)) == ".xml"]
  } else {
    dir.create(file.path(tempdir(), "txc"))
    message(paste0(Sys.time(), " Unzipping data to temp folder"))
    utils::unzip(path_in, exdir = file.path(tempdir(), "txc"))
    message(paste0(Sys.time(), " Unzipping complete"))

    files <- list.files(file.path(tempdir(), "txc"),
                        pattern = ".xml",
                        full.names = TRUE,
                        recursive = TRUE)

  }

  if(length(files) == 0){
    stop("No XML files found")
  } else {
    message(length(files), " xml files have been found")
  }


  files <- files[order(file.size(files), decreasing = TRUE)] # Large to small give optimum performance

  if (ncores == 1) {
    message(paste0(Sys.time(), " Importing TransXchange files, single core"))
    res_all <- pbapply::pblapply(files,
                                 transxchange_import_try,
                                 run_debug = TRUE,
                                 full_import = FALSE,
                                 try_mode = try_mode)
    res_all_message <- res_all[sapply(res_all, class) == "character"]
    res_all <- res_all[sapply(res_all, class) == "list"]
    if(length(res_all_message) > 0){
      message(" ")
      message("Failed to import files: ")
      res_all_message <- unlist(res_all_message)
      message(paste(res_all_message, collapse = ",  "))
    }
    message(paste0(Sys.time(), " Converting to GTFS, single core"))
    gtfs_all <- pbapply::pblapply(res_all,
                                  transxchange_export_try,
                                  run_debug = TRUE,
                                  cal = cal,
                                  naptan = naptan,
                                  scotland = scotland,
                                  try_mode = try_mode
    )
  } else {
    message(paste0(Sys.time(), " Importing TransXchange files, multicore"))

    pb <- utils::txtProgressBar(max = length(files), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress, preschedule = FALSE)
    cl <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cl)
    boot <- foreach::foreach(i = seq_len(length(files)), .options.snow = opts)
    res_all <- foreach::`%dopar%`(boot, {
      transxchange_import_try(files[i],
                              try_mode = try_mode)
    })
    parallel::stopCluster(cl)
    rm(cl, boot, opts, pb, progress)

    res_all_message <- res_all[sapply(res_all, class) == "character"]
    res_all <- res_all[sapply(res_all, class) == "list"]
    if(length(res_all_message) > 0){
      message(" ")
      message("Failed to import files: ")
      res_all_message <- unlist(res_all_message)
      message(paste(res_all_message, collapse = ",  "))
    } else {
      message(" ")
      message("All files imported")
    }

    message(" ")
    message(paste0(Sys.time(), " Converting to GTFS, multicore"))

    pb <- utils::txtProgressBar(min = 0, max = length(res_all), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress, preschedule = FALSE)
    cl <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cl)
    boot <- foreach::foreach(i = seq_len(length(res_all)), .options.snow = opts)
    gtfs_all <- foreach::`%dopar%`(boot, {
      transxchange_export_try(res_all[[i]],
                          cal = cal,
                          naptan = naptan,
                          scotland = scotland,
                          try_mode = try_mode)
      # setTxtProgressBar(pb, i)
    })

    parallel::stopCluster(cl)
    rm(cl, boot, opts, pb, progress)
  }

  unlink(file.path(tempdir(), "txc"), recursive = TRUE)

  gtfs_all_message <- gtfs_all[sapply(gtfs_all, class) == "character"]
  gtfs_all <- gtfs_all[sapply(gtfs_all, class) == "list"]
  if(length(gtfs_all_message) > 0){
    message(" ")
    message("Failed to convert files: ")
    gtfs_all_message <- unlist(gtfs_all_message)
    message(paste(gtfs_all_message, collapse = ",  "))
  } else {
    message(" ")
    message("All files converted")
  }


  message(" ")
  message(paste0(Sys.time(), " Merging GTFS objects"))


  gtfs_merged <- try(gtfs_merge(gtfs_all, force = force_merge))

  if (class(gtfs_merged) == "try-error") {
    message("Merging failed, returing unmerged GFTS object for analysis")
    return(gtfs_all)
  }
  return(gtfs_merged)
}
