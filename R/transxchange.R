#' TransXchange to GTFS
#'
#' @details
#' Convert transxchange files to GTFS
#'
#' @param path_in Path to zipped transxchange files
#' @param path_out Path to where GTFS files should be saved
#' @param name name that should be given to the gtfs file, without the .zip extension
#' @param silent Logical, should progress be shown
#' @param ncores Numeric, When parallel processing how many cores to use
#' @param cal Calendar object from get_bank_holidays()
#' @param naptan Naptan stop locations from get_naptan()
#' @param scotland character, should Scottish bank holidays be used?
#'     Can be "auto" (defualt), "yes", "no". If "auto" and path_in ends with "S.zip"
#'     Scottish bank holidays will be used, otherwise England and Wales bank holidays
#'     are used.
#'
#' @details
#'
#' This is a meta fucntion which aids TransXchange to GTFS conversion. It simple runs
#' transxchange_import(), transxchange_export(), gtfs_merge(), write_gtfs()
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
                              path_out,
                              name = "gtfs",
                              silent = TRUE,
                              ncores = 1,
                              cal = get_bank_holidays(),
                              naptan = get_naptan(),
                              scotland = "auto") {
  if (ncores == 1) {
    message(paste0(Sys.time(), " This will take some time, make sure you use 'ncores' to enable multi-core processing"))
  }

  # Are we in Scotland?
  if(scotland == "yes"){
    scotland <- TRUE
  } else if(scotland == "no"){
    scotland <- FALSE
  } else if(scotland == "auto"){
    # Decide where we are
    loc <- substr(path_in, nchar(path_in) - 5, nchar(path_in))
    if(loc == "/S.zip"){
      scotland <- TRUE
      warning("Using Scottish Bank Holidays")
    } else {
      scotland <- TRUE
    }
  } else{
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

    files <- list.files(file.path(tempdir(), "txc"), pattern = ".xml", full.names = TRUE)
  }

  message(length(files), " xml files have been found")

  files <- files[order(file.size(files), decreasing = TRUE)] # Large to small give optimum performance

  if (ncores == 1) {
    message(paste0(Sys.time(), " Importing TransXchange files, single core"))
    res_all <- pbapply::pblapply(files, transxchange_import, run_debug = TRUE, full_import = FALSE)
    message(paste0(Sys.time(), " Converting to GTFS, single core"))
    gtfs_all <- pbapply::pblapply(res_all, transxchange_export, run_debug = TRUE,
                                  cal = cal, naptan = naptan, scotland = scotland)
  } else {
    message(paste0(Sys.time(), " Importing TransXchange files, multicore"))

    pb <- utils::txtProgressBar(max = length(files), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    cl <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cl)
    boot <- foreach::foreach(i = files, .options.snow = opts)
    res_all <- foreach::`%dopar%`(boot, transxchange_import(i))
    parallel::stopCluster(cl)
    rm(cl, boot, opts, pb, progress)

    message(" ")
    message(paste0(Sys.time(), " Converting to GTFS, multicore"))

    pb <- utils::txtProgressBar(min = 0, max = length(res_all), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    cl <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cl)
    boot <- foreach::foreach(i = seq_len(length(res_all)), .options.snow = opts)
    gtfs_all <- foreach::`%dopar%`(boot, {
      transxchange_export(res_all[[i]], cal = cal, naptan = naptan, scotland = scotland)
      # setTxtProgressBar(pb, i)
    })

    parallel::stopCluster(cl)
    rm(cl, boot, opts, pb, progress)
  }
  message(" ")
  message(paste0(Sys.time(), " Merging GTFS objects"))


  gtfs_merged <- try(gtfs_merge(gtfs_all))

  if (class(gtfs_merged) == "try-error") {
    message("Merging failed, returing unmerged GFTS object for analysis")
    return(gtfs_all)
  }

  message(paste0(Sys.time(), " Writing GTFS file"))
  write_gtfs(gtfs = gtfs_merged, folder = path_out, name = name)
  unlink(file.path(tempdir(), "txc"), recursive = TRUE)
}
