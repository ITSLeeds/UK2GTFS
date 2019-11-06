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
#'
#' @details
#'
#' @export


transxchange2gtfs <- function(path_in,
                              path_out,
                              name = "gtfs",
                              silent = TRUE,
                              ncores = 1,
                              cal = get_bank_holidays(),
                              naptan = get_naptan()) {
  if (ncores == 1) {
    message(paste0(Sys.time(), " This will take some time, make sure you use 'ncores' to enable multi-core processing"))
  }
  # zips <- list.files(path_in, pattern = ".zip", full.names = TRUE)
  # if (length(zips) > 0) {
  #   if (!silent) {
  #     message(paste0(Sys.time(), " unzipping folders"))
  #   }
  #   dir.create(file.path(tempdir(), "txc"))
  #   foo <- pbapply::pblapply(zips, function(x) {
  #     utils::unzip(x, exdir = file.path(tempdir(), "txc"))
  #   })
  # } else {
  #   stop("No zip folders found in path_in")
  # }
  nrow(cal)
  nrow(naptan)


  message(paste0(Sys.time(), " Unzipping data to temp folder"))
  dir.create(file.path(tempdir(), "txc"))
  utils::unzip(path_in, exdir = file.path(tempdir(), "txc"))


  files <- list.files(file.path(tempdir(), "txc"), pattern = ".xml", full.names = TRUE)
  files = files[order(file.size(files))]

  # TO balance progress bars interleave files
  seq_mid <- floor(length(files) / 2)
  seq_up <- seq(1, seq_mid)
  seq_down <- seq(length(files), seq_mid + 1)
  files = files[c(rbind(seq_up, seq_down))]


  if (ncores == 1) {
    message(paste0(Sys.time(), " Importing TransXchange files"))
    res_all <- pbapply::pblapply(files, transxchange_import, run_debug = TRUE, full_import = FALSE)
    message(paste0(Sys.time(), " Converting to GTFS"))
    gtfs_all <- pbapply::pblapply(res_all, transxchange_export, run_debug = TRUE, cal = cal, naptan = naptan)
  } else {
    cl <- parallel::makeCluster(ncores)
    # parallel::clusterExport(
    #   cl = cl,
    #   varlist = c("files", "cal", "naptan"),
    #   envir = environment()
    # )
    parallel::clusterEvalQ(cl, {
      library(UK2GTFS)
    })
    pbapply::pboptions(use_lb = TRUE)
    message(paste0(Sys.time(), " Importing TransXchange files"))
    res_all <- pbapply::pblapply(files,
      transxchange_import,
      run_debug = TRUE,
      full_import = FALSE,
      cl = cl
    )
    message(paste0(Sys.time(), " Converting to GTFS"))
    gtfs_all <- pbapply::pblapply(res_all,
      transxchange_export,
      run_debug = TRUE,
      cal = cal,
      naptan = naptan,
      cl = cl
    )
    parallel::stopCluster(cl)
    rm(cl)
  }
  message(paste0(Sys.time(), " Merging GTFS objects"))
  gtfs_merged <- gtfs_merge(gtfs_all)

  message(paste0(Sys.time(), " Writing GTFS file"))
  write_gtfs(gtfs = gtfs_merged, folder = path_out, name = name)
  unlink(file.path(tempdir(), "txc"))
}
