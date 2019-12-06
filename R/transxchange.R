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
                              naptan = get_naptan()) {
  if (ncores == 1) {
    message(paste0(Sys.time(), " This will take some time, make sure you use 'ncores' to enable multi-core processing"))
  }

  if(length(path_in) > 1){
    message("Parsing provided xml files")
    files <- path_in[substr(path_in, nchar(path_in)-4+1, nchar(path_in)) == ".xml"]
  } else {
    dir.create(file.path(tempdir(), "txc"))
    message(paste0(Sys.time(), " Unzipping data to temp folder"))
    utils::unzip(path_in, exdir = file.path(tempdir(), "txc"))
    message(paste0(Sys.time(), " Unzipping complete"))

    files <- list.files(file.path(tempdir(), "txc"), pattern = ".xml", full.names = TRUE)
  }

  message(length(files)," xml files have been found")

  #nrow(cal)
  #nrow(naptan)


  files <- files[order(file.size(files), decreasing = TRUE)] # Large to small give optimum performance

  # TO balance progress bars interleave files
  #seq_mid <- floor(length(files) / 2)
  #seq_up <- seq(1, seq_mid)
  #seq_down <- seq(length(files), seq_mid + 1)
  #files = files[c(rbind(seq_up, seq_down))]



  if (ncores == 1) {
    message(paste0(Sys.time(), " Importing TransXchange files, single core"))
    res_all <- pbapply::pblapply(files, transxchange_import, run_debug = TRUE, full_import = FALSE)
    message(paste0(Sys.time(), " Converting to GTFS, single core"))
    gtfs_all <- pbapply::pblapply(res_all, transxchange_export, run_debug = TRUE, cal = cal, naptan = naptan)
  } else {
    # cl <- parallel::makeCluster(ncores)
    # # parallel::clusterExport(
    # #   cl = cl,
    # #   varlist = c("files", "cal", "naptan"),
    # #   envir = environment()
    # # )
    # parallel::clusterEvalQ(cl, {
    #   library(UK2GTFS)
    # })
    # pbapply::pboptions(use_lb = FALSE)
    # res_all <- pbapply::pblapply(files,
    #                              transxchange_import,
    #                              run_debug = TRUE,
    #                              full_import = FALSE,
    #                              cl = cl
    # )

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
    # cl <- parallel::makeCluster(ncores)
    # # parallel::clusterExport(
    # #   cl = cl,
    # #   varlist = c("files", "cal", "naptan"),
    # #   envir = environment()
    # # )
    # parallel::clusterEvalQ(cl, {
    #   library(UK2GTFS)
    # })
    # gtfs_all <- pbapply::pblapply(res_all,
    #   transxchange_export,
    #   run_debug = TRUE,
    #   cal = cal,
    #   naptan = naptan,
    #   cl = cl
    # )
    # parallel::stopCluster(cl)
    pb <- utils::txtProgressBar(min = 0, max = length(res_all), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    cl <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cl)
    boot <- foreach::foreach(i = 1:length(res_all), .options.snow = opts)
    gtfs_all <- foreach::`%dopar%`(boot, {
        transxchange_export(res_all[[i]], cal = cal, naptan = naptan)
        #setTxtProgressBar(pb, i)
    })

    # gtfs_all <- foreach::foreach(i = 1:length(res_all), .options.snow = opts) foreach::`%dopar%` {
    #   transxchange_export(res_all[[i]], cal = cal, naptan = naptan)
    # }
    # boot <- foreach::foreach(i = 1:length(res_all) .options.snow = opts)
    # gtfs_all <- foreach::`%dopar%`(boot, transxchange_export(i))
    parallel::stopCluster(cl)
    rm(cl, boot, opts, pb, progress)


  }
  message(" ")
  message(paste0(Sys.time(), " Merging GTFS objects"))


  gtfs_merged <- try(gtfs_merge(gtfs_all))

  if(class(gtfs_merged) == "try-error"){
    message("Merging failed, returing unmerged GFTS object for analysis")
    return(gtfs_all)
  }

  message(paste0(Sys.time(), " Writing GTFS file"))
  write_gtfs(gtfs = gtfs_merged, folder = path_out, name = name)
  unlink(file.path(tempdir(), "txc"), recursive = TRUE)
}
