#' Try and Import a TransXchange XML file
#'
#' Wrapper around transxchange_import()
#'
#' @param file character, path to an XML file e.g. "C:/data/file.xml"
#' @param run_debug logical, if TRUE extra checks are performed, default FALSE
#' @param full_import logical, if false data no needed for GTFS is excluded
#' @noRd
transxchange_import_try <- function(file, run_debug, full_import){
  res <- try(transxchange_import(file, run_debug = run_debug, full_import = full_import))

  if("try-error" %in% class(res)){

    return(paste0(file," with error: ",res[1]))
  } else {
    return(res)
  }
}
