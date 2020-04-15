#' Download Test Files
#'
#' Download small examples of ATOC CIF and TransXchange files. These are mainly
#' meant for internal testing. But can also be used to check the package is
#' correctly installed.
#'
#' @param path path to folder
#' @param type If "atoc" then downloads ATOC CIF, else TransXchange
#' @export

dl_example_file <- function(path, type = "atoc"){
  url1 <- "https://github.com/ITSLeeds/UK2GTFS/releases/download/0.002/atoc.zip"
  url2 <- "https://github.com/ITSLeeds/UK2GTFS/releases/download/0.002/transxchange.zip"
  if(type == "atoc"){
    utils::download.file(url = url1,
                         destfile = file.path(path,"atoc.zip"))
  } else {
    utils::download.file(url = url2,
                         destfile = file.path(path,"transxchange.zip"))
  }
  return(TRUE)
}



