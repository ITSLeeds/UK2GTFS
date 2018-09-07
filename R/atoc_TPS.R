#' Import TPS File
#'
#' @details
#' Requires the internet and a transportapi.com API key.
#'
#' @param file Path to TPS File
#'
#' @export
#'
import_TPS = function(file){
  tps <- XML::xmlParse(file)
}
