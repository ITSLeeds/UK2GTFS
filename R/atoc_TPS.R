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
  tps <- xml2::read_xml(file)
  tps <- xml2::as_list(tps)
  bar = tps$doc

  xmlfile <- xmlTreeParse(file)
  topxml <- xmlRoot(xmlfile)
  topxml <- xmlSApply(topxml, function(x) xmlSApply(x, xmlValue))

  data_df <- xmlToDataFrame(url)

}


#' Import CORPUS File
#'
#' @details
#' Requires the internet and a transportapi.com API key.
#'
#' @param file Path to TPS File
#'
#' @export
#'
import_CORPUS = function(file){
  corpus <- jsonlite::fromJSON(file)
  corpus <- corpus[[1]]
  tps <- xml2::read_xml(file)
  foo <- xml2::as_list(tps)
  bar = tps$doc
}
