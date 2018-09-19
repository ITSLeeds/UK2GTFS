#' Import TPS File
#'
#' @details
#' Requires the internet and a transportapi.com API key.
#'
#' @param file Path to TPS File
#'
#' @export
#'
file = "D:/Users/earmmor/OneDrive - University of Leeds/NetworkRail/Reference Data/TPS_Data/XML_p.xml"
#'
import_TPS = function(file){
  tps <- xml2::read_xml(file)
  tps <- xml2::as_list(tps)
  bar = tps$doc

  xmlfile <- XML::xmlTreeParse(file)
  topxml <- XML::xmlRoot(xmlfile)
  topxml <- XML::xmlSApply(topxml, function(x) XML::xmlSApply(x, XML::xmlValue))

  #data_df <- XML::xmlToDataFrame(file)

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
