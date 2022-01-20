#' Get naptan
#'
#' Download the NaPTAN stop locations for more information on NaPTAN see
#' https://data.gov.uk/dataset/ff93ffc1-6656-47d8-9155-85ea0b8f2251/national-public-transport-access-nodes-naptan
#' @param url character, url to the csv format NaPTAN
#' @param naptan_extra data frame of missing stops default uses `naptan_missing`
#' @return data frame of stop locations
#' @details TransXchange does not store the location of bus stops, so this
#'   functions downloads them from the offical DfT source. NaPTAN has some
#'   missing bus stops which are added by UK2GTFS. See `naptan_missing`
#'
#'
#'
#' @export

get_naptan <- function(url = "https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=csv", naptan_extra = naptan_missing) {

  dir.create("temp_naptan")
  utils::download.file(url = url, destfile = "temp_naptan/Stops.csv", mode = "wb", quiet = TRUE)
  naptan <- readr::read_csv("temp_naptan/Stops.csv", progress = FALSE, show_col_types = FALSE)
  unlink("temp_naptan", recursive = TRUE)
  #file.remove("naptan.zip")

  # clean file
  naptan <- naptan[, c("ATCOCode", "NaptanCode", "CommonName", "Easting", "Northing")]
  names(naptan) <- c("stop_id", "stop_code", "stop_name", "Easting", "Northing")

  naptan <- sf::st_as_sf(naptan, coords = c("Easting", "Northing"), crs = 27700)
  naptan <- sf::st_transform(naptan, 4326)
  naptan <- cbind(sf::st_drop_geometry(naptan), sf::st_coordinates(naptan))
  names(naptan) <- c("stop_id", "stop_code", "stop_name", "stop_lon", "stop_lat")

  naptan$stop_lon <- format(round(naptan$stop_lon, 6), scientific = FALSE)
  naptan$stop_lat <- format(round(naptan$stop_lat, 6), scientific = FALSE)

  # Append alterative tags
  naptan_extra <- naptan_extra[!naptan_extra$stop_id %in% naptan$stop_id,]
  naptan <- rbind(naptan, naptan_extra)

  return(naptan)
}


# get_naptan <- function(url = "http://naptan.app.dft.gov.uk/datarequest/GTFS.ashx", naptan_extra = naptan_missing) {
#   utils::download.file(url = url, destfile = "naptan.zip", mode = "wb")
#   dir.create("temp_naptan")
#   utils::unzip("naptan.zip", exdir = "temp_naptan")
#   naptan <- utils::read.csv("temp_naptan/Stops.txt", stringsAsFactors = FALSE, sep = "\t")
#   unlink("temp_naptan", recursive = TRUE)
#   file.remove("naptan.zip")
#
#   # clean file
#   names(naptan) <- c("stop_id", "stop_code", "stop_name", "stop_lat", "stop_lon", "stop_url", "vehicle_type")
#   naptan <- naptan[, names(naptan_extra)]
#
#   # format
#   naptan$stop_lon <- format(round(naptan$stop_lon, 6), scientific = FALSE)
#   naptan$stop_lat <- format(round(naptan$stop_lat, 6), scientific = FALSE)
#
#   # Append extra data
#   naptan_extra <- naptan_extra[!naptan_extra$stop_id %in% naptan$stop_id, ]
#   naptan <- rbind(naptan, naptan_extra)
#
#   return(naptan)
# }
