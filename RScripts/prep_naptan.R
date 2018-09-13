# clean naptan for gtfs
library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

naptan = read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Routing/Naptan/NaPTANcsv/Stops.csv")
naptan = naptan[,c("ATCOCode","NaptanCode","CommonName","Latitude","Longitude","StopType")]

# corpus <- jsonlite::fromJSON("D:/Users/earmmor/OneDrive - University of Leeds/NetworkRail/Reference Data/CORPUSExtract.json/corpus-out.json")
# corpus <- corpus[[1]]
# corpus$TIPLOC[corpus$TIPLOC == ""] <- NA
# corpus$TIPLOC[corpus$TIPLOC == " "] <- NA
#
# foo <- dplyr::left_join(naptan, corpus, by = c("ATCOCode" = "NLC"))
#
# library(osmdata)
# bb = getbb("Great Britain")
# #q <- opq(bbox = c(50, -11, 61, 2)) %>%
# q <- opq(bbox = c(50, -1, 52, 2)) %>%
#   add_osm_feature(key = 'ref:tiploc')
# res = osmdata_sf(q = q)

railrefs = read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Routing/Naptan/NaPTANcsv/RailReferences.csv", stringsAsFactors = F)
railrefs = railrefs[,c("AtcoCode","TiplocCode","CrsCode","StationName","Easting","Northing")]
railrefs = st_as_sf(railrefs, coords = c("Easting","Northing"), crs = 27700)
railrefs = st_transform(railrefs, 4326)



# coords = as.data.frame(st_coordinates(railrefs))
# names(coords) = c("stop_lat","stop_lon")
railrefs = as.data.frame(railrefs)
# railrefs$geometry = NULL
# railrefs = cbind(railrefs,coords)
railrefs = railrefs[,c("TiplocCode","StationName","geometry")]

tiplocs = st_read("data/tiplocs.geojson", stringsAsFactors = F)
tiplocs = as.data.frame(tiplocs)

join = left_join(tiplocs, railrefs, by = c("stop_id" = "TiplocCode"))
join$geometry = ifelse(lengths(join$geometry.y) == 2, join$geometry.y, join$geometry.x)
join$stop_name = ifelse(is.na(join$StationName), join$stop_name, join$StationName)
join$valid = ifelse(is.na(join$StationName), 0,1)

join = join[,c("stop_id","stop_code","stop_name","valid","geometry")]
join = st_sf(join, crs = 4326)

st_write(join, "data/tiplocs.geojson", delete_dsn = T)

qtm(tiplocs, dots.col = "red") +
  qtm(railrefs)


###################

tiplocs = st_read("data/tiplocs.geojson")
railrefs_sub = railrefs[!railrefs$TiplocCode %in% tiplocs$stop_id,]
names(railrefs_sub) = c("AtcoCode","stop_id","stop_code","stop_name", "geometry")
railrefs_sub$valid = 1
railrefs_sub = railrefs_sub[,c("stop_id","stop_code","stop_name","valid","geometry")]

tiplocs2 = rbind(tiplocs,railrefs_sub)

st_write(tiplocs2, "data/tiplocs.geojson", delete_dsn = T)


#######
tiplocs = st_read("data/tiplocs.geojson")

open_tiplocs = read.csv("data/open_tiplocs.csv", stringsAsFactors = F)
open_tiplocs = open_tiplocs[!open_tiplocs$TIPLOC %in% tiplocs2$stop_id,]
open_tiplocs = st_as_sf(open_tiplocs, coords = c("EASTING", "NORTHING"), crs = 27700)
names(open_tiplocs) = c("stop_id","stop_name","geometry")
open_tiplocs$stop_code = NA
open_tiplocs$valid = NA
open_tiplocs = open_tiplocs[,c("stop_id","stop_code","stop_name","valid","geometry")]
open_tiplocs = st_transform(open_tiplocs, 4326)

tiplocs3 = rbind(tiplocs,open_tiplocs)

st_write(tiplocs3, "data/tiplocs.geojson", delete_dsn = T)
