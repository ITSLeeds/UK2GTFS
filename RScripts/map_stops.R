library(dplyr)
library(sf)

stops_all = stops
stops_missing = read.csv("data/missing_tioplocs.csv", stringsAsFactors = F)

stops_all = bind_rows(stop_times_all,stop_times_missing)
stops_all = stops_all[!is.na(stops_all$stop_lat),]
stops_all = st_as_sf(stops_all, coords = c("stop_lon","stop_lat"), crs = 4326)
st_write(stops_all,"data/stops.geojson")
