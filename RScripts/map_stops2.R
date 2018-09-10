library(dplyr)
library(sf)

stops = st_read("data/stops.geojson")
stops$X = NULL
stops = as.data.frame(stops)
stops_fix = st_read("data/stops_fixed.shp")
st_crs(stops_fix) = 27700
stops_fix = st_transform(stops_fix, 4326)
stops_fix = as.data.frame(stops_fix)
stops_fix = stops_fix[,c("stop_code","geometry")]
railway = st_read("data/railways_clean_simplified36.shp")

stops_join = left_join(stops, stops_fix, by = c("stop_id" = "stop_code"))

stops_join$dist = as.numeric(st_distance(stops_join$geometry.x, stops_join$geometry.y, by_element = T))
stops_join$geometry.y[6]

stops_join$geometry.final = ifelse(lengths(stops_join$geometry.y) == 0,stops_join$geometry.x,stops_join$geometry.y)
