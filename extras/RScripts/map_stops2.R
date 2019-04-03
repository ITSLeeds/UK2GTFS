library(dplyr)
library(sf)
library(tmap)
tmap_mode("view")

stops = st_read("data/stops.geojson")
stops$X = NULL
stops = as.data.frame(stops)
stops_fix = st_read("data/stops_fixed.shp")
st_crs(stops_fix) = 27700
stops_fix = st_transform(stops_fix, 4326)
stops_fix = as.data.frame(stops_fix)
stops_fix = stops_fix[,c("stop_code","geometry")]
railway = st_read("data/railways_clean_simplified36.shp")
st_crs(railway) = 27700

stops_join = left_join(stops, stops_fix, by = c("stop_id" = "stop_code"))


stops_join$geometry.y[6]

stops_join$geometry.final = ifelse(lengths(stops_join$geometry.y) == 0,stops_join$geometry.x,stops_join$geometry.y)

stops_join$geometry.final = st_sfc(stops_join$geometry.final, crs = 4326)
#stops_join$dist = st_distance(stops_join$geometry.x, stops_join$geometry.final, by_element = T)
#summary(stops_join$dist)
stops_join = stops_join[,c("stop_id","stop_code","stop_name","valid","geometry.final")]
names(stops_join) = c("stop_id","stop_code","stop_name","valid","geometry")
stops_join = st_sf(stops_join)
rm(stops,stops_fix)
railway = st_transform(railway, 4326)

st_write(stops_join,"data/tiplocs.geojson")
st_write(railway,"data/railway.geojson")

railway_buff1000 = st_buffer(st_transform(railway,27700), 1000)
railway_buff1000 = st_transform(railway_buff1000, 4326)

railway_buff10 = st_buffer(st_transform(railway,27700), 50)
railway_buff10 = st_transform(railway_buff10, 4326)

stops_near = stops_join[railway_buff10,]
stops_far = stops_join[!stops_join$stop_id %in% stops_near$stop_id,]
stops_far = stops_far[railway_buff1000,]

qtm(stops_far)# +
#  qtm(railway_buff)

st_write(stops_far,"data/tiplocs_far.geojson", delete_dsn = T)
