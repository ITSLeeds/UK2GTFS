# make railway
library(sf)
library(tmap)

railway = st_read("./data/railway.geojson")
lightrail = st_read("D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/light_rail.shp")
nottinham = st_read("D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/nottingham_tram.shp")

nottinham = st_transform(nottinham, 4326)

railway$id = 1
railway$type = "rail"
lightrail$id = 1
nottinham$id = 1
lightrail$type = "light_rail"
nottinham$type = "light_rail"

railway = railway[,c("id","type","geometry")]
lightrail = lightrail[,c("id","type","geometry")]
nottinham = nottinham[,c("id","type","geometry")]

join = rbind(railway, lightrail)
join = rbind(join, lightrail)
join$id = 1:nrow(join)


st_write(join,"./data/railway.geojson", delete_dsn = T)

#######

split = st_read("./data/railways_split.shp")
split = split[,c("id","type")]
st_write(split,"./data/railway.geojson", delete_dsn = T)
