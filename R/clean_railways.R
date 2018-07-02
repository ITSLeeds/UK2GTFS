# Clean the rail map

# process

# 1) Doanload from Geofabric
# 2) Convert with osmconvert.exe
# 3) Read into QGIS and expoet as database
# 4) Export just rail tracks
# 5) Import inot ArGIS
# 6) Use "Merge Divided Roads" to reduce parallel tracks to single lines
# 7) use below code to futher simplify the map

library(sf)
library(lwgeom)

rail = st_read("D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways_clean_simplified34.shp")
#rail = rail[rail$railway %in% c("light_rail","rail","subway","tram"),]
#rail = rail[,c("OBJECTID")]
#rail$railway = as.character(rail$railway)
# rail = st_transform(rail, 27700)
# object.size(rail)
# rail = st_simplify(rail, dTolerance = 1)
# object.size(rail)
# rail = st_transform(rail, 4326)
#rail = st_make_valid(rail)
#rail = rail[st_geometry_type(rail) %in% c("LINESTRING","MULTILINESTRING"),]
#rail = rail[st_is_simple(rail),] # remove the self interecting lines (mostly sidings and miniture railways)
rail$id = 1:nrow(rail)
rail$Id = NULL
rail = st_cast(rail, "LINESTRING", do_split = T, group_or_split = T)
rail$railway = "rail"
st_write(rail,"D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways_simplified.geojson", delete_dsn = TRUE)




points = st_read("D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways_clean_simplified_points9.shp")
points.dup = points[duplicated(points$geometry),]
st_crs(points.dup) = 27700
#qtm(points.dup)
st_write(points.dup, "D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways_clean_simplified_points_dup.shp", delete_dsn = TRUE)

