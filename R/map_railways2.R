# Download the raid network
#path_in = "D:/Users/earmmor/OneDrive - University of Leeds/Routing/ttis898.zip"
#devtools::install_github("ATFutures/dodgr")

library(sf)
library(dodgr)
library(RANN)
library(tmap)
library(dplyr)
library(lwgeom)
library(stplanr)
tmap_mode("view")

rail = st_read("D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways_clean_simplified35_split2.shp")
names(rail)= c("id","geometry")
rail$railway = "rail"

st_crs(rail) =  27700
rail = st_transform(rail, 4326)

# buffer out as we only want stops near the rail lines
# rail_buff = st_transform(rail, 27700)
# rail_buff = st_buffer(rail_buff, 200, nQuadSegs = 2)
# rail_buff = st_transform(rail_buff, 4326)

# Find the Stations
stations = read.csv("D:/Users/earmmor/OneDrive - University of Leeds/Routing/gtfs/stops.txt")
#stations = st_transform(stations, 27700)
# stations_coods = st_coordinates(stations)
# stations = cbind(stations,stations_coods)
# stations = as.data.frame(stations)
# stations = stations[,c("TIPLOC.Code","X","Y")]
# stations1 = stations[,c("X","Y")]
# rm(rail_buff)

# make a graph
wts = c(1)
names(wts) = as.character(unique(rail$railway))
rail = rail[,c("id","railway")]

graph <- weight_streetnet(rail, type_col = "railway", wt_profile = wts, id_col = "id")

# from = graph[,c("from_lat","from_lon")]
# # from = stations[,c("stop_lat", "stop_lon")]
# from = sample_n(from, 3)
# from = as.matrix(from)
#
# to = graph[,c("to_lat","to_lon")]
# # to = stations[,c("stop_lat", "stop_lon")]
# to = sample_n(to, 3)
# to = as.matrix(to)

# match stations to graph points
verts <- dodgr_vertices (graph)
verts = st_as_sf(verts, coords = c("x","y"))
st_crs(verts) = 4326
verts = verts[,"id"]
verts = st_transform(verts, 27700)
verts.coords = as.data.frame(st_coordinates(verts))
verts$x = verts.coords$X
verts$y = verts.coords$Y
verts = as.data.frame(verts)
verts = verts[,c("id","x","y")]


stations.bng = st_as_sf(stations, coords = c("stop_lon","stop_lat"))
st_crs(stations.bng) = 4326
stations.bng = st_transform(stations.bng, 27700)
stations.bng.coords = as.data.frame(st_coordinates(stations.bng))
stations.bng$x = stations.bng.coords$X
stations.bng$y = stations.bng.coords$Y
stations.bng = as.data.frame(stations.bng)
stations.bng = stations.bng[,c("stop_id","x","y")]

near = nn2(data = verts[,c("x","y")], query = stations.bng[,c("x","y")], k = 1)
near.dist = near[["nn.dists"]]
near.dist = near.dist[,1]
near.index = near[["nn.idx"]]
near.index = near.index[,1]
stations.bng$vert = near.index
stations.bng$dist = near.dist

stations.bng = st_as_sf(stations.bng, coords = c("x","y"))
st_crs(stations.bng) = 27700
#st_write(stations.bng, "D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/stops.shp")

qtm(stations.bng[stations.bng$dist > 500,])


from = as.character(stations.bng$vert[stations.bng$stop_id %in% c("LDS","KGX","BRI","EDB")])
to = as.character(stations.bng$vert[stations.bng$stop_id %in% c("MCV","AFK","NCL","EXC")])


dp <- dodgr_paths (graph, from = from, to = to)
paths = paths2sf(dp, graph)
qtm(paths, lines.lwd = 3, lines.col = "id")


stop()

#############################################################################################
# from <- graph$from_id[1:2]
# to <- graph$to_id[406:407]
#from <- matrix(stplanr::geo_code("green park station london"), ncol = 2)
#to <- matrix(stplanr::geo_code("victoria station london"), ncol = 2)
from <- graph$from_id[!duplicated(graph$way_id)]
to <- graph$from_id[!duplicated(graph$way_id)]
dp <- dodgr_paths (graph, from = from, to = to)

path1 <- verts [match (dp [[1]] [[2]], verts$id), ]
points <- verts [match (as.character(c(from,to)), verts$id), ]

plot(path1$x,path1$y)
points(points$x, points$y, col = "red", bg = "red", pch = 21)


paths2sf <- function(dp, graph){
  verts <- dodgr_vertices (graph)
  count = 0
  paths = list()
  for(i in 1:length(dp)){
    message(paste0(Sys.time()," ",i))
    dp.sub = dp[[i]]
    for(j in 1:length(dp.sub)){
      path <- verts[match (dp.sub[[j]], verts$id), ]
      if(nrow(path) > 0){
        count = count + 1
        path = matrix(c(path$x, path$y), ncol = 2)
        path = st_linestring(path)
        paths[[count]] = path
      }else{
        # do nothing
      }
    }
  }
  paths = st_sfc(paths)
  paths = data.frame(id = seq(1,length(paths)), geometry = paths)
  paths = st_sf(paths)
  st_crs(paths) = 4326
  return(paths)
}




#investigate why there are so many diconnecte graphss
table = as.data.frame(table(graph$component))

graph2sf = function(graph){

  intfun = function(i){
    ls <- st_linestring(rbind(c(graph$from_lon[i],graph$from_lat[i]),c(graph$to_lon[i],graph$to_lat[i])))
    return(ls)
  }
  geom = lapply(1:nrow(graph), intfun)
  geom = st_sfc(geom)


  graph = graph[,c("geom_num","edge_id","from_id","to_id","d","d_weighted","way_id","component")]
  graph$geometry = geom
  graph = st_sf(graph)
  st_crs(graph) = 4326
  return(graph)
}

graph.sf = graph2sf(graph)
st_write(graph.sf,"D:/Users/earmmor/OneDrive - University of Leeds/Routing/osm/railways_simplified_components.shp", delete_dsn = TRUE)
#plot(graph.sf, col = graph$component)

subgraph = dodgr_components(graph)

subgraphs = as.data.frame(graph.tram[,c("edge_id","component")])
subgraphs = subgraphs %>%
            group_by(component) %>%
            summarise(n = n())

for(i in 1:length(subgraphs)){
  graph_tmp = graph[graph$component == subgraphs$component[1],]
  graph_tmp_vert = dodgr_vertices(graph_tmp)
  graph_tmp_vert = st_as_sf(graph_tmp_vert, coords = c("x","y"))
  graph_tmp_vert$id = 1
  graph_tmp_vert = graph_tmp_vert %>%
                    dplyr::group_by(id) %>%
                    dplyr::summarize(m = mean(n)) %>%
                    st_cast("LINESTRING")
}

#foo = dodgr_paths(graph = graph, from = c(65308,19865), to = c(33560,82708))
#foo = dodgr_paths(graph = graph, from = c(32286,9868), to = c(82706,38457))
foo = dodgr_paths(graph = graph, from = c(38457,38432), to = c(38440,38432))
route = graph2sf(graph[graph$from_id %in% as.integer(foo[[1]][[1]]),])
qtm(route)

#match stations to the graph
verts = dodgr_vertices(graph)
pts <- match_pts_to_graph (dodgr_vertices(graph), stations1)
pts <- verts$id[pts]

stations$graphid = pts
# names of those vertices

# contract the graph
cnt = dodgr_contract_graph(graph, verts = pts)
cnt.g = cnt$graph

#routes
cnt.g = cnt.g[order(cnt.g$component),]

from <- c(leeds,kingscross)
to <- c(kingscross,leeds)
dp <- dodgr_paths(graph, from = from, to = to)
dp2 <- unlist(dp,recursive=F)
verts <- dodgr_vertices (graph)

res = list()
for(i in 1:length(dp2)){
  message(i)
  name = names(dp2)[i]
  if(length(dp2[[i]]) > 1){
    path1 <- verts [match (dp2[[i]], verts$id), ]
    path1$id = name
    path1 = st_as_sf(path1, coords = c("x","y"))
    path1.line = path1 %>% dplyr::group_by(id) %>% dplyr::summarize(m = mean(n)) %>% st_cast("LINESTRING")
    st_crs(path1.line) = 4326
    res[[i]] = path1.line
    rm(path1,path1.line,name)
  }else{
    message("no route")
  }

}

res = res[!sapply(res,is.null)]
res = dplyr::bind_rows(res)
res <- as.data.frame(res)
res$geometry <- st_sfc(res$geometry)
res <- st_sf(res)
st_crs(res) <- 4326


qtm(res[])

g.sample = dodgr_sample(graph, nverts = 1000)

qtm(path1.line, lines.lwd = 3)


################






#foo = match_pts_to_graph(dodgr_vertices(graph), stations1)




# Get of the verticies
vt = dodgr_vertices(graph)
vt1 = vt[,c("x","y")]
stations1 = stations[,c("X","Y")]

nearest = nn2(data = vt1, query = stations1, k = 1)
nearest.index = as.vector(nearest[[1]])
nearest.dist = as.vector(nearest[[2]])
hist(nearest.dist, breaks = 60)

vt.tokeep = nearest.index[nearest.dist < 0.01] # exclude the cases where the match has been at a vast distance
vt.tokeep = vt$id[vt.tokeep]



library(stplanr)
library(igraph)
sln = SpatialLinesNetwork(rail)
clus = cluster_infomap(sln@g)
length(membership(clus))
sln@sl
V(sln@g)$clus = membership(clus)

verts = igraph::as_data_frame(sln@g, "vertices")
verts$id = rownames(verts)
verts$nb = sln@nb

nn = sln@nb


dceomp = decompose.graph(sln@g)
sub1 = igraph::as_data_frame(dceomp[[1]], "vertices")
