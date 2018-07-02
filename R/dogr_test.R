os_roads_bristol = os_roads_bristol
colnm <- "formOfWay"
wts <- c (0.1, 0.2, 0.8, 1)
names (wts) <- unique (os_roads_bristol [[colnm]])
graph <- weight_streetnet(os_roads_bristol, wt_profile = wts,  type_col = colnm, id_col = "identifier")
# nrow(os_roads_bristol)  #29 lines
# length(unique(net$component)) #29 discrete subgraphs
#
# devtools::install_github("ATFutures/dodgr", force = T)
#
# packageVersion("dodgr")

# new test

from = graph[,c("from_lat","from_lon")]
# from = stations[,c("stop_lat", "stop_lon")]
from = sample_n(from, 3)
from = as.matrix(from)

to = graph[,c("to_lat","to_lon")]
# to = stations[,c("stop_lat", "stop_lon")]
to = sample_n(to, 3)
to = as.matrix(to)

from = sample (graph$from_id, size = 3)
to = sample (graph$to_id, size = 3)

dp <- dodgr_paths (graph, from = from, to = to)
