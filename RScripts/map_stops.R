library(dplyr)

stops_all = stops
stops_missing = read.csv("data/missing_tioplocs.csv", stringsAsFactors = F)

stops_all = bind_rows(stop_times_all,stop_times_missing)
