# matchRoutes = function(i){
#   if(i != length_todo){
#     schedule.sub = schedule[c(i,i+1), ]
#     sub.rowID1 = schedule.sub$rowID[1]
#     sub.rowID2 = schedule.sub$rowID[2]
#     sub.UID = schedule.sub$`Train UID`[1]
#     stop_times.sub = stop_times[stop_times$rowID > sub.rowID1,]
#     rows.todo = stop_times.sub$rowID[stop_times.sub$rowID < sub.rowID2]
#   }else{
#     schedule.sub = schedule[i, ]
#     sub.rowID1 = schedule.sub$rowID[1]
#     sub.UID = schedule.sub$`Train UID`[1]
#     rows.todo = stop_times$rowID[stop_times$rowID > sub.rowID1]
#   }
#
#   res = data.frame(rowID = rows.todo, trip_id = sub.UID)
#   return(res)
#
#   #if(!silent){
#   #  if(i %% 1000 == 0){
#   #    message(paste0(Sys.time()," matched ",round(i/length_todo,1),"% of routes"))
#   #  }
#   #}
#   #End of loop
#   #rm(schedule.sub,sub.rowID1,sub.UID,res)
# }




summary(calendar$keep)
foo = calendar[!calendar$keep,]

calendar.p = calendar[calendar$`STP indicator` == "P",]
#days = substring(calendar$`Days Run`[1:2], seq(1, 7, 1), seq(1, 7, 1) )


names(calendar.p) = c("service_id","start_date", "end_date","STP indicator","rowID","monday","tuesday","wednesday",
                      "thursday","friday","saturday","sunday")



#Work Our which trips match which stops
#length_todo = nrow(schedule)
#length_stops = seq(from = 1, to = nrow(stop_times))
#trip_ids = list()
#microbenchmark(matchRoutes(1))
