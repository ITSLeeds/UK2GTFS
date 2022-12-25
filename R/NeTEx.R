# #netex
#
# path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NeTEx/NeTEx samples/RBUS/RBUS_26_Inbound_Adult£1.30Sgl_aae41d08-15c5-4fef-bf58-e8188410605e_637558887515183445.xml"
#
#
# import_netex <- function(path){
#   xml <- xml2::read_xml(path)
#
#   dataObjects <- xml2::xml_child(xml, "d1:dataObjects")
#
#   dataObjects1 <- xml2::xml_child(dataObjects, 1)
#   dataObjects2 <- xml2::xml_child(dataObjects, 2)
#
#   dataObjects1_frames <- xml2::xml_child(dataObjects1, "d1:frames")
#   dataObjects1_frames <- xml2::xml_children(dataObjects1_frames)
#
#   dataObjects1_Services <- xml2::xml_child(dataObjects1_frames, "d1:ServiceFrame")
#   dataObjects1_Resources <- xml2::xml_child(dataObjects1_frames, "d1:ResourceFrame")
#   dataObjects1_Fares <- xml2::xml_find_all(dataObjects1_frames, "d1:FareFrame")
#
#   # Service
#   lines <- xml2::xml_find_all(dataObjects1_Services, "d1:lines") # Basic details about the line
#   stoppoints <- xml2::xml_find_all(dataObjects1_Services, "d1:scheduledStopPoints")
#   stoppoints <- netex_stoppoints(stoppoints)
#
#   # Fares - muliple fares per service?
#   fare_network_data <- dataObjects1_Fares[xml2::xml_attr(dataObjects1_Fares, "responsibilitySetRef") == "network_data"]
#   fare_network_tariffs <- dataObjects1_Fares[xml2::xml_attr(dataObjects1_Fares, "responsibilitySetRef") == "tariffs"]
#
#   fareID <- xml2::xml_attr(fare_network_data, "id")
#   fare_network_data <- xml2::xml_children(fare_network_data)
#   fareZones <- xml2::xml_find_all(fare_network_data, "d1:FareZone")
#
#   fareZones_list <- list()
#   #Loop over fareZones
#   for(j in seq_len(length(fareZones))){
#     fz <- fareZones[j]
#     sp <- xml2::xml_child(fz, "d1:members")
#     sp <- xml2::xml_children(sp)
#
#     StopPoint = xml2::xml_text(sp)
#     StopPointRef = xml2::xml_attr(sp, "ref")
#     StopPointVersion = xml2::xml_attr(sp, "version")
#
#     if(length(StopPoint) == 0){StopPoint <- NA_character_}
#     if(length(StopPointRef) == 0){StopPointRef <- NA_character_}
#     if(length(StopPointVersion) == 0){StopPointVersion <- NA_character_}
#
#
#     fzdf <- data.frame(FareZoneid = xml2::xml_attr(fz, "id"),
#                        FareZoneversion = xml2::xml_attr(fz, "version"),
#                        FareZoneName = xml2::xml_text(xml2::xml_child(fz,"d1:Name")),
#                        StopPoint = StopPoint,
#                        StopPointRef = StopPointRef,
#                        StopPointVersion = StopPointVersion
#                      )
#     fareZones_list[[j]] <- fzdf
#   }
#   fareZones <- data.table::rbindlist(fareZones_list)
#
#   fareZones$fareID <- fareID
#
#
#   # TODO: Tariffs
#   tariffs <- list()
#   for(i in seq_len(length(fare_network_tariffs))){
#     tr <- fare_network_tariffs[1]
#   }
#
#
#
# }
#
#
# # Imports netex toppoints
# netex_stoppoints <- function(stoppoints){
#   stoppoints <- xml2::xml_children(stoppoints)
#
#   imp_func <- function(x){
#
#     topoplace <- xml2::xml_find_all(x, "d1:TopographicPlaceView")
#     topoplace <- xml2::xml_children(topoplace)
#
#     res <- data.frame(
#       Name = xml2::xml_text(xml2::xml_child(x, "d1:Name")),
#       id = xml2::xml_attr(x, "id"),
#       TopographicPlaceRef = xml2::xml_attr(topoplace, "ref")[xml2::xml_name(topoplace) == "TopographicPlaceRef"],
#       TopoName = xml2::xml_text(topoplace[xml2::xml_name(topoplace) == "Name"])
#     )
#
#     return(res)
#   }
#
#   result <- lapply(stoppoints, imp_func)
#   result <- data.table::rbindlist(result)
#   return(result)
# }
