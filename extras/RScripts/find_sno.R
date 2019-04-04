# Find services non operation
dir = "E:/OneDrive - University of Leeds/Routing/TransitExchangeData/data_20180515"
files = list.files(dir, full.names = T, recursive = T, pattern = ".xml")

find_sno <- function(file){
  xml = xml2::read_xml(file)
  Services = xml2::xml_child(xml,"d1:Services")
  Services <- xml2::xml_child(Services,1)
  Services <- xml2::as_list(Services)
  Services_NonOperation <- Services$OperatingProfile$SpecialDaysOperation
  if(length(Services_NonOperation) > 1){
    stop(file)
  }
}

foo <- pbapply::pblapply(files, find_sno)

find_sno2 <- function(file){
  xml = xml2::read_xml(file)
  VehicleJourneys = xml2::xml_child(xml,"d1:VehicleJourneys")
  VehicleJourneys = xml2::as_list(VehicleJourneys)
  Services_NonOperation <- VehicleJourneys$OperatingProfile$SpecialDaysOperation
  if(length(Services_NonOperation) > 1){
    stop(file)
  }
}

foo2 <- pbapply::pblapply(files, find_sno2)

find_sno3 <- function(i){
  foo <- res_batch[[i]]
  VehicleJourneys_exclude <- foo[["VehicleJourneys_exclude"]]
  VehicleJourneys_include <- foo[["VehicleJourneys_include"]]
  if(nrow(VehicleJourneys_exclude) > 0 & nrow(VehicleJourneys_include) > 0){
    stop(i)
  }else{
    message(paste0(i," ",nrow(VehicleJourneys_include)))
  }
}

foo2 <- pbapply::pblapply(1:length(res_batch), find_sno3)
