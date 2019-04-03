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
