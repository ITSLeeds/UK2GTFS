file = "C:/Users/Malcolm/Downloads/Traveline/EA/ea_20-1-A-y08-1.xml"


transxchange_import <- function(file){
  xml_data = XML::xmlToList(file)

  StopPoints = xml_data["StopPoints"]
  StopPoints = StopPoints[[1]]
  StopPoints <- data.frame(matrix(unlist(StopPoints), nrow=length(StopPoints), byrow=T),stringsAsFactors=FALSE)

  RouteSections = xml_data["RouteSections"]

  Routes = xml_data["Routes"]
  Routes = Routes[[1]]
  Routes <- data.frame(matrix(unlist(Routes), nrow=length(Routes), byrow=T),stringsAsFactors=FALSE)

  jps_clean = function(jps){
    nms = c("From.Activity","From.StopPointRef","From.TimingStatus","From..attrs.SequenceNumber",
            "To.Activity","To.StopPointRef","To.TimingStatus","To..attrs.SequenceNumber",
            "RouteLinkRef","RunTime",".attrs.id")
    jps = lapply(jps,unlist)
    jps = lapply(jps,function(x){x[nms]})
    jps = jps[names(jps) == "JourneyPatternTimingLink"]
    jps = data.frame(jps ,stringsAsFactors=FALSE)
    jps = t(jps)
    jps = jps[1:(nrow(jps)-1),]
    jps = as.data.frame(jps)
    row.names(jps) = seq(1,nrow(jps))
    return(jps)
  }

  JourneyPatternSections = xml_data["JourneyPatternSections"][[1]]
  JourneyPatternSections = lapply(JourneyPatternSections,jps_clean)

  Operators = xml_data["Operators"][[1]]
  Operators <- data.frame(matrix(unlist(Operators), nrow=length(Operators), byrow=T),stringsAsFactors=FALSE)

  Services = xml_data["Services"][[1]]
  #Services = Services[[1]]
  #foo = unlist(Services)
  #Services <- data.frame(matrix(unlist(Services), nrow=length(Services), byrow=T),stringsAsFactors=FALSE)

  VehicleJourneys = xml_data["VehicleJourneys"][[1]]

  vj_clean <- function(vj){
    # break simple and complex parts
    vj_simple = vj[c("PrivateCode","VehicleJourneyCode","ServiceRef","LineRef",
                   "JourneyPatternRef","DepartureTime")]
    vj_simple = unlist(vj_simple)
    vj_op = vj["OperatingProfile"]
    # this is complicated need to see differetn examples
  }

  foo = VehicleJourneys[[1]]
}
