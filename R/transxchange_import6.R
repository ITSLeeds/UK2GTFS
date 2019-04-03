#' Import a TransXchange XML file
#'
#' @param file character, path to an XML file e.g. "C:/data/file.xml"
#' @param export character, path to folder to save results, or NULL to return results
#' @param run_debug logical, if TRUE extra checks are performed, default FALSE
#' @param full_import logical, if false data no needed for GTFS is excluded
#'
#' @export
#' If export is NULL returns a list of data.frames else saves results to the `export` folder as a RDS file
#'
#' @details
#' This function imports the raw transXchange XML files and converts them to a R readable format.
#'

transxchange_import <- function(file, export = NULL, run_debug = FALSE, full_import = FALSE){
  if(run_debug){
    message(paste0(Sys.time()," doing file ",file))
  }

  xml = xml2::read_xml(file)
  if(!is.null(export)){
    if(!dir.exists(export)){
      message("Export directory does not exist")
      stop()
    }
  }
  ## StopPoints ##########################################
  StopPoints = xml2::xml_child(xml,"d1:StopPoints")
  StopPoints = import_stoppoints(StopPoints)

  ## RouteSections ##########################################
  if(full_import){
    RouteSections = xml2::xml_child(xml,"d1:RouteSections")
    RouteSections = xml2::as_list(RouteSections)

    rs_clean <- function(rs){
      rs_attr = attributes(rs)$id
      rs = rs[names(rs) == "RouteLink"]
      rs <- lapply(rs, function(x){tmp <- x$Distance
      ids <- attributes(x)$id
      if(is.null(tmp)){tmp <- NA}
      x$LinkID <- ids
      x$Distance <- tmp
      x <- x[c("From","To","Distance","Direction","LinkID")]
      return(x)})
      rs =  data.frame(matrix(unlist(rs), nrow=length(rs), byrow=T),stringsAsFactors=FALSE)
      names(rs) = c("From","To","Distance","Direction","LinkID")
      rs$SectionID = rs_attr
      return(rs)
    }
    RouteSections = lapply(RouteSections, rs_clean)
    RouteSections = dplyr::bind_rows(RouteSections)
    RouteSections[] <- lapply( RouteSections, factor)
  }else{
    RouteSections = NA
  }


  ## Routes ##########################################
  Routes <- xml2::xml_child(xml,"d1:Routes")
  Routes <- import_routes(Routes)

  ## JourneyPatternSections ##########################################
  JourneyPatternSections <- xml2::xml_child(xml,"d1:JourneyPatternSections")
  JourneyPatternSections <- import_journeypatternsections(JourneyPatternSections)

  ## Operators ##########################################
  Operators <- xml2::xml_child(xml,"d1:Operators")
  if(run_debug){
    if(xml2::xml_length(Operators) > 1){
      message("More than one Operators")
      stop()
    }
  }
  Operators <- xml2::xml_child(Operators,1)
  Operators <- xml2::as_list(Operators)
  Operators <- unlist(Operators)
  Operators <- as.data.frame(t(Operators))


  ## Services ##########################################
  Services <- xml2::xml_child(xml,"d1:Services")
  if(run_debug){
    if(xml2::xml_length(Services) > 1){
      stop("More than one service")
    }
  }
  Services <- import_services(Services)
  StandardService <- Services$StandardService
  Services_main <- Services$Services_main
  SpecialDaysOperation <- Services$SpecialDaysOperation
  rm(Services)

  ## VehicleJourneys ##########################################
  VehicleJourneys = xml2::xml_child(xml,7)
  VehicleJourneys = xml2::as_list(VehicleJourneys)

  vj_clean <- function(vj,run_debug){
    # break simple and complex parts
    vj_simple = vj[c("PrivateCode","VehicleJourneyCode","ServiceRef","LineRef",
                     "JourneyPatternRef","DepartureTime")]
    vj_simple = data.frame(t(unlist(vj_simple)), stringsAsFactors = F)
    vj_op = vj[["OperatingProfile"]]
    vjtls = vj[names(vj) == "VehicleJourneyTimingLink"]
    if(length(vjtls) < 1){
      #message("VehicleJourneyTimingLinks are missing")
      vjtls = NULL
    }else{
      vjtl_clean <- function(vjtl){
        vjtl_attr <- attributes(vjtl)$id
        vjtl_ref <- vjtl$JourneyPatternTimingLinkRef[[1]]
        vjtl_from <- vjtl$From
        if(length(vjtl_from) == 0){vjtl_from <- NA}else{message("data in VJTL from");stop()}
        vjtl_to <- vjtl$To
        if(length(vjtl_to) == 0){vjtl_to <- NA}else{message("data in VJTL to");stop()}
        if(!all(names(vjtl) %in% c("JourneyPatternTimingLinkRef", "From","To"))){message("extra data in VJTL");stop()}
        vjtl = c(vjtl_attr, vjtl_ref, vjtl_from, vjtl_to)
        names(vjtl) = c("id","JourneyPatternTimingLinkRef","From","To")
        return(vjtl)
      }
      vjtls <- lapply(vjtls,vjtl_clean)
      vjtls <- data.frame(t(as.data.frame(vjtls)))
      row.names(vjtls) <- seq(1,nrow(vjtls))
    }

    op_rdw = names(vj_op$RegularDayType$DaysOfWeek) # Regular Day of the Week
    op_rdho = names(vj_op$RegularDayType) # Holidays Only
    op_rdho = op_rdho[op_rdho != "DaysOfWeek"]
    op_dno = vj_op$SpecialDaysOperation$DaysOfNonOperation # Days of non operation
    op_do = vj_op$SpecialDaysOperation$DaysOfOperation # Day of operation
    if(!is.null(op_dno)){
      op_dno = data.frame(matrix(unlist(op_dno), nrow=length(op_dno), byrow=T),stringsAsFactors=FALSE)
      op_dno$VehicleJourneyCode = vj_simple$VehicleJourneyCode
      names(op_dno) = c("StartTime","EndTime","VehicleJourneyCode")
    }
    if(!is.null(op_do)){
      op_do = data.frame(matrix(unlist(op_do), nrow=length(op_do), byrow=T),stringsAsFactors=FALSE)
      op_do$VehicleJourneyCode = vj_simple$VehicleJourneyCode
      names(op_do) = c("StartTime","EndTime","VehicleJourneyCode")
    }
    op_bh_dno = names(vj_op$BankHolidayOperation$DaysOfNonOperation) # Bank Holidays non operation
    op_bh_do = names(vj_op$BankHolidayOperation$DaysOfOperation) # Bank Holidays operation

    vj_simple$days = paste0(c(op_rdw,op_rdho), collapse = " ")
    vj_simple$BankHolidaysOperate = paste0(op_bh_do, collapse = " ")
    vj_simple$BankHolidaysNoOperate = paste0(op_bh_dno, collapse = " ")


    #### Check disable in working code
    if(run_debug){
      chk = vj_op

      chk$RegularDayType$DaysOfWeek = NULL
      chk$RegularDayType$HolidaysOnly = NULL
      chk$SpecialDaysOperation$DaysOfNonOperation = NULL
      chk$SpecialDaysOperation$DaysOfOperation = NULL
      chk$BankHolidayOperation$DaysOfNonOperation = NULL
      chk$BankHolidayOperation$DaysOfOperation = NULL
      chk <- unlist(sapply(chk, names))
      if(length(chk) != 0){
        message("Unexpected Structure in Operating Profile")
        print(str(vj_op))
        stop()
      }

      if(!all(c(op_rdw,op_rdho) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday",
                                       "Saturday","Sunday","MondayToFriday","HolidaysOnly",
                                       "MondayToSunday","Weekend"))){
        message("Unexpected Week Structure")
        print(str(op_rdw))
        stop()
      }
      if(!is.null(op_dno)){
        if(ncol(op_dno) > 3){
          message("Unexpected Days of Non Operation Structure")
          print(str(op_dno))
          stop()
        }
      }
      if(!is.null(op_do)){
        if(ncol(op_do) > 3){
          message("Unexpected Days of Operation Structure")
          print(str(op_do))
          stop()
        }
      }

    }
    result <- list(vj_simple,op_dno,op_do, vjtls)
    return(result)
  }

  VehicleJourneys <- lapply(VehicleJourneys, vj_clean, run_debug = run_debug)
  VehicleJourneys_exclude <- lapply(VehicleJourneys,`[[`,2)
  VehicleJourneys_exclude <- dplyr::bind_rows(VehicleJourneys_exclude)
  VehicleJourneys_include <- lapply(VehicleJourneys,`[[`,3)
  VehicleJourneys_include <- dplyr::bind_rows(VehicleJourneys_include)

  if(full_import){
    VehicleJourneysTimingLinks <- lapply(VehicleJourneys,`[[`,4)
    VehicleJourneysTimingLinks <- dplyr::bind_rows(VehicleJourneysTimingLinks)
    VehicleJourneysTimingLinks[] <- lapply(VehicleJourneysTimingLinks, factor)
  }else{
    VehicleJourneysTimingLinks <- NA
  }

  VehicleJourneys <- lapply(VehicleJourneys,`[[`,1)
  VehicleJourneys <- dplyr::bind_rows(VehicleJourneys)

  VehicleJourneys[] <- lapply(VehicleJourneys, factor)
  VehicleJourneys_exclude[] <- lapply(VehicleJourneys_exclude, factor)
  VehicleJourneys_include[] <- lapply(VehicleJourneys_include, factor)


  ## Final Steps ##########################################

  finalres <- list(JourneyPatternSections, Operators, Routes,
                   RouteSections, Services_main, StandardService,
                   Services_NonOperation, StopPoints, VehicleJourneys,
                   VehicleJourneys_exclude,VehicleJourneys_include, VehicleJourneysTimingLinks)
  names(finalres) = c("JourneyPatternSections", "Operators", "Routes",
                      "RouteSections", "Services_main","StandardService",
                      "Services_NonOperation", "StopPoints", "VehicleJourneys",
                      "VehicleJourneys_exclude","VehicleJourneys_include", "VehicleJourneysTimingLinks")

  if(!is.null(export)){
    filename <- unlist(strsplit(file,"/"))
    filename <- filename[length(filename)]
    filename <- gsub(".xml","",filename)
    saveRDS(finalres,paste0(export,"/",filename,".Rds"))
    return(NULL)
  }else{
    return(finalres)
  }


}
