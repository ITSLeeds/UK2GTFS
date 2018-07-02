#Tweaked Version of stplanr function

sum_network_routes <- function(sln, start, end, sumvars, combinations = FALSE) {

  if (!is(sln, "SpatialLinesNetwork") & !is(sln, "sfNetwork")) {
    stop("sln is not a SpatialLinesNetwork or sfNetwork.")
  }
  if (missing(start) | missing(end)) {
    stop("start or end is missing")
  }
  if (length(start) != length(end) && combinations == FALSE) {
    stop("start and end not the same length.")
  }

  if (combinations == FALSE) {
    routesegs <- lapply(1:length(start), function(i) {
      unlist(igraph::get.shortest.paths(sln@g, start[i], end[i], output="epath")$epath)
    })

    if (is(sln, "sfNetwork")) {
      if (is(sln, "sfNetwork") & "sf" %in% (.packages()) == FALSE) {
        stop("Load the sf package, e.g. with\nlibrary(sf)")
      }
      routecoords <- mapply(function(routesegs, start) {
        linecoords <- sf::st_coordinates(sln@sl[routesegs,])
        linecoords <- lapply(1:max(linecoords[,'L1']), function(x){
          linecoords[which(linecoords[,'L1'] == x),]
        })
        join_spatiallines_coords_sf(linecoords, sln@g$x[start], sln@g$y[start])
      },
      routesegs, start, SIMPLIFY = FALSE)

    } else {
      routecoords <- mapply(function(routesegs, start) {
        join_spatiallines_coords(sln@sl[routesegs,],sln@g$x[start],sln@g$y[start])
      },
      routesegs, start, SIMPLIFY = FALSE)
    }

    routecoords <- lapply(1:length(start), function(i) {
      if(nrow(routecoords[[i]]) > 0){
        routecoords[[i]]
      } else {
        matrix(c(sln@g$x[start[i]], sln@g$y[start[i]], sln@g$x[end[i]], sln@g$y[end[i]]), byrow=TRUE, nrow=2)
      }
    })

  } else {
    message("Not Supported")
    stop()
  }

  if (is(sln, "sfNetwork")) {

    routedata <- setNames(data.frame(cbind(1:length(routesegs), do.call(rbind, lapply(routesegs, function(routesegs, sumvars) {
      matrix(
        sapply(1:length(sumvars),
               FUN=function(j) {
                 if(length(routesegs) == 0) { NA } else { sum(sln@sl[routesegs,][[sumvars[j]]]) }
               }
        ), nrow=1)
    }, sumvars)))), c('ID',paste0('sum_',sumvars)))
    routedata$pathfound <- ifelse(unlist(lapply(routesegs, function(x){length(x)})) == 0, FALSE, TRUE)

    sldf <- dplyr::bind_rows(
      lapply(
        1:length(routecoords),
        function(x){
          as.data.frame(routecoords[[x]]) %>%
            dplyr::mutate(linenum = x)
        })
    ) %>%
      sf::st_as_sf(
        coords = utils::head(colnames(.),-2),
        crs = sf::st_crs(sln@sl)$epsg
      )


      #%>%
      dplyr::group_by(.data$linenum) %>%
      dplyr::summarise() %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::bind_cols(routedata) %>%
      dplyr::select(-.data$linenum)

  } else {

    message("Not Supported")
    stop()

  }

  return(sldf)

}
