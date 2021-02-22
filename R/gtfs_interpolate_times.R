#' Interpolate stop times
#'
#' Sometimes bus timetables do not give unique stop times to every stop. Instead
#' several stops in an row are given the same stop time. This function
#' interpolates the stop times so that each bust stop is given a unique arrival
#' and departure time.
#'
#'
#' @param gtfs named list of data.frames
#' @param ncores number of cores to use
#' @export
#'

gtfs_interpolate_times <- function(gtfs, ncores = 1){
  stop_times <- gtfs$stop_times

  if(class(stop_times$arrival_time) != "Period"){
    stop("arrival_time is not class Period")
  }

  if(class(stop_times$departure_time) != "Period"){
    stop("arrival_time is not class Period")
  }

  stop_times <- dplyr::group_by(stop_times, trip_id)
  stop_times <- dplyr::group_split(stop_times)

  if(ncores == 1){
    stop_times <- pbapply::pblapply(stop_times, stops_interpolate)
  } else {
    cl <- parallel::makeCluster(ncores)
    stop_times <- pbapply::pblapply(stop_times,
                                    stops_interpolate,
                                    cl = cl
    )
    parallel::stopCluster(cl)
    rm(cl)
  }
  stop_times <- dplyr::bind_rows(stop_times)

  gtfs$stop_times <- stop_times
  return(gtfs)

}


stops_interpolate <- function(x){
  # Check for duplicates times
  if(any(duplicated(x$arrival_time))){
    # Check in correct order
    x <- x[order(x$stop_sequence),]
    # Identify Break pooints
    x$arr_char <- as.character(x$arrival_time)
    x$dup_arr <- duplicated(x$arr_char)
    x$batch <- cumsum(!x$dup_arr)
    btchs <- as.data.frame(table(x$batch))
    btchs$Var1 <- as.numeric(as.character(btchs$Var1))
    #x$arrival_time2 <- x$arrival_time
    for(i in 1:nrow(btchs)){
      frq <- btchs$Freq[i]
      if(frq != 1){
        btch <- btchs$Var1[i]
        tstart <- x$arrival_time[x$batch == btch]
        tstart <- tstart[1]
        tend <- x$arrival_time[x$batch == (btch + 1)]
        tend <- tend[1]
        interval <- (seconds(tend - tstart) / (frq)) * c(0:(frq-1))
        newtimes <- tstart + interval
        x$arrival_time[x$batch == btch] <- newtimes
      }
    }
    chk <- x$departure_time < x$arrival_time
    x$departure_time[chk] <- x$arrival_time[chk]
  }
  x$dup_arr <- NULL
  x$batch <- NULL
  x$arr_char <- NULL
  return(x)
}
