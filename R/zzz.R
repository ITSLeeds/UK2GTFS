# Run when package loads
# bear in mind this runs in every worker process as we start them and load this package into the worker's namespace
.onLoad <- function(libname, pkgname){

  tryCatch({
    if( TRUE == UK2GTFS_option_updateCachedDataOnLibaryLoad() )
    {
      update_data( timeout=10 )
    }
  }, error = function(err) {
    warning(Sys.time(), " Process id=", Sys.getpid(), " threw errors during package load while calling update_data() :", err)
  })

}


