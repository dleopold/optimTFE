#' Title
#'
#' @return
#' @export
#'
explore_solutions <- function(){
  # TODO Add check to see if package is installed
  pkg <- tryCatch(
    find.package("optimTFE.explorer"),
    error = function(e) return ()
  )
  if(is.null(pkg)){
    message("the optimTFE.explorer package must be installed")
    return(invisible())
  }
  optimTFE.explorer::run_app()
}
