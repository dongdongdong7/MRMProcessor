#' @export
MRMProcessor_GUI <- function(){
  appDir <- system.file("GUI", package = "MRMProcessor")
  if(appDir == ""){
    stop("Could not find GUI file, please check you install step!")
  }
  shiny::runApp(appDir, display.mode = "normal")
}
