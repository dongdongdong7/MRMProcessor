#' @title read_windowInfo
#' @description
#' Read windowInfo excel.
#'
#' @param windowInfo_path windowInfo_path.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' windowInfo <- read_windowInfo(windowInfo_path = "D:/fudan/Projects/2023/MRMProcessor/Progress/build_package/test_data_info/windowInfo_car.xlsx")
read_windowInfo <- function(windowInfo_path){
  windowInfo_path <- normalizePath(windowInfo_path)
  windowInfo <- openxlsx::read.xlsx(windowInfo_path, sheet = 1)
  if(any(duplicated(windowInfo$analyteName))){
    stop(paste0(paste0(windowInfo$analyteName[which(duplicated(windowInfo$analyteName))], collapse = ", "), " is duplicated"))
  }
  return(windowInfo)
}
#' @title read_sampleInfo
#' @description
#' Read sampleInfo excel.
#'
#' @param sampleInfo_path sampleInfo_path.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' sampleInfo <- read_sampleInfo("D:/fudan/Projects/2023/MRMProcessor/Progress/build_package/test_data_info/sampleInfo_car.xlsx")
read_sampleInfo <- function(sampleInfo_path){
  sampleInfo_path <- normalizePath(sampleInfo_path)
  sampleInfo <- openxlsx::read.xlsx(sampleInfo_path, sheet = 1)
  return(sampleInfo)
}
