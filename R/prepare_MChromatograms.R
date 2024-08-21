#' @title prepare_MChromatograms
#' @description
#' Associate the MChromatograms object with the windowInfo.
#'
#' @param MChromatograms MChromatograms object.
#' @param windowInfo windowInfo.
#'
#' @return A new MChromatograms.
#' @export
#'
#' @examples
#' MChromatograms <- prepare_MChromatograms(MChromatograms = MChromatograms, windowInfo = windowInfo)
prepare_MChromatograms <- function(MChromatograms, windowInfo = windowInfo, unit = "min"){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  if(unit == "min") mag <- 60
  else mag <- 1
  chrs <- lapply(1:ncol, function(j) {
    lapply(1:nrow, function(i) {
      Chromatogram <- MChromatograms[i, j]
      chrInfo <- attributes(Chromatogram)$chrInfo
      windowName <- purrr::pluck(strsplit(regmatches(chrInfo$chromatogramId, regexpr("name=(.+)", chrInfo$chromatogramId, perl = TRUE)), "name=")[[1]], 2)
      attributes(Chromatogram)$windowName <- windowName
      windowInfo_tmp <- windowInfo[windowInfo$windowName == windowName, ]
      if(nrow(windowInfo_tmp) != 0){
        tmpList <- lapply(1:nrow(windowInfo_tmp), function(l) {
          Chromatogram_new <- Chromatogram
          attributes(Chromatogram_new)$analyteName <- windowInfo_tmp[l, ]$analyteName
          attributes(Chromatogram_new)$expectRt <- windowInfo_tmp[l, ]$expectRt * mag
          attributes(Chromatogram_new)$relatedIS <- windowInfo_tmp[l, ]$relatedIS
          attributes(Chromatogram_new)$relatedQual <- windowInfo_tmp[l, ]$relatedQual
          attributes(Chromatogram_new)$relatedCor <- windowInfo_tmp[l, ]$relatedCor
          attributes(Chromatogram_new)$analyteType <- windowInfo_tmp[l, ]$analyteType
          attributes(Chromatogram_new)$initialCon <- windowInfo_tmp[l, ]$initialCon
          Chromatogram_new
        })
      }else stop(paste0(windowName, ", windowName maybe wrong!"))
      tmpList
    })
  })
  chrs <- unlist(chrs)
  MSnbase::MChromatograms(chrs,
                          ncol = ncol)
}
.getRow4analyteType <- function(MChromatograms, analyteType){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  analyteTypeMat <- unlist(lapply(1:ncol, function(j) {
    sapply(1:nrow, function(i) {
      attributes(MChromatograms[i,j])$analyteType
    })
  }))
  analyteTypeMat <- matrix(analyteTypeMat, ncol = ncol)
  which(analyteTypeMat[, 1] %in% analyteType)
}

