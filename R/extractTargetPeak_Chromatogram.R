#' @title extractTargetPeak_Chromatogram
#' @description
#' The target peaks are extracted based on the expected retention time and stored in the peak attribute.
#'
#' @param Chromatogram Chromatogram object.
#' @param expectRt expectRt.
#' @param tolRt tolRt.
#'
#' @return A new Chromatogram.
#' @export
#'
#' @examples
#' Chromatogram_new <- extractTargetPeak_Chromatogram(MChromatograms[6,2], expectRt = 30, tolRt = 5)
#' plotChromatogram(Chromatogram_new, text.colour = "purple", targetPeak = FALSE)
#' plotChromatogram(Chromatogram_new, text.colour = "purple", targetPeak = TRUE)
extractTargetPeak_Chromatogram <- function(Chromatogram, expectRt, tolRt = 5){
  if(!is.null(attributes(Chromatogram)$peaksInfo)){
    apex_rt <- sapply(attributes(Chromatogram)$peaksInfo, function(x) {x["apex"]})
    target_idx <- which(which.min(abs(apex_rt - expectRt)) & abs(apex_rt - expectRt) < tolRt)
    if(length(target_idx) == 1){
      attributes(Chromatogram)$peak <- attributes(Chromatogram)$peaksInfo[target_idx]
      return(Chromatogram)
    }else if(length(target_idx) > 1) stop("target_idx > 1")
    else return(Chromatogram)
  }else{
    return(Chromatogram)
  }
}
