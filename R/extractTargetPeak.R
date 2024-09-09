#' @title extractTargetPeak_Chromatogram
#' @description
#' The target peaks are extracted based on the expected retention time and stored in the peak attribute.
#'
#' @param Chromatogram Chromatogram object.
#' @param targetRt targetRt, The target retention time is different from the expected retention time.
#' The target retention time is manually specified and most of the time it is the same as the expected retention time,
#' but it is subject to batch effects, which need to be changed manually. If tagretRt is NA, targetRt = expectRt.
#' @param tolRt tolRt.
#'
#' @return A new Chromatogram.
#' @export
#'
#' @examples
#' Chromatogram_new <- extractTargetPeak_Chromatogram(MChromatograms[6,2], targetRt = 30, tolRt = 5)
#' plotChromatogram(Chromatogram_new, text.colour = "purple", targetPeak = FALSE)
#' plotChromatogram(Chromatogram_new, text.colour = "purple", targetPeak = TRUE)
extractTargetPeak_Chromatogram <- function(Chromatogram, targetRt = NA, tolRt = 5){
  if(is.na(targetRt)){
    if(!is.null(attributes(Chromatogram)$expectRt)) targetRt <- attributes(Chromatogram)$expectRt
    else stop("You should run prepare_MChromatograms function first!")
  }
  if(!is.null(attributes(Chromatogram)$peaksInfo)){
    apex_rt <- sapply(attributes(Chromatogram)$peaksInfo, function(x) {x["apex"]})
    target_idx <- which(seq.int(length(apex_rt)) == which.min(abs(apex_rt - targetRt)) & abs(apex_rt - targetRt) < tolRt)
    if(length(target_idx) == 1){
      attributes(Chromatogram)$targetPeak <- attributes(Chromatogram)$peaksInfo[target_idx]
      return(Chromatogram)
    }else if(length(target_idx) > 1) stop("target_idx > 1")
    else return(Chromatogram)
  }else{
    attributes(Chromatogram)$targetPeak <- NULL
    return(Chromatogram)
  }
}
#' @title extractTargetPeak_MChromatograms
#' @description
#' extractTargetPeak_MChromatograms.
#'
#'
#' @param MChromatograms MChromatograms object.
#' @param rows rows.
#' @param cols cols.
#' @param targetRt targetRt. If tagretRt is NA, targetRt = expectRt.
#' @param tolRt tolRt.
#'
#' @return MChromatograms object.
#' @export
#'
#' @examples
#' test <- extractTargetPeak_MChromatograms(MChromatograms, rows = 1:3, cols = 1:10, targetRt = NA)
#' rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
#' MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS, cols = 1:10, targetRt = NA)
extractTargetPeak_MChromatograms <- function(MChromatograms, rows, cols, targetRt = NA, tolRt = 5){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  chrs_all <- MChromatograms[,1:ncol , drop = TRUE]
  combinations <- expand.grid(rows, cols)
  colnames(combinations) <- c("i", "j")
  chrs_idx <- sapply(1:nrow(combinations), function(l) {
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    (j - 1) * nrow + i
  })
  chrs <- lapply(1:nrow(combinations), function(l) {
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    Chromatogram <- MChromatograms[i, j]
    Chromatogram <- extractTargetPeak_Chromatogram(Chromatogram, targetRt = targetRt, tolRt = tolRt)
    return(Chromatogram)
  })
  chrs_all[chrs_idx] <- chrs
  MSnbase::MChromatograms(chrs_all,
                          ncol = ncol)
}
