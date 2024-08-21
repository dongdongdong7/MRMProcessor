#' @title rtCorrection_IS
#' @description
#' Retention time correction for the internal standard and calculation of deltaRt.
#'
#' @param MChromatograms MChromatograms object.
#'
#' @return MChromatograms object.
#' @export
#'
#' @examples
#' rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
#' MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS, cols = 1:10, targetRt = NA)
#' MChromatograms_new <- rtCorrection_IS(MChromatograms = MChromatograms)
rtCorrection_IS <- function(MChromatograms){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  rows <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
  cols <- 1:ncol
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
    if(is.null(attributes(Chromatogram)$peak)) return(Chromatogram)
    deltaRt <- as.numeric(attributes(Chromatogram)$peak[[1]]["apex"] - attributes(Chromatogram)$expectRt)
    attributes(Chromatogram)$deltaRt <- deltaRt
    if(!is.null(attributes(Chromatogram)$peaksInfo)){
      peaks_idx <- lapply(attributes(Chromatogram)$peaksInfo, function(x) {
        apex_idx <- which(x["apex"] == attributes(Chromatogram)$rtime)
        start_idx <- which(x["start"] == attributes(Chromatogram)$rtime)
        end_idx <- which(x["end"] == attributes(Chromatogram)$rtime)
        c(apex_idx = apex_idx, start_idx = start_idx, end_idx = end_idx)
      })
    }
    if(!is.null(attributes(Chromatogram)$peak)){
      peak_idx <- lapply(attributes(Chromatogram)$peak, function(x) {
        apex_idx <- which(x["apex"] == attributes(Chromatogram)$rtime)
        start_idx <- which(x["start"] == attributes(Chromatogram)$rtime)
        end_idx <- which(x["end"] == attributes(Chromatogram)$rtime)
        c(apex_idx = apex_idx, start_idx = start_idx, end_idx = end_idx)
      })
    }
    attributes(Chromatogram)$rtime <- attributes(Chromatogram)$rtime - deltaRt
    if(!is.null(attributes(Chromatogram)$peaksInfo)){
      peaksInfo <- attributes(Chromatogram)$peaksInfo
      if(length(peaksInfo) != length(peaks_idx)) stop("peaksInfo length is different with peaks_idx")
      peaksInfo_new <- lapply(1:length(peaksInfo), function(k) {
        peaksInfo_tmp <- peaksInfo[[k]]
        peaks_idx_tmp <- peaks_idx[[k]]
        peaksInfo_tmp["apex"] <- attributes(Chromatogram)$rtime[peaks_idx_tmp["apex_idx"]]
        peaksInfo_tmp["start"] <- attributes(Chromatogram)$rtime[peaks_idx_tmp["start_idx"]]
        peaksInfo_tmp["end"] <- attributes(Chromatogram)$rtime[peaks_idx_tmp["end_idx"]]
        return(peaksInfo_tmp)
      })
      attributes(Chromatogram)$peaksInfo <- peaksInfo_new
    }
    if(!is.null(attributes(Chromatogram)$peak)){
      peak <- attributes(Chromatogram)$peak
      if(length(peak) != length(peak_idx)) stop("length(peak) is different with length(peak_idx)")
      peak_new <- lapply(1:length(peak), function(k) {
        peak_tmp <- peak[[k]]
        peak_idx_tmp <- peak_idx[[k]]
        peak_tmp["apex"] <- attributes(Chromatogram)$rtime[peak_idx_tmp["apex_idx"]]
        peak_tmp["start"] <- attributes(Chromatogram)$rtime[peak_idx_tmp["start_idx"]]
        peak_tmp["end"] <- attributes(Chromatogram)$rtime[peak_idx_tmp["end_idx"]]
        return(peak_tmp)
      })
      attributes(Chromatogram)$peak <- peak_new
    }
    return(Chromatogram)
  })
  chrs_all[chrs_idx] <- chrs
  MSnbase::MChromatograms(chrs_all,
                          ncol = ncol)
}

# rtCorrection_analyte <- function(MChromatograms){
#
# }
