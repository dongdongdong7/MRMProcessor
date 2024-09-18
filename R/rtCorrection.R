#' @title rtCorrection_IS
#' @description
#' Retention time correction for the internal standard and calculation of deltaRt.
#'
#' @param MChromatograms MChromatograms object.
#' @param rows rows.If rows is NA, rows is rows_IS.
#' @param cols cols.
#'
#' @return MChromatograms object.
#' @export
#'
#' @examples
#' rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
#' MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS, cols = 1:10, targetRt = NA)
#' MChromatograms_new <- rtCorrection_IS(MChromatograms = MChromatograms)
rtCorrection_IS <- function(MChromatograms, rows = NA, cols){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  if(any(is.na(rows))) rows <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
  else rows <- rows
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
    if(is.null(attributes(Chromatogram)$targetPeak)) return(Chromatogram)
    deltaRt <- as.numeric(attributes(Chromatogram)$targetPeak[[1]]["apex"] - attributes(Chromatogram)$expectRt)
    attributes(Chromatogram)$deltaRt <- deltaRt
    if(!is.null(attributes(Chromatogram)$peaksInfo)){
      peaks_idx <- lapply(attributes(Chromatogram)$peaksInfo, function(x) {
        apex_idx <- which(x["apex"] == attributes(Chromatogram)$rtime)
        start_idx <- which(x["start"] == attributes(Chromatogram)$rtime)
        end_idx <- which(x["end"] == attributes(Chromatogram)$rtime)
        c(apex_idx = apex_idx, start_idx = start_idx, end_idx = end_idx)
      })
    }
    if(!is.null(attributes(Chromatogram)$targetPeak)){
      peak_idx <- lapply(attributes(Chromatogram)$targetPeak, function(x) {
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
    if(!is.null(attributes(Chromatogram)$targetPeak)){
      peak <- attributes(Chromatogram)$targetPeak
      if(length(peak) != length(peak_idx)) stop("length(peak) is different with length(peak_idx)")
      peak_new <- lapply(1:length(peak), function(k) {
        peak_tmp <- peak[[k]]
        peak_idx_tmp <- peak_idx[[k]]
        peak_tmp["apex"] <- attributes(Chromatogram)$rtime[peak_idx_tmp["apex_idx"]]
        peak_tmp["start"] <- attributes(Chromatogram)$rtime[peak_idx_tmp["start_idx"]]
        peak_tmp["end"] <- attributes(Chromatogram)$rtime[peak_idx_tmp["end_idx"]]
        return(peak_tmp)
      })
      attributes(Chromatogram)$targetPeak <- peak_new
    }
    return(Chromatogram)
  })
  chrs_all[chrs_idx] <- chrs
  MSnbase::MChromatograms(chrs_all,
                          ncol = ncol)
}

#' @title rtCorrection_analyte
#' @description
#' Retention time correction for analyte. This step is preceded by rtCorrection_IS.
#'
#' @param MChromatograms MChromatograms.
#' @param rows rows.
#' @param cols cols.
#' @param thread thread.
#' @param deltaRt deltaRt.
#'
#' @return MChromatograms object.
#' @export
#'
#' @examples
#' MChromatograms_new <- rtCorrection_analyte(MChromatograms = MChromatograms_new, rows = NA, cols = cols_batch1)
rtCorrection_analyte <- function(MChromatograms, rows = NA, cols, thread = 4, deltaRt = NA){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  if(any(is.na(rows))) rows <- c(.getRow4analyteType(MChromatograms, analyteType = "Quant"), .getRow4analyteType(MChromatograms, analyteType = "Qual"))
  else rows <- rows
  chrs_all <- MChromatograms[,1:ncol , drop = TRUE]
  combinations <- expand.grid(rows, cols)
  colnames(combinations) <- c("i", "j")
  chrs_idx <- sapply(1:nrow(combinations), function(l) {
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    (j - 1) * nrow + i
  })
  if(nrow(combinations) < 100) thread <- 1 # if loop number is small, do not set too much thread number.
  pb <- utils::txtProgressBar(max = nrow(combinations), style = 3)
  loop <- function(l){
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    Chromatogram <- MChromatograms[i, j]
    if(!is.null(attributes(Chromatogram)$deltaRt)) return(Chromatogram)
    relatedIS <- attributes(Chromatogram)$relatedIS
    row_IS <- .getRow4analyteName(MChromatograms, analyteNameVec = relatedIS)
    if(length(row_IS) > 1) stop("analytyName cannot have duplicates!")
    Chromatogram_IS <- MChromatograms[row_IS, j]
    if(is.null(attributes(Chromatogram_IS)$deltaRt) & is.na(deltaRt)) return(Chromatogram)
    else{
      if(is.na(deltaRt)){
        deltaRt <- attributes(Chromatogram_IS)$deltaRt
        attributes(Chromatogram)$deltaRt <- deltaRt
      }else{
        attributes(Chromatogram)$deltaRt <- deltaRt
      }
    }
    if(!is.null(attributes(Chromatogram)$peaksInfo)){
      peaks_idx <- lapply(attributes(Chromatogram)$peaksInfo, function(x) {
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
    return(Chromatogram)
  }
  if(thread == 1){
    chrs <- lapply(1:nrow(combinations), function(l) {
      utils::setTxtProgressBar(pb, l)
      loop(l)
    })
  }else if(thread > 1){
    cl <- snow::makeCluster(thread)
    doSNOW::registerDoSNOW(cl)
    opts <- list(progress = function(n) utils::setTxtProgressBar(pb,
                                                                 n))
    chrs <- unlist(foreach::`%dopar%`(foreach::foreach(l = 1:nrow(combinations),
                                                       .export = c(".getRow4analyteName", "deltaRt"),
                                                       .packages = c("MSnbase"),
                                                       .options.snow = opts),
                                      {
                                        loop(l)
                                      }))
    snow::stopCluster(cl)
    gc()
  }
  chrs_all[chrs_idx] <- chrs
  MSnbase::MChromatograms(chrs_all,
                          ncol = ncol)
}
