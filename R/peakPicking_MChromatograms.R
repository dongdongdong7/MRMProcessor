#' @title peakPicking_MChromatograms
#' @description
#' Peak-picking on a MChromatograms object.
#' This function is used for the first peak picking of the MChromatograms object.
#'
#' @param MChromatograms MChromatograms object.
#' @param noise noise.
#' @param noiseMag noiseMag.
#' @param smoothPara smoothPara.
#' @param baselinePara baselinePara.
#' @param peakPara peakPara.
#' @param thread thread.
#' @param unit unit.
#'
#' @return A new MChromatograms object.
#' @export
#'
#' @examples
#' MChromatograms <- peakPicking_MChromatograms(MChromatograms = MChromatograms, thread = 4)
peakPicking_MChromatograms <- function(MChromatograms, noise = NA, noiseMag = 2,
                                       smoothPara = get_smoothPara(), baselinePara = get_baselinePara(),
                                       peakPara = get_peakPara(),
                                       thread = 1, unit = "min"){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  pData <- MChromatograms@phenoData@data
  fData <- MChromatograms@featureData@data
  chrs_all <- MChromatograms[,1:ncol , drop = TRUE]
  combinations <- expand.grid(1:nrow, 1:ncol)
  colnames(combinations) <- c("i", "j")
  pb <- utils::txtProgressBar(max = nrow(combinations), style = 3)
  loop <- function(l){
    #if(l == 20) browser()
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    Chromatogram <- MChromatograms[i, j]
    if(unit == "min") attributes(Chromatogram)$rtime <- attributes(Chromatogram)$rtime * 60
    Chromatogram <- peakPicking_Chromatogram(Chromatogram = Chromatogram, noise = noise, noiseMag = noiseMag,
                                             smoothPara = smoothPara, baselinePara = baselinePara,
                                             peakPara = peakPara)
    attributes(Chromatogram)$sample_name <- pData$file[j]
    attributes(Chromatogram)$chrInfo <- fData[i, ]
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
                                                       .export = c("peakPicking_Chromatogram"),
                                                       .packages = c("MSnbase"),
                                                       .options.snow = opts),
                                      {
                                        loop(l)
                                      }))
    snow::stopCluster(cl)
    gc()
  }
  MSnbase::MChromatograms(chrs, phenoData = MChromatograms@phenoData, featureData = MChromatograms@featureData,
                          ncol = ncol)
}

#' @title peakPicking_MChromatograms2
#' @description
#' Peak-picking on a MChromatograms object.
#' This function is used for the second peak picking of the MChromatograms object.
#'
#' @param MChromatograms MChromatograms object.
#' @param rows rows.
#' @param cols cols.
#' @param noise noise.
#' @param noiseMag noiseMag.
#' @param smoothPara smoothPara. Here smooth is FALSE (default), because smoothing has usually already been performed once.
#' @param baselinePara baselinePara.
#' @param peakPara peakPara.
#' @param thread thread.
#'
#' @return A new MChromatograms object.
#' @export
#'
#' @examples
#' MChromatograms <- peakPicking_MChromatograms(MChromatograms = MChromatograms, rows = 1:nrow(MChromatograms), cols = 1:ncol(MChromatograms), thread = 4)
peakPicking_MChromatograms2 <- function(MChromatograms, rows, cols, noise = NA, noiseMag = 2,
                                       smoothPara = get_smoothPara(), baselinePara = get_baselinePara(),
                                       peakPara = get_peakPara(),
                                       thread = 1){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  chrs_all <- MChromatograms[,1:ncol , drop = TRUE]
  combinations <- expand.grid(rows, cols)
  colnames(combinations) <- c("i", "j")
  chrs_idx <- sapply(1:nrow(combinations), function(l) {
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    (j - 1) * nrow + i
  })
  pb <- utils::txtProgressBar(max = nrow(combinations), style = 3)
  loop <- function(l){
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    Chromatogram <- MChromatograms[i, j]
    smoothPara_old <- attributes(Chromatogram)$smoothPara
    if(smoothPara$smooth) smoothPara_new <- smoothPara
    else smoothPara_new <- smoothPara_old
    Chromatogram <- peakPicking_Chromatogram(Chromatogram = Chromatogram, noise = noise, noiseMag = noiseMag,
                                             smoothPara = smoothPara, baselinePara = baselinePara,
                                             peakPara = peakPara)
    attributes(Chromatogram)$smoothPara <- smoothPara_new
    return(Chromatogram)
  }
  if((length(rows) * length(cols)) < 20) thread <- 1 # If the number of peakPicking objects is small, force the number of threads to be 1
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
                                                       .export = c("peakPicking_Chromatogram"),
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
