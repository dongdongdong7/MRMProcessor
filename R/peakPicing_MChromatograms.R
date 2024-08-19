#' @title peakPicking_MChromatograms
#' @description
#' Peak-picking on a MChromatograms object.
#'
#' @param MChromatograms MChromatograms object.
#' @param noise noise.
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
peakPicking_MChromatograms <- function(MChromatograms, noise = NA,
                                       smoothPara = get_smoothPara(), baselinePara = get_baselinePara(),
                                       peakPara = get_peakPara(),
                                       thread = 1, unit = "min"){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  pb <- utils::txtProgressBar(max = ncol, style = 3)
  if(thread == 1){
    chrs <- unlist(lapply(1:ncol, function(j) {
      utils::setTxtProgressBar(pb, j)
      loop <- function(j){
        lapply(1:nrow, function(i) {
          Chromatogram <- MChromatograms[i, j]
          if(unit == "min") attributes(Chromatogram)$rtime <- attributes(Chromatogram)$rtime * 60
          Chromatogram <- peakPicking_Chromatogram(Chromatogram = Chromatogram, noise = noise,
                                                   smoothPara = smoothPara, baselinePara = baselinePara,
                                                   peakPara = peakPara)
        })
      }
      loop(j)
    }))
  }else if(thread > 1){
    cl <- snow::makeCluster(thread)
    doSNOW::registerDoSNOW(cl)
    opts <- list(progress = function(n) utils::setTxtProgressBar(pb,
                                                                 n))
    chrs <- unlist(foreach::`%dopar%`(foreach::foreach(j = 1:ncol,
                                                       .export = c("peakPicking_Chromatogram"),
                                                       .packages = c("MSnbase"),
                                                       .options.snow = opts),
                                      {
                                        loop <- function(j){
                                          lapply(1:nrow, function(i) {
                                            Chromatogram <- MChromatograms[i, j]
                                            if(unit == "min") attributes(Chromatogram)$rtime <- attributes(Chromatogram)$rtime * 60
                                            Chromatogram <- peakPicking_Chromatogram(Chromatogram = Chromatogram, noise = noise,
                                                                                     smoothPara = smoothPara, baselinePara = baselinePara, peakPara = peakPara)
                                          })
                                        }
                                        loop(j)
                                      }))
    snow::stopCluster(cl)
    gc()
  }
  MSnbase::MChromatograms(chrs, phenoData = MChromatograms@phenoData, featureData = MChromatograms@featureData,
                          ncol = ncol)
}
