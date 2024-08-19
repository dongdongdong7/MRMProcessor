#' @title peakPicking_Chromatogram
#' @description
#' Peak-picking on a Chromatogram object.
#'
#' @param Chromatogram A Chromatogram object.
#' @param noise noise.
#' @param smoothPara smoothPara.
#' @param baselinePara baselinePara.
#' @param peakPara peakPara.
#'
#' @return A new Chromatogram.
#' @export
#'
#' @examples
#' Chromatogram <- data[1,1]
#' Chromatogram@rtime <- Chromatogram@rtime * 60
#' Chromatogram <- peakPicking_Chromatogram(Chromatogram = Chromatogram)
peakPicking_Chromatogram <- function(Chromatogram, noise = NA,
                                     smoothPara = get_smoothPara(), baselinePara = get_baselinePara(),
                                     peakPara = get_peakPara()){
  int <- Chromatogram@intensity
  rt <- Chromatogram@rtime
  attributes(Chromatogram)$smoothPara <- smoothPara
  attributes(Chromatogram)$baselinePara <- baselinePara
  attributes(Chromatogram)$peakPara <- peakPara
  int <- smoothFun(int, smoothPara = smoothPara)
  Chromatogram@intensity <- int
  if(is.na(noise)) noise0 <- noiseEs(int = int, prepare = FALSE)
  else if(noise > 0) noise0 <- noise
  else stop("noise should > 0 !")
  attributes(Chromatogram)$noise <- noise0
  baseline <- baselineEs(int = int, rt = rt, baselinePara = baselinePara)
  attributes(Chromatogram)$baseline <- baseline
  peaksInfo <- peakPicking(int = int, rt = rt, noise = noise, smoothPara = get_smoothPara(smooth = FALSE), baselinePara = baselinePara, peakPara = peakPara)
  attributes(Chromatogram)$peaksInfo <- peaksInfo
  return(Chromatogram)
}
