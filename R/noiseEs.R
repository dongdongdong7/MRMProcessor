.prepare_noiseEs <- function(int){
  yi <- sort(int)
  x <- seq(1, length(yi))
  xi <- sapply(unique(yi), function(tmp) {
    which(yi == tmp)[1]
  })
  yi <- yi[xi]
  y <- pracma::pchip(xi, yi, x)
  return(y)
}
#' @title noiseEs
#' @description
#' Estimates the noise of a chromatogram, which can be used to find the apex of a chromatographic peak.
#'
#' @param int intensity vector.
#' @param prepare TRUE or FALSE.
#' @param mag mag of sd.
#'
#' @return noise.
#' @export
#'
#' @examples
#' noise <- noiseEs(data[1,1]@intensity)
#' plot(data[1,1]@intensity, type = "l")
#' lines(rep(noise, length(data[1,1]@intensity)))
noiseEs <- function(int, prepare = TRUE, mag = 3){
  intensity <- sort(int[int > 0])
  if(prepare) intensity <- .prepare_noiseEs(intensity)
  intLength <- length(intensity)
  if(intLength == 1) return(intensity)
  for(i in 2:intLength){
    int <- intensity[i + 1]
    int_vec <- intensity[1:i]
    noiEsti <- mean(int_vec) + mag * sd(int_vec)
    if(int > noiEsti) break
  }
  noise <- intensity[i]
  return(noise)
}

# This is the previous version, which counts backwards and for a few data points will be wrong.
.noiseEs <- function(int, prepare = FALSE, mag = 3){
  intensity <- sort(int[int > 0])
  if(prepare) intensity <- .prepare_noiseEs(intensity)
  intLength <- length(intensity)
  if(intLength == 1) return(intensity)
  for(i in (intLength - 1):1){
    if(i == 1) break
    int <- intensity[i + 1]
    int_vec <- intensity[i:1]
    noiEsti <- mean(int_vec) + mag * sd(int_vec)
    if(int < noiEsti) break
  }
  noise <- intensity[i + 1]
  return(noise)
}

# noiseEs_test1 <- function(int, prepare = TRUE, mag = 3){
#   intensity <- sort(int[int > 0])
#   if(prepare) intensity <- .prepare_noiseEs(intensity)
#   intLength <- length(intensity)
#   if(intLength == 1) return(intensity)
#   tmp <- sapply(2:intLength, function(i) {
#     int <- intensity[i + 1]
#     int_vec <- intensity[1:i]
#     noiEsti <- mean(int_vec) + mag * sd(int_vec)
#   })
#   intensity[which(intensity[c(-1, -2)] > tmp[-length(tmp)])[1] + 1]
# }
#
# a <- Sys.time()
# noiseEs_test1(MChromatograms[55,1]@intensity)
# Sys.time() - a
