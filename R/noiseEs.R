#' @title noiseEs
#' @description
#' Estimates the noise of a chromatogram, which can be used to find the apex of a chromatographic peak.
#'
#' @param int intensity vector.
#'
#' @return noise.
#' @export
#'
#' @examples
#' noise <- noiseEs(data[1,1]@intensity)
#' plot(data[1,1]@intensity, type = "l")
#' lines(rep(noise, length(data[1,1]@intensity)))
noiseEs <- function(int){
  intensity <- int
  intLength <- length(intensity)
  if(intLength == 1) return(intensity)
  for(i in (intLength - 1):1){
    if(i == 1) break
    int <- intensity[i + 1]
    int_vec <- intensity[i:1]
    noiEsti <- mean(int_vec) + 3 * sd(int_vec)
    if(int < noiEsti) break
  }
  noise <- intensity[i + 1]
  return(noise)
}
