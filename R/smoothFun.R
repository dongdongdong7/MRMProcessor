.smoothMean <- function(int, size = 3){
  if(size %% 2 == 0){
    stop("size should be a singular!")
  }
  half_size <- floor(size / 2)
  N <- length(int)
  special_idx <- c(seq_len(half_size), rev((N:(N - half_size + 1))))
  smooth_int <- sapply(1:N, function(i) {
    if(i %in% special_idx){
      return(int[i])
    }
    y <- sapply((i - half_size):(i + half_size), function(j){
      int[j]
    })
    smooth_int <- mean(y)
  })
  return(smooth_int)
}
.smoothSg <- function(int, p = 3, n = p + 3 - p%%2, m = 0, ts = 1){
  smooth_int <- signal::sgolayfilt(int, p = p, n = n, m = m, ts = ts)
  return(smooth_int)
}
#' @title get_smoothPara
#' @description
#' Get smooth parameters list.
#'
#' @param smooth Whether to perform smooth.
#' @param method Smooth method, sg or mean.
#' @param size SmoothMean size.
#' @param p sg p.
#' @param n sg n.
#' @param m sg m.
#' @param ts sg ts.
#'
#' @return A parameter list.
#' @export
#'
#' @examples
#' smoothPara <- get_smoothPara()
get_smoothPara <- function(smooth = TRUE, method = "mean", size = 3,
                           p = 3, n = p + 3 - p%%2, m = 0, ts = 1){
  if(method != "mean" & method != "sg") stop("method must be mean or sg!")
  if(size %% 2 == 0){
    stop("size should be a singular!")
  }
  return(list(smooth = smooth, method = method,
              size = size,
              p = p, n = n, m = m, ts = 1))
}
#' @title smoothFun
#' @description
#' smooth function.
#'
#'
#' @param int intensity vector.
#' @param smoothPara smoothPara
#'
#' @return intensity vector after smooth.
#' @export
#'
#' @examples
#' plot(smoothFun(data[1,1]@intensity), type = "l")
smoothFun <- function(int, smoothPara = get_smoothPara()){
  if(smoothPara$smooth){
    if(smoothPara$method == "mean"){
      int <- .smoothMean(int, size = smoothPara$size)
    }else if(smoothPara$method == "sg"){
      int <- .smoothSg(int, p = smoothPara$p, n = smoothPara$n, m = smoothPara$m, ts = smoothPara$ts)
    }else stop("Method is wrong!")
  }
  return(int)
}
