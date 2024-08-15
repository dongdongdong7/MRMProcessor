.inflect <- function(x, threshold = 1){
  up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
  down <-  sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  a    <- cbind(x,up,down)
  list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
}
.fineEstimation <- function(xs, ys, int, tol_m = 0.5){
  top_idx <- .inflect(ys, 1)$maxima
  bottom_idx <- .inflect(int, 1)$minima
  bottom_idx <- unique(c(1, bottom_idx, length(int)))
  bottom_idx <- setdiff(bottom_idx, top_idx)
  idx_seq_list <- lapply(1:(length(bottom_idx) - 1), function(x){
    idx_seq <- bottom_idx[x]:bottom_idx[x + 1]
  })
  idx_seq_logical <- rep(FALSE, length(idx_seq_list))
  for(i in 1:length(top_idx)){
    apex_idx <- top_idx[i]
    bottom_idx_tmp <- bottom_idx[which(int[bottom_idx] < ys[apex_idx])]
    idx_logical <- which(sapply(idx_seq_list, function(x){
      apex_idx %in% x
    }))
    if(length(idx_logical) == 0){
      next
    }
    if(!idx_seq_logical[idx_logical]){
      idx_seq_logical[idx_logical] <- TRUE
    }else{
      next
    }
    start_idx <- bottom_idx_tmp[which(apex_idx > bottom_idx_tmp)[length(which(apex_idx > bottom_idx_tmp))]]
    end_idx <- bottom_idx_tmp[which(apex_idx < bottom_idx_tmp)[1]]
    if(length(start_idx) == 0 | length(end_idx) == 0){
      next
    }
    if(is.na(start_idx) | is.na(end_idx)){
      next
    }
    xs_norm <- (xs - min(xs)) / (max(xs) - min(xs))
    ys_norm <- (ys - min(int)) / (max(int) - min(int))
    A <- c(xs_norm[apex_idx], ys_norm[apex_idx])
    B <- c(xs_norm[start_idx], ys_norm[start_idx])
    C <- c(xs_norm[end_idx], ys_norm[end_idx])
    Alength <- sqrt((B[1] - C[1])^2 + (B[2] - C[2])^2)
    Blength <- sqrt((A[1] - C[1])^2 + (A[2] - C[2])^2)
    Clength <- sqrt((B[1] - A[1])^2 + (B[2] - A[2])^2)
    COSA <- (Blength^2 + Clength^2 - Alength^2) / (2*Blength*Clength)
    COSB <- (Alength^2 + Clength^2 - Blength^2) / (2*Alength*Clength)
    COSC <- (Blength^2 + Alength^2 - Clength^2) / (2*Blength*Alength)
    angleA <- acos(COSA) * (180 / pi)
    angleB <- acos(COSB) * (180 / pi)
    angleC <- acos(COSC) * (180 / pi)
    if(angleB > tol_m | angleC > tol_m){
      a <- (ys[end_idx] - ys[start_idx]) / (xs[end_idx] - xs[start_idx])
      b <- (ys[start_idx] - a * xs[start_idx])
      ys[(start_idx + 1):(end_idx - 1)] <- a * xs[(start_idx + 1):(end_idx - 1)] + b
    }else{
      next
    }
  }
  return(ys)
}
.get_baselinePara <- function(threshold = 1, tol_m = 30, loops = 6){
  return(list(threshold = threshold, tol_m = tol_m, loops = loops))
}

#' @title baselineEs
#' @description
#' Estimate baseline.
#'
#' @param int intensity vector.
#' @param rt retention time vector.
#' @param baselinePara baselinePara.
#'
#' @return A basline vector.
#' @export
#'
#' @examples
#' plot(data[1,1]@intensity, type = "l")
#' baseline <- baselineEs(int = data[1,1]@intensity, rt = data[1,1]@rtime)
#' lines(baseline)
baselineEs <- function(int, rt, baselinePara = .get_baselinePara()){
  bottom_idx <- .inflect(int, baselinePara$threshold)$minima
  bottom_idx <- unique(c(1, bottom_idx, length(int)))
  if(length(bottom_idx) < 3){
    bottom_idx <- sort(unique(c(bottom_idx, order(int)[1:3])))
  }
  xs <- rt
  ys <- pracma::pchip(rt[bottom_idx], int[bottom_idx], xs)
  for(loop in 1:baselinePara$loops){
    tryCatch({ys <- .fineEstimation(xs, ys, int, tol_m = baselinePara$tol_m)},
             error = function(e){
               return(ys <- ys)
             })
  }
  baseline <- sapply(1:length(ys), function(i) {
    if(ys[i] > int[i]) return(int[i])
    else return(ys[i])
  })
  return(baseline)
}
