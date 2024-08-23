.getLine <- function(A, B){
  slope <- (B[2] - A[2]) / (B[1] - A[1])
  intercept <- A[2] - slope * A[1]
  return(c(slope = slope, intercept = intercept))
}
.edgeTrack_crude <- function(int, rt, baseline, preNum = 3, tol_m = 30, multiSmooth = TRUE){
  aboveTHidx <- which(int > baseline)
  candidateSegInd <- split(aboveTHidx, cumsum(c(1, diff(aboveTHidx) != 1)))
  pointNum <- sapply(candidateSegInd, length)
  intMax <- sapply(candidateSegInd, function(x) {
    max(int[x])
  })
  candidateSegInd <- candidateSegInd[[which.max(intMax)]]
  if(length(candidateSegInd) <= 3) return(NULL)
  idx1 <- candidateSegInd[1] - 1
  if(idx1 < 1) idx1 <- 1
  idx2 <- candidateSegInd[length(candidateSegInd)] + 1
  if(idx2 > length(int)) idx2 <- length(int)
  idx <- unique(c(idx1, candidateSegInd, idx2))
  int_new <- int[idx]
  rt_new <- rt[idx]
  baseline_new <- baseline[idx]
  apex_idx <- which.max(int_new)
  int_a <- int_new[1:apex_idx];int_b <- int_new[apex_idx:length(int_new)]
  if(multiSmooth){
    while(!all(diff(int_a) >0) | !all(diff(int_b) < 0)){
      int_new <- .smoothMean(int_new, size = 3)
      apex_idx <- which.max(int_new)
      int_a <- int_new[1:apex_idx];int_b <- int_new[apex_idx:length(int_new)]
    }
  }
  #browser()
  int_norm <- (int_new - min(int_new)) / (max(int_new) - min(int_new))
  rt_norm <- (rt_new - min(rt_new)) / (max(rt_new) - min(rt_new))
  a_start <- apex_idx - ((preNum - 1) / 2) - 1
  if(a_start <= 1) a_start <- 1
  b_start <- apex_idx + ((preNum - 1) / 2) + 1
  if(b_start >= length(int_norm)) b_start <- length(int_norm)
  a <- a_start;b <- b_start
  while(a!=1){
    a0 <- a - 1
    if(a == 1) a0 <- a  + 1
    A <- c(rt_norm[a], int_norm[a]);A0 <- c(rt_norm[a0], int_norm[a0])
    tmpA <- .getLine(A, A0);slopeA <- tmpA["slope"];interceptA <- tmpA["intercept"]
    angleA0 <- atan(abs(slopeA)) * (180 / pi)
    if(angleA0 <= tol_m){break}
    else if(angleA0 > tol_m){a <- a - 1;if(a < 1){a <- 1}}
  }
  while(b!=length(int_norm)){
    b0 <- b + 1
    if(b == length(int_norm)) b0 <- b - 1
    B <- c(rt_norm[b], int_norm[b]);B0 <- c(rt_norm[b0], int_norm[b0])
    tmpB <- .getLine(B, B0);slopeB <- tmpB["slope"];interceptB <- tmpB["intercept"]
    angleB0 <- atan(abs(slopeB)) * (180 / pi)
    if(angleB0 <= tol_m){break}
    else if(angleB0 > tol_m){b <- b + 1;if(b > length(int_norm)) b <- length(int_norm)}
  }
  a_end <- a;b_end <- b
  rt_start <-  rt_new[a_end];rt_end <- rt_new[b_end]
  return(c(start = rt_start, end = rt_end))
}
#' @title get_peakPara
#' @description
#' Get peakPara list.
#'
#' @param sn sn threshold.
#' @param preNum preNum.
#' @param extend extend.
#' @param tol_m tol_m.
#' @param multiSmooth Whether or not to give multiple smoothing to the ZOI region, which helps the accuracy of edge finding.
#' @param cal_ZOI_baseline Whether or not to calculate a baseline for the ZOI separately, which helps the accuracy of edge finding.
#' @param fwhm This is the parameter used in the matchfilter function to determine the multi-peak case.
#' The smaller the value the easier it is to find multiple peaks.
#' @param snthresh This is the parameter used in the matchfilter function to determine the multi-peak case.
#' @param peakWidth peakWidth for CentWave algorithm.
#' @param xcms xcms methods, CentWave and Matched
#'
#' @return A list.
#' @export
#'
#' @examples
#' peakPara <- get_peakPara()
get_peakPara <- function(sn = 3, preNum = 3, extend = 5, tol_m = 10, multiSmooth = TRUE, cal_ZOI_baseline = TRUE, fwhm = NA, snthresh = 0.5, peakWidth = NA, xcms = "CentWave"){
  return(list(sn = sn, preNum = preNum, extend = extend, tol_m = tol_m, multiSmooth = multiSmooth, cal_ZOI_baseline = cal_ZOI_baseline, fwhm = fwhm, snthresh = snthresh, peakWidth = peakWidth, xcms = xcms))
}

#' @title peakPicking
#' @description
#' This is a peak-picking function for the MRM targeting window
#'
#' @param int MRM window intensity vector.
#' @param rt MRM window retention time vector.
#' @param noise noise, NA or set by user. If NA, noiseEs function will be used.
#' @param smoothPara smoothPara.
#' @param baselinePara baselinePara.
#' @param sn sn threshold.
#' @param preNum preNum.
#' @param tol_m tol_m.
#' @param snthresh This is the parameter used in the matchfilter function to determine the multi-peak case.
#' @param fwhm This is the parameter used in the matchfilter function to determine the multi-peak case.
#' The smaller the value the easier it is to find multiple peaks.
#' @param cal_ZOI_baseline Whether or not to calculate a baseline for the ZOI separately, which helps the accuracy of edge finding.
#' @param multiSmooth Whether or not to give multiple smoothing to the ZOI region, which helps the accuracy of edge finding
#' @param extend extend.
#'
#' @return A list.
#' @export
#'
#' @examples
#' row <- 200;col <- 2
#' plot(x=data[row,col]@rtime * 60,y = data[row,col]@intensity, type = "l")
#' plot(x=data[row,col]@rtime * 60,y = smoothFun(data[row,col]@intensity), type = "l")
#' baseline <- baselineEs(int = smoothFun(data[row,col]@intensity), rt = data[row,col]@rtime * 60)
#' lines(x = data[row,col]@rtime * 60, y = baseline)
#' noise0 <- noiseEs(smoothFun(data[row,col]@intensity))
#' noiseEs(data[row,col]@intensity, prepare = FALSE)
#' abline(h = noise0)
#' plot(sort(data[row,col]@intensity))
#' peakPicking(int = data[row,col]@intensity, rt = data[row,col]@rtime * 60, noise = 1000, peakPara = get_peakPara())
peakPicking <- function(int, rt, noise = NA,
                        smoothPara = get_smoothPara(), baselinePara = get_baselinePara(), peakPara = get_peakPara()){
  # peakPara
  sn <- peakPara$sn;preNum <- peakPara$preNum;extend <- peakPara$extend;tol_m <- peakPara$tol_m
  multiSmooth <- peakPara$multiSmooth;cal_ZOI_baseline <- peakPara$cal_ZOI_baseline
  fwhm <- peakPara$fwhm;snthresh <- peakPara$snthresh
  peakWidth <- peakPara$peakWidth;xcms <- peakPara$xcms

  int <- smoothFun(int, smoothPara = smoothPara)
  if(is.na(noise)) noise0 <- noiseEs(int)
  else if(noise > 0) noise0 <- noise
  else stop("noise should > 0 !")
  baseline <- baselineEs(int = int, rt = rt, baselinePara = baselinePara)
  aboveTHidx <- which(int > noise0)
  if(length(aboveTHidx) == 0) return(NULL)
  candidateSegInd <- split(aboveTHidx, cumsum(c(1, diff(aboveTHidx) != 1)))
  candidateSegInd <- candidateSegInd[sapply(candidateSegInd, function(i) {
    if(length(which(int[i] > noise0)) > preNum) return(TRUE)
    else return(FALSE)
  })]
  if(length(candidateSegInd) == 0) return(NULL)
  #browser()
  ZOIList <- lapply(1:length(candidateSegInd), function(i) {
    extend1 <- extend
    extend2 <- extend
    idx <- candidateSegInd[[i]]
    idxLength <- length(idx)
    idx1 <- idx[1] - extend1
    if(idx1 < 1) idx1 <- 1
    if(i != 1){
      while(idx1 %in% candidateSegInd[[i - 1]] | idx1 < min(candidateSegInd[[i - 1]])){
        extend1 <- extend1 - 1
        idx1 <- idx[1] - extend1
      }
    }
    if(idx1 < 1) idx1 <- 1
    idx2 <- idx[idxLength] + extend2
    if(idx2 > length(int)) idx2 <- length(int)
    if(i != length(candidateSegInd)){
      while(idx2 %in% candidateSegInd[[i + 1]] | idx2 > max(candidateSegInd[[i + 1]])){
        extend2 <- extend2 - 1
        idx2 <- idx[idxLength] + extend2
      }
    }
    if(idx2 > length(int)) idx2 <- length(int)
    idx <- unique(c(idx1:idx[1], idx, idx[idxLength]:idx2))
    ZOI_int <- int[idx]
    ZOI_rt <- rt[idx]
    if(cal_ZOI_baseline){
      ZOI_baseline <- baselineEs(int = ZOI_int, rt = ZOI_rt, baselinePara = baselinePara)
    }else{
      ZOI_baseline <- baseline[idx]
    }
    ZOIWidth <- (max(ZOI_rt) - min(ZOI_rt)) / 2 # half
    if(is.na(fwhm)) fwhm <- ZOIWidth / 2 # round(ZOIWidth / 2)
    else fwhm <- fwhm
    if(any(is.na(peakWidth))) peakWidth <- c(fwhm / 2, fwhm * 2)
    if(!(xcms == "CentWave" | xcms == "MatchedFilter")) stop("xcms method wrong!")
    tmp <- tryCatch({
      if(xcms == "CentWave") xcms::peaksWithCentWave(ZOI_int, ZOI_rt, snthresh = snthresh, peakwidth = peakWidth)
      else if(xcms == "MatchedFilter") xcms::peaksWithMatchedFilter(ZOI_int, ZOI_rt, fwhm = fwhm, snthresh = snthresh)
    },
    error = function(e){
      matrix(NA, nrow = 0, ncol = 8)
    })
    if(nrow(tmp) > 1) { # Multi peaks
      top_idx <- sort(sapply(1:nrow(tmp), function(j) {
        which(dplyr::near(ZOI_rt, tmp[j, "rt"], tol = 0.005))
      }))
      bottom_idx <- sapply(1:(length(top_idx) - 1), function(j) {
        a_idx <- top_idx[j];b_idx <- top_idx[j + 1]
        int_tmp <- ZOI_int
        int_tmp[1:a_idx] <- NA;int_tmp[b_idx:length(int_tmp)] <- NA
        which.min(int_tmp)
      })
      bottom_idx <- c(1, bottom_idx, length(ZOI_int))
      multiZOIList <- lapply(top_idx, function(t) {
        if(t %in% bottom_idx) return(NULL)
        a_idx <- bottom_idx[bottom_idx < t];a_idx <- a_idx[length(a_idx)]
        b_idx <- bottom_idx[bottom_idx > t];b_idx <- b_idx[1]
        ZOI_int_t <- ZOI_int[a_idx:b_idx]
        ZOI_rt_t <- ZOI_rt[a_idx:b_idx]
        ZOI_baseline_t <- ZOI_baseline[a_idx:b_idx]
        edge <- .edgeTrack_crude(int = ZOI_int_t, ZOI_rt_t, baseline = ZOI_baseline_t, preNum = preNum, tol_m = tol_m, multiSmooth = multiSmooth)
        return(edge)
      })
      return(multiZOIList)
    }else{
      ZOI <- .edgeTrack_crude(int = ZOI_int, rt = ZOI_rt, baseline = ZOI_baseline, preNum = preNum, tol_m = tol_m, multiSmooth = multiSmooth)
      return(ZOI)
    }
  })
  ZOIList <- purrr::list_flatten(ZOIList)
  snVec <- sapply(ZOIList, function(x) {
    if(is.null(x)) return(-1)
    idx <- which(rt == x["start"]):which(rt == x["end"])
    rt_tmp <- rt[idx]
    int_tmp <- int[idx]
    baseline_tmp <- mean(baseline[idx])
    if(baseline_tmp < 1) baseline_tmp <- 1
    sn_tmp <- max(int_tmp) / baseline_tmp
    if(sn_tmp > sn) return(sn_tmp)
    else return(-1)
  })
  ZOIList <- ZOIList[snVec > sn]
  if(length(ZOIList) == 0) return(NULL)
  snVec <- snVec[snVec > sn]
  areaVec <- sapply(ZOIList, function(x) {
    idx <- which(rt == x["start"]):which(rt == x["end"])
    rt_tmp <- rt[idx]
    int_tmp <- int[idx]
    baseline_tmp <- mean(baseline[idx])
    sum(((max(rt_tmp) - min(rt_tmp)) / length(rt_tmp)) * (int_tmp - baseline_tmp))
  })
  apexVec <- sapply(ZOIList, function(x) {
    idx <- which(rt == x["start"]):which(rt == x["end"])
    rt_tmp <- rt[idx]
    int_tmp <- int[idx]
    rt_tmp[which.max(int_tmp)]
  })
  maxoVec <- sapply(ZOIList, function(x) {
    idx <- which(rt == x["start"]):which(rt == x["end"])
    int_tmp <- int[idx]
    int_tmp[which.max(int_tmp)]
  })
  IEVec <- sapply(ZOIList, function(x) {
    idx <- which(rt == x["start"]):which(rt == x["end"])
    int_tmp <- int[idx]
    cal_IE(int = int_tmp)
  })
  ZOIList <- lapply(1:length(ZOIList), function(i) {
    c(apex = apexVec[i], ZOIList[[i]], sn = snVec[i], maxo = maxoVec[i], area = areaVec[i], IE = IEVec[i])
  })
  return(ZOIList)
}

