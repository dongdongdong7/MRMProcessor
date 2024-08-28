.approx_chrDf <- function(chrDf_x, chrDf_y){
  tmp <- approx(x = chrDf_x$rt, y = chrDf_x$intensity, xout = chrDf_y$rt)
  extra_chrDf <- dplyr::tibble(intensity = tmp$y, rt = tmp$x)
  chrDf_x_new <- rbind(chrDf_x, extra_chrDf) %>% dplyr::arrange(rt)

  tmp <- approx(x = chrDf_y$rt, y = chrDf_y$intensity, xout = chrDf_x$rt)
  extra_chrDf <- dplyr::tibble(intensity = tmp$y, rt = tmp$x)
  chrDf_y_new <- rbind(chrDf_y, extra_chrDf) %>% dplyr::arrange(rt)

  return(list(chrDf_x = chrDf_x_new, chrDf_y = chrDf_y_new))
}
.cosine_similarity <- function(vec1, vec2) {
  vec1[is.na(vec1)] <- 0
  vec2[is.na(vec2)] <- 0

  # 计算点积
  dot_product <- sum(vec1 * vec2)

  # 计算范数
  norm_vec1 <- sqrt(sum(vec1^2))
  norm_vec2 <- sqrt(sum(vec2^2))

  # 计算余弦相似度
  cosine_sim <- dot_product / (norm_vec1 * norm_vec2)

  return(cosine_sim)
}
.calCor_shape <- function(chrDf1, chrDf2, method = "direct"){
  if(method == "apex"){
    align_vectors <- function(A, B) {
      # find apex
      peak_A <- which.max(A)
      peak_B <- which.max(B)

      # length of two vector
      left_len <- max(peak_A, peak_B) - 1
      right_len <- max(length(A) - peak_A, length(B) - peak_B)

      # new length
      aligned_length <- left_len + right_len + 1
      new_A <- rep(NA, aligned_length)
      new_B <- rep(NA, aligned_length)

      # alignment A
      start_A <- left_len - (peak_A - 1) + 1
      end_A <- start_A + length(A) - 1
      new_A[start_A:end_A] <- A

      # alignment B
      start_B <- left_len - (peak_B - 1) + 1
      end_B <- start_B + length(B) - 1
      new_B[start_B:end_B] <- B

      return(list(aligned_A = new_A, aligned_B = new_B))
    }
    intTmp <- align_vectors(A = chrDf1$intensity, B = chrDf2$intensity)
    int1 <- intTmp[[1]];int2 <- intTmp[[2]]
  }else if(method == "direct"){
    int1 <- chrDf1$intensity
    int2 <- chrDf2$intensity
  }else stop("method wrong!")
  cor_shape <- round(cor(int1, int2, method = "pearson", use = "pairwise.complete.obs"), 4)
  if(is.na(cor_shape)) cor_shape <- 0
  return(cor_shape)
}
#' @title align2Chromatogram
#' @description
#' Calculate two Chromatogram object alignment scores.
#'
#' @param Chromatogram1 Chromatogram object.
#' @param Chromatogram2 Chromatogram object.
#' @param cosMag Magnification of cosScore.
#' @param corMag Magnification of corScore
#' @param method cor method. direct or apex.
#'
#' @return A numeric score.
#' @export
#'
#' @examples
#' align2Chromatogram(Chromatogram1 = MChromatograms[3, 1], Chromatogram2 = MChromatograms[3, 2])
align2Chromatogram <- function(Chromatogram1, Chromatogram2, cosMag = 0.5, corMag = 0.5, method = "direct"){
  if(is.null(attributes(Chromatogram1)$targetPeak) | is.null(attributes(Chromatogram2)$targetPeak)) return(0)
  peak_x <- attributes(Chromatogram1)$targetPeak[[1]]
  peak_y <- attributes(Chromatogram2)$targetPeak[[1]]
  rt_x <- attributes(Chromatogram1)$rtime
  idx_x <- which(rt_x >= peak_x["start"] & rt_x <= peak_x["end"])
  rt_x <- rt_x[idx_x]
  int_x <- attributes(Chromatogram1)$intensity
  int_x <- int_x[idx_x]
  rt_y <- attributes(Chromatogram2)$rtime
  idx_y <- which(rt_y >= peak_y["start"] & rt_y <= peak_y["end"])
  rt_y <- rt_y[idx_y]
  int_y <- attributes(Chromatogram2)$intensity
  int_y <- int_y[idx_y]
  chrDf_x <- data.frame(rt = rt_x, intensity = int_x)
  chrDf_y <- data.frame(rt = rt_y, intensity = int_y)
  tmp <- .approx_chrDf(chrDf_x = chrDf_x, chrDf_y = chrDf_y)
  cosScore <- .cosine_similarity(vec1 = tmp$chrDf_x$intensity, vec2 = tmp$chrDf_y$intensity)
  corScore <- .calCor_shape(chrDf1 = tmp$chrDf_x, chrDf2 = tmp$chrDf_y, method = method)
  alignScore <- cosMag * cosScore + corMag * corScore
  return(alignScore)
}
#' @title align2ChromatogramVstandard
#' @description
#' Using a standard Chromatogram to find the most suitable target peaks in another Chromatogram.
#'
#' @param Chromatogram_standard Chromatogram_standard.
#' @param Chromatogram_await Chromatogram_await.
#' @param cosMag cosMag.
#' @param corMag corMag.
#' @param method method.
#' @param scoreTh scoreTh.
#'
#' @return A new Chromatogram object.
#' @export
#'
#' @examples
#' plotChromatogram(MChromatograms[6,2], targetPeak = TRUE)
#' attributes(MChromatograms[6,2])
#' MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = 6, cols = 2, targetRt = NA)
#' plotChromatogram(MChromatograms[6,9], targetPeak = TRUE)
#' MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = 6, cols = 9, targetRt = 26)
#' attributes(MChromatograms[6,9])$peak <- NULL
#' plotChromatogram(MChromatograms[6, 9], targetPeak = FALSE)
#' Chromatogram_await_new <- align2ChromatogramVstandard(Chromatogram_standard = MChromatograms[6, 2], Chromatogram_await = MChromatograms[6, 9])
#' plotChromatogram(Chromatogram_await_new, targetPeak = TRUE)
align2ChromatogramVstandard <- function(Chromatogram_standard, Chromatogram_await, cosMag = 0.5, corMag = 0.5, method = "direct", scoreTh = 0.9){
  if(is.null(attributes(Chromatogram_standard)$targetPeak)) stop("Standard chromatogram do not have standard peak!")
  if(is.null(attributes(Chromatogram_await)$peaksInfo)) return(Chromatogram_await)
  peaksInfo_await <- attributes(Chromatogram_await)$peaksInfo
  scoreVec <- sapply(peaksInfo_await, function(x) {
    Chromatogram_tmp <- extractTargetPeak_Chromatogram(Chromatogram_await, targetRt = x["apex"])
    align2Chromatogram(Chromatogram1 = Chromatogram_standard, Chromatogram2 = Chromatogram_tmp, cosMag = cosMag, corMag = corMag, method = method)
  })
  peakIdx <- which(scoreVec == max(scoreVec) & scoreVec > scoreTh)
  if(length(peakIdx) == 1) attributes(Chromatogram_await)$targetPeak <- peaksInfo_await[peakIdx]
  else stop("length(peakIdx) must be 1!")
  return(Chromatogram_await)
}
