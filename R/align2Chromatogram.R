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
#'
#' @return A numeric score.
#' @export
#'
#' @examples
#' align2Chromatogram(Chromatogram1 = MChromatograms_new[3, 1], Chromatogram2 = MChromatograms_new[3, 2])
align2Chromatogram <- function(Chromatogram1, Chromatogram2){
  chrDf_x <- data.frame(rt = attributes(Chromatogram1)$rtime, intensity = attributes(Chromatogram1)$intensity)
  chrDf_y <- data.frame(rt = attributes(Chromatogram2)$rtime, intensity = attributes(Chromatogram2)$intensity)
  browser()
  tmp <- .approx_chrDf(chrDf_x = chrDf_x, chrDf_y = chrDf_y)
  cosScore <- .cosine_similarity(vec1 = tmp$chrDf_x$intensity, vec2 = tmp$chrDf_y$intensity)
  corScore <- .calCor_shape(chrDf1 = tmp$chrDf_x, chrDf2 = tmp$chrDf_y, method = "direct")
  alignScore <- 0.5 * cosScore + 0.5 * corScore
  return(alignScore)
}
