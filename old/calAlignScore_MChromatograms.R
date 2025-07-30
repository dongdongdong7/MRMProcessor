#' @title calAlignScore_MChromatograms
#' @description
#' Specify the rows and columns of MChromatograms to compute the alignScore.
#'
#' @param MChromatograms MChromatograms object.
#' @param rows rows.
#' @param cols cols.
#' @param standard_cols standard_cols. If NA, default all 1.
#' @param cosMag cosMag.
#' @param corMag corMag.
#' @param method method.
#'
#' @return A scoreList.
#' @export
#'
#' @examples
#' calAlignScore_MChromatograms(MChromatograms = MChromatograms, row = 22:23, cols = 1:ncol, standard_col = c(1,1))
calAlignScore_MChromatograms <- function(MChromatograms, rows, cols, standard_cols = NA,
                                         cosMag = 0.5, corMag = 0.5, method = "direct"){
  if(any(is.na(standard_cols))) standard_cols <- rep(1, length(rows))
  if(any(!standard_cols %in% cols)) stop("standard_col is not in cols!")
  scoreList <- lapply(1:length(standard_cols), function(l) {
    i <- rows[l];standard_col <- standard_cols[l]
    Chromatogram_standard <- MChromatograms[i, standard_col]
    sapply(cols, function(j) {
      Chromatogram_tmp <- MChromatograms[i, j]
      align2Chromatogram(Chromatogram1 = Chromatogram_standard, Chromatogram2 = Chromatogram_tmp,
                         cosMag = cosMag, corMag = corMag, method = method)
    })
  })
  return(scoreList)
}
