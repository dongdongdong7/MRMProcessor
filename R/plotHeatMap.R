.plotHeatMap <- function(matrix){
  nrow <- nrow(matrix)
  ncol <- ncol(matrix)
  df <- expand.grid(1:nrow, 1:ncol)
  colnames(df) <- c("i", "j")
  valueVec <- sapply(1:nrow(df), function(l) {
    i <- df[l, ]$i;j <- df[l, ]$j
    if(matrix[i, j] < 0) return(0)
    else return(matrix[i, j])
  })
  df$value <- valueVec
  df$i <- factor(as.character(df$i), levels = rev(unique(as.character(df$i))))
  df$j <- factor(as.character(df$j), levels = unique(as.character(df$j)))
  p <- ggplot2::ggplot(df, ggplot2::aes(j, i, fill = value)) +
    ggplot2::geom_tile(color = "black", size = 0.5) +
    #ggplot2::coord_equal() +
    ggplot2::scale_fill_gradient(low = "white", high = "purple", limits = c(0, 1)) +
    hrbrthemes::theme_ipsum(axis_text_size = 0, axis_title_size = 0) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   legend.position = "none")
  # test <- test %>%
  #   dplyr::mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))
  # p <- plotly::ggplotly(p, tooltip="text")
  p
}
#' @title plotHeatMap_MChromatograms
#' @description
#' Plotting a heat map for the MChromatograms object.
#'
#' @param MChromatograms MChromatograms object.
#' @param rows rows.
#' @param cols cols.
#' @param standard_cols standard_cols, default is NA, it means all standard_col is 1.User can change it, eg. c(3,4,1,3).
#' @param cosMag cosMag.
#' @param corMag corMag.
#' @param method method.
#'
#' @return An ggplot2 object.
#' @export
#'
#' @examples
#' plotHeatMap_MChromatograms(MChromatograms = MChromatograms, rows = rows_IS, cols = cols_batch1, standard_cols = NA)
plotHeatMap_MChromatograms <- function(MChromatograms, rows, cols, standard_cols = NA,
                                       cosMag = 0.5, corMag = 0.5, method = "direct"){
  scoreList <- calAlignScore_MChromatograms(MChromatograms = MChromatograms, row = rows, cols = cols, standard_cols = standard_cols,
                                                 cosMag = cosMag, corMag = corMag, method = method)
  scoreMatrix <- matrix(purrr::list_c(scoreList), nrow = length(rows), byrow = TRUE)
  nrow <- nrow(scoreMatrix)
  ncol <- ncol(scoreMatrix)
  df <- expand.grid(1:nrow, 1:ncol)
  colnames(df) <- c("i", "j")
  valueVec <- sapply(1:nrow(df), function(l) {
    i <- df[l, ]$i;j <- df[l, ]$j
    if(scoreMatrix[i, j] < 0) return(0)
    else return(scoreMatrix[i, j])
  })
  df2 <- expand.grid(rows, cols)
  colnames(df2) <- c("i", "j")
  sampleNameVec <- sapply(1:nrow(df2), function(l) {
    i <- df2[l, ]$i;j <- df2[l, ]$j
    strsplit(basename(attributes(MChromatograms[i, j])$sample_name), ".", fixed = TRUE)[[1]][1]
  })
  analyteNameVec <- sapply(1:nrow(df2), function(l) {
    i <- df2[l, ]$i;j <- df2[l, ]$j
    attributes(MChromatograms[i, j])$analyteName
  })
  df$value <- valueVec
  df$sampleName <- sampleNameVec
  df$analyteName <- analyteNameVec
  df$i <- factor(as.character(df$i), levels = rev(unique(as.character(df$i))))
  df$j <- factor(as.character(df$j), levels = unique(as.character(df$j)))
  df <- df %>%
    dplyr::mutate(text = paste0("analyteName: ", analyteName, "\n", "sampleName: ", sampleName))
  p <- ggplot2::ggplot(df, ggplot2::aes(j, i, text = text)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "black") +
    #ggplot2::geom_bin2d(bins = 20) +
    #ggplot2::coord_equal() +
    ggplot2::scale_fill_gradient(low = "white", high = "purple", limits = c(0, 1)) +
    #hrbrthemes::theme_ipsum(axis_text_size = 0, axis_title_size = 0) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   legend.position = "none")
  p <- plotly::ggplotly(p, tooltip="text")
  p
}

