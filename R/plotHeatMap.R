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
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradient(low = "white", high = "purple", limits = c(0, 1)) +
    #hrbrthemes::theme_ipsum(axis_text_size = 0, axis_title_size = 0) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   legend.position = "none")
  p <- plotly::ggplotly(p, tooltip="text")
  p
}

.calculate_dimensions <- function(n, x = 1080, y = 1920) {
  # 行列比
  ratio <- c(x, y)

  # 行数和列数的初始值
  r <- 1
  c <- round(r * (ratio[2] / ratio[1]))  # 计算初始列数

  # 寻找合适的行数和列数
  while (TRUE) {
    if (r * c >= n) {
      return(c(rows = r, cols = c))
    }
    r <- r + 1
    c <- round(r * (ratio[2] / ratio[1]))  # 更新列数
  }
}

#' @title plotHeatMap_MChromatogramsRow
#' @description
#' This is a heat map for individual substances.
#'
#' @param MChromatograms MChromatograms.
#' @param row row.
#' @param cols cols.
#' @param standard_cols standard_cols.
#' @param cosMag cosMag.
#' @param corMag corMag.
#' @param method method.
#'
#' @return A plotly object.
#' @export
#'
#' @examples
#' plotHeatMap_MChromatogramsRow(MChromatograms, row = rows_IS[1], cols = cols_batch1)
plotHeatMap_MChromatogramsRow <- function(MChromatograms, row, cols, standard_cols = NA,
                                          cosMag = 0.5, corMag = 0.5, method = "direct"){
  if(length(row)!=1) stop("length of row must be 1!")
  areaVec <- sapply(cols, function(j) {
    if(!is.null(attributes(MChromatograms[row, j])$targetPeak)){
      targetPeak_tmp <- attributes(MChromatograms[row, j])$targetPeak[[1]]
      as.numeric(targetPeak_tmp["area"])
    }else return(0)
  })
  if(is.na(standard_cols)) standard_cols <- cols[which.max(areaVec)]
  else{
    if(!standard_cols %in% cols){
      warnings("standard_cols is not in cols!")
      standard_cols <- cols[1]
    }
  }
  scoreList <- calAlignScore_MChromatograms(MChromatograms, rows = row, cols = cols, standard_cols = standard_cols,
                                            cosMag = cosMag, corMag = corMag, method = method)
  chrs <- MChromatograms[row, cols, drop = TRUE]
  dimensions <- .calculate_dimensions(n = length(chrs), x = 1080 / 3)
  matrixRow <- as.numeric(dimensions["rows"])
  matrixCol <- as.numeric(dimensions["cols"])
  fill_matrix_with_na <- function(vec, n_rows, n_cols) {
    # 创建一个足够大的矩阵，初始值为 NA
    mat <- t(matrix(NA, nrow = n_rows, ncol = n_cols))

    # 填充矩阵
    mat[1:length(vec)] <- vec

    return(t(mat))
  }
  scoreMatrix <- fill_matrix_with_na(purrr::list_c(scoreList), n_rows = matrixRow, n_cols = matrixCol)
  nrow <- nrow(scoreMatrix)
  ncol <- ncol(scoreMatrix)
  df <- expand.grid(1:ncol, 1:nrow)
  colnames(df) <- c("j", "i")
  valueVec <- sapply(1:nrow(df), function(l) {
    i <- df[l, ]$i;j <- df[l, ]$j
    if(is.na(scoreMatrix[i, j])) return(NA)
    if(scoreMatrix[i, j] < 0) return(0)
    else return(scoreMatrix[i, j])
  })
  sampleNameVec <- sapply(1:length(chrs), function(l) {
    strsplit(basename(attributes(chrs[[l]])$sample_name), ".", fixed = TRUE)[[1]][1]
  })
  analyteNameVec <- sapply(1:length(chrs), function(l) {
    attributes(chrs[[l]])$analyteName
  })
  df$value <- valueVec
  df$sampleName[!is.na(df$value)] <- sampleNameVec
  df$analyteName[!is.na(df$value)] <- analyteNameVec
  df$i <- factor(as.character(df$i), levels = rev(unique(as.character(df$i))))
  df$j <- factor(as.character(df$j), levels = unique(as.character(df$j)))
  df <- df %>%
    dplyr::mutate(text = paste0("analyteName: ", analyteName, "\n", "sampleName: ", sampleName))
  df$text[is.na(df$value)] <- ""
  p <- ggplot2::ggplot(df, ggplot2::aes(j, i, text = text)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "black") +
    #ggplot2::geom_bin2d(bins = 20) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradient(low = "white", high = "purple", limits = c(0, 1)) +
    #hrbrthemes::theme_ipsum(axis_text_size = 0, axis_title_size = 0) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   legend.position = "none")
  p <- plotly::ggplotly(p, tooltip="text")
  p
}
