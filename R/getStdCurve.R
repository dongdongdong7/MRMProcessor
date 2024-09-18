#' @title GetStdCurve
#' @description
#' Generate standard curve list.
#'
#' @param MChromatograms MChromatograms object.
#' @param row A Quant row.
#' @param batchName batchName.
#' @param weights weights.None, 1/x, 1/x^2.
#' @param delete Serial numbers of the samples to be deleted, sorted in the order of injection.
#' @param zero Whether the marking curve passes the zero point.
#' @param rows_IS rows_IS.
#' @param cols_std cols_std.
#' @param cols_batchs cols_batchs.
#'
#' @return A stdCurve list.
#' @export
#'
#' @examples
#' rows_Quant <- .getRow4analyteType(MChromatograms, analyteType = c("Quant"))
#' rows_IS <- .getRow4analyteType(MChromatograms, analyteType = c("IS"))
#' stdCurveRes <- GetStdCurve(MChromatograms, row = rows_Quant[1], batchName = "batch1", delete = c(10, 12))
GetStdCurve <- function(MChromatograms, row, batchName, weights = "1/x^2", delete = c(), zero = FALSE, rows_IS = NULL, cols_std = NULL, cols_batchs = NULL){

  if(attributes(MChromatograms[row, 1])$analyteType != "Quant") stop("Must be Quant!")

  if(is.null(cols_std) | is.null(cols_batchs)) cols_std <- intersect(.getCol4typeName(MChromatograms, typeName = "std"), .getCol4batchName(MChromatograms, batchName = batchName))
  else cols_std <- intersect(cols_std, cols_batchs[[batchName]])
  # analyte
  analyteName <- attributes(MChromatograms[row, 1])$analyteName
  dilutionRatioVec <- sapply(cols_std, function(j) {
    attributes(MChromatograms[row, j])$dilutionRatio
  })
  areaVec <- as.numeric(sapply(cols_std, function(j) {
    if(is.null(attributes(MChromatograms[row, j])$targetPeak)) return(NA)
    else return(attributes(MChromatograms[row, j])$targetPeak[[1]]["area"])
  }))
  initialConVec <- sapply(cols_std, function(j) {
    attributes(MChromatograms[row, j])$initialCon
  })
  injectOrderVec <- sapply(cols_std, function(j) {
    attributes(MChromatograms[row, j])$injectOrder
  })
  df_analyte <- data.frame(injectOrder = injectOrderVec, initialCon = initialConVec,
                           dilutionRatio = dilutionRatioVec, area = areaVec)
  df_analyte <- df_analyte %>%
    dplyr::arrange(injectOrder)
  # IS
  ISName <- attributes(MChromatograms[row, cols_std[1]])$relatedIS
  if(is.null(rows_IS)) row_IS <- .getRow4analyteName(MChromatograms, analyteNameVec = c(ISName))
  else row_IS <- rows_IS[names(rows_IS) == ISName]
  areaVec <- as.numeric(sapply(cols_std, function(j) {
    if(is.null(attributes(MChromatograms[row_IS, j])$targetPeak)) return(NA)
    else return(attributes(MChromatograms[row_IS, j])$targetPeak[[1]]["area"])
  }))
  initialConVec <- sapply(cols_std, function(j) {
    attributes(MChromatograms[row_IS, j])$initialCon
  })
  injectOrderVec <- sapply(cols_std, function(j) {
    attributes(MChromatograms[row_IS, j])$injectOrder
  })
  df_IS <- data.frame(injectOrder = injectOrderVec, initialCon = initialConVec,
                           area = areaVec)
  df_IS <- df_IS %>%
    dplyr::arrange(injectOrder)
  df <- data.frame(concentrationRatio = (df_analyte$initialCon * df_analyte$dilutionRatio) / df_IS$initialCon[1],
                   areaRatio = df_analyte$area / df_IS$area)
  df$injectOrder <- 1:nrow(df)
  df$type <- rep("save", nrow(df))
  df$type[df$injectOrder %in% delete] <- "delete"
  df_fit <- df %>%
    dplyr::filter(type == "save" & !is.na(areaRatio))
  if(nrow(df_fit) <= 3) return(NULL)
  if(zero){
    df_fit <- rbind(data.frame(concentrationRatio = 0.000001, areaRatio = 0.000001, injectOrder = 0, type = "save"), df_fit)
  }
  if(weights == "none") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit)
  else if(weights == "1/x") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit, weights = 1 / concentrationRatio)
  else if(weights == "1/x^2") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit, weights = 1 / (concentrationRatio)^2)
  slope <- as.numeric(coef(fit)[2])
  intercept <- as.numeric(coef(fit)[1])
  r_squared <- summary(fit)$r.squared

  stdCurveRes <- list(analyteName = analyteName, batchName = batchName, ISName = ISName,
                      slope = slope, intercept = intercept, r_squared = r_squared,
                      weights = weights ,delete = delete, zero = zero, df = df)
  return(stdCurveRes)
}

GetStdCurve_MChromatograms <- function(MChromatograms, sampleInfo, weights = "1/x^2", zero = FALSE){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  rows_Quant <- .getRow4analyteType(MChromatograms, analyteType = c("Quant"))
  batchNameVector <- unique(sampleInfo$batchName)
  cols_batchs <- lapply(batchNameVector, function(x) .getCol4batchName(MChromatograms = MChromatograms, batchName = x))
  names(cols_batchs) <- batchNameVector
  cols_std <- .getCol4typeName(MChromatograms, typeName = "std")
  rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
  combinations <- expand.grid(rows_Quant, sapply(cols_batchs, function(x) purrr::pluck(x, 1)))
  colnames(combinations) <- c("i", "j")
  pb <- utils::txtProgressBar(max = nrow(combinations), style = 3)
  chrs_idx <- sapply(1:nrow(combinations), function(l) {
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    (j - 1) * nrow + i
  })
  chrs <- lapply(1:nrow(combinations), function(l) {
    utils::setTxtProgressBar(pb, l)
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    Chromatogram <- MChromatograms[i, j]
    stdCurveRes <- GetStdCurve(MChromatograms, row = i, batchName = attributes(MChromatograms[i,j])$batchName, rows_IS = rows_IS, cols_std = cols_std, cols_batchs = cols_batchs)
    attributes(Chromatogram)$stdCurveRes <- stdCurveRes
    return(Chromatogram)
  })
  chrs_all[chrs_idx] <- chrs
  MSnbase::MChromatograms(chrs_all,
                          ncol = ncol)
}

cal_concentration <- function(MChromatograms, sampleInfo){
  rows_Quant <- .getRow4analyteType(MChromatograms, analyteType = "Quant")
  batchNameVector <- unique(sampleInfo$batchName)
  cols_batchs <- lapply(batchNameVector, function(x) .getCol4batchName(MChromatograms = MChromatograms, batchName = x))
  names(cols_batchs) <- batchNameVector
  cols_real <- .getCol4typeName(MChromatograms, typeName = "real")
  analyteNameVector <- sapply(rows_Quant, function(i) {attributes(MChromatograms[i, 1])$analyteName})
  sampleNameVector <- sapply(cols_real, function(j) {
    strsplit(basename(attributes(MChromatograms[1, j])$sample_name), split = ".", fixed = TRUE)[[1]][1]
  })
  rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
  dfList <- lapply(rows_Quant, function(i) {
    conVec <- sapply(cols_real, function(j) {
      if(is.null(attributes(MChromatograms[i, j])$targetPeak)) area_analyte <- 0
      else area_analyte <- as.numeric(attributes(MChromatograms[i, j])$targetPeak[[1]]["area"])
      row_IS <- rows_IS[names(rows_IS) == attributes(MChromatograms[i, j])$relatedIS]
      if(is.null(attributes(MChromatograms[row_IS, j])$targetPeak)) area_IS <- 0
      else area_IS <- as.numeric(attributes(MChromatograms[row_IS, j])$targetPeak[[1]]["area"])
      if(area_analyte == 0 | area_IS == 0) return(NA)
      batchTmp <- batchNameVector[sapply(cols_batchs, function(x) {j %in% x})]
      idx <- cols_batchs[[batchTmp]][1]
      stdCurveRes_tmp <- attributes(MChromatograms[i, idx])$stdCurveRes
      if(is.null(stdCurveRes_tmp)) return(NA)
      slope <- stdCurveRes_tmp$slope
      intercept <- stdCurveRes_tmp$intercept
      con <- round((((area_analyte / area_IS) - intercept) / slope) * attributes(MChromatograms[row_IS, j])$initialCon, 4)
      return(con)
    })
    df <- as.data.frame(matrix(conVec, nrow = 1))
    return(df)
  })
  df <- purrr::list_rbind(dfList)
  rownames(df) <- analyteNameVector
  colnames(df) <- sampleNameVector
  tb <- df %>%
    tibble::rownames_to_column("analyteName")
}

generate_area <- function(MChromatograms){
  analyteNameVector <- sapply(1:nrow(MChromatograms), function(i) {attributes(MChromatograms[i, 1])$analyteName})
  sampleNameVector <- sapply(1:ncol(MChromatograms), function(j) {
    strsplit(basename(attributes(MChromatograms[1, j])$sample_name), split = ".", fixed = TRUE)[[1]][1]
  })
  dfList <- lapply(1:nrow(MChromatograms), function(i) {
    areaVec <- sapply(1:ncol(MChromatograms), function(j) {
      if(is.null(attributes(MChromatograms[i, j])$targetPeak)) area <- 0
      else area <- as.numeric(attributes(MChromatograms[i, j])$targetPeak[[1]]["area"])
      return(area)
    })
    df <- as.data.frame(matrix(areaVec, nrow = 1))
    return(df)
  })
  df <- purrr::list_rbind(dfList)
  rownames(df) <- analyteNameVector
  colnames(df) <- sampleNameVector
  tb <- df %>%
    tibble::rownames_to_column("analyteName")
}

#' @title plotStdCurve
#' @description
#' Plotting the standard curve.
#'
#' @param stdCurveRes stdCurveRes.
#'
#' @return A plotly object.
#' @export
#'
#' @examples
#' plotStdCurve(stdCurveRes)
plotStdCurve <- function(stdCurveRes){
  if(is.null(stdCurveRes)) return(plotly::ggplotly(ggplot2::ggplot(data = NULL)))
  df <- stdCurveRes$df;analyteName <- stdCurveRes$analyteName;ISName <- stdCurveRes$ISName;
  slope <- stdCurveRes$slope;intercept <- stdCurveRes$intercept;r_squared <- stdCurveRes$r_squared;
  delete <- stdCurveRes$delete;weights = stdCurveRes$weights
  df <- df %>%
    dplyr::mutate(text = paste0("concentrationRatio: ", round(concentrationRatio, 4), "\n",
                         "areaRatio: ", round(areaRatio, 4), "\n",
                         "injectOrder: ", injectOrder))

  df_line <- data.frame(intercept = intercept, slope = slope)

  if(intercept >= 0) intercept_text <- paste0("+ ", round(intercept, 4))
  else intercept_text <- paste0("- ", round(abs(intercept), 4))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = concentrationRatio, y = areaRatio, text = text)) +
    ggplot2::geom_point(ggplot2::aes(color = type)) +
    ggplot2::scale_color_manual(values = c("delete" = "gray", "save" = "blue")) +
    ggplot2::annotate("text",
                      x = (max(df$concentrationRatio) - min(df$concentrationRatio)) * 0.2,
                      y = (max(df$areaRatio, na.rm = TRUE) - min(df$areaRatio, na.rm = TRUE)) * 0.8,
                      label = paste("y =", round(slope, 4),
                                    "x ", intercept_text,
                                    "\nR2 = ", round(r_squared, 4),
                                    "\nweights = ",weights ),
                      color = "red", hjust = 0, vjust = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Concentration Ratio", y = "Area Ratio",
                  title = paste0(analyteName, " (IS: ", ISName, ")")) +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 15),
                   axis.text = ggplot2::element_text(size = 10),
                   title = ggplot2::element_text(size = 15),
                   legend.position = "none") +
    ggplot2::geom_abline(data = df_line, ggplot2::aes(intercept = intercept, slope = slope), color = "red")
  p <- plotly::ggplotly(p, tooltip="text")
  p
}
