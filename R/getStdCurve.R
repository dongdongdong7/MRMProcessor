#' @title GetStdCurve
#' @description
#' Generate standard curve list.
#'
#' @param MChromatograms MChromatograms object.
#' @param row A Quant row.
#' @param batchName batchName.
#' @param weights weights.None, 1/x, 1/x^2.
#' @param delete Serial numbers of the samples to be deleted, sorted in the order of injection.
#'
#' @return A stdCurve list.
#' @export
#'
#' @examples
#' rows_Quant <- .getRow4analyteType(MChromatograms, analyteType = c("Quant"))
#' rows_IS <- .getRow4analyteType(MChromatograms, analyteType = c("IS"))
#' stdCurveRes <- GetStdCurve(MChromatograms, row = rows_Quant[1], batchName = "batch1", delete = c(10, 12))
GetStdCurve <- function(MChromatograms, row, batchName, weights = "1/x^2", delete = c()){

  if(attributes(MChromatograms[row, 1])$analyteType != "Quant") stop("Must be Quant!")

  cols_std <- intersect(.getCol4typeName(MChromatograms, typeName = "std"), .getCol4batchName(MChromatograms, batchName = batchName))
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
  row_IS <- .getRow4analyteName(MChromatograms, analyteNameVec = c(ISName))
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
    dplyr::filter(type == "save")
  if(weights == "none") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit)
  else if(weights == "1/x") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit, weights = 1 / concentrationRatio)
  else if(weights == "1/x^2") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit, weights = 1 / (concentrationRatio)^2)
  slope <- as.numeric(coef(fit)[2])
  intercept <- as.numeric(coef(fit)[1])
  r_squared <- summary(fit)$r.squared

  stdCurveRes <- list(analyteName = analyteName, batchName = batchName, ISName = ISName,
                      slope = slope, intercept = intercept, r_squared = r_squared,
                      weights = weights ,delete = delete, df = df)
  return(stdCurveRes)
}

GetStdCurve_MChromatograms <- function(MChromatograms, sampleInfo, weights = "1/x^2"){
  rows_Quant <- .getRow4analyteType(MChromatograms, analyteType = c("Quant"))
  batchNameVector <- unique(sampleInfo$batchName)
  cols_batchs <- lapply(batchNameVector, function(x) .getCol4batchName(MChromatograms = MChromatograms, batchName = x))
  names(cols_batchs) <- batchNameVector
  stdCurveResList <- lapply(rows_Quant, function(i) {
    lapply(batchNameVector, function(x) {
      GetStdCurve(MChromatograms, row = i, batchName = x)
    })
  })
  stdCurveResList <- purrr::list_flatten(stdCurveResList)
  k <- 1
  for(i in rows_Quant){
    for(j in sapply(cols_batchs, function(x) purrr::pluck(x, 1))){
      attributes(MChromatograms[i, j])$stdCurveRes <- stdCurveResList[[k]]
      k <- k + 1
    }
  }
  return(MChromatograms)
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
  df <- stdCurveRes$df;analyteName <- stdCurveRes$analyteName;ISName <- stdCurveRes$ISName;
  slope <- stdCurveRes$slope;intercept <- stdCurveRes$intercept;r_squared <- stdCurveRes$r_squared;
  delete <- stdCurveRes$delete;weights = stdCurveRes$weights
  df <- df %>%
    dplyr::mutate(text = paste0("concentrationRatio: ", round(concentrationRatio, 4), "\n",
                         "areaRatio: ", round(areaRatio, 4), "\n",
                         "injectOrder: ", injectOrder))

  df_line <- data.frame(intercept = intercept, slope = slope)

  if(intercept >= 0) intercept_text <- paste0("+ ", round(intercept), 4)
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
