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
