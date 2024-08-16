.plotFun <- function(int, rt, xlim = NA, ylim = NA,
                     intColour = "black", intLinewidth = 1,
                     axis.title.size = 15, axis.text.size = 10){
  df <- data.frame(int = int, rt = rt)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = rt)) +
    ggplot2::geom_line(ggplot2::aes(y = int), col = intColour, linewidth = intLinewidth) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Retention Time", y = "Intensity") +
    ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                   axis.text = ggplot2::element_text(size = axis.text.size))
  return(p)
}
