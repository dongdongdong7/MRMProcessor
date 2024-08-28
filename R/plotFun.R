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
  if(!any(is.na(xlim))) p <- p + ggplot2::xlim(xlim)
  if(!any(is.na(ylim))) p <- p + ggplot2::ylim(ylim)
  return(p)
}

#' @title plotChromatogram
#' @description
#' Plot a Chromatogram object.
#'
#' @param Chromatogram Chromatogram object.
#' @param xlim xlim.
#' @param ylim ylim.
#' @param intColour intColour.
#' @param intLinewidth intLineidth.
#' @param axis.title.size Size of coordinate axis headings.
#' @param axis.text.size Size of axis labels.
#' @param title.size Title Size.
#' @param text.size Size of Q3-Q1 text.
#' @param text.colour Colour of Q3-Q1 text
#' @param fillColour Colour of peaks.
#' @param targetPeak Whether to plot only the target peaks.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' plotChromatogram(MChromatograms[42,8], text.colour = "purple")
plotChromatogram <- function(Chromatogram,
                             xlim = NA, ylim = NA,
                             intColour = "black", intLinewidth = 1,
                             axis.title.size = 15, axis.text.size = 10,
                             title.size = 15, text.size = 15, text.colour = "red",
                             fillColour = "gray", targetPeak = TRUE){
  p <- .plotFun(int = Chromatogram@intensity, rt = Chromatogram@rtime,
                xlim = xlim, ylim = ylim,
                intColour = intColour, intLinewidth = intLinewidth,
                axis.title.size = axis.title.size, axis.text.size = axis.text.size)
  p$data$baseline <- attributes(Chromatogram)$baseline
  sample_name <- basename(attributes(Chromatogram)$sample_name)
  chrInfo <- attributes(Chromatogram)$chrInfo
  if(!is.null(attributes(Chromatogram)$analyteName)) analyte_name <- attributes(Chromatogram)$analyteName
  else analyte_name <- purrr::pluck(strsplit(regmatches(chrInfo$chromatogramId, regexpr("name=(.+)", chrInfo$chromatogramId, perl = TRUE)), "name=")[[1]], 2)
  p <- p +
    ggplot2::labs(title = analyte_name, subtitle = sample_name) +
    ggplot2::theme(title = ggplot2::element_text(size = title.size)) +
    ggplot2::annotation_custom(
      grob = grid::textGrob(paste0(Chromatogram@precursorMz[1], " - ", Chromatogram@productMz[1]),
                            x = grid::unit(0.05, "npc"),  # 使用相对单位
                            y = grid::unit(0.95, "npc"),  # 使用相对单位
                            just = c("left", "top"),
                            gp = grid::gpar(col = text.colour, fontsize = text.size))
    ) +
    ggplot2::geom_line(ggplot2::aes(y = attributes(Chromatogram)$baseline), col = "red", linewidth = 1, linetype = "dashed") +
    ggplot2::annotate("segment", x = min(Chromatogram@rtime), xend = max(Chromatogram@rtime), y = attributes(Chromatogram)$noise, yend = attributes(Chromatogram)$noise, linetype = "dashed")
  if(targetPeak){
    peaksInfo <- attributes(Chromatogram)$targetPeak
  }else{
    peaksInfo <- attributes(Chromatogram)$peaksInfo
  }
  if(!is.null(peaksInfo)){
    for(i in 1:length(peaksInfo)){
      #browser()
      df <- p$data
      peakInfo <- peaksInfo[[i]]
      x_start <- peakInfo["start"];x_end <- peakInfo["end"]
      df_new <- subset(df, rt == x_start | rt == x_end)
      p <- p +
        ggplot2::geom_ribbon(data = subset(df, rt >= x_start & rt <= x_end),
                             ggplot2::aes(ymin = baseline, ymax = int), fill = fillColour, alpha = 0.5) +
        ggplot2::geom_point(data = subset(df, rt == x_start | rt == x_end),
                            ggplot2::aes(x = rt, y = int), col = "black") +
        ggplot2::annotate("segment", x = df_new$rt, xend = df_new$rt, y = df_new$baseline, yend = df_new$int, linetype = "dashed")
    }
  }
  p
}

.min_matrix_dimensions <- function(n) {
  if (n <= 0) {
    stop("请输入一个正整数")
  }

  # 计算可能的行数
  possible_rows <- 1:floor(sqrt(n))

  # 使用 sapply 来找到能整除 n 的行数
  divisors <- sapply(possible_rows, function(i) {
    if (n %% i == 0) {
      return(i)
    } else {
      return(NA)
    }
  })

  # 移除 NA 值
  divisors <- na.omit(divisors)

  # 选择最后一个合适的行数
  rows <- tail(divisors, 1)
  cols <- n / rows

  return(c(rows, cols))
}
#' @title plotMChromatograms
#' @description
#' Plot a MChromatograms object.
#'
#' @param MChromatograms MChromatograms object.
#' @param rows rows.
#' @param cols cols.
#' @param ncol The number of picture col.
#' @param xlim xlim.
#' @param ylim ylim.
#' @param intColour intColour.
#' @param intLinewidth intLineidth.
#' @param axis.title.size Size of coordinate axis headings.
#' @param axis.text.size Size of axis labels.
#' @param title.size Title Size.
#' @param text.size Size of Q3-Q1 text.
#' @param text.colour Colour of Q3-Q1 text
#' @param fillColour Colour of peaks.
#' @param targetPeak Whether to plot only the target peaks.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' plotMChromatograms(MChromatograms, rows = 1, cols = 1:4, ncol = 2, title.size = 12)
plotMChromatograms <- function(MChromatograms, rows, cols, ncol = NA,
                               xlim = NA, ylim = NA,
                               intColour = "black", intLinewidth = 1,
                               axis.title.size = 15, axis.text.size = 10,
                               title.size = 15, text.size = 15, text.colour = "red",
                               fillColour = "gray", targetPeak = TRUE){
  p_list <- lapply(rows, function(i) {
    lapply(cols, function(j) {
      plotChromatogram(MChromatograms[i, j],
                       xlim = xlim, ylim = ylim,
                       intColour = intColour, intLinewidth = intLinewidth,
                       axis.title.size = axis.title.size, axis.text.size = axis.text.size,
                       title.size = title.size, text.size = text.size, text.colour = text.colour,
                       fillColour = fillColour, targetPeak = targetPeak)
    })
  })
  p_list <- purrr::list_flatten(p_list)
  if(is.na(ncol)) ncol <- .min_matrix_dimensions(length(p_list))[2]
  else ncol <- ncol
  cowplot::plot_grid(plotlist = p_list, ncol = ncol)
}
