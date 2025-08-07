# chromatogram class
# Barry Song
# 250724

#' @title chromatogram
#' @description
#' The chromatogram is an R object that stores all the information in a MRM window, including the
#' retention time and intensity used to construct a chromatogram, as well as the basic information
#' and peaks for this MRM window.
#' @export
#' @examples
#' rtime <- seq(50, 200, 0.5)
#' intensity <- findChromPeaks::gaussian_peak(rtime = rtime, peakRt = 100, peakWidth = 20, peakHeight = 10000)
#' chr <- chromatogram$new(rtime = rtime, intensity = intensity, Q1 = 200.321, Q3 = 100.123, analyteName = "test1", windowName = "test1", expectRt = 100, analyteType = "IS")
chromatogram <- R6::R6Class(
  classname = "chromatogram",
  class = TRUE, portable = TRUE, cloneable = TRUE, lock_objects = TRUE, lock_class = TRUE,

  public = list(
    #' @field rtime `numeric()`, retention time of chromatogram
    rtime = NULL,
    #' @field intensity `numeric()`, intensity of chromatogram
    intensity = NULL,
    #' @field peaks `matrix()`, peaks information of chromatogram
    peaks = NULL,
    #' @field targetPeak `matrix()`, target peak of this window
    targetPeak = NULL,
    #' @field rt_diff_tol `numeric()`, tolerance for retention time differences between two peaks that are same analytes
    rt_diff_tol = NULL,
    #' @field rtdifference `numeric()`, difference between rt of targetPeak and expectRt
    rtdifference = NULL,
    #' @field Q1 `numeric(1)`, Q1 for MRM window
    Q1 = NULL,
    #' @field Q3 `numeric(1)`, Q3 for MRM window
    Q3 = NULL,
    #' @field analyteName `character(1)`, analyte's name
    analyteName = NULL,
    #' @field windowName `character(1)`, window's name
    windowName = NULL,
    #' @field expectRt `numeric(1)`, expect rt of target peak in MRM window, it is based on the reference experiment
    expectRt = NULL,
    #' @field analyteType `character(1)` with IS or Analyte
    analyteType = NULL,
    #' @field relatedIS `chrarcter(1)`, IS name, if analyteType is IS, it is NA
    relatedIS = NULL,
    #' @field sampleName `character(1)`, sample name
    sampleName = NULL,
    #' @field peakwidth `numeric(2)` with the lower and upper boun of the expected peak width.
    peakwidth = NULL,
    #' @field snthresh `numeric(1)` defining the signal to noise ratio cutoff.
    snthresh = NULL,
    #' @field minPs `integer(1)`, the ROI region requires a minimum of minPs of signals greater than the noise.
    minPs = NULL,
    #' @field noise `numeric(1)`, noise of chromatogram.
    noise = NULL,
    #' @field estimateNoise `logical(1)`, whether to estimate noise
    estimateNoise = NULL,
    #' @field extendLengthMSW `logical(1)`, please see: [xcms::centWave]
    extendLengthMSW = NULL,
    #' @field r2thresh `numeric(1)` threshold of peak shape.
    r2thresh = NULL,
    #' @field csthresh `numeric(1)` threshold of cs.
    csthresh = NULL,
    #' @field rtshift `numeric(1)`, retention time shift
    rtshift = NULL,
    #' @field rtcorrect `numeric(1)`, record the difference from the last rt correction for use in restoring the rt
    rtcorrect = NULL,

    #' @description
    #' Creates a new instance of chromatogram
    #' @param rtime `numeric()`, retention time of chromatogram
    #' @param intensity `numeric()`, intensity of chromatogram
    #' @param Q1 `numeric(1)`, Q1 for MRM window
    #' @param Q3 `numeric(1)`, Q3 for MRM window
    #' @param analyteName `character(1)`, analyte's name
    #' @param windowName `character(1)`, window's name
    #' @param expectRt `numeric(1)`, expect rt of target peak in MRM window
    #' @param analyteType `character(1)` with IS or Analyte
    #' @param relatedIS `chrarcter(1)`, IS name, if analyteType is IS, it is NA
    #' @param sampleName `character(1)`, sample name
    initialize = function(rtime, intensity, Q1, Q3, analyteName, windowName, expectRt, analyteType, relatedIS, sampleName){
      if(length(rtime) != length(intensity)){
        warnings("The length of rtime does not match the length of intensity")
        self$rtime <- numeric()
        self$intensity <- numeric()
      }else{
        self$rtime <- rtime
        self$intensity <- intensity
      }
      self$Q1 <- Q1
      self$Q3 <- Q3
      self$analyteName <- analyteName
      self$windowName <- windowName
      self$expectRt <- expectRt
      self$analyteType <- analyteType
      self$relatedIS <- relatedIS
      self$sampleName <- sampleName
    },

    #' @description
    #' Print chromatogram instance
    print = function(){
      if(is.null(self$peaks)) pn <- 0
      else pn <- nrow(self$peaks)
      cat(paste0("analyte name: ", self$analyteName, "\n",
                 "window name: ", self$windowName, "\n",
                 "analyte type: ", self$analyteType, "\n",
                 "peaks: ", pn, "\n"))
    },

    #' @description
    #' Find chromatographic peaks using CentWave
    #' @param peakwidth `numeric(2)` with the lower and upper boun of the expected peak width.
    #' @param snthresh `numeric(1)` defining the signal to noise ratio cutoff.
    #' @param minPs `integer(1)`, the ROI region requires a minimum of minPs of signals greater than the noise.
    #' @param noise `numeric(1)`, noise of chromatogram.
    #' @param estimateNoise `logical(1)`, whether to estimate noise
    #' @param extendLengthMSW `logical(1)`, please see: [xcms::centWave]
    #' @param r2thresh `numeric(1)` threshold of peak shape.
    #' @param csthresh `numeric(1)` threshold of cs.
    findPeaks_chr = function(peakwidth = c(5, 20), snthresh = 10, minPs = 3, noise = 100, estimateNoise = TRUE, extendLengthMSW = TRUE, r2thresh = 0.6, csthresh = 0.2){
      if(is.null(self$intensity) | is.null(self$rtime)){
        message("intensity and rtime can not be null when peak picking")
        return()
      }
      if(length(self$intensity) == 0 | length(self$rtime) == 0){
        message("intensity and rtime length can not be 0 when peak picking")
        return()
      }
      ps <- findChromPeaks::findChromPeaks_CWT(int = self$intensity, rt = self$rtime,
                                               peakwidth = peakwidth, snthresh = snthresh, minPs = minPs, noise = noise,
                                               estimateNoise = estimateNoise, extendLengthMSW = extendLengthMSW,
                                               r2thresh = r2thresh, csthresh = csthresh)
      self$peaks <- ps
      # Each change to the peak picking parameters means running the peak picking function again
      self$peakwidth <- peakwidth
      self$snthresh <- snthresh
      self$minPs <- minPs
      self$noise <- noise
      self$estimateNoise <- estimateNoise
      self$extendLengthMSW <- extendLengthMSW
      self$r2thresh <- r2thresh
      self$csthresh <- csthresh
    },

    #' @description
    #' Extract targte peak in a chrmatogram
    #' @param rt `numeric(1)`, rt of target peak, if it is NULL, rt will be expectRt
    #' @param rt_diff_tol `numeric(1)`, tolerance for retention time differences between two peaks that are same analytes
    extract_targetPeak_chr = function(rt = NULL, rt_diff_tol = 10){
      self$targetPeak <- NULL
      self$rtdifference <- NULL
      if(is.null(rt)) rt <- self$expectRt
      if(!is.null(self$peaks) & !is.null(rt)){
        if(nrow(self$peaks) != 0){
          ps <- self$peaks[abs(self$peaks[, "rt"] - rt) < rt_diff_tol, , drop = FALSE]
          if(nrow(ps) > 0){
            self$targetPeak <- ps[which.min(abs(ps[, "rt"] - rt)), , drop = FALSE]
            self$rtdifference <- as.numeric(self$targetPeak[1, "rt"] - rt)
          }
        }
      }
      self$rt_diff_tol <- rt_diff_tol
    },

    #' @description
    #' Calculate retention time shift based on target peak of IS winodw
    #' @param chr_grid `ChrGrid()`, the ChrGrid to which this chromatogram belongs
    cal_rtshift_chr = function(chr_grid = NULL){
      if(self$analyteType == "IS"){
        tp <- self$targetPeak
        if(!is.null(tp)){
          if(nrow(tp) == 1){
            self$rtshift <- as.numeric(tp[1, "rt"] - self$expectRt)
          }else{
            self$rtshift <- NULL
          }
        }else{
          self$rtshift <- NULL
        }
      }
      else{
        if(!is.null(chr_grid)){
          i_IS <- which(chr_grid$windowInfo$analyteName == self$relatedIS)
          j_IS <- which(chr_grid$sampleInfo$sampleName == self$sampleName)
          chr_IS <- chr_grid$get(i_IS, j_IS)
          self$rtshift <- chr_IS$rtshift
        }else{
          warning("if you want to calculate an analyte, you need provide ChrGrid")
        }
      }
    },

    #' @description
    #' Correct retention time shift
    correct_rtshift_chr = function(){
      if(!is.null(self$rtshift) & is.null(self$rtcorrect)){
        self$rtime <- self$rtime - self$rtshift
        self$peaks[, "rt"] <- self$peaks[, "rt"] - self$rtshift
        self$peaks[, "rtmin"] <- self$peaks[, "rtmin"] - self$rtshift
        self$peaks[, "rtmax"] <- self$peaks[, "rtmax"] - self$rtshift
        self$targetPeak[, "rt"] <- self$targetPeak[, "rt"] - self$rtshift
        self$targetPeak[, "rtmin"] <- self$targetPeak[, "rtmin"] - self$rtshift
        self$targetPeak[, "rtmax"] <- self$targetPeak[, "rtmax"] - self$rtshift
        self$rtcorrect <- self$rtshift
      }
    },

    #' @description
    #' Restore retention time from correction
    drop_rtshift_chr = function(){
      if(!is.null(self$rtcorrect)){ # has been drop or nerver be corrected
        self$rtime <- self$rtime + self$rtcorrect
        self$peaks[, "rt"] <- self$peaks[, "rt"] + self$rtcorrect
        self$peaks[, "rtmin"] <- self$peaks[, "rtmin"] + self$rtcorrect
        self$peaks[, "rtmax"] <- self$peaks[, "rtmax"] + self$rtcorrect
        self$targetPeak[, "rt"] <- self$targetPeak[, "rt"] + self$rtcorrect
        self$targetPeak[, "rtmin"] <- self$targetPeak[, "rtmin"] + self$rtcorrect
        self$targetPeak[, "rtmax"] <- self$targetPeak[, "rtmax"] + self$rtcorrect
        self$rtcorrect <- NULL
      }
    },

    #' @description
    #' Plot chromatogram
    #' @param target `logical(1)`, whether to plot only the target peak
    plot_chr = function(target = FALSE){
      df <- data.frame(int = self$intensity, rt = self$rtime)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = rt)) +
        ggplot2::geom_line(ggplot2::aes(y = int), col = "black", linewidth = 1) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Retention Time", y = "Intensity") +
        ggplot2::annotate("text", x = Inf, y = Inf, label = paste0(self$Q1, " - ", self$Q3), color = "red", size = 3,
                          hjust = 1.1, vjust = 1.1)
        # ggplot2::annotation_custom(
        #   grob = grid::textGrob(paste0(self$Q1, " - ", self$Q3, "\n",
        #                                self$analyteName, "\n",
        #                                self$windowName, "\n",
        #                                self$sampleName),
        #                         x = grid::unit(0.05, "npc"),  # 使用相对单位
        #                         y = grid::unit(0.95, "npc"),  # 使用相对单位
        #                         just = c("left", "top"),
        #                         gp = grid::gpar(col = "red", fontsize = 9))
        # )
      if(target){
        peaksInfo <- self$targetPeak
      }else{
        peaksInfo <- self$peaks
      }
      if(!is.null(peaksInfo)){
        if(nrow(peaksInfo) != 0){
          for(i in 1:nrow(peaksInfo)){
            x_start <- peaksInfo[i, "rtmin"]
            x_end <- peaksInfo[i, "rtmax"]
            df_new <- subset(df, rt == x_start | rt == x_end)
            p <- p +
              ggplot2::geom_ribbon(data = subset(df, rt >= x_start & rt <= x_end),
                                   ggplot2::aes(ymin = 0, ymax = int), fill = "grey", color = NA, alpha = 0.5) +
              ggplot2::annotate("segment", x = df_new$rt, xend = df_new$rt, y = 0, yend = df_new$int,
                                    linetype = "dashed", color = "red", linewidth = 1)
          }
        }
      }
      p
    }
  )
)
