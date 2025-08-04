# ChrGrid class
# Barry Song
# 250724

#' @title ChrGrid Object
#' @description
#' ChrGrid stores all chromatograms in a matrix format, with the number of rows corresponding to the number of target
#' analytes and the number of columns corresponding to the number of samples.
#' @export
#' @examples
#' chr_grid <- ChrGrid$new(m = 2, n = 2, chrs_list = list(chr, chr, chr, chr))
#' chr_grid$findPeaks_ChrGrid()
ChrGrid <- R6::R6Class(
  classname = "ChrGrid",
  class = TRUE, portable = TRUE, cloneable = FALSE, lock_objects = TRUE, lock_class = TRUE,

  public = list(
    #' @field chrs_list `list(m*n)`, with m * n chrmatrogams object.
    chrs_list = NULL,
    #' @field dim `integer(2)` c(m,n), m is the number of target analytes and n is the number of samples.
    dim = NULL,
    #' @field unit `character(1)`, retention time unit, min or sec
    unit = NULL,
    #' @field dataDir `character()`, raw data folder (shiny)
    dataDir = NULL,
    #' @field windowInfoPath `character()`, window information path (shiny)
    windowInfoPath = NULL,
    #' @field windowInfo `data.frame()`, user's window information
    windowInfo = NULL,
    #' @field sampleInfoPath `character()`, sample information path (shiny)
    sampleInfoPath = NULL,
    #' @field sampleInfo `data.frame()`, user's sample information
    sampleInfo = NULL,

    #' @description
    #' Create a new instance of ChrGrid
    #' @param m `integer(1)`, the number of analytes
    #' @param n `integer(1)`, the number of samples
    #' @param unit `character(1)`, retention time unit, min or sec
    #' @param chrs_list `list()`, the list of chromatogram instances
    #' @param windowInfo `data.frame()`, user's window information
    #' @param sampleInfo `data.frame()`, user's sample information
    initialize = function(m, n, unit = c("min", "sec"), chrs_list, windowInfo = NULL, sampleInfo = NULL){
      self$dim <- c(m, n)
      self$unit <- match.arg(unit)
      self$windowInfo <- windowInfo
      self$sampleInfo <- sampleInfo
      if(length(chrs_list) != m*n) stop("The length of chrs_list must match m*n!")
      self$chrs_list <- chrs_list
    },

    #' @description
    #' Obtain chromatogram based on index
    #' @param i `integer(1)`, ith analyte
    #' @param j `integer(1)`, jth sample
    get = function(i, j){
      if(missing(i) & missing(j)){
        return(self$chrs_matrix)
      }
      if(missing(i) | missing(j)){
        stop("You need to specify the chrs you want to obtain.")
      }
      self$chrs_list[[(j - 1) * self$dim[1] + i]]
    },

    #' @description
    #' Set chromatogram based on index
    #' @param i `integer(1)`, ith analyte
    #' @param j `integer(1)`, jth sample
    #' @param chr `chromatogram()`, new chromatogram instance
    set = function(i, j, chr){
      if(missing(i) | missing(j)){
        stop("Both indices must be provided for assignment")
      }
      self$chrs_list[[(j - 1) * self$dim[1] + i]] <- chr
      invisible(self)
    },

    #' @description
    #' Print ChrGrid instance
    print = function(){
      cat("ChrGrid with dimensions:", self$dim[1], "x", self$dim[2], "\n")
    },

    #' @description
    #' Find peaks in ChrGrid
    #' @param peakwidth `numeric(2)` with the lower and upper boun of the expected peak width.
    #' @param snthresh `numeric(1)` defining the signal to noise ratio cutoff.
    #' @param minPs `integer(1)`, the ROI region requires a minimum of minPs of signals greater than the noise.
    #' @param noise `numeric(1)`, noise of chromatogram.
    #' @param estimateNoise `logical(1)`, whether to estimate noise
    #' @param extendLengthMSW `logical(1)`, please see: [xcms::centWave]
    #' @param r2thresh `numeric(1)` threshold of peak shape.
    #' @param csthresh `numeric(1)` threshold of cs.
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    findPeaks_ChrGrid = function(peakwidth = c(5, 20), snthresh = 10, minPs = 3, noise = 100, estimateNoise = TRUE, extendLengthMSW = TRUE, r2thresh = 0.6, csthresh = 0.2,
                                 thread = 1, shinyProgress = NULL){
      if(is.null(shinyProgress)){
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
          total = length(self$chrs_list),
          width = 60
        )
        progress_update <- function(nn){
          pb$tick()
        }
      }else{
        maxValue <- shinyProgress$getMax()
        if(maxValue != length(self$chrs_list)) stop("maxValue != length(chr_grid$chrs_list)")
        progress_update <- function(nn){
          shinyProgress$set(value = nn, message = "Find peaks...: ", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr = self$chrs_list, nn = 1:length(self$chrs_list),
                                                   .options.snow = opts),
                                  {
                                    chr$findPeaks_chr(peakwidth = peakwidth, snthresh = snthresh, minPs = minPs, noise = noise,
                                                      estimateNoise = estimateNoise, extendLengthMSW = extendLengthMSW,
                                                      r2thresh = r2thresh, csthresh = csthresh)
                                    chr
                                  })
      snow::stopCluster(cl)
      gc()
      self$chrs_list <- resLt
    },

    #' @description
    #' Extend ChrGrid based on windowInfo.
    #' The extension needs to be executed because the number of analytes may be greater than the number of windows
    extend_ChrGrid = function(){
      if(self$unit == "min") mag <- 60
      else mag <- 1
      n <- nrow(self$sampleInfo)
      m <- nrow(self$windowInfo)
      resLt <- lapply(1:n, function(j) {
        current_windowName_vec <- sapply(1:self$dim[1], function(k) {
          self$get(k, j)$windowName
        })
        sampleName <- self$sampleInfo[j, "sampleName"]
        lapply(1:m, function(i) {
          analyteName <- self$windowInfo[i, "analyteName"]
          windowName <- self$windowInfo[i, "windowName"]
          expectRt <- self$windowInfo[i, "expectRt"] * mag
          analyteType <- self$windowInfo[i, "analyteType"]
          relatedIS <- self$windowInfo[i, "relatedIS"]
          l <- match(windowName, current_windowName_vec)
          if(length(l) == 1){
            chr_tmp <- self$get(l, j)$clone()
            chr_tmp$analyteName <- analyteName
            chr_tmp$expectRt <- expectRt
            chr_tmp$analyteType <- analyteType
            chr_tmp$relatedIS <- relatedIS
            return(chr_tmp)
          }else if(length(l) == 0){
            return(chromatogram$new(rtime = numeric(), intensity = numeric(),
                                    Q1 = self$windowInfo[i, "Q1"], Q3 = self$windowInfo[i, "Q3"],
                                    analyteName = analyteName, windowName = windowName,
                                    expectRt = expectRt, analyteType = analyteType, relatedIS = relatedIS,
                                    sampleName = sampleName))
          }else{
            stop("Multi match: ", windowName)
          }
        })
      })
      self$dim <- c(m, n)
      self$chrs_list <- unlist(resLt)
    },

    #' @description
    #' Extract targte peak in chrmatograms of ChrGrid
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    #' @param rt `numeric(1)`, rt of target peak, if it is NULL, rt will be expectRt
    #' @param rt_diff_tol `numeric(1)`, tolerance for retention time differences between two peaks that are same analytes
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    extract_targetPeak_ChrGrid = function(i, j, rt = NULL, rt_diff_tol = 10,
                                          thread = 1, shinyProgress = NULL){
      if(missing(i) & missing(j)){
        i_seq <- 1:self$dim[1]
        j_seq <- 1:self$dim[2]
      }else if(!missing(i) & missing(j)){
        i_seq <- i
        j_seq <- 1:self$dim[2]
      }else if(missing(i) & !missing(j)){
        i_seq <- 1:self$dim[1]
        j_seq <- j
      }else{
        i_seq <- i
        j_seq <- j
      }
      index <- lapply(j_seq, function(j_){
        (j_ - 1) * self$dim[1] + i_seq
      })
      chr_list_tmp <- lapply(index, function(x) {
        self$chrs_list[x]
      })
      if(is.null(shinyProgress)){
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
          total = length(j_seq),
          width = 60
        )
        progress_update <- function(nn){
          pb$tick()
        }
      }else{
        maxValue <- shinyProgress$getMax()
        if(maxValue != length(j_seq)) stop("maxValue != length(j_seq)")
        progress_update <- function(nn){
          shinyProgress$set(value = nn, message = "Extract target peak...", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, nn = 1:length(j_seq),
                                                   .options.snow = opts),
                                  {
                                    lapply(chr_list, function(chr){
                                      chr$extract_targetPeak_chr(rt = rt, rt_diff_tol = rt_diff_tol)
                                      chr
                                    })
                                  })
      snow::stopCluster(cl)
      gc()
      self$chrs_list[index] <- unlist(resLt)
    },

    #' @description
    #' Calculate retention time shift based on target peak of IS window
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    cal_rtshift = function(i, j){
      IS_i <- which(self$windowInfo$analyteType == "IS")
      IS_j <- 1:self$dim[2]
      IS_name <- self$windowInfo$analyteName[IS_i]

      analyte_i <- which(self$windowInfo$analyteType != "IS")
      analyte_j <- 1:self$dim[2]

      if(missing(i) & missing(j)){
        IS_i_ <- IS_i
        IS_j_ <- IS_j

        analyte_i_ <- analyte_i
        analyte_j_ <- analyte_j
        relatedIS_name <- self$windowInfo$relatedIS[analyte_i_]
        relatedIS_i <- match(relatedIS_name, IS_name)
      }else if(!missing(i) & missing(j)){
        IS_i_ <- IS_i[IS_i %in% i]
        IS_j_ <- IS_j

        analyte_i_ <- analyte_i[analyte_i %in% i]
        analyte_j_ <- analyte_j
        relatedIS_name <- self$windowInfo$relatedIS[analyte_i_]
        relatedIS_i <- match(relatedIS_name, IS_name)
      }else if(missing(i) & !missing(j)){
        IS_i_ <- IS_i
        IS_j_ <- IS_j[IS_j %in% j]

        analyte_i_ <- analyte_i
        analyte_j_ <- analyte_j[analyte_j %in% j]
        relatedIS_name <- self$windowInfo$relatedIS[analyte_i_]
        relatedIS_i <- match(relatedIS_name, IS_name)
      }else{
        IS_i_ <- IS_i[IS_i %in% i]
        IS_j_ <- IS_j[IS_j %in% j]

        analyte_i_ <- analyte_i[analyte_i %in% i]
        analyte_j_ <- analyte_j[analyte_j %in% j]
        relatedIS_name <- self$windowInfo$relatedIS[analyte_i_]
        relatedIS_i <- match(relatedIS_name, IS_name)
      }
      if(length(IS_i_) >0 & length(IS_j_) > 0){
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
          total = length(IS_i_) * length(IS_j_),
          width = 60
        )
        message("Calculate rtshift of IS...")
        for(i_ in IS_i_){
          for(j_ in IS_j_){
            pb$tick()
            chr_tmp <- self$get(i_, j_) # not copy
            tp <- chr_tmp$targetPeak
            if(is.null(tp)){
              message(paste0(i_, "-", j_, " do not have target peak"))
              chr_tmp$rtshift <- NULL
              next
            }
            if(nrow(tp) == 1){
              chr_tmp$rtshift <-  as.numeric(tp[1, "rt"] - chr_tmp$expectRt)
            }else{
              message(paste0(i, "-", j, " do not have target peak"))
              chr_tmp$rtshift <- NULL
            }
          }
        }
      }
      if(length(analyte_i_) > 0 & length(analyte_j_) > 0){
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
          total = length(analyte_i_) * length(analyte_j_),
          width = 60
        )
        message("Assign rtshift for analyte...")
        for(l in 1:length(analyte_i_)){
          for(j_ in analyte_j_){
            pb$tick()
            i_ <- analyte_i_[l]
            chr_IS <- self$get(relatedIS_i[l], j_)
            chr_analyte <- self$get(i_, j_)
            chr_analyte$rtshift <- chr_IS$rtshift
          }
        }
      }
    },

    #' @description
    #' Correct retention time shift
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    correct_rtshift = function(i, j){
      if(missing(i) & missing(j)){
        i_seq <- 1:self$dim[1]
        j_seq <- 1:self$dim[2]
      }else if(!missing(i) & missing(j)){
        i_seq <- i
        j_seq <- 1:self$dim[2]
      }else if(missing(i) & !missing(j)){
        i_seq <- 1:self$dim[1]
        j_seq <- j
      }else{
        i_seq <- i
        j_seq <- j
      }
      pb <- progress::progress_bar$new(
        format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
        total = length(i_seq) * length(j_seq),
        width = 60
      )
      for(i_ in i_seq){
        for(j_ in j_seq){
          pb$tick()
          chr_tmp <- self$get(i_,j_)
          if(!is.null(chr_tmp$rtcorrect)){
            message(paste0(i_, "-", j_, " has been correct"))
            next
          }
          if(is.null(chr_tmp$rtshift)) next
          chr_tmp$rtime <- chr_tmp$rtime - chr_tmp$rtshift
          chr_tmp$peaks[, "rt"] <- chr_tmp$peaks[, "rt"] - chr_tmp$rtshift
          chr_tmp$peaks[, "rtmin"] <- chr_tmp$peaks[, "rtmin"] - chr_tmp$rtshift
          chr_tmp$peaks[, "rtmax"] <- chr_tmp$peaks[, "rtmax"] - chr_tmp$rtshift
          chr_tmp$targetPeak[, "rt"] <- chr_tmp$targetPeak[, "rt"] - chr_tmp$rtshift
          chr_tmp$targetPeak[, "rtmin"] <- chr_tmp$targetPeak[, "rtmin"] - chr_tmp$rtshift
          chr_tmp$targetPeak[, "rtmax"] <- chr_tmp$targetPeak[, "rtmax"] - chr_tmp$rtshift
          chr_tmp$rtcorrect <- chr_tmp$rtshift
        }
      }
    },

    #' @description
    #' Restore retention time from correction
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    drop_rtshift = function(i, j){
      if(missing(i) & missing(j)){
        i_seq <- 1:self$dim[1]
        j_seq <- 1:self$dim[2]
      }else if(!missing(i) & missing(j)){
        i_seq <- i
        j_seq <- 1:self$dim[2]
      }else if(missing(i) & !missing(j)){
        i_seq <- 1:self$dim[1]
        j_seq <- j
      }else{
        i_seq <- i
        j_seq <- j
      }
      pb <- progress::progress_bar$new(
        format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
        total = length(i_seq) * length(j_seq),
        width = 60
      )
      for(i_ in i_seq){
        for(j_ in j_seq){
          pb$tick()
          chr_tmp <- self$get(i_,j_)
          if(is.null(chr_tmp$rtcorrect)) next
          chr_tmp$rtime <- chr_tmp$rtime + chr_tmp$rtcorrect
          chr_tmp$peaks[, "rt"] <- chr_tmp$peaks[, "rt"] + chr_tmp$rtcorrect
          chr_tmp$peaks[, "rtmin"] <- chr_tmp$peaks[, "rtmin"] + chr_tmp$rtcorrect
          chr_tmp$peaks[, "rtmax"] <- chr_tmp$peaks[, "rtmax"] + chr_tmp$rtcorrect
          chr_tmp$targetPeak[, "rt"] <- chr_tmp$targetPeak[, "rt"] + chr_tmp$rtcorrect
          chr_tmp$targetPeak[, "rtmin"] <- chr_tmp$targetPeak[, "rtmin"] + chr_tmp$rtcorrect
          chr_tmp$targetPeak[, "rtmax"] <- chr_tmp$targetPeak[, "rtmax"] + chr_tmp$rtcorrect
          chr_tmp$rtcorrect <- NULL
        }
      }
    }
  )
)
