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
    #' @field extended `logical(1)`, whether extended
    extended = FALSE,
    #' @field stdcurve_list `list()` a list of all StdCurve
    stdcurve_list = NULL,

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
      if(i > self$dim[1] | i < 1 | j > self$dim[2] | j < 1){
        message("Invalid i or j")
        return(NULL)
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
    #' Smooth intensity using Savitzky-Golay
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    #' @param p `integer(1)`, filter order
    #' @param n `integer(1)`, filter length (must be odd)
    #' @param m `integer(1)`, return the m-th derivative of the filter coefficients
    #' @param ts `integer(1)`, time scaling factor
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    smooth_ChrGrid = function(i, j,
                              p = 3, n = p + 3 - p%%2, m = 0, ts = 1,
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
        if(maxValue != length(chr_list_tmp)) stop("maxValue != length(j_seq)")
        progress_update <- function(nn){
          shinyProgress$set(value = nn, message = "Smooth... ", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, nn = 1:length(j_seq),
                                                   .options.snow = opts),
                                  {
                                    lapply(chr_list, function(chr) {
                                      chr$smooth_chr(p = p, n = n, m = m, ts = ts)
                                      chr
                                    })
                                  })
      snow::stopCluster(cl)
      gc()
      self$chrs_list[unlist(index)] <- unlist(resLt)
    },

    #' @description
    #' Desmooth intensity
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    desmooth_ChrGrid = function(i, j,
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
        if(maxValue != length(chr_list_tmp)) stop("maxValue != length(j_seq)")
        progress_update <- function(nn){
          shinyProgress$set(value = nn, message = "Desmooth... ", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, nn = 1:length(j_seq),
                                                   .options.snow = opts),
                                  {
                                    lapply(chr_list, function(chr) {
                                      chr$desmooth_chr()
                                      chr
                                    })
                                  })
      snow::stopCluster(cl)
      gc()
      self$chrs_list[unlist(index)] <- unlist(resLt)
    },

    #' @description
    #' Find peaks in ChrGrid
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
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
    findPeaks_ChrGrid = function(i, j,
                                 peakwidth = c(5, 20), snthresh = 10, minPs = 3, noise = 100, estimateNoise = TRUE, extendLengthMSW = TRUE, r2thresh = 0.6, csthresh = 0.2,
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
      index <- unlist(index)
      chr_list_tmp <- self$chrs_list[index]
      if(is.null(shinyProgress)){
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
          total = length(chr_list_tmp),
          width = 60
        )
        progress_update <- function(nn){
          pb$tick()
        }
      }else{
        maxValue <- shinyProgress$getMax()
        if(maxValue != length(chr_list_tmp)) stop("maxValue != length(chr_list_tmp)")
        progress_update <- function(nn){
          shinyProgress$set(value = nn, message = "Find peaks... ", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr = chr_list_tmp, nn = 1:length(chr_list_tmp),
                                                   .options.snow = opts),
                                  {
                                    chr$findPeaks_chr(peakwidth = peakwidth, snthresh = snthresh, minPs = minPs, noise = noise,
                                                      estimateNoise = estimateNoise, extendLengthMSW = extendLengthMSW,
                                                      r2thresh = r2thresh, csthresh = csthresh)
                                    chr
                                  })
      snow::stopCluster(cl)
      gc()
      self$chrs_list[index] <- resLt
    },

    #' @description
    #' Extend ChrGrid based on windowInfo.
    #' The extension needs to be executed because the number of analytes may be greater than the number of windows
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    extend_ChrGrid = function(thread = 1, shinyProgress = NULL){
      if(self$extended) return() # if self$extended == TRUE do nothing
      if(self$unit == "min") mag <- 60
      else mag <- 1
      n <- nrow(self$sampleInfo)
      m <- nrow(self$windowInfo)
      m_ <- self$dim[1]
      n_ <- self$dim[2]
      chr_list_tmp <- lapply(1:n_, function(j) {
        lapply(1:m_, function(i){
          self$get(i, j)
        })
      })
      windowInfo <- self$windowInfo
      sampleInfo <- self$sampleInfo
      if(is.null(shinyProgress)){
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
          total = n,
          width = 60
        )
        progress_update <- function(nn){
          pb$tick()
        }
      }else{
        maxValue <- shinyProgress$getMax()
        if(maxValue != n) stop("maxValue != nrow(sampleInfo)")
        progress_update <- function(nn){
          shinyProgress$set(value = nn, message = "Extend ChrGrid...", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, nn = 1:n_,
                                                   .options.snow = opts),
                                  {
                                    current_windowName_vec <- sapply(1:m_, function(k) {
                                      chr_list[[k]]$windowName
                                    })
                                    sampleName <- sampleInfo$sampleName[nn]
                                    lapply(1:m, function(i) {
                                      analyteName <- windowInfo$analyteName[i]
                                      windowName <- windowInfo$windowName[i]
                                      expectRt <- windowInfo$expectRt[i] * mag
                                      analyteType <- windowInfo$analyteType[i]
                                      relatedIS <- windowInfo$relatedIS[i]
                                      l <- match(windowName, current_windowName_vec)
                                      if(length(l) == 1){
                                        chr_tmp <- chr_list[[l]]$clone()
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
      snow::stopCluster(cl)
      gc()
      # resLt <- lapply(1:n, function(j) {
      #   current_windowName_vec <- sapply(1:self$dim[1], function(k) {
      #     self$get(k, j)$windowName
      #   })
      #   sampleName <- self$sampleInfo[j, "sampleName"]
      #   lapply(1:m, function(i) {
      #     analyteName <- self$windowInfo[i, "analyteName"]
      #     windowName <- self$windowInfo[i, "windowName"]
      #     expectRt <- self$windowInfo[i, "expectRt"] * mag
      #     analyteType <- self$windowInfo[i, "analyteType"]
      #     relatedIS <- self$windowInfo[i, "relatedIS"]
      #     l <- match(windowName, current_windowName_vec)
      #     if(length(l) == 1){
      #       chr_tmp <- self$get(l, j)$clone()
      #       chr_tmp$analyteName <- analyteName
      #       chr_tmp$expectRt <- expectRt
      #       chr_tmp$analyteType <- analyteType
      #       chr_tmp$relatedIS <- relatedIS
      #       return(chr_tmp)
      #     }else if(length(l) == 0){
      #       return(chromatogram$new(rtime = numeric(), intensity = numeric(),
      #                               Q1 = self$windowInfo[i, "Q1"], Q3 = self$windowInfo[i, "Q3"],
      #                               analyteName = analyteName, windowName = windowName,
      #                               expectRt = expectRt, analyteType = analyteType, relatedIS = relatedIS,
      #                               sampleName = sampleName))
      #     }else{
      #       stop("Multi match: ", windowName)
      #     }
      #   })
      # })
      self$dim <- c(m, n)
      self$chrs_list <- unlist(resLt)
      self$extended <- TRUE
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
      self$chrs_list[unlist(index)] <- unlist(resLt)
    },

    #' @description
    #' Calculate retention time shift based on target peak of IS window
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress_IS this parameter is used to receive shiny Progress instance
    #' @param shinyProgress_Analyte this parameter is used to receive shiny Progress instance
    cal_rtshift_ChrGrid = function(i, j, thread = 1, shinyProgress_IS = NULL, shinyProgress_Analyte = NULL){
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
        index <- lapply(IS_j_, function(j_){
          (j_ - 1) * self$dim[1] + IS_i_
        })
        chr_list_tmp <- lapply(index, function(x) {
          self$chrs_list[x]
        })
        if(is.null(shinyProgress_IS)){
          pb <- progress::progress_bar$new(
            format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
            total = length(IS_j_),
            width = 60
          )
          progress_update <- function(nn){
            pb$tick()
          }
        }else{
          maxValue <- shinyProgress_IS$getMax()
          if(maxValue != length(IS_j_)) stop("maxValue != length(IS_j_)")
          progress_update <- function(nn){
            shinyProgress_IS$set(value = nn, message = "Calculate rtshift for IS...: ", detail = paste0(nn, " / ", maxValue))
          }
        }
        message("Calculate rtshift of IS...")
        opts <- list(progress = progress_update)
        cl <- snow::makeCluster(thread)
        doSNOW::registerDoSNOW(cl)
        resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, nn = 1:length(IS_j_),
                                                     .options.snow = opts),
                                    {
                                      lapply(chr_list, function(chr_tmp){
                                        tp <- chr_tmp$targetPeak
                                        if(is.null(tp)){
                                          chr_tmp$rtshift <- NULL
                                          return(chr_tmp)
                                        }
                                        if(nrow(tp) == 1){
                                          chr_tmp$rtshift <-  as.numeric(tp[1, "rt"] - chr_tmp$expectRt)
                                        }else{
                                          chr_tmp$rtshift <- NULL
                                        }
                                        return(chr_tmp)
                                      })
                                    })
        snow::stopCluster(cl)
        gc()
        self$chrs_list[unlist(index)] <- unlist(resLt)
        # for(i_ in IS_i_){
        #   for(j_ in IS_j_){
        #     pb$tick()
        #     chr_tmp <- self$get(i_, j_) # not copy
        #     tp <- chr_tmp$targetPeak
        #     if(is.null(tp)){
        #       message(paste0(i_, "-", j_, " do not have target peak"))
        #       chr_tmp$rtshift <- NULL
        #       next
        #     }
        #     if(nrow(tp) == 1){
        #       chr_tmp$rtshift <-  as.numeric(tp[1, "rt"] - chr_tmp$expectRt)
        #     }else{
        #       message(paste0(i, "-", j, " do not have target peak"))
        #       chr_tmp$rtshift <- NULL
        #     }
        #   }
        # }
      }
      if(length(analyte_i_) > 0 & length(analyte_j_) > 0){
        index <- lapply(analyte_j_, function(j_) {
          (j_ - 1) * self$dim[1] + analyte_i_
        })
        chr_list_tmp <- lapply(index, function(x) {
          self$chrs_list[x]
        })
        index_IS <- lapply(analyte_j_, function(j_) {
          (j_ - 1) * self$dim[1] + relatedIS_i
        })
        chr_list_IS_tmp <- lapply(index_IS, function(x) {
          self$chrs_list[x]
        })
        if(is.null(shinyProgress_Analyte)){
          pb <- progress::progress_bar$new(
            format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
            total = length(analyte_j_),
            width = 60
          )
          progress_update <- function(nn){
            pb$tick()
          }
        }else{
          maxValue <- shinyProgress_Analyte$getMax()
          if(maxValue != length(analyte_j_)) stop("maxValue != length(analyte_j_)")
          progress_update <- function(nn){
            shinyProgress_Analyte$set(value = nn, message = "Calculate rtshift for Analyte...", detail = paste0(nn, " / ", maxValue))
          }
        }
        message("Assign rtshift for analyte...")
        opts <- list(progress = progress_update)
        cl <- snow::makeCluster(thread)
        doSNOW::registerDoSNOW(cl)
        resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, chr_list_IS = chr_list_IS_tmp, nn = 1:length(analyte_j_),
                                                     .options.snow = opts),
                                    {
                                      lapply(1:length(chr_list), function(l) {
                                        chr_analyte <- chr_list[[l]]
                                        chr_IS <- chr_list_IS[[l]]
                                        chr_analyte$rtshift <- chr_IS$rtshift
                                        chr_analyte
                                      })
                                    })
        snow::stopCluster(cl)
        gc()
        # for(l in 1:length(analyte_i_)){
        #   for(j_ in analyte_j_){
        #     pb$tick()
        #     i_ <- analyte_i_[l]
        #     chr_IS <- self$get(relatedIS_i[l], j_)
        #     chr_analyte <- self$get(i_, j_)
        #     chr_analyte$rtshift <- chr_IS$rtshift
        #   }
        # }
        self$chrs_list[unlist(index)] <- unlist(resLt)
      }
    },

    #' @description
    #' Correct retention time shift
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    correct_rtshift_ChrGrid = function(i, j, thread = 1, shinyProgress = NULL){
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
          shinyProgress$set(value = nn, message = "Correct rtshift...", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, nn = 1:length(j_seq),
                                                   .options.snow = opts),
                                  {
                                    lapply(chr_list, function(chr){
                                      if(!is.null(chr$rtcorrect)){
                                        return(chr)
                                      }
                                      if(is.null(chr$rtshift)){
                                        return(chr)
                                      }
                                      chr$rtime <- chr$rtime - chr$rtshift
                                      chr$peaks[, "rt"] <- chr$peaks[, "rt"] - chr$rtshift
                                      chr$peaks[, "rtmin"] <- chr$peaks[, "rtmin"] - chr$rtshift
                                      chr$peaks[, "rtmax"] <- chr$peaks[, "rtmax"] - chr$rtshift
                                      chr$targetPeak[, "rt"] <- chr$targetPeak[, "rt"] - chr$rtshift
                                      chr$targetPeak[, "rtmin"] <- chr$targetPeak[, "rtmin"] - chr$rtshift
                                      chr$targetPeak[, "rtmax"] <- chr$targetPeak[, "rtmax"] - chr$rtshift
                                      chr$rtdifference <- chr$rtdifference - chr$rtshift
                                      chr$rtcorrect <- chr$rtshift
                                      return(chr)
                                    })
                                  })
      snow::stopCluster(cl)
      gc()
      self$chrs_list[unlist(index)] <- unlist(resLt)
      # for(i_ in i_seq){
      #   for(j_ in j_seq){
      #     pb$tick()
      #     chr_tmp <- self$get(i_,j_)
      #     if(!is.null(chr_tmp$rtcorrect)){
      #       message(paste0(i_, "-", j_, " has been correct"))
      #       next
      #     }
      #     if(is.null(chr_tmp$rtshift)) next
      #     chr_tmp$rtime <- chr_tmp$rtime - chr_tmp$rtshift
      #     chr_tmp$peaks[, "rt"] <- chr_tmp$peaks[, "rt"] - chr_tmp$rtshift
      #     chr_tmp$peaks[, "rtmin"] <- chr_tmp$peaks[, "rtmin"] - chr_tmp$rtshift
      #     chr_tmp$peaks[, "rtmax"] <- chr_tmp$peaks[, "rtmax"] - chr_tmp$rtshift
      #     chr_tmp$targetPeak[, "rt"] <- chr_tmp$targetPeak[, "rt"] - chr_tmp$rtshift
      #     chr_tmp$targetPeak[, "rtmin"] <- chr_tmp$targetPeak[, "rtmin"] - chr_tmp$rtshift
      #     chr_tmp$targetPeak[, "rtmax"] <- chr_tmp$targetPeak[, "rtmax"] - chr_tmp$rtshift
      #     chr_tmp$rtcorrect <- chr_tmp$rtshift
      #   }
      # }
    },

    #' @description
    #' Restore retention time from correction
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    #' @param thread `integer(1)`, thread number in parallel
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    drop_rtshift_ChrGrid = function(i, j, thread = 1, shinyProgress = NULL){
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
          shinyProgress$set(value = nn, message = "Drop rtshift...", detail = paste0(nn, " / ", maxValue))
        }
      }
      opts <- list(progress = progress_update)
      cl <- snow::makeCluster(thread)
      doSNOW::registerDoSNOW(cl)
      resLt <- foreach::`%dopar%`(foreach::foreach(chr_list = chr_list_tmp, nn = 1:length(j_seq),
                                                   .options.snow = opts),
                                  {
                                    lapply(chr_list, function(chr){
                                      if(is.null(chr$rtcorrect)){ # has been drop
                                        return(chr)
                                      }
                                      chr$rtime <- chr$rtime + chr$rtcorrect
                                      chr$peaks[, "rt"] <- chr$peaks[, "rt"] + chr$rtcorrect
                                      chr$peaks[, "rtmin"] <- chr$peaks[, "rtmin"] + chr$rtcorrect
                                      chr$peaks[, "rtmax"] <- chr$peaks[, "rtmax"] + chr$rtcorrect
                                      chr$targetPeak[, "rt"] <- chr$targetPeak[, "rt"] + chr$rtcorrect
                                      chr$targetPeak[, "rtmin"] <- chr$targetPeak[, "rtmin"] + chr$rtcorrect
                                      chr$targetPeak[, "rtmax"] <- chr$targetPeak[, "rtmax"] + chr$rtcorrect
                                      chr$rtdifference <- chr$rtdifference + chr$rtshift
                                      chr$rtcorrect <- NULL
                                      return(chr)
                                    })
                                  })
      snow::stopCluster(cl)
      gc()
      self$chrs_list[unlist(index)] <- unlist(resLt)
      # for(i_ in i_seq){
      #   for(j_ in j_seq){
      #     pb$tick()
      #     chr_tmp <- self$get(i_,j_)
      #     if(is.null(chr_tmp$rtcorrect)) next
      #     chr_tmp$rtime <- chr_tmp$rtime + chr_tmp$rtcorrect
      #     chr_tmp$peaks[, "rt"] <- chr_tmp$peaks[, "rt"] + chr_tmp$rtcorrect
      #     chr_tmp$peaks[, "rtmin"] <- chr_tmp$peaks[, "rtmin"] + chr_tmp$rtcorrect
      #     chr_tmp$peaks[, "rtmax"] <- chr_tmp$peaks[, "rtmax"] + chr_tmp$rtcorrect
      #     chr_tmp$targetPeak[, "rt"] <- chr_tmp$targetPeak[, "rt"] + chr_tmp$rtcorrect
      #     chr_tmp$targetPeak[, "rtmin"] <- chr_tmp$targetPeak[, "rtmin"] + chr_tmp$rtcorrect
      #     chr_tmp$targetPeak[, "rtmax"] <- chr_tmp$targetPeak[, "rtmax"] + chr_tmp$rtcorrect
      #     chr_tmp$rtcorrect <- NULL
      #   }
      # }
    },

    #' @description
    #' Remove peaks and target peak information in ChrGrid
    #' @param i `integer()`, analyte index
    #' @param j `integer()`, sample index
    #' @param shinyProgress this parameter is used to receive shiny Progress instance
    blank_ChrGrid = function(i, j, shinyProgress = NULL){
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
      if(is.null(shinyProgress)){
        pb <- progress::progress_bar$new(
          format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
          total = length(i_seq) * length(j_seq),
          width = 60
        )
        progress_update <- function(nn){
          pb$tick()
        }
      }else{
        maxValue <- shinyProgress$getMax()
        if(maxValue != length(i_seq) * length(j_seq)) stop("maxValue != length(i_seq) * length(j_seq)")
        progress_update <- function(nn){
          shinyProgress$set(value = nn, message = "Blank...", detail = paste0(nn, " / ", maxValue))
        }
      }
      nn <- 1
      for(i in i_seq){
        for(j in j_seq){
          progress_update(nn)
          self$get(i, j)$blank_chr()
          nn <- nn + 1
        }
      }
    },

    #' @description
    #' Plot rtdifference picture
    #' @param i `integer(1)`, index of one analyte
    #' @param j `integer()`, sample index
    plot_rtdifference = function(i, j){
      if(missing(i)){
        stop("i is missing!")
      }
      if(missing(j)){
        j <- 1:self$dim[2]
      }
      rtdifference_vec <- sapply(j, function(j_) {
        rtdiff <- self$get(i, j_)$rtdifference
        if(is.null(rtdiff)) rtdiff <- NA
        rtdiff
      })
      df <- data.frame(
        j = j,
        rtdifference = rtdifference_vec
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(x = j, y = rtdifference)) +
        ggplot2::geom_point(size = 3, ) +
        ggplot2::geom_line() +
        ggplot2::theme_bw()
    },

    #' @description
    #' Specify the Quant index
    #' @param Quant_i `integer(1)`, Quant index of one analyte
    #' @param batchName `character(1)`, batch name
    #' @param areaType `character(1)`, into, intb or maxo
    #' @param weights `character(1)`, weights of standard curve
    #' @param delete `integer()`, which points need to be deleted form stdcurve_df
    #' @param zero `logical(1)`, does the standard curve pass through the zero point
    build_stdcurve = function(Quant_i, batchName, areaType = c("into", "intb", "maxo"),
                              weights = c("none", "1/x", "1/x^2"), delete = integer(), zero = FALSE){
      if(self$windowInfo$analyteType[Quant_i] == "IS") stop("Quant_i can not be IS!")
      if(!batchName %in% unique(self$sampleInfo$batchName)) stop("batchName do not exist!")
      weights <- match.arg(weights)
      areaType <- match.arg(areaType)
      j_std <- which(self$sampleInfo$typeName == "std" & self$sampleInfo$batchName == batchName)
      # Quant
      areaVec_Quant <- sapply(j_std, function(j) {
        area <- self$get(Quant_i, j)$targetPeak[1, areaType]
        if(is.null(area)) area <- NA
        area
      })
      # IS
      IS_name <- self$windowInfo$relatedIS[Quant_i]
      IS_i <- which(self$windowInfo$analyteName == IS_name)
      areaVec_IS <- sapply(j_std, function(j) {
        area <- self$get(IS_i, j)$targetPeak[1, areaType]
        if(is.null(area)) area <- NA
        area
      })
      StdCurve$new(analyteName = self$windowInfo$analyteName[Quant_i], relatedIS = IS_name, batchName = batchName,
                   weights = weights, delete = delete, zero = zero,
                   areaVec_Quant = areaVec_Quant, areaVec_IS = areaVec_IS,
                   initialCon_Quant = self$windowInfo$initialCon[Quant_i], initialCon_IS = self$windowInfo$initialCon[IS_i],
                   dilutionRatioVec = self$sampleInfo$dilutionRatio[j_std])
    },

    #' @description
    #' Get stdcurve list for ChrGrid
    #' @param Quant_i `integer()` Quant index
    #' @param batchName `character()`, batch name
    GetStdCurve_ChrGrid = function(Quant_i, batchName){
      # initialize
      if(is.null(self$stdcurve_list)){
        Quant_i_all <- which(self$windowInfo$analyteType != "IS")
        batchName_all <- unique(self$sampleInfo$batchName)
        stdcurve_name <- unlist(lapply(batchName_all, function(batchName_) {
          paste0(batchName_, "_", Quant_i_all)
        }))
        self$stdcurve_list <- lapply(stdcurve_name, function(name_) {
          NULL
        })
        names(self$stdcurve_list) <- stdcurve_name
      }
      if(missing(Quant_i)){
        Quant_i <- which(self$windowInfo$analyteType != "IS")
      }
      if(missing(batchName)){
        batchName <- unique(self$sampleInfo$batchName)
      }
      stdcurve_name <- unlist(lapply(batchName, function(batchName_) {
        paste0(batchName_, "_", Quant_i)
      }))
      self$stdcurve_list[stdcurve_name] <- unlist(lapply(batchName, function(batchName_) {
        lapply(Quant_i, function(Quant_i_) {
          self$build_stdcurve(Quant_i = Quant_i_, batchName = batchName_)
        })
      }))
    },

    #' @description
    #' Calculate concentration for ChrGrid
    #' @param areaType `character(1)`, into, intb or maxo
    CalConcentration_ChrGrid = function(areaType = c("into", "intb", "maxo")){
      areaType <- match.arg(areaType)
      Quant_i <- which(self$windowInfo$analyteType != "IS")
      batchNameVec <- unique(self$sampleInfo$batchName)

      conDF <- purrr::list_rbind(
        lapply(Quant_i, function(i) {
          conList <- lapply(batchNameVec, function(batchName) {
            real_j <- which(self$sampleInfo$batchName == batchName & self$sampleInfo$typeName == "real")
            stdcurve <- self$stdcurve_list[[paste0(batchName, "_", i)]]
            IS_i <- which(self$windowInfo$analyteName == stdcurve$relatedIS)
            conVec <- sapply(real_j, function(j) {
              area_Quant <- self$get(i, j)$targetPeak[1, areaType]
              area_IS <- self$get(IS_i, j)$targetPeak[1, areaType]
              if(is.null(area_Quant) | is.null(area_IS) | is.null(stdcurve$intercept) | is.null(stdcurve$slope)) return(NA)
              as.numeric(round((((area_Quant / area_IS) - stdcurve$intercept) / stdcurve$slope) * stdcurve$initialCon_IS, 4))
            })
            names(conVec) <- self$sampleInfo$sampleName[real_j]
            return(conVec)
          })
          con <- unlist(conList)
          df <- as.data.frame(matrix(con, nrow = 1), row.names = self$windowInfo$analyteName[i])
          colnames(df) <- names(con)
          return(df)
        })
      )
      conDF <- data.table::data.table(conDF, keep.rownames = "analyteName")
      return(conDF)
    }
  )
)
