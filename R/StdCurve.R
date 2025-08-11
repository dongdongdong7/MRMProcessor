# StdCurve class
# Barry Song
# 250811

#' @title StdCurve
#' @description
#' The StdCurve is an R object that stores all the information about standard curve of one analyte in one batch
#' @export
#' @examples
#' # example code
#'
StdCurve <- R6::R6Class(
  classname = "StdCurve",
  class = TRUE, portable = TRUE, cloneable = FALSE, lock_objects = TRUE, lock_class = TRUE,

  public = list(
    #' @field analyteName `character(1)`, analyte name
    analyteName = NULL,
    #' @field relatedIS IS name
    relatedIS = NULL,
    #' @field batchName `character(1)`, batch name
    batchName = NULL,
    #' @field weights `character(1)`, weights of standard curve
    weights = NULL,
    #' @field delete `integer()`, which points need to be deleted form stdcurve_df
    delete = NULL,
    #' @field zero `logical(1)`, does the standard curve pass through the zero point
    zero = NULL,
    #' @field areaVec_Quant `numeric()`, area vector of Quant in std sample
    areaVec_Quant = NULL,
    #' @field areaVec_IS `numeric()`, area vector of IS in std sample
    areaVec_IS = NULL,
    #' @field initialCon_Quant `numeric(1)`, initial concentration of Quant
    initialCon_Quant = NULL,
    #' @field initialCon_IS `numeric(1)`, initial concentration of IS
    initialCon_IS = NULL,
    #' @field dilutionRatioVec `numeric(1)`, dilution ratio of std sample
    dilutionRatioVec = NULL,
    #' @field stdcurve_df `data.frame()`, stores concentration ratio and area ratio
    stdcurve_df = NULL,
    #' @field slope `numeric(1)`, slope of standard curve
    slope = NULL,
    #' @field intercept `numeirc(1)`, intercept of standard curve
    intercept = NULL,
    #' @field r_squared `numeric(1)`, r squared of standard curve
    r_squared = NULL,

    #' @description
    #' Creates a new instance of StdCurve
    #' @param analyteName `character(1)`, analyte name
    #' @param relatedIS `character(1)`, IS name
    #' @param batchName `character(1)`, batch name
    #' @param weights `character(1)`, weights of standard curve
    #' @param delete `integer()`, which points need to be deleted form stdcurve_df
    #' @param zero `logical(1)`, does the standard curve pass through the zero point
    #' @param areaVec_Quant `numeric()`, area vector of Quant in std sample
    #' @param areaVec_IS `numeric()`, area vector of IS in std sample
    #' @param initialCon_Quant `numeric(1)`, initial concentration of Quant
    #' @param initialCon_IS `numeric(1)`, initial concentration of IS
    #' @param dilutionRatioVec `numeric(1)`, dilution ratio of std sample
    initialize = function(analyteName, relatedIS, batchName,
                          weights, delete, zero,
                          areaVec_Quant, areaVec_IS, initialCon_Quant, initialCon_IS, dilutionRatioVec){
      self$analyteName <- analyteName
      self$relatedIS <- relatedIS
      self$batchName <- batchName
      self$weights <- weights
      self$delete <- delete
      self$zero <- zero
      self$areaVec_Quant <- areaVec_Quant
      self$areaVec_IS <- areaVec_IS
      self$initialCon_Quant <- initialCon_Quant
      self$initialCon_IS <- initialCon_IS
      self$dilutionRatioVec <- dilutionRatioVec
      stdcurve_df <- data.frame(
        std = 1:length(dilutionRatioVec),
        concentrationRatio = (initialCon_Quant * dilutionRatioVec) / initialCon_IS,
        areaRatio = areaVec_Quant / areaVec_IS
      )
      stdcurve_df$type <- "save"
      if(length(delete) > 0){
        stdcurve_df$type[delete] <- "delete"
      }
      self$stdcurve_df <- stdcurve_df
      df_fit <- stdcurve_df[stdcurve_df$type == "save" & !is.na(stdcurve_df$areaRatio), ]
      if(nrow(df_fit) <= 3) return()
      if(zero){
        df_fit <- rbind(
          data.frame(std = 0, concentrationRatio = 0.000001, areaRatio = 0.000001, type = "save"),
          df_fit
        )
      }
      if(weights == "none") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit)
      else if(weights == "1/x") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit, weights = 1 / concentrationRatio)
      else if(weights == "1/x^2") fit <- lm(areaRatio ~ concentrationRatio, data = df_fit, weights = 1 / (concentrationRatio)^2)
      self$slope <- as.numeric(coef(fit)[2])
      self$intercept <- as.numeric(coef(fit)[1])
      self$r_squared <- summary(fit)$r.squared
    },

    #' @description
    #' Print StdCurve instance
    print = function(){
      if(is.null(self$r_squared)) r2 <- "none"
      else r2 <- round(self$r_squared, digits = 2)
      cat(paste0("analyte name: ", self$analyteName, "\n",
                 "batch name: ", self$batchName, "\n",
                 "r2: ", r2, "\n"))
    },

    #' @description
    #' Plot the standard curve
    plot_StdCurve = function(){
      df_line <- data.frame(
        intercept = self$intercept, slope = self$slope
      )
      p <- ggplot2::ggplot(data = self$stdcurve_df, ggplot2::aes(x = concentrationRatio, y = areaRatio)) +
        ggplot2::geom_point(ggplot2::aes(color = type)) +
        ggplot2::scale_color_manual(values = c("delete" = "gray", "save" = "blue")) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Concentration Ratio", y = "Area Ratio",
                      title = paste0(self$analyteName, " - ", self$batchName)) +
        ggplot2::theme(
          legend.position = "none"
        ) +
        ggplot2::geom_abline(data = df_line, ggplot2::aes(intercept = intercept, slope = slope), color = "red")
      p
    }
  )
)
