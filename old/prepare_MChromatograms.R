#' @title prepare_MChromatograms
#' @description
#' Associate the MChromatograms object with the windowInfo.
#'
#' @param MChromatograms MChromatograms object.
#' @param windowInfo windowInfo.
#' @param sampleInfo sampleInfo.
#' @param unit unit.
#' @param thread thread.
#'
#' @return A new MChromatograms.
#' @export
#'
#' @examples
#' MChromatograms <- prepare_MChromatograms(MChromatograms = MChromatograms, windowInfo = windowInfo)
prepare_MChromatograms <- function(MChromatograms, windowInfo = windowInfo, sampleInfo = sampleInfo, unit = "min", thread = 4){
  nrow <- nrow(MChromatograms)
  ncol <- ncol(MChromatograms)
  if(unit == "min") mag <- 60
  else mag <- 1
  combinations <- expand.grid(1:nrow, 1:ncol)
  colnames(combinations) <- c("i", "j")
  pb <- utils::txtProgressBar(max = nrow(combinations), style = 3)
  loop <- function(l){
    i <- combinations[l, ]$i;j <- combinations[l, ]$j
    Chromatogram <- MChromatograms[i, j]
    chrInfo <- attributes(Chromatogram)$chrInfo
    windowName <- trimws(purrr::pluck(strsplit(regmatches(chrInfo$chromatogramId, regexpr("name=(.+)", chrInfo$chromatogramId, perl = TRUE)), "name=")[[1]], 2))
    attributes(Chromatogram)$windowName <- windowName
    windowInfo_tmp <- windowInfo[windowInfo$windowName == windowName, ]
    sample_name <- strsplit(basename(attributes(Chromatogram)$sample_name), ".", fixed = TRUE)[[1]][1]
    sampleInfo_tmp <- sampleInfo[sampleInfo$sampleName == sample_name, ]
    if(nrow(windowInfo_tmp) != 0 & nrow(sampleInfo_tmp) == 1){
      tmpList <- lapply(1:nrow(windowInfo_tmp), function(l) {
        Chromatogram_new <- Chromatogram
        attributes(Chromatogram_new)$analyteName <- windowInfo_tmp[l, ]$analyteName
        attributes(Chromatogram_new)$expectRt <- windowInfo_tmp[l, ]$expectRt * mag
        attributes(Chromatogram_new)$relatedIS <- windowInfo_tmp[l, ]$relatedIS
        attributes(Chromatogram_new)$relatedQual <- windowInfo_tmp[l, ]$relatedQual
        attributes(Chromatogram_new)$relatedCor <- windowInfo_tmp[l, ]$relatedCor
        attributes(Chromatogram_new)$analyteType <- windowInfo_tmp[l, ]$analyteType
        attributes(Chromatogram_new)$initialCon <- windowInfo_tmp[l, ]$initialCon

        attributes(Chromatogram_new)$batchName = sampleInfo_tmp$batchName
        attributes(Chromatogram_new)$injectOrder <- sampleInfo_tmp$injectOrder
        attributes(Chromatogram_new)$typeName <- sampleInfo_tmp$typeName
        attributes(Chromatogram_new)$dilutionRatio <- sampleInfo_tmp$dilutionRatio
        Chromatogram_new
      })
    }else stop(paste0(windowName, " | ",sample_name,  ", windowName or sampleName maybe wrong!"))
    tmpList
  }
  if(thread == 1){
    chrs <- lapply(1:nrow(combinations), function(l) {
      utils::setTxtProgressBar(pb, l)
      loop(l)
    })
  }else if(thread > 1){
    cl <- snow::makeCluster(thread)
    doSNOW::registerDoSNOW(cl)
    opts <- list(progress = function(n) utils::setTxtProgressBar(pb,
                                                                 n))
    chrs <- foreach::`%dopar%`(foreach::foreach(l = 1:nrow(combinations),
                                                .export = c(),
                                                .packages = c("MSnbase", "purrr"),
                                                .options.snow = opts),
                               {
                                 loop(l)
                               })
    snow::stopCluster(cl)
    gc()
  }
  chrs <- unlist(chrs)
  MSnbase::MChromatograms(chrs,
                          ncol = ncol)
}
.getRow4analyteType <- function(MChromatograms, analyteType){
  nrow <- nrow(MChromatograms)
  analyteTypeVec <- sapply(1:nrow, function(i) {
    attributes(MChromatograms[i,1])$analyteType
  })
  analyteNameVec_all <- sapply(1:nrow, function(i) {
    attributes(MChromatograms[i, 1])$analyteName
  })
  rows <- which(analyteTypeVec %in% analyteType)
  names(rows) <- analyteNameVec_all[rows]
  return(rows)
}
.getCol4batchName <- function(MChromatograms, batchName){
  ncol <- ncol(MChromatograms)
  batchNameVec <- sapply(1:ncol, function(j) {
    attributes(MChromatograms[1, j])$batchName
  })
  cols <- which(batchNameVec %in% batchName)
  names(cols) <- batchNameVec[cols]
  return(cols)
}
.getRow4analyteName <- function(MChromatograms, analyteNameVec){
  nrow <- nrow(MChromatograms)
  analyteNameVec_all <- sapply(1:nrow, function(i) {
    attributes(MChromatograms[i, 1])$analyteName
  })
  as.numeric(sapply(analyteNameVec, function(x) {
    which(analyteNameVec_all == x)
  }))
}
.getCol4sampleName <- function(MChromatograms, sampleNameVec){
  ncol <- ncol(MChromatograms)
  sampleNameVec_all <- sapply(1:ncol, function(j) {
    strsplit(basename(attributes(MChromatograms[1, j])$sample_name), split = ".", fixed = TRUE)[[1]][1]
  })
  as.numeric(sapply(sampleNameVec, function(x) {
    which(sampleNameVec_all == x)
  }))
}
.getCol4typeName <- function(MChromatograms, typeName){
  ncol <- ncol(MChromatograms)
  typeNameVec <- sapply(1:ncol, function(j) {
    attributes(MChromatograms[1, j])$typeName
  })
  which(typeNameVec == typeName)
}
