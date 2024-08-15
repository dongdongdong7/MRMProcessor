.openMSfile <- function (x)
{
  if (missing(x) || length(x) != 1)
    stop("parameter 'x' has to be of length 1")
  mzR::openMSfile(x, backend = NULL)
}
.combine_data.frame <- function (x, cols)
{
  if (!length(x))
    stop("length of 'x' must be > 0")
  if (!all(unlist(lapply(x, is.data.frame))))
    stop("all elements in 'x' need to be a data.frame")
  fd <- do.call(rbind, x)
  if (missing(cols))
    cols <- colnames(fd)
  else {
    if (!all(cols %in% colnames(fd)))
      stop("All columns specified with 'cols' have to be present in the",
           " data.frames")
  }
  nr <- vapply(x, nrow, 1)
  dfIds <- rep(seq_along(x), nr)
  colIds <- do.call(paste, fd[, cols, drop = FALSE])
  repl <- vapply(split(dfIds, colIds), function(d) max(table(d)),
                 1)
  o <- order(colIds)
  fd <- fd[o, ]
  colIds <- colIds[o]
  fd <- fd[rep(which(!duplicated(colIds)), repl), ]
  rownames(fd) <- NULL
  fd
}
.polarity_char <- function (x)
{
  if (!all(x %in% c(-1, 0, 1)))
    stop("Polarity is expected to take only values 1, -1 and 0")
  x[x < 0] <- NA
  ifelse(x == 1, "+", "-")
}

#' @title readMRMData
#' @description
#' Read MRM data function with parallel mode.
#'
#' @param files files path.
#' @param thread thread.
#'
#' @return A MChromatograms object.
#' @export
#'
#' @examples
#' file_dir <- "D:/fudan/Projects/2023/MRMProcessor/Progress/Experiment1_Load raw data/231225/Data/Carnitine/"
#' patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
#' patterns <- paste0(patterns, collapse = "|")
#' file_path <- list.files(file_dir, pattern = patterns)
#' file_path <- paste0(file_dir, file_path)
#' data <- readMRMData(files = file_path, thread = 4)
readMRMData <- function(files, thread = 1){
  files <- normalizePath(files)
  hdr_list <- lapply(files, function(x) {
    msf <- .openMSfile(x)
    if (!is(msf, "mzRpwiz"))
      stop("Can only extract chromatogram information from a mzML file",
           " using the 'proteowizard' backend")
    hdr <- mzR::chromatogramHeader(msf)
    mzR::close(msf)
    hdr[!is.na(hdr$precursorIsolationWindowTargetMZ) | !is.na(hdr$productIsolationWindowTargetMZ), , drop = FALSE]
  })
  lens <- unlist(lapply(hdr_list, nrow))
  if (any(lens == 0)){
    stop("file(s) ", paste0("'", files[lens == 0], "'",
                            collapse = ", "), " do not contain SRM chromatogram data")
  }
  fdata <- .combine_data.frame(hdr_list, cols = c("polarity",
                                                  "precursorIsolationWindowTargetMZ", "productIsolationWindowTargetMZ",
                                                  "precursorCollisionEnergy",
                                                  "chromatogramIndex"))
  fdata_ids <- paste0(.polarity_char(fdata$polarity), " Q1=",
                      fdata$precursorIsolationWindowTargetMZ, " Q3=", fdata$productIsolationWindowTargetMZ,
                      " collisionEnergy=", fdata$precursorCollisionEnergy,
                      " chromatogramIndex=", fdata$chromatogramIndex)
  pdata <- data.frame(file = files, stringsAsFactors = FALSE)
  loop <- function(j){
    file <- files[j];hdr <- hdr_list[[j]];idx <- j
    current_ids <- paste0(.polarity_char(hdr$polarity),
                          " Q1=", hdr$precursorIsolationWindowTargetMZ,
                          " Q3=", hdr$productIsolationWindowTargetMZ,
                          " collisionEnergy=", hdr$precursorCollisionEnergy,
                          " chromatogramIndex=", hdr$chromatogramIndex)
    if (length(current_ids) != length(unique(current_ids)))
      warning("file ", basename(file), " contains multiple ",
              "chromatograms with identical polarity, precursor ",
              "and product m/z values", call. = FALSE)
    res_chrs <- replicate(nrow(fdata), MSnbase::Chromatogram(fromFile = idx))
    msf <- .openMSfile(file)
    chr_data <- mzR::chromatogram(msf, hdr$chromatogramIndex)
    mzR::close(msf)
    for (i in seq_len(nrow(hdr))) {
      idx_to_place <- which(lengths(res_chrs) == 0 &
                              fdata_ids == current_ids[i])[1]
      if (is.na(idx_to_place))
        stop("Got more redundant chromatograms than expected")
      res_chrs[[idx_to_place]] <- MSnbase::Chromatogram(rtime = chr_data[[i]][,
                                                                      1], intensity = chr_data[[i]][, 2], precursorMz = hdr$precursorIsolationWindowTargetMZ[i],
                                                productMz = hdr$productIsolationWindowTargetMZ[i],
                                                fromFile = idx)
    }
    res_chrs
  }
  pb <- utils::txtProgressBar(max = length(files), style = 3)
  if(thread == 1){
    chrs <- unlist(lapply(1:length(files), function(j) {
      utils::setTxtProgressBar(pb, j)
      loop(j)
    }))
  }else if(thread > 1){
    cl <- snow::makeCluster(thread)
    doSNOW::registerDoSNOW(cl)
    opts <- list(progress = function(n) utils::setTxtProgressBar(pb,
                                                                 n))
    chrs <- unlist(foreach::`%dopar%`(foreach::foreach(j = 1:length(files),
                                                      .export = c(".polarity_char", ".openMSfile"),
                                                      .packages = c("mzR", "MSnbase"),
                                                      .options.snow = opts),
                                     {
                                       loop(j)
                                     }))
    snow::stopCluster(cl)
    gc()
  }else stop("thread is wrong!")
  MSnbase::MChromatograms(chrs, phenoData = pdata, featureData = fdata,
                          ncol = length(files))
}


