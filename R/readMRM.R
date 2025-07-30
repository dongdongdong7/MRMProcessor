# open .mzML file using mzR
.openMSfile <- function (x)
{
  if (missing(x) || length(x) != 1)
    stop("parameter 'x' has to be of length 1")
  mzR::openMSfile(x, backend = NULL)
}
# combine data.frame x with cols and remove duplicates
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
# 0 -> -; 1 -> +
.polarity_char <- function (x)
{
  if (!all(x %in% c(-1, 0, 1)))
    stop("Polarity is expected to take only values 1, -1 and 0")
  x[x < 0] <- NA
  ifelse(x == 1, "+", "-")
}

#' @title Read MRM data
#' @description
#' Read MRM data function with parallel mode and convert them to ChrGrid
#' row number of ChrGrid is number of windowName here, and m will change if analyte number larger than window number
#'
#' @param files files path.
#' @param unit `character(1)`, retention time unit, min or sec
#' @param thread thread.
#'
#' @return ChrGrid object.
#' @export
#'
#' @examples
#' files_dir <- "../Carnitine_SBR/MRMprocesser/"
#' patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
#' patterns <- paste0(patterns, collapse = "|")
#' files_path <- list.files(files_dir, pattern = patterns)
#' files_path <- paste0(files_dir, files_path)
#' windowInfo_path <- "../Carnitine_SBR/MRMprocesser/windowInfo_car_cll.xlsx"
#' sampleInfo_path <- "../Carnitine_SBR/MRMprocesser/sampleInfo_car.xlsx"
#' windowInfo <- openxlsx::read.xlsx(windowInfo_path, sheet = 1)
#' sampleInfo <- openxlsx::read.xlsx(sampleInfo_path, sheet = 1)
#' chr_grid <- readMRMData(files = files_path, windowInfo = windowInfo, sampleInfo = sampleInfo[1:10, ])
readMRMData <- function(files, unit = c("min", "sec"), windowInfo, sampleInfo, thread = 1){
  files <- normalizePath(files)
  unit <- match.arg(unit)
  if(unit == "min") mag <- 60
  else if(unit == "sce") mag <- 1
  else stop("unit should be min or sec!")
  n <- nrow(sampleInfo) # sample number
  windowName_uniqueVec <- unique(windowInfo$windowName)
  m <- length(windowName_uniqueVec) # window number
  if(nrow(sampleInfo) != length(files)){
    warnings("The number of rows in sampleInfo does not match the exact number in the original data folder!")
  }
  # Extract file name(sampleName)
  files_name <- stringr::str_extract(basename(files), ".*(?=\\.mzML)")
  # Match to sampleInfo
  sampleInfo$samplePath <- files[match(sampleInfo$sampleName, files_name)]
  if(any(is.na(sampleInfo$samplePath))) stop("samplePath of sampleInfo can not be NA!")
  # window information from each sample
  hdr_list <- lapply(sampleInfo$samplePath, function(x) {
    msf <- .openMSfile(x)
    if (!is(msf, "mzRpwiz"))
      stop("Can only extract chromatogram information from a mzML file",
           " using the 'proteowizard' backend")
    hdr <- mzR::chromatogramHeader(msf)
    Encoding(hdr[, 1]) <- "UTF-8"
    mzR::close(msf)
    hdr[!is.na(hdr$precursorIsolationWindowTargetMZ) | !is.na(hdr$productIsolationWindowTargetMZ), , drop = FALSE]
  })
  lens <- unlist(lapply(hdr_list, nrow))
  if (any(lens == 0)){
    stop("file(s) ", paste0("'", sampleInfo$samplePath[lens == 0], "'",
                            collapse = ", "), " do not contain SRM chromatogram data")
  }
  diff_lens <- diff(lens)
  if(any(diff_lens !=0)){stop(paste0("The number of windows in the ", which(diff_lens != 0), " sample differs from the number of windows in windowInfo!"))}
  cols <- c("polarity",
            "precursorIsolationWindowTargetMZ", "productIsolationWindowTargetMZ",
            "precursorCollisionEnergy",
            "chromatogramIndex")
  fd <- do.call(rbind, hdr_list)
  if (!all(cols %in% colnames(fd)))
    stop("All columns specified with 'cols' have to be present in the",
         " data.frames")
  dfIds <- rep(seq_along(hdr_list), lens)
  colIds <- do.call(paste, fd[, cols, drop = FALSE])
  table_colIds <- table(colIds)
  # extra_colIds are inconsistent with other samples
  extra_colIds <- names(table_colIds)[which(table_colIds != n)]
  if(length(extra_colIds) > 0){
    extra_sample_idx <- which(sapply(1:nrow(hdr_list), function(i) {
      hdr <- hdr_list[[i]]
      colIds_i <- do.call(paste, hdr[, cols, drop = FALSE])
      if(any(colIds_i %in% extra_colIds)) return(TRUE)
    }))
    stop(paste0("The file ", sampleInfo$samplePath[extra_colIds], " have extra window ", extra_colIds))
  }
  repl <- vapply(split(dfIds, colIds), function(d) max(table(d)),
                 1)
  o <- order(colIds)
  fd <- fd[o, ]
  colIds <- colIds[o]
  fd <- fd[rep(which(!duplicated(colIds)), repl), ]
  rownames(fd) <- NULL
  if(nrow(fd) != length(unique(windowInfo$windowName))) message("The number of windows in files are different with windowInfo!")
  # fd_ids <- paste0(.polarity_char(fd$polarity), " Q1=",
  #                  fd$precursorIsolationWindowTargetMZ, " Q3=", fd$productIsolationWindowTargetMZ,
  #                  " collisionEnergy=", fd$precursorCollisionEnergy,
  #                  " chromatogramIndex=", fd$chromatogramIndex)
  # pdata <- data.frame(file = files, stringsAsFactors = FALSE)
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent | ELA: :elapsedfull | ETA: :eta",
    total = nrow(sampleInfo),
    width = 60
  )
  progress_update <- function(nn){
    pb$tick()
  }
  opts <- list(progress = progress_update)
  cl <- snow::makeCluster(thread)
  doSNOW::registerDoSNOW(cl)
  resLt <- foreach::`%dopar%`(foreach::foreach(sample_path = sampleInfo$samplePath, hdr = hdr_list, nn = 1:nrow(sampleInfo),
                                               .packages = c("R6", "mzR"),
                                               .export = c(".polarity_char", ".openMSfile", "chromatogram"),
                                               .options.snow = opts),
                              {
                                # current_ids <- paste0(.polarity_char(hdr$polarity),
                                #                       " Q1=", hdr$precursorIsolationWindowTargetMZ,
                                #                       " Q3=", hdr$productIsolationWindowTargetMZ,
                                #                       " collisionEnergy=", hdr$precursorCollisionEnergy,
                                #                       " chromatogramIndex=", hdr$chromatogramIndex)
                                # if (length(current_ids) != length(unique(current_ids)))
                                #   warning("file ", basename(file), " contains multiple ",
                                #           "chromatograms with identical polarity, precursor ",
                                #           "and product m/z values", call. = FALSE)
                                current_fd <- hdr[, c("chromatogramId", "precursorIsolationWindowTargetMZ", "productIsolationWindowTargetMZ"), drop = FALSE]
                                current_fd$name <- stringr::str_extract(current_fd$chromatogramId, "(?<=name=).*")
                                lg <- sapply(current_fd$name, function(x) any(is.na(x)))
                                if(any(lg)) stop(paste0("file ", basename(sample_path), " is missing information about window name"))
                                msf <- .openMSfile(sample_path)
                                chr_data <- mzR::chromatogram(msf, hdr$chromatogramIndex)
                                mzR::close(msf)
                                chrs_list <- lapply(1:length(windowName_uniqueVec), function(i) {
                                  analyteName <- NA
                                  windowName <- windowName_uniqueVec[i]
                                  j <- match(windowName, current_fd$name)
                                  Q1 <- current_fd[j, "precursorIsolationWindowTargetMZ"]
                                  Q3 <- current_fd[j, "productIsolationWindowTargetMZ"]
                                  expectRt <- NA
                                  analyteType <- NA
                                  relatedIS <- NA
                                  return(chromatogram$new(rtime = chr_data[[j]]$rtime * mag, intensity = chr_data[[j]]$intensity,
                                                          Q1 = Q1, Q3 = Q3,
                                                          analyteName = analyteName, windowName = windowName,
                                                          expectRt = expectRt, analyteType = analyteType, relatedIS = relatedIS))
                                })
                                chrs_list
                              })
  snow::stopCluster(cl)
  gc()
  resLt <- unlist(resLt)
  chrs_grid <- ChrGrid$new(m = m, n = n, unit = unit, chrs_list = resLt, windowInfo = windowInfo, sampleInfo = sampleInfo)
  chrs_grid
}
