# Record the approximate MRMProcessor workflow in a this script.
# 1. Load data and info
file_dir <- "D:/fudan/Projects/2023/MRMProcessor/Progress/Experiment1_Load raw data/231225/Data/Carnitine/"
patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
patterns <- paste0(patterns, collapse = "|")
file_path <- list.files(file_dir, pattern = patterns)
file_path <- paste0(file_dir, file_path)
MChromatograms <- readMRMData(files = file_path[1:40], thread = 4) # only 10 sample for test.
windowInfo <- read_windowInfo(windowInfo_path = "D:/fudan/Projects/2023/MRMProcessor/Progress/build_package/test_data_info/windowInfo_car.xlsx")
sampleInfo <- read_sampleInfo("D:/fudan/Projects/2023/MRMProcessor/Progress/build_package/test_data_info/sampleInfo_car.xlsx")
# 2. Peak picking (first)
# The first time peakPicing is performed,
# where the first peakPicking is performed on the MChromatograms
# using either the default parameters or the specified parameters.
MChromatograms <- peakPicking_MChromatograms(MChromatograms = MChromatograms, noiseMag = 2, thread = 4, unit = "min", peakPara = get_peakPara(tol_m = 1, snthresh = 0.5, xcms = "BOTH"))
# Note: The function adjusts the retention time unit according to the unit parameter
# 3. Prepare MChromatograms
# Associate the MChromatograms object with the windowInfo.
MChromatograms <- prepare_MChromatograms(MChromatograms = MChromatograms, windowInfo = windowInfo, sampleInfo = sampleInfo, unit = "min", thread = 4)
# 4. IS check
# The expected retention time was first used to determine the target peaks for each IS.
rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
cols_batch1 <- .getCol4batchName(MChromatograms = MChromatograms, batchName = "batch1")
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS, cols = cols_batch1, targetRt = NA, tolRt = 10)
# After that, user need to manually review each IS
# Here are some ways to quickly help users review
plotHeatMap_MChromatograms(MChromatograms = MChromatograms, rows = rows_IS, cols = cols_batch1, standard_cols = NA)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[7], cols = cols_batch1)
# (1) Inaccurate estimation of baseline levels of peak-seeking parameters
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C4-DC-d3"))
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = check_i, cols = cols_batch1, baselinePara = get_baselinePara(tol_m = 1, loops = 10))
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
# (1) Inaccurate estimation of baseline levels of peak-seeking parameters
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("MM-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 28, targetPeak = FALSE)
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = check_i, cols = 28, baselinePara = get_baselinePara(tol_m = 5, loops = 10), peakPara = get_peakPara(xcms = "BOTH", tol_m = 1))
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 28, targetPeak = FALSE)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = 28)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
# (2) Interference peak near expected retention time (C12-d3)
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C12-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = 2, targetRt = 418)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
# (2) Interference peak near expected retention time (C14-d3)
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C14-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = cols_batch1, targetRt = 450)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
# (2) Interference peak near expected retention time (isoC5-d3)
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("isoC5-d3"))
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = FALSE)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = cols_batch1, targetRt = 235)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
# (2) Interference peak near expected retention time (C16-d3)
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C16-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 35, targetPeak = TRUE)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = 35, targetRt = 476)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 35, targetPeak = TRUE)
# (3) Noise level is low.
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C4-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = FALSE)
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = check_i, cols = cols_batch1, noiseMag = 3, peakPara = get_peakPara(tol_m = 1, xcms = "BOTH"))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
# (4) Change different xcms parameters.
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C4-OH-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 17, targetPeak = FALSE)
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = check_i, cols = 17, peakPara = get_peakPara(xcms = "ORIGN", fwhm = 10))
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 17, targetPeak = FALSE)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = 17)
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
# (4) Change different xcms parameters.
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C26-d9-85"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 28, targetPeak = FALSE)
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = check_i, cols = 28, peakPara = get_peakPara(xcms = "ORIGN", fwhm = 9))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = check_i, cols = 28)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = 28, targetPeak = TRUE)
# (5) PeakPicking and extract no problem, but rt is not corrected.
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("isoC5-OH-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)
# (5) PeakPicking and extract no problem, but rt is not corrected.
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("C5-DC-d3"))
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = check_i, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = cols_batch1, targetPeak = TRUE)

plotHeatMap_MChromatograms(MChromatograms = MChromatograms, rows = rows_IS, cols = cols_batch1, standard_cols = NA)

# Before retention time correction, the graph applies within a batch; after correction, it applies to the entire batch.
# 4.2 We need to calculate IS's deltaRt and correct the rtime. (You need to make sure the IS are all correct)
MChromatograms_new <- rtCorrection_IS(MChromatograms = MChromatograms, rows = NA, cols = cols_batch1)
plotHeatMap_MChromatograms(MChromatograms = MChromatograms_new, rows = rows_IS, cols = cols_batch1, standard_cols = NA)
# 5. Analyte retention time correction
MChromatograms_new <- rtCorrection_analyte(MChromatograms = MChromatograms_new, rows = NA, cols = cols_batch1)
rows_analyte <- c(.getRow4analyteType(MChromatograms = MChromatograms_new, analyteType = "Quant"),
                  .getRow4analyteType(MChromatograms = MChromatograms_new, analyteType = "Qual"))
MChromatograms_new <- extractTargetPeak_MChromatograms(MChromatograms = MChromatograms_new,
                                                       rows = rows_analyte, cols = cols_batch1,
                                                       targetRt = NA, tolRt = 10)
plotHeatMap_MChromatogramsRow(MChromatograms_new, row = rows_analyte[12], cols = cols_batch1, standard_cols = NA)
plotMChromatograms(MChromatograms_new, rows = rows_analyte[12], cols = cols_batch1, targetPeak = FALSE)

check_i <- .getRow4analyteName(MChromatograms_new, analyteNameVec = c("C0std-85_CE20"))
plotMChromatograms(MChromatograms = MChromatograms_new, rows = check_i, cols = 22, targetPeak = TRUE)
MChromatograms_new <- peakPicking_MChromatograms2(MChromatograms_new, noiseMag = 3,rows = check_i, cols = 22, peakPara = get_peakPara(tol_m = 1, snthresh = 0.1, xcms = "BOTH"))
MChromatograms_new <- extractTargetPeak_MChromatograms(MChromatograms_new, rows = check_i, cols = 22)
plotMChromatograms(MChromatograms = MChromatograms_new, rows = check_i, cols = 22, targetPeak = TRUE)
plotHeatMap_MChromatogramsRow(MChromatograms_new, row = rows_analyte[4], cols = cols_batch1, standard_cols = NA)
