# Record the approximate MRMProcessor workflow in a this script.
# 1. Load data and info
file_dir <- "D:/fudan/Projects/2023/MRMProcessor/Progress/Experiment1_Load raw data/231225/Data/Carnitine/"
patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
patterns <- paste0(patterns, collapse = "|")
file_path <- list.files(file_dir, pattern = patterns)
file_path <- paste0(file_dir, file_path)
MChromatograms <- readMRMData(files = file_path[1:10], thread = 4) # only 10 sample for test.
windowInfo <- read_windowInfo(windowInfo_path = "D:/fudan/Projects/2023/MRMProcessor/Progress/build_package/test_data_info/windowInfo_car.xlsx")
sampleInfo <- read_sampleInfo("D:/fudan/Projects/2023/MRMProcessor/Progress/build_package/test_data_info/sampleInfo_car.xlsx")
# 2. Peak picking (first)
# The first time peakPicing is performed,
# where the first peakPicking is performed on the MChromatograms
# using either the default parameters or the specified parameters.
MChromatograms <- peakPicking_MChromatograms(MChromatograms = MChromatograms, thread = 4, unit = "min", peakPara = get_peakPara(tol_m = 1, snthresh = 0.1, xcms = "BOTH"))
# Note: The function adjusts the retention time unit according to the unit parameter
# Checking the effect of algorithms.
plotMChromatograms(MChromatograms, rows = 6, cols = 1:10, targetPeak = FALSE, ncol = 5)
# Noise evaluation too high, manually adjusted down and change the fwhm.
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = 6, cols = 1:10, noise = 250, peakPara = get_peakPara(snthresh = 0, fwhm = 1, xcms = "MatchedFilter"))
plotMChromatograms(MChromatograms, rows = 6, cols = 1:10, targetPeak = FALSE, ncol = 5)
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
# scoreList_test <- calAlignScore_MChromatograms(MChromatograms = MChromatograms, row = rows_IS, cols = cols_batch1, standard_cols = NA,
#                              cosMag = 0.5, corMag = 0.5, method = "direct")
# scoreMatrix_test <- matrix(purrr::list_c(scoreList_test), nrow = length(rows_IS), byrow = TRUE)
# .plotHeatMap(scoreMatrix_test)
check_i <- .getRow4analyteName(MChromatograms = MChromatograms, analyteNameVec = c("isoC5-OH-d3", "C5-DC-d3"))
check_j <- .getCol4sampleName(MChromatograms = MChromatograms, sampleNameVec = c("SBRA021A230425_CF40ul_3_p1-a08182", "SBRA021A230425_CF40ul_3_p1-a05965"))
plotMChromatograms(MChromatograms = MChromatograms, rows = check_i, cols = check_j, targetPeak = TRUE)
# Before retention time correction, the graph applies within a batch; after correction, it applies to the entire batch.
# 4.2 We need to calculate IS's deltaRt and correct the rtime
MChromatograms_new <- rtCorrection_IS(MChromatograms = MChromatograms, rows = NA, cols = cols_batch1)
plotMChromatograms(MChromatograms = MChromatograms_new, rows = check_i, cols = check_j, targetPeak = TRUE)
plotHeatMap_MChromatograms(MChromatograms = MChromatograms_new, rows = rows_IS, cols = cols_batch1, standard_cols = NA)
check_i <- .getRow4analyteName(MChromatograms_new, analyteNameVec = c("C4-DC-d3"))
check_j <- .getCol4sampleName(MChromatograms_new, sampleNameVec = c("SBRA021A230425_CF40ul_3_p1-a08171"))
# We can easily find the problem peak
plotMChromatograms(MChromatograms = MChromatograms_new, rows = check_i, cols = c(1, check_j), targetPeak = TRUE)
MChromatograms_new <- peakPicking_MChromatograms2(MChromatograms_new, rows = check_i, cols = check_j, noise = 10000, smoothPara = get_smoothPara(smooth = TRUE),baselinePara = get_baselinePara(tol_m = 5), peakPara = get_peakPara(xcms = "BOTH", fwhm = 10))
plotMChromatograms(MChromatograms = MChromatograms_new, rows = check_i, cols = c(1, check_j), targetPeak = FALSE)
plotHeatMap_MChromatograms(MChromatograms = MChromatograms_new, rows = rows_IS, cols = cols_batch1, standard_cols = NA)
MChromatograms_new <- extractTargetPeak_MChromatograms(MChromatograms_new, rows = check_i, cols = check_j, targetRt = NA, tolRt = 10)
MChromatograms_new <- rtCorrection_IS(MChromatograms = MChromatograms_new, rows = check_i, cols = check_j)
# Now, all square is purple.
plotHeatMap_MChromatograms(MChromatograms = MChromatograms_new, rows = rows_IS, cols = cols_batch1, standard_cols = NA)
