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
MChromatograms <- peakPicking_MChromatograms(MChromatograms = MChromatograms, thread = 4, unit = "min", peakPara = get_peakPara(tol_m = 1, snthresh = 0.1))
plotMChromatograms(MChromatograms, rows = 34, cols = 1:10, targetPeak = FALSE, ncol = 5) # 6 需要更改noise数值, 12需要更改tol_m
plotChromatogram(peakPicking_Chromatogram(MChromatograms[24, 8], peakPara = get_peakPara(snthresh = 0.1, sn = 1, xcms = "BOTH", multiSmooth = TRUE, tol_m = 1)), targetPeak = FALSE)
# Note: The function adjusts the retention time unit according to the unit parameter
# 3. Prepare MChromatograms
# Associate the MChromatograms object with the windowInfo.
MChromatograms <- prepare_MChromatograms(MChromatograms = MChromatograms, windowInfo = windowInfo)
# 4. IS check
# The expected retention time was first used to determine the target peaks for each IS.
rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
ncol <- ncol(MChromatograms)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS, cols = 1:ncol, targetRt = NA, tolRt = 10)
# After that, user need to manually review each IS
# Here are some ways to quickly help users review
scoreList_test <- calAlignScore_MChromatograms(MChromatograms = MChromatograms, row = rows_IS, cols = 1:ncol, standard_cols = NA,
                             cosMag = 0.5, corMag = 0.5, method = "direct")
scoreMatrix_test <- matrix(purrr::list_c(scoreList_test), nrow = length(rows_IS), byrow = TRUE)
.plotHeatMap(scoreMatrix_test)
plotMChromatograms(MChromatograms, rows = rows_IS[c(8, 9)], cols = 1:10, ncol = 5)
# We can find an error in C4-d3, and we can manually fix it.
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms = MChromatograms, rows = rows_IS[9], cols = 1:ncol, targetRt = 190, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[c(8, 9)], cols = 1:10, ncol = 5, targetPeak = FALSE)
# Before retention time correction, the graph applies within a batch; after correction, it applies to the entire batch.
plotChromatogram(MChromatograms[rows_IS[20], 4])

attributes(MChromatograms[rows_IS[9], 1])
plotChromatogram(peakPicking_Chromatogram(MChromatograms[rows_IS[9], 1], peakPara = get_peakPara(snthresh = 0.5, fwhm = NA)), targetPeak = FALSE)
plotChromatogram(peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[9], cols = 1)[rows_IS[9], 1], targetPeak = FALSE)

