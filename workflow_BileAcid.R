# 这是使用胆汁酸数据的工作流程测试
# 1. 数据加载
file_dir <- "D:/fudan/Projects/2023/MRMProcessor/Progress/Experiment1_Load raw data/231225/Data/BileAcid/"
patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
patterns <- paste0(patterns, collapse = "|")
file_path <- list.files(file_dir, pattern = patterns)
file_path <- paste0(file_dir, file_path)
MChromatograms <- readMRMData(files = file_path, thread = 4)
windowInfo <- read_windowInfo(windowInfo_path = "D:/fudan/Projects/2023/MRMProcessor/Progress/Experiment1_Load raw data/231225/Data/BileAcid/windowInfo.xlsx")
sampleInfo <- read_sampleInfo("D:/fudan/Projects/2023/MRMProcessor/Progress/Experiment1_Load raw data/231225/Data/BileAcid/sampleInfo.xlsx")
# 2. 第一次寻峰
MChromatograms <- peakPicking_MChromatograms(MChromatograms = MChromatograms, thread = 5, unit = "min")
# 3. 准备MChromatograms对象
# 将MChromatograms与sampleInfo和windowInfo联系
MChromatograms <- prepare_MChromatograms(MChromatograms = MChromatograms,
                                         windowInfo = windowInfo, sampleInfo = sampleInfo,
                                         unit = "min", thread = 4)
# 4. 内标审核
# 使用预期保留时间进行目标峰提取
rows_IS <- .getRow4analyteType(MChromatograms = MChromatograms, analyteType = "IS")
batchNameVec <- unique(sampleInfo$batchName)
cols_batchs <- lapply(batchNameVec, function(x) .getCol4batchName(MChromatograms = MChromatograms, batchName = x))
length(cols_batchs) # 2 这批样本共有两个批次
# batch1
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS, cols = cols_batchs[[1]], targetRt = NA, tolRt = 10)
# D4-LCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[1], cols = cols_batchs[[1]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[1]], peakPara = get_peakPara(xcms = "BOTH", sn = 1))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[1]], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[1]])
# D4-CDCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[3], cols = cols_batchs[[1]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[3], cols = cols_batchs[[1]],
                                              noise = 200)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[3], cols = cols_batchs[[1]], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[3], cols = cols_batchs[[1]])
# D4-CA-2
# This is a example for noise evaluation logic,
# The noise in the first chromatogram is high with noiseMag set to 3, but after testing, using noiseMage = 2.
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[11], cols = cols_batchs[[1]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[11], cols = cols_batchs[[1]][1],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH"))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[11], cols = cols_batchs[[1]][1], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[11], cols = cols_batchs[[1]])
# D5-a-MCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[16], cols = cols_batchs[[1]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[16], cols = cols_batchs[[1]][c(3, 5)],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH"))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[16], cols = cols_batchs[[1]][c(3, 5)], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[16], cols = cols_batchs[[1]])
# D5-a-MCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[18], cols = cols_batchs[[1]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[18], cols = cols_batchs[[1]][c(6, 9, 12)],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH"))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[18], cols = cols_batchs[[1]][c(6, 9, 12)], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[18], cols = cols_batchs[[1]])
# D4-GUDCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[23], cols = cols_batchs[[1]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[23], cols = cols_batchs[[1]][c(5, 6, 7, 11)],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH"))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[23], cols = cols_batchs[[1]][c(5, 6, 7, 11)], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[23], cols = cols_batchs[[1]])
# D4-GCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[28], cols = cols_batchs[[1]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[28], cols = cols_batchs[[1]][c(6, 7, 10)],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH"))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[28], cols = cols_batchs[[1]][c(6, 7, 10)], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[28], cols = cols_batchs[[1]])
# batch2
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS, cols = cols_batchs[[2]], targetRt = NA, tolRt = 10)
# D4-LCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[1], cols = cols_batchs[[2]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[2]],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH", sn = 1))
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[2]][14],
                                              noise = 40000, peakPara = get_peakPara(xcms = "BOTH", sn = 1))
plotMChromatograms(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[2]], targetPeak = FALSE)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[2]], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[1], cols = cols_batchs[[2]])
# D4-CDCA-2 blank
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[3], cols = cols_batchs[[2]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[3], cols = cols_batchs[[2]],
                                              noise = 200)
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[3], cols = cols_batchs[[2]], targetRt = NA, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[3], cols = cols_batchs[[2]])
# D4-CDCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[4], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[4], cols = cols_batchs[[2]], targetRt = 497, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[4], cols = cols_batchs[[2]])
# D4-UDCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[5], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[5], cols = cols_batchs[[2]],
                                                   targetRt = 383, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[5], cols = cols_batchs[[2]])
# D4-UDCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[6], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[6], cols = cols_batchs[[2]],
                                                   targetRt = 383, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[6], cols = cols_batchs[[2]])
# D4-DCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[7], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[7], cols = cols_batchs[[2]],
                                                   targetRt = 513, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[7], cols = cols_batchs[[2]])
# D4-DCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[8], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[8], cols = cols_batchs[[2]],
                                                   targetRt = 513, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[8], cols = cols_batchs[[2]])
# D4-CA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[11], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[11], cols = cols_batchs[[2]],
                                                   targetRt = 370, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[11], cols = cols_batchs[[2]])
# D4-CA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[12], cols = cols_batchs[[2]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[12], cols = cols_batchs[[2]],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH", sn = 1))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[12], cols = cols_batchs[[2]],
                                                   targetRt = 370, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[12], cols = cols_batchs[[2]])
# D5-b-MCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[14], cols = cols_batchs[[2]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[14], cols = cols_batchs[[2]][c(1, 7)],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH", sn = 1))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[14], cols = cols_batchs[[2]][c(1, 7)],
                                                   targetRt = 288, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[14], cols = cols_batchs[[2]])
# D5-a-MCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[16], cols = cols_batchs[[2]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[16], cols = cols_batchs[[2]],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH", sn = 1, tol_m = 1))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[16], cols = cols_batchs[[2]],
                                                   targetRt = 288, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[16], cols = cols_batchs[[2]])
# D5-w-MCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[17], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[17], cols = cols_batchs[[2]],
                                                   targetRt = 274, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[17], cols = cols_batchs[[2]])
#D5-a-MCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[18], cols = cols_batchs[[2]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[18], cols = cols_batchs[[2]],
                                              noiseMag = 2, peakPara = get_peakPara(xcms = "BOTH", sn = 1, tol_m = 1))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[18], cols = cols_batchs[[2]],
                                                   targetRt = 287, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[18], cols = cols_batchs[[2]])
# D4-GLCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[19], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[19], cols = cols_batchs[[2]],
                                                   targetRt = 521, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[19], cols = cols_batchs[[2]])
# D4-GLCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[20], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[20], cols = cols_batchs[[2]],
                                                   targetRt = 521, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[20], cols = cols_batchs[[2]])
# D4-GCDCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[21], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[21], cols = cols_batchs[[2]],
                                                   targetRt = 376, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[21], cols = cols_batchs[[2]])
# D4-GCDCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[22], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[22], cols = cols_batchs[[2]],
                                                   targetRt = 376, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[22], cols = cols_batchs[[2]])
# D4-GUDCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[23], cols = cols_batchs[[2]])
MChromatograms <- peakPicking_MChromatograms2(MChromatograms, rows = rows_IS[23], cols = cols_batchs[[2]],
                                              noiseMag = 3, peakPara = get_peakPara(xcms = "BOTH", sn = 1, tol_m = 1))
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[23], cols = cols_batchs[[2]],
                                                   targetRt = 270, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[23], cols = cols_batchs[[2]], targetPeak = TRUE)
# D4-GDCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[24], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[24], cols = cols_batchs[[2]],
                                                   targetRt = 397, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[24], cols = cols_batchs[[2]])
# D4-GUDCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[25], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[25], cols = cols_batchs[[2]],
                                                   targetRt = 273, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[25], cols = cols_batchs[[2]])
# D4-GDCA-1
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[26], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[26], cols = cols_batchs[[2]],
                                                   targetRt = 397, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[26], cols = cols_batchs[[2]])
# D4-GUDCA-3
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[27], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[27], cols = cols_batchs[[2]],
                                                   targetRt = 273, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[27], cols = cols_batchs[[2]])
# D4-GCA-2
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[28], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[28], cols = cols_batchs[[2]],
                                                   targetRt = 275, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[28], cols = cols_batchs[[2]])
#
plotHeatMap_MChromatogramsRow(MChromatograms = MChromatograms, row = rows_IS[29], cols = cols_batchs[[2]])
MChromatograms <- extractTargetPeak_MChromatograms(MChromatograms, rows = rows_IS[29], cols = cols_batchs[[2]],
                                                   targetRt = 275, tolRt = 10)
plotMChromatograms(MChromatograms, rows = rows_IS[29], cols = cols_batchs[[2]])
