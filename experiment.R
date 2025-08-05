files_dir <- "../Carnitine_SBR/MRMprocesser/"
patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
patterns <- paste0(patterns, collapse = "|")
files_path <- list.files(files_dir, pattern = patterns)
files_path <- paste0(files_dir, files_path)
windowInfo_path <- "../Carnitine_SBR/MRMprocesser/windowInfo_car_cll.xlsx"
sampleInfo_path <- "../Carnitine_SBR/MRMprocesser/sampleInfo_car.xlsx"
windowInfo <- openxlsx::read.xlsx(windowInfo_path, sheet = 1)
sampleInfo <- openxlsx::read.xlsx(sampleInfo_path, sheet = 1)
chr_grid <- readMRMData(files = files_path, windowInfo = windowInfo, sampleInfo = sampleInfo[1:5, ], thread = 2)
chr_grid$findPeaks_ChrGrid(peakwidth = c(2, 20), snthresh = 3, thread = 3)
chr_grid$extend_ChrGrid(thread = 1)
chr_grid$extract_targetPeak_ChrGrid(thread = 1)
chr_grid$get(200,1)$cal_rtshift_chr(chr_grid = chr_grid)
chr_grid$get(1,1)$extract_targetPeak_chr()
chr_grid$cal_rtshift(thread = 4)
chr_grid$cal_rtshift(4,3,thread = 1)
chr_grid$correct_rtshift(thread = 4)
chr_grid$drop_rtshift(thread = 4)
chr_grid$get(124,4)$expectRt
chr_grid$get(125,4)$expectRt
chr_grid$get(125,4)$rtdifference
chr_grid$get(125,4)$peaks
chr_grid$get(125,5)$plot_chr(target = FALSE)
chr_grid$get(15, 4)$plot_chr()
chr_grid$get(15, 4)$expectRt
chr_grid$get(15, 4)$rtdifference
chr_grid$get(15, 4)$rtshift

x <- c(1, 3, 4, 9, 8, 6, 5)  # 时间序列 1
y <- c(1, 2, 3, 4, 5, 6, 7)  # 时间序列 2

# 可视化
plot(x, type = "l", col = "blue", lwd = 2, xlab = "Time", ylab = "Value", ylim = c(0, 10))
lines(y, col = "red", lwd = 2)
legend("topright", legend = c("x", "y"), col = c("blue", "red"), lwd = 2)

# 计算 DTW 对齐
alignment <- dtw::dtw(x, y, keep = TRUE)

# 查看 DTW 距离
print(alignment$distance)  # 输出最小累积距离

# 可视化对齐路径
plot(alignment, type = "two", col = c("blue", "red"), lwd = 2,
     xlab = "Index (x)", ylab = "Index (y)", main = "DTW Alignment Path")
