files_dir <- "../Carnitine_SBR/MRMprocesser/"
patterns <- c(".mzXML", ".mzxml", ".mzML", ".mzml")
patterns <- paste0(patterns, collapse = "|")
files_path <- list.files(files_dir, pattern = patterns)
files_path <- paste0(files_dir, files_path)
windowInfo_path <- "../Carnitine_SBR/MRMprocesser/windowInfo_car_cll.xlsx"
sampleInfo_path <- "../Carnitine_SBR/MRMprocesser/sampleInfo_car.xlsx"
windowInfo <- openxlsx::read.xlsx(windowInfo_path, sheet = 1)
sampleInfo <- openxlsx::read.xlsx(sampleInfo_path, sheet = 1)
chr_grid <- readMRMData(files = files_path, windowInfo = windowInfo, sampleInfo = sampleInfo[1:10, ])
chr_grid$findPeaks_ChrGrid(thread = 2)
chr_grid$extend_ChrGrid()
chr_grid$extract_targetPeak_ChrGrid()
chr_grid$cal_rtshift()
chr_grid$correct_rtshift()
chr_grid$extract_targetPeak_ChrGrid()
chr_grid$drop_rtshift()
