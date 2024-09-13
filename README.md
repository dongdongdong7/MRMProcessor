# MRMProcessor

## Install

```R
# install.packages("BiocManager")
# install.packages("devtools")
BiocManager::install("xcms")
devtools::install_github("dongdongdong7/MRMProcessor")
devtools::install_github("rstudio/gridlayout")
```

### Workflow

```R
readMRMData
peakPicking_MChromatograms
prepare_MChromatograms
extractTargetPeak_MChromatograms # IS
rtCorrection_IS
```



