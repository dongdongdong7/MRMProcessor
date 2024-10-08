% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peakPicking.R
\name{get_peakPara}
\alias{get_peakPara}
\title{get_peakPara}
\usage{
get_peakPara(
  sn = 3,
  above = "baseline",
  preNum = 3,
  extend = 5,
  tol_m = 10,
  multiSmooth = TRUE,
  cal_ZOI_baseline = TRUE,
  fwhm = NA,
  snthresh = 0.5,
  peakWidth = NA,
  xcms = "BOTH"
)
}
\arguments{
\item{sn}{sn threshold.}

\item{above}{ZOI creating method.}

\item{preNum}{preNum.}

\item{extend}{extend.}

\item{tol_m}{tol_m.}

\item{multiSmooth}{Whether or not to give multiple smoothing to the ZOI region, which helps the accuracy of edge finding.}

\item{cal_ZOI_baseline}{Whether or not to calculate a baseline for the ZOI separately, which helps the accuracy of edge finding.}

\item{fwhm}{This is the parameter used in the matchfilter function to determine the multi-peak case.
The smaller the value the easier it is to find multiple peaks.}

\item{snthresh}{This is the parameter used in the matchfilter function to determine the multi-peak case.}

\item{peakWidth}{peakWidth for CentWave algorithm.}

\item{xcms}{xcms methods, CentWave and Matched}
}
\value{
A list.
}
\description{
Get peakPara list.
}
\examples{
peakPara <- get_peakPara()
}
