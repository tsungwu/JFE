\name{JFE}

\alias{JFE}

\title{
Display the JFE User Interface
}


\encoding{latin1}


\description{
Start the JFE GUI (graphical user interface)
}

\usage{JFE()
}


\details{
After loading the package, in the command prompt, type JFE() to start it. JFE is a menu-driven GUI designed to support the analysis of financial time series data with the aid of several R packages. The version 1.1 focuses on: Firstly, price visualization, including technical charting(by package quantmod); secondly, assets selection based on Performance index(by package PerformanceAnalytics); thirdly, portfolio optimization (by package \code{"fPORTFOLIO"}).\cr
This command is an internal function to start the JFE GUI. To avoid unexpected problems of time series object, the imported data must be time series object (xts, or timeSeries) loaded by either  .RData or .rda, file of .csv or other format is not supported; that is to say, users have only to know how to construct a R time-series object.\cr
If execution of All-in-one from backtesting fails, then it is a problem associated with undocumented functions. Please re-install this package from Github via devtools::install_github("tsungwu/JFE"), detailed are also explained in Github and http://web.ntnu.edu.tw/~tsungwu/R_DevOps/R_DevOps.htm.
}

\value{
Generate a menu-driven GUI
}

\examples{

JFE()

}



\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.
}
