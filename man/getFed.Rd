\name{getFed}
\alias{getFed}

\encoding{latin1}

\title{Download financial and economic time series data from the Fed}

\description{
It downloads financial and economic time series data from the Fed.
}
\usage{
getFed(var.name="UNRATE", from="1900-01-01",end=Sys.Date(), do.plot=TRUE)
}

\arguments{
  \item{var.name}{
The name of variable as used by <"https://fred.stlouisfed.org">. For example, "UNRATE" denotes unemployment rate, "CPIAUCSL" is monthly consumer price index, "GDP" is Gross domestic product, and "ICSA" is the initial claim seasonally adjusted. User please check with the web for symbol.
}

  \item{from}{The starting date, the default is 1900-01-01 to ensure the very beginning.

}

  \item{end}{The ending date of data retrieved, the default is Sys.Date().}


  \item{do.plot}{Whether to plot the retrieved data, the default is TRUE. Please set this value to be FALSE to avoid too many plotting, if you want to loop more downloads. It applies functionseriesPlot() from package fBasics.}

}


\details{

This function connects with <"https://fred.stlouisfed.org"> and downloads the specified market data. The rownames of downloaded data is embedded by the timestamp already.
}


\value{

  \item{data}{The data retrieved.}

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{

getFed(var.name="UNRATE")$data

}
