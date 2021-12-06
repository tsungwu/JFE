\name{downloadStockAI}
\alias{downloadStockAI}

\encoding{latin1}

\title{Download time series data from stock-ai.com}

\description{
It downloads financial and economic time series data from the web stock-ai.com.
}
\usage{
downloadStockAI(key="5edl69aag5",var.name="TWECO", from="2006-01-01", to="2015-12-31",
                             showdata=TRUE)
}

\arguments{
  \item{key}{
key issued from the web, default is "5edl69aag5".
}

  \item{var.name}{The name of variable to be downloaded, the default is Taiwan's economic growth "TWECO", one variable for each download. The default time range is ten years.

}

  \item{from}{The start date of var.name, the default is "2006-01-01".
}

  \item{to}{The end date of var.name, the default is "2015-12-31".

}

\item{showdata}{Whether the downloaded variable is shown on screen. The default is TRUE.

}


}


\details{

This function connects with stock-ai.com and downloads the specified data.
}



\value{

  \item{y}{The timeSeries data object}

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{
# Cross-validation takes time, example below is commented.
#library(timeSeries)
#downloadStockAI(var.name="UNRATE") #US unemployment rate
#downloadStockAI() #Taiwan economic growth rate

}

