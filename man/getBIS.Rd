\name{getBIS}
\alias{getBIS}

\encoding{latin1}

\title{Download time series data from Bank of International Settlement}

\description{
It downloads effective exchange rates from Bank of International Settlement.
}
\usage{
getBIS(sheet="Real", type="broad")
}

\arguments{
  \item{sheet}{
The name of spreedsheet of effective exchange rates (EER)of BIS, it has two options: "Real" and "Nominal", the default is "Real" for REER.
}

  \item{type}{The type of EER, it has two types: "broad" and "narrow". The default is "broad".

}


}


\details{

This function connects with <"https://www.bis.org/statistics/eer/"> and downloads the specified data. The rownames of downloaded data embeds the timestamp already, which can be directly transformed into time series via, as.timeSeries.
}


\value{

  \item{data}{The data object.}
  \item{country.info}{The country information with abbreviated symbol.}
  \item{data.info}{The information about effective exchange rates.}

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{
output=getBIS(sheet=c("Nominal","Real")[1], type=c("broad","narrow")[1])
output$data
output$data.info
output$country.info

}
