\name{getTWSE.fiveSecond}
\alias{getTWSE.fiveSecond}

\encoding{latin1}

\title{Download Download 5-second index price from the Taiwan Stock Exchange}

\description{
It downloads Download 5-second financial index from the Taiwan Stock Exchange.
}
\usage{
getTWSE.fiveSecond(ymd=NULL,skip=2,index.names=NULL)
}

\arguments{
  \item{ymd}{
The year-month-day format of data retrived, for example, "2022-05-03". If unspecified, the default is the Sys.date(); if Saturday, it moves back to Friday; if Sunday, it moves to Monday.
}

  \item{skip}{The rows to skip, since the web usually contains empty space or notes in the beginning, currently, skip 2 lines is OK, however, it skipped 1. Therefore, the parameter can be checked if it changes again.

}

\item{index.names}{The colnames (names of index price) of the dataset are originally in Chinese characters, to avoid language problem, we changed it to V1~V34. Users may offer your own colnames here.}

}



\details{

This function connects with <"https://www.twse.com.tw/exchangeReport/MI_5MINS_INDEX"> and downloads the specified market data. The rownames of downloaded data embeds the timestamp already.
}


\value{

  \item{data}{The data retrieved.}

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{

#getTWSE.fiveSecond(ymd="2022-05-03",skip=2,index.names=NULL)$data

}
