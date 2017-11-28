\name{IBM}
\alias{IBM}
\docType{data}
\title{
Daily Price Data of IBM
}

\encoding{latin1}

\description{
Daily price data of IBM, 2007/4/24~2017/4/21
}
\usage{data("IBM")}
\format{
  A xts object with 2518 observations on the following 5 variables.
  \describe{
    \item{\code{Open}}{A numeric vector, open price}
    \item{\code{High}}{A numeric vector, maximum price}
    \item{\code{Low}}{A numeric vector, minimum price}
    \item{\code{Close}}{A numeric vector, close price}
    \item{\code{Volume}}{A numeric vector, trading volume}

  }
}


\details{
Daily stock price data of IBM. Transform this dataset into IBM.RData and use it as an example dataset for Price and Returns Analytics. The code easy: data(IBM);save(IBM,file="IBM.RData")
}
\source{
Yahoo finance.
}
