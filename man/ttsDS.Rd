\name{ttsDS}
\alias{ttsDS}

\encoding{latin1}

\title{Generates the data structure that is used for training(estimation) and validation}

\description{
This function generates the dataset's lag structure that is used for the estimation of ttsCaret, ttsAutoML and ttsLSTM.
}
\usage{
ttsDS(y,x=NULL, arOrder=2,xregOrder=0,type=NULL)
}

\arguments{
  \item{y}{
The time series of target variable, or the dependent variable, with \code{timeSeries} format.
}

  \item{x}{ The time series of input variables, or the independent variables, with \code{timeSeries} format.
}

\item{arOrder}{The autoregressive order of the target variable, which may be sequentially specifed like arOrder=1:5; or discontinuous lags like arOrder=c(1,3,5); zero is not allowed.

}

\item{xregOrder}{The distributed lag structure of the input variables, which may be sequentially specifed like xregOrder=0:5; or discontinuous lags like xregOrder=c(0,3,5); zero is allowed since contemporaneous correlation is allowed.

}

\item{type}{The additional input variables. We have four selection:\cr "none"=no other variables,\cr "trend"=inclusion of time dummy, \cr "season"=inclusion of seasonal dummies, \cr "both"=inclusion of both trend and season. No default.

}



}


\details{

This function generates the dataset's lag structure that is used for estimating ttsCaret, ttsAutoML, and ttsLSTM functions.
}



\value{

  \item{DF}{The data used by arOrder, xregOrder, and type}

}


\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{
#library(timeSeries)
data(macrodata)
Y <- macrodata[,"unrate"]
X <- macrodata[,-1]
head(ttsDS(y=Y, x=X, arOrder=c(1:2),xregOrder=c(0,1), type="both"))


}

