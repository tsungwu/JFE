\name{riskOptimalPortfolio}
\alias{riskOptimalPortfolio}

\encoding{latin1}

\title{Compute risk optimal portfolios maxDD, aveDD and CDaR}

\description{
It calls \code{FRAPO} to compute risk optimal portfolio satisfying the constraint of draw downs and returns a S4 object of class \code{fPORTFOLIO}.
}
\usage{
riskOptimalPortfolio(data, Type="AveDD",value)
}

\arguments{
  \item{data}{
timeSeries object of price data. Please remember the asset data must be price, not returns.
}

  \item{Type}{
Drawdown types, we call package FRAPO to support three methods:"maxDD","aveDD",and "CDaR".  For details, please see document of package FRAPO.
}

  \item{value}{ Positive numerical number for Type.

}

}
\details{

The risk optimal portfolio  calls \code{FRAPO} and wrapp the results as a S4 object of class \code{fPORTFOLIO}, all get functions of \code{fPORTFOLIO} are applicable.
}
\value{
returns an S4 object of class \code{"fPORTFOLIO"}.
}
\references{
Roncalli Thierry, \emph{Introduction to Risk parity and Budgeting}, 2014, CRC inc..\cr
See also packages \code{fPORTFOLIO} and \code{FRAPO}
}
\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University.

}

\examples{
# Risk optimal portfolio takes time, example below is commented.
#data(LPP2005,package="fPortfolio")
Data =  fPortfolio::LPP2005[,1:6]#select 6 assets price
Data.RET=timeSeries::returns(Data) # Transform into returns to compute VALUE below
#VALUE=abs(mean(drawdowns(apply(Data.RET,1,mean))))
#output=riskOptimalPortfolio(Data,Type="AveDD",value=VALUE) # data input must be price.
#show(output)
#getWeights(output)
#getCovRiskBudgets(output)
}

