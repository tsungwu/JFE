\name{riskParityPortfolio}
\alias{riskParityPortfolio}

\encoding{latin1}

\title{Compute risk parity portfolio}

\description{
It calls \code{FRAPO} to compute portfolio weights with equal risk contribution, or equal covariance risk budget, then returns a S4 object of class \code{fPORTFOLIO}.
}
\usage{
riskParityPortfolio(data, covmat="cov", strategy="minrisk",Type="MV")
}

\arguments{
  \item{data}{
timeSeries object of returns data
}

  \item{covmat}{
Function to compute mltvariate covariance matrix, we support five methods:"cov","ledoitWolf","shrink","shrinkCC","slpm". The default is sample covariance "cov".
}

  \item{strategy}{
strategyPortfolio as in package fPortfolio, we support 5 cases in fPortfolio package: "GMVP","maxreturn","minrisk", "tangency" and "All Assets". The default is "minrisk".
}

  \item{Type}{
portfolio type as in package fPortfolio, the default is "MV".
}

}
\details{

The risk parity portfolio has two options: the first is to select a subset of assets and compute risk parity weights. To this end, we implement one of four portfolio strategies: "GMVP","maxreturn","minrisk", "tangency". The idea is that each portfolio strategy will pick the desirable assets by assigning weights, the assets with non-zero weights are selected ones; afterwards, we compute risk parity weights of these assets. Secondly, for "All Assets", all assets are included and compute an optimal weight vector satisfying risk parity condition,namely, equal risk contribution or covariance risk budget.\cr

    \bold{GMVP or Global minimum risk Portfolio:}
    The function \code{minvariancePortfolio} returns the portfolio
    with the minimal risk on the efficient frontier. To find the
    minimal risk point the target risk returned by the function
    \code{efficientPortfolio} is minimized.

    \bold{tangency or maximal returns/risk ratio Portfolio:}
    The function \code{tangencyPortfolio} returns the portfolio with
    the highest return/risk ratio on the efficient frontier. For the
    Markowitz portfolio this is the same as the Sharpe ratio. To find
    this point on the frontier the return/risk ratio calculated from
    the target return and target risk returned by the function .

    \bold{minrisk or Minumum Risk:}
     The function \code{minriskPortfolio} is an efficient portfolio which lies on the efficient
    frontier. The \code{efficientPortfolio} function returns the properties of
    the efficient portfolio as an S4 object of class \code{fPORTFOLIO}

    \bold{maxreturn or Maximum Return Portfolio:}
    The function \code{maxreturnPortfolio} returns the portfolio
    with the maximal return for a fixed target risk. \cr

    Risk parity portfolio calls \code{FRAPO}, which requires symmetric covariance matrices, so far we support only five covariance methods.
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
data(assetReturns)
assetReturns=assetReturns[,11:15]
output=riskParityPortfolio(assetReturns, covmat="cov", strategy="minrisk")
show(output)
getWeights(output)
getCovRiskBudgets(output)
}

