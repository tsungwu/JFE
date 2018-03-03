\name{Portfolio-covFunctions}



\alias{covLedoit}
\alias{covStudent}
\alias{GMVPStrategy}
\alias{GoldSach}
\alias{ShrinkCC}
\alias{SKCov}


\title{User defined functions to perform portfolio backtesting}

\encoding{UTF-8}

\description{

    Covariance estimators and GMVP strategy functions used for portfolio
    backtesting.

}


\usage{
covLedoit(data,spec = NULL)
covStudent(data,spec = NULL)
GMVPStrategy(data, spec = portfolioSpec(), constraints = "LongOnly",
    backtest = portfolioBacktest())
GoldSach(data,spec = NULL)
ShrinkCC(data,spec = NULL)
SKCov(data)
}



\arguments{


    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to
        convert your time series.
        }
    \item{backtest}{
        an S4 object of class \code{fPFOLIOBACKTEST} as returned by the
        function \code{portfolioBacktest}.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }
    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }

}


\details{

    \bold{covLedoit:}\cr

    A wrapper for Ledoit-Wolf Shrinkage covariance estimator

    The function requires two arguments: \code{data} and
    \code{spec}, see above.


    \bold{covStudent:}\cr

    Defines Student t covariance estimator

    The function requires two arguments: \code{data} and
    \code{spec}, see above.

    \bold{GMVPStrategy:}\cr

    A pre-defined tangency portfolio strategy.

    The function requires four arguments: \code{data}, \code{spec},
    \code{constraints} and \code{backtest}, see above.



    \bold{GoldSach:}\cr

    Defines a Litterman-Winkelmann(1998) covariance estimator

    The function requires two arguments: \code{data} and
    \code{spec}, see above.


    \bold{ShrinkCC:}\cr

    Defines a function for Ledoit-Wolf covariance estimator with constant correlation.

    The function requires two arguments: \code{data} and
    \code{spec}, see above.

    \bold{SKCov:}\cr

    Defines a function for Ledoit-Wolf covariance estimator

    The function requires one argument: \code{data}, see above.

}

\value{

    \code{covLedoit}\cr
    function returns a covariance matrix .

    \code{covStudent}\cr
    function returns a covariance matrix .

    \code{GMVPStrategy}\cr
    function returns a S4 object of class \code{"fPORTFOLIO"}.

     \code{GoldSach}\cr
    function returns a covariance matrix.

     \code{ShrinkCC}\cr
    function returns a covariance matrix.

     \code{SKCov}\cr
    function returns a covariance matrix.

}

\references{
Kent J. T., D. E. Tyler and Y. Vardi (1994). A curious likelihood identity for the multivariate t distribution.  Communications in Statistics—Simulation and Computation 23, 441–453.\cr
Ledoit, O. and M. Wolf (2003) Improved estimation of the covariance matrix of returns with an application to portfolio selection.  Journal of Empirical Finance 10, 603-621.\cr
Ledoit, O. and M. Wolf (2004) Honey, I shrunk the sample covariance matrix. Journal of Portfolio Management 30, 110-119.\cr
Robert Litterman and Kurt Winkelmann (1998) Estimating Covariance Matrices. Risk Management Series, Gold Sachs.
}

\author{
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}

\keyword{models}
