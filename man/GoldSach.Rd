\name{GoldSach}

\alias{GoldSach}

\title{
Goldman-Sach covariance estimator for portfolio optimization.
}


\encoding{latin1}


\description{
Goldman-Sach covariance estimator with constant correlation
}

\usage{GoldSach(x,spec = NULL)
}
\arguments{
  \item{x}{Time series object}
  \item{spec}{NULL}

  }


\details{
Generate a Litterman-Winkelmann covariance estimator proposed in Goldman-Sachs Risk Management Series. In JFE package, we fix rho to be 0.95.
}

\value{
Generate a Goldman-Sach covariance covariance estimator.
}

\references{
Robert Litterman and Kurt Winkelmann (1998) Estimating Covariance Matrices. Risk Management Series, Gold Sachs.
}

\author{
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}
