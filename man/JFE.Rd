\name{JFE}

\alias{JFE}

\title{
Display the JFE User Interface
}

\encoding{latin1}

\description{
Start the JFE GUI (graphical user interface)
}

\usage{JFE()
}


\details{
After loading the package, in the command prompt, type JFE() to start the GUI. JFE is a platform-dependent GUI designed to support the analysis of financial time series data, with specific focus on, firstly, assets selection based on Performance index; secondly, portfolio optimization.\cr
This command is an internal function to start the JFE GUI. To avoid unexpected problems of time series object, the imported data must be time series object (xts, or timeSeries) loaded by either  .RData or .rda;
}

\value{
Generate a menu-driven GUI
}

\examples{
JFE()
#
}



\author{
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}



