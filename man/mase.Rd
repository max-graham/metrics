% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error.R
\name{mase}
\alias{mase}
\title{MASE}
\usage{
mase(x, target, m = 1, na.rm = FALSE)
}
\arguments{
\item{x}{numeric vector; the forecast to evaluate.}

\item{target}{numeric vector; the target values.}

\item{m}{atomic integer; the seasonal period. Defaults to 1 if non-seasonal.}

\item{na.rm}{logical; should NAs be removed before calculation?}
}
\value{
An atomic numeric vector containing the calculated MASE.
}
\description{
Calculates the Mean Absolute Scaled Error(MASE) between x and the
target. This is intended to be used for measuring the accuracy of a
time series forecast, since it reports the error in the context of the
naive forecast (random walk for non-seasonal, or a seasonal naive otherwise).
}
\note{
This will return \code{Inf} when the last \code{length(x) - m}
  values in target are all the same.
}
\examples{
\dontrun{mase(c(1, 2, 3))}            #> Error: different lengths (3, 0)
mase(c(1, 2, 3), c(1, 4, 4))          #> 0.66667
mase(c(1, 2, NA, 4), c(2, 2, 5, NA))  #> NA
mase(c(1, 2, 3, NA, 4),
     c(2, 2, 5, NA, NA),
     na.rm = TRUE)                    #> 0.66667
}
