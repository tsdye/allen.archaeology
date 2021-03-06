% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{allen.compare.indeterminate.intervals}
\alias{allen.compare.indeterminate.intervals}
\title{Compare indefinite intervals}
\usage{
allen.compare.indeterminate.intervals(result.vector, mcmc.chains)
}
\arguments{
\item{result.vector}{A result vector}

\item{mcmc.chains}{A list of four MCMC chains where the first and second
members represent the start and end of the first interval, respectively, and
the third and fourth represent the start and end of the second interval,
respectively.}
}
\description{
Compare two indefinite intervals estimated by four MCMC chains and tally the
relations recorded there.  Report the results as a named vector.
}
\author{
Thomas S. Dye
}
