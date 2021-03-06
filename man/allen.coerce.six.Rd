% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{allen.coerce.six}
\alias{allen.coerce.six}
\title{Convert a 13 value result vector to a 6 value vector}
\usage{
allen.coerce.six(result.vector, include.indistinct = TRUE)
}
\arguments{
\item{result.vector}{A result vector or an object than can be converted
to a result vector}

\item{include.indistinct}{When TRUE (default), randomly assign tallies for
relations with indistinct endpoints to an adjacent relation.  When FALSE,
ignore tallies for relations with indistinct endpoints.}
}
\description{
Converts a full result vector to a vector representing relations
with distinct endpoints. By default, the tallies for relations
with indistinct endpoints are randomly assigned to one of the
relations adjacent in the Nokel lattice.  Optionally, the tallies
for relations with indistinct endpoints can be ignored.
}
\author{
Thomas S. Dye
}
