\name{tab2data}

\alias{tab2data}

\title{Transform tab to data}

\description{
Transform a contingency table to a data fram useful to construct a model.
}


\usage{
tab2data(tab)}

\arguments{
     \item{tab}{A 2D Bradley-Terry model object as returned by ‘bt2d’}
}

\details{The estimating algorithm is described in Causeur and Husson
     (2005).}


\author{D. Causeur and F. Husson}

\seealso{ \code{\link{bt2d}}, \code{\link{summary.bt2d}}, \code{\link{plot.bt2d}}}

\examples{
     data(crunchy)
     crunchy = tab2data(crunchy)
}

