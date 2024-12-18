\name{summary.bt2d}

\alias{summary.bt2d}

\title{Two-dimensional Bradley-Terry modeling}

\description{
Produces summaries of the results of the ‘bt2d’ fitting function..
}

\usage{
\method{summary}{bt2d}(object, \dots)
}

\arguments{
\item{object}{A 2D Bradley-Terry model object as returned by ‘bt2d’}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\value{
 The form of the value returned by ‘summary.bt2d’ is the same
     as for the result of ‘summary.glm’ in package ‘stats’.
}

\references{Causeur, D. and Husson, F. (2005). A 2-dimensional extension of
     the Bradley-Terry model for paired comparisons.  Journal of
     Statistical Planning and Inference. 135, 245-259.}

\author{D. Causeur and F. Husson}

\seealso{\code{\link{bt2d}}, \code{\link{anovabt2d}}, \code{\link{plot.bt2d}}}

\examples{
     data(crunchy)
     crunchy = tab2data(crunchy)
     crunchy2d = bt2d(crunchy)
     anovabt2d(crunchy2d)
     summary(crunchy2d)
     plot(crunchy2d)
}
