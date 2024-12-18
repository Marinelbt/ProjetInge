\name{plot.bt2d}

\alias{plot.bt2d}

\title{Two-dimensional Bradley-Terry modeling}

\description{
Provides the 2D representation of the compared items in the same
     graph as the standard Bradley-Terry scores. Confidence ellipses
     can be added to the graph in order to figure out the variability
     of estimation.
}

\usage{
\method{plot}{bt2d}(x, lab = names(x$lambda1), alpha = 0.05, deltax = 0.025, 
                 deltay = 0.025, lty = 1, col = "blue", lwd = 2, ellipse = TRUE, \dots)
}

\arguments{
\item{x}{A 2D Bradley-Terry model object as returned by ‘bt2d’}
\item{lab}{Labels of the compared labels. By default, these labels are
          extracted from the names of the 2D Bradley-Terry scores.}
\item{alpha}{Type-I level for the confidence ellipses}
\item{deltax}{Add a small horizontal distance between the labels and the
          points (as a fraction of the x-axis range).}
\item{deltay}{Add a small vertical distance between the labels and the
          points (as a fraction of the y-axis range).}
\item{lty}{Line type for the confidence ellipses (see ‘plot’ for
          details)}
\item{col}{Color for the 2D Bradley-Terry scores}
\item{lwd}{Line width for the confidence ellipses (see ‘plot’ for
          details)}
 \item{ellipse}{If TRUE, confidence ellipses are drawn.}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\details{The estimating algorithm is described in Causeur and Husson
     (2005).}

\value{
Returns a plot
}

\references{Causeur, D. and Husson, F. (2005). A 2-dimensional extension of
     the Bradley-Terry model for paired comparisons.  Journal of
     Statistical Planning and Inference. 135, 245-259.}

\author{D. Causeur and F. Husson}

\seealso{\code{\link{bt2d}}, \code{\link{anovabt2d}}, \code{\link{summary.bt2d}}}

\examples{
     data(crunchy)
     crunchy = tab2data(crunchy)
     crunchy2d = bt2d(crunchy)
     anovabt2d(crunchy2d)
     summary(crunchy2d)
     plot(crunchy2d)
}
