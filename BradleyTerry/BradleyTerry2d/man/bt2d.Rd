\name{anovabt2d}

\alias{anovabt2d}

\title{Two-dimensional Bradley-Terry modeling}

\description{
Compute the analysis of deviance table for the 2D Bradley-Terry
     model.
}


\usage{
anovabt2d(model)}

\arguments{
     \item{model}{A 2D Bradley-Terry model object as returned by ‘bt2d’}
}

\details{The estimating algorithm is described in Causeur and Husson
     (2005).}


\value{
     This (generic) function returns an object of class ‘anova’ as
     defined in package ‘stats’.  These objects represent
     analysis-of-variance and analysis-of-deviance tables.  It produces
     a table which tests whether the 2D model is significant wrt the
     standard Bradley-Terry model.

     The print method for ‘anova’ objects prints tables in a ‘pretty’
     form.
}

\references{Causeur, D. and Husson, F. (2005). A 2-dimensional extension of
     the Bradley-Terry model for paired comparisons.  Journal of
     Statistical Planning and Inference. 135, 245-259.}

\author{D. Causeur and F. Husson}

\seealso{ \code{\link{bt2d}}, \code{\link{summary.bt2d}}, \code{\link{plot.bt2d}}}

\examples{
     data(crunchy)
     crunchy = tab2data(crunchy)
     crunchy2d = bt2d(crunchy)
     anovabt2d(crunchy2d)
     summary(crunchy2d)
     plot(crunchy2d)
}

