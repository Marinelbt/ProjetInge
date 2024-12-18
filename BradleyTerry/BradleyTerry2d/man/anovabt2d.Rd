\name{bt2d}

\alias{bt2d}

\title{Two-dimensional Bradley-Terry modeling}

\description{
Fits a 2D Bradley-Terry model using an iterative Newton-Raphson
     algorithm.
}

\usage{
bt2d(data, tol = 1e-04, max.iter = 20)}

\arguments{
     \item{data}{a data frame with 3 columns named Winner, Loser and Freq.
          'Winner' and 'Loser' give the label of the winner and the
          loser respectively and 'Freq' gives the number of times
          'Winner' has beaten 'Loser'. Consequently the number of rows
          of data is the number of possible pairwise comparisons. Use
          ‘tab2data’ to convert contingency tables giving the
          number of times Winners (rows) have beaten Losers (columns)
          into the 3 column data.frame form.}
     \item{tol}{Limiting value used to stop the estimating algorithm. If the
          difference between the residual deviances of two succesive
          models in the iterative algorithm is lower than ‘tol’
          (1e-04 by default), then the algorithm stops.}
     \item{max.iter}{Maximum number of iterations in the estimating algorithm (20
          by default).}
}

\details{The estimating algorithm is described in Causeur and Husson
     (2005).}


\value{
Returns several tabs:
\item{lambda1}{2D Bradley-Terry scores on the x-axis}
\item{lambda2}{2D Bradley-Terry scores on the y-axis}
\item{lambda}{Standard Bradley-Terry scores}
\item{res.deviance2}{Residual deviance of the 2D Bradley-Terry model}
\item{res.df2}{Number of residual degrees of freedom of the 2D Bradley-Terry
          model}
\item{res.deviance1}{Residual deviance of the standard Bradley-Terry model}
\item{res.df1}{Number of residual degrees of freedom of the standard
          Bradley-Terry model}
\item{res.deviance0}{Residual deviance of the null model (all items are
          equivalent)}
\item{res.df0}{Number of residual degrees of freedom of the null model}
\item{varscores}{Variance matrix of the concatenated vector of 2D
          Bradley-Terry scores}
\item{s}{Summary of the ‘glm’ model used to fit the standard
          Bradley-Terry model}
\item{iter}{Number of iterations before the algorithm stops}
\item{residuals}{Deviance residuals of the 2D Bradley-Terry model}
}

\references{Causeur, D. and Husson, F. (2005). A 2-dimensional extension of
     the Bradley-Terry model for paired comparisons.  Journal of
     Statistical Planning and Inference. 135, 245-259.}

\author{D. Causeur and F. Husson}

\seealso{ \code{\link{anovabt2d}}, \code{\link{summary.bt2d}}, \code{\link{plot.bt2d}}}

\examples{
     data(crunchy)
     crunchy = tab2data(crunchy)
     crunchy2d = bt2d(crunchy)
     anovabt2d(crunchy2d)
     summary(crunchy2d)
     plot(crunchy2d)
}
