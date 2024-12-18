\name{crunchy}

\alias{crunchy}

\docType{data}

\title{Crunchy cata}

\description{
The data used here refer to crispness.
}

\usage{data(crunchy)}

\format{
A contingency table
}

\source{
Department of statistics and computer science, Institut Agro
}

\examples{
     data(crunchy)
     crunchy = tab2data(crunchy)
     crunchy2d = bt2d(crunchy)
     anovabt2d(crunchy2d)
     summary(crunchy2d)
     plot(crunchy2d)
}

\keyword{datasets}
