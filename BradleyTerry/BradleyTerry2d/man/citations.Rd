\name{citations}

\alias{citations}

\docType{data}

\title{Citations cata}

\description{
The data used here refer to citations.
}

\usage{data(crunchy)}

\format{
A contingency table
}

\source{
Department of statistics and computer science, Institut Agro
}

\examples{
     data(citations)
     citations = tab2data(citations)
     citations2d = bt2d(citations)
     anovabt2d(citations2d)
     summary(citations2d)
     plot(citations2d)
}

\keyword{datasets}
