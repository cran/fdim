\name{slopeopt}
\alias{slopeopt}
\title{
Calculation of the optimal slope of the straight line of regression of the points returned by 'fdim'.
}
\description{
This function obtains the straight line of regression of the introduced points. If this one does not fulfill the criterion of distance of the residuals, the remotest point is eliminated and it becomes to calculate the straight line. So on until the criterion is fulfilled.
}
 
\usage{
slopeopt(X, Alpha=0.5, Debug=FALSE)
}

\arguments{
\item{X}{Matrix with the first column for size of cubic elements and the second one for the number of elements with points inside.}
\item{Alpha}{Confidence parameter for distance criteria to the regression linear model.}
\item{Debug}{Boolean parameter setting the level of information to be shown.}
}

\value{
\emph{slopeopt} returns parameters of the regression line.

The subobjects returned by \emph{slopeopt} are:

\item{IsOk}{FALSE=Regression straight line not find}
\item{points}{Points used to calculate the regression straight line.}
}


\author{
Francisco Javier Martinez de Pison. \email{francisco.martinez@die.unirioja.es}\cr
Joaquin Ordieres Mere.			\email{joaquin.ordieres@dim.unirioja.es}
}

\seealso{
\code{\link{introfdim}}, \code{\link{fdim}}, \code{\link{makefract}}
}
\keyword{robust}

