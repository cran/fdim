\name{makefract}
\alias{makefract}
\alias{makeline} 	
\alias{makehplane}
\alias{makessphere}
\alias{makesphere}
\alias{makesurface}

\title{
Functions to create data example for the library 'fdim'.
}
\description{
Several functions to create clouds of points forming lines, surfaces and spheres...}
\usage{

makeline(NumN=100, DimM=3, Longit=100)
makehplane(NumN=100, DimM=3, Longit=100) 
makessphere(NumN=100, L=100)
makesphere(NumN=100, L=100)
makesurface(E1, E2, NumN=1000)

}

\arguments{
\item{NumN}{Number of points to be generated.}
\item{DimM}{Dimension of global space where the object is generated.}
\item{Longit}{Lenght of segment to be built.}

\item{L}{Radius of sphere to be generated.}

\item{E1,E2}{Coefficients of an exponential model modelling the surface 
                    \eqn{z=x^{E1}+y^{E2})}{z=x^E1+y^E2} where \eqn{-2 < x,y < 2}{-2< x,y <2} }
}

\author{
Francisco Javier Martinez de Pison. \email{francisco.martinez@dim.unirioja.es}\cr
Joaquin Ordieres Mere.              \email{joaquin.ordieres@dim.unirioja.es}\cr
Manuel Castejon Limas.              \email{manuel.castejon@dim.unirioja.es}\cr
Fco. Javier de Cos Juez.            \email{francisco-javier.de-cos@dim.unirioja.es}\cr}

\seealso{
\code{\link{introfdim}}, \code{\link{fdim}}, \code{\link{slopeopt}}
}
\keyword{data}
