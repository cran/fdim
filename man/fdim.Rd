\name{fdim}
\alias{fdim}
\title{
Calculation of generalizated fractal dimension of Data-Sets.
}
\description{
Returns the Fractal Dimension of data-frames.
}
\usage{
fdim(X, BaseR=2, Mnmax=TRUE, nMax=9, NumMinP=1, q=0, Alpha=0.2, PlotF=FALSE)
}
\arguments{
The goal is to calculate the generalizated fractal dimension which can be written:
\cr
Dq=(1/(q-1))*log2(sum(pow(Pj,q)))/log2(r) when (r->0), \cr
where Pj is the probability that the element "j" has model points inside.

\item{X}{Data frame object containing the real data.}
\item{BaseR}{Factor for generation of cubic elements covering the space of state.}
\item{Mnmax}{Criteria for stopping the algorithm. TRUE for make sense the parameter nMax as steps of splitting the global box. In other case the algorithm stops when the number of boxes is greather than the half of number of points.}
\item{nMax}{Number of steps in the splitting process. (the exponent of the BaseR to determine the size of elementary box.)}
\item{NumMinP}{Minimum number of points inside of an elementary box to consider it is filled by points in terms of algorithm.}
\item{q}{Order of fractal dimension to be calculated. (0: Hausdorff Dimension, 0,999999: Information Dimension, 2: Correlation Dimension).}
\item{Alpha}{Confidence parameter to exclude some points if they are quite far in order to evaluate the slope of the linear regresion model.}
\item{PlotF}{Boolean for make possible to plot points directly.}
}
\value{
\emph{fdim} returns fdim, the value of data's q-dimension.

The subobjects returned by \emph{fdim} are:

\item{fdim}{Estimated q fractal dimension.}
\item{points}{Points (size of elements, number of elements with points inside).}
\item{slopeisOK}{Inform us about the reliability of the \emph{fdim} value.}
\item{coefficients}{Structure of linear regression suitable for function \emph{coef}.}
\item{pointsdif}{Number of separated points.}
\item{residual}{Residual of linear regression model.}
\item{sumSQRresidual}{Squared sum of residuals for each point.}
\item{allpoints}{All points calculated, even when they should be removed for the regression model.}
\item{range}{Range of all points}
\item{correlationdim}{All points calculated to obtain the correlation dimension (q=2)}
\item{informationdim}{All points calculated to obtain the information dimension (q=0.9999999999)}
}
\author{
Francisco Javier Martinez de Pison. \email{francisco.martinez@die.unirioja.es}\cr
Joaquin Ordieres Mere.			\email{joaquin.ordieres@dim.unirioja.es}
}

\references{
Halsey C.T., Mogens H.J., Kandanoff L.P., Procaccia I., Shraiman B.I. \bold{"Fractal Measures and their singularities: The caracterization of strange sets"}. Physical Review  vol 33, nº 2. 1986\cr \cr
Roberts J.A., Cronin A. \bold{"Unbiased estimation of multi-fractal dimensions of finite data sets"} \url{http://www.sci.usq.edu.au/pub/MC/staff/robertsa/multif.htm} . July 1996.\cr \cr
David M. Alexander, Phil Sheridan, Paul D. Bourke, Otto Konstandatos. \bold{"Global and local similarity of the primary visual cortex: mechanisms of orientation preference"}. HELNET - International Workshop on Neural Networks, September 1997\cr \cr
Geoffrey B. West, James H. Brown, Brian J. Enquist. \bold{"The Fourth Dimension of Life: Fractal Geometry and Allometric Scaling of Organisms"}., Santa Fe Institute of Research. 1999\cr \cr
Christo Faloutsos, Volker Gaede. \bold{"Analisys of n-dimensional Quadtrees Using the Hausdorff Fractal Dimension"}. Mumbai (Bombay), Proceedings of the 22nd  VLDB Conference, India, 1996.\cr \cr
Alberto Belussi, Christo Faloutsos. \bold{"Estimating the Selectivity of Spatial Queries Using the 'Correlation' Fractal Dimension"}., Zurich, Switzerland, Proceedings of the 21st VLDB Conference, 1995.\cr \cr
Menéndez Fernández C.; Ordieres Meré J.; Ortega Fernández F. \bold{"Importance of information pre-processing importance in the improvement of neural networks results."}. International Journal on Expert System and Neural Networks, Vol. 13, No. 2, pp. 95-103. May 1996.\cr \cr
}

\seealso{
\code{\link{introfdim}}, \code{\link{slopeopt}}, \code{\link{makefract}}
}

\examples{
#Example 1
library(fdim)
data <- makeline(10000,3,100)   	# Build a data-set if the user doesn't has it.
df <- fdim(data,q=0,Alpha=0.3)  	# Calculation of fractal dimension
print (df$fdim)				# Show us the q-dimension value.
print (df)				# Show us relevant information about theestimated fractal dimension.

plot(df$allpoints)			# make a plot in order to see the ipoints individually
abline(coef(df),col=2)			# draw a line in order to see the correlation
					# If we think the confidence criteria must be changed ...
df2 <- slopeopt(as.matrix(df$allpoints),Alpha=0.5)
summary(df2)				# Show us the quality of the approach.

#Example 2
data <- makessphere(1000)		#Build a data-set with points making a Sphere Surface
if (require(xgobi)) xgobi(data)		#Show data	
df <- fdim(data,q=0,Alpha=0.2,PlotF=TRUE)  	# Calculation of fractal dimension
print(df$fdim)				#Fractal Dimension
}
\keyword{robust}
