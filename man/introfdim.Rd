\name{introfdim}
\alias{introfdim}
\title{ Introduction to the calculation of the dimension fractal.  }

\description{ Shows the theoretical basis regarding the fractal dimension measurement.  }

\details{
Fractals burst into the open in early 1970s. Their breathtaking beauty 
captivated many a layman and a professional alike. 
Striking fractal images can often be obtained with very elementary means. 
However, the definition of fractals is far from being trivial and depends 
on a formal definition of dimension.
It takes a few chapters of an Advanced Analysis book to rigorously define
 a notion of dimension. 
The important thing is that the notion is not unique and even more importantly,
 for a given set, various definitions may 
lead to different numerical results. When the results differ the set is called fractal. 
Or in the words of Benoit Mandelbrot, the father of fractals:
\emph{A fractal is by definition a set for which the Hausdorff-Besicovitch dimension
 strictly exceeds the topological dimension.}

The topological dimension of a smooth curve is, as one would expect,
 one and that of a sphere is two which may 
seem very intuitive. However, the formal definition was only given in 1913
 by the Dutch mathematician 
L. Brouwer (1881-1966). A (solid) cube has the topological dimension of three because
 in any decomposition of 
the cube into smaller bricks there always are points that belong to at least four 
\eqn{\left(3+1\right)}{(3+1)} bricks.
The Brouwer dimension is obviously an integer. 
The Hausdorff-Besicovitch dimension, on the other hand, may be a fraction. 
Formal definition of this quantity requires a good deal of the Measure Theory. 
But fortunately for a class of sets Hausdorff-Besicovitch dimension can be easily evaluated. 
This sets are known as the self-similar fractals and, because of that ease, the property of 
self-similarity is often 
considered to be germane to fractals in general. 
The applet below illustrates the idea of self-similarity.


The similarity dimension of the snowflake curve is finite. This is arrived at in the following 
manner. 
If it were a straight line we could split it into to smaller segments each half the length 
of the "parent" line.  The length of the line would be the sum of the two smaller segments. 
If we were talking about areas, then taking a 
square and splitting it into 4 smaller squares with areas \eqn{\frac{1}{4}}{1/4} 
of the "parent" square.  We would observe that the four smaller areas sum up to the original 
size. Notice that when the side of a square is halved, 
its area decreases by the factor of 4 which is \eqn{\left(\frac{1}{2}\right)^2}{(1/2)^2}. 
For a cube, acting similarly, decreasing its size by a factor of 2, 
results in smaller cubes each with the volume \eqn{\frac{1}{8}=\left(\frac{1}{2}\right)^3}{(1/8)=(1/2)^3} 
of the "parent" cube. We can detect a commonality in these 
three examples. Given a shape of size \eqn{S}{S}. It's split into \eqn{N}{N} similar smaller 
shapes each with the size \eqn{\frac{S}{N}}{S/N} so 
that \eqn{N\left(\frac{S}{N}\right)=S}{N*(S/N)=S}. In each of the three cases we used a 
different function \eqn{S}{S}. If a is a linear dimension of the shape we 
have \eqn{S(a)=a}{S(a)=a} for a line segment and \eqn{S(a)=a^2}{S(a)=a^2} and \eqn{S(a)=a^3}{S(a)=a^3} 
for the square and cube, respectively. Thus, \eqn{N\left(\frac{S}{N}\right)=S}{N*(S/N)=S} can 
be rewritten as \eqn{N\left(\frac{a}{M}\right)^D=a^D}{N*(a/M)^D=a^D} where \eqn{a}{a} 
is the "linear size" of the shape, \eqn{M}{M} is the number of linear parts, and \eqn{N}{N} is the 
total number of the resulting smaller shapes. This gives \eqn{NM^{-D}=1}{NM^(-D)=1} or 
\eqn{N=M^D}{N=M^(-D)}. In all three cases we took \eqn{M=2}{M=2} and \eqn{D}{D} 
was successively 1, 2, and 3. We see that 
\eqn{D=\frac{\log\left(N\right)}{\log\left(M\right)}}{D=log(N)/log(M)}
is what we would call the dimension in all three cases.

This quantity \eqn{D}{D} is known as the similarity dimension. It applies to shapes that are composed 
of a few copies of themselves whose "linear" size is smaller than that of the "parent" 
shape by a factor of \eqn{M}{M}. Returning to the snowflake, 
we have \eqn{N=4}{N=4} and \eqn{M=3.}{M=3} In this case 
\eqn{D=\frac{\log\left(4\right)}{\log \left(3\right)}}{D=log(4)/log(3)} is somewhere between 1 and 2. 

The Koch's snowflake has no self-intersection and is obtained from a line segment as an image 
of a continuous function.  By one of Brouwer's theorems this function preserves the 
topological dimension of the segment (which is, of course 1). 
Finally, the curve has topological dimension 1 whereas its Hausdorff-Besicovitch 
dimension is \eqn{\frac{\log\left(4\right)}{\log\left(3\right)}}{log(4)/log(3)}. 


The main funcions are \emph{fdim} and \emph{slopeopt}, the first one for 
calculating the object with pairs (size of elements, number of elements with points inside).
In order to fulfill the objective of the function it evaluate the slope of the linear
regression model by using a first estimation of confidence.
After the first stimation and in order to make afordable to check other confidence parameters
we provide the second function. By this way the user only evaluate the sequence of points
once. After that he can change the confidence parameters and calculate the slope.

The other functions are complementary, in order to provide sets of points 
(plane, line, sphere, and so on).
}

\author{
Francisco Javier Martinez de Pison. \email{francisco.martinez@dim.unirioja.es}\cr
Joaquin Ordieres Mere.              \email{joaquin.ordieres@dim.unirioja.es}\cr
Manuel Castejon Limas.              \email{manuel.castejon@dim.unirioja.es}\cr
Fco. Javier de Cos Juez.            \email{francisco-javier.de-cos@dim.unirioja.es}\cr}

\references{
Halsey C.T., Mogens H.J., Kandanoff L.P., Procaccia I., Shraiman B.I. 
\bold{"Fractal Measures and their singularities: The caracterization of strange sets"}. 
Physical Review  vol 33, nº 2. 1986\cr \cr
Roberts J.A., Cronin A. \bold{"Unbiased estimation of multi-fractal dimensions of finite data sets"} 
\url{http://www.sci.usq.edu.au/pub/MC/staff/robertsa/multif.htm} . July 1996.\cr \cr
David M. Alexander, Phil Sheridan, Paul D. Bourke, Otto Konstandatos. 
\bold{"Global and local similarity of the primary visual cortex: mechanisms of orientation preference"}. 
HELNET - International Workshop on Neural Networks, September 1997\cr \cr
Geoffrey B. West, James H. Brown, Brian J. Enquist. 
\bold{"The Fourth Dimension of Life: Fractal Geometry and Allometric Scaling of Organisms"}., 
Santa Fe Institute of Research. 1999\cr \cr
Christo Faloutsos, Volker Gaede. 
\bold{"Analisys of n-dimensional Quadtrees Using the Hausdorff Fractal Dimension"}. 
Mumbai (Bombay), Proceedings of the 22nd  VLDB Conference, India, 1996.\cr \cr
Alberto Belussi, Christo Faloutsos. 
\bold{"Estimating the Selectivity of Spatial Queries Using the 'Correlation' Fractal Dimension"}., 
Zurich, Switzerland, Proceedings of the 21st VLDB Conference, 1995.\cr \cr
Menéndez Fernández C.; Ordieres Meré J.; Ortega Fernández F. 
\bold{"Importance of information pre-processing importance in the improvement 
                                    of neural networks results."}. 
International Journal on Expert System and Neural Networks, Vol. 13, No. 2, pp. 95-103. May 1996.\cr \cr
}
\seealso{
\code{\link{fdim}}, \code{\link{slopeopt}}, \code{\link{makefract}}
}
\keyword{documentation}
