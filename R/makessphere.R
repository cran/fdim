makessphere <- function(NumN=100, L=100)
{

	MatD <- matrix(0,NumN,3)

	for (h in 1:NumN)
	{
		Ang1 <- runif(1)*2*pi
		Ang2 <- runif(1)*2*pi
		LSom <- L*cos(Ang2)
		MatD[h,1] <- LSom*cos(Ang1)
		MatD[h,2] <- LSom*sin(Ang1)
		MatD[h,3] <- L*sin(Ang2)
	}
	return(MatD)
}
