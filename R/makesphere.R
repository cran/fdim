makesphere <- function(NumN=100, L=100)
{

	MatD <- matrix(0,NumN,3)

	for (h in 1:NumN)
	{
		LF <- runif(1)*L
		Ang1 <- runif(1)*2*pi
		Ang2 <- runif(1)*2*pi
		LSom <- LF*cos(Ang2)
		MatD[h,1] <- LSom*cos(Ang1)
		MatD[h,2] <- LSom*sin(Ang1)
		MatD[h,3] <- LF*sin(Ang2)
	}
	return(MatD)
}
