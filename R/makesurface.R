makesurface <- function (E1, E2, NumN=1000)
{
	X <- matrix(0,NumN,3)
	for (Indice in 1:NumN)
	{
		PosX <- (runif(1)*4)-2
		PosY <- (runif(1)*4)-2
		X[Indice,1] <- PosX
		X[Indice,2] <- PosY
		X[Indice,3] <- (PosX^E1)+(PosY^E2)
	}
return(X)
}

