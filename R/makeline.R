makeline <- function(NumN=100, DimM=3, Longit=100)
{

	Vector <- runif(DimM)*Longit
	K <- runif(NumN);
	MatD <- t(Vector*t(matrix(rep(K,DimM),NumN,DimM)))
	return(MatD)
}
