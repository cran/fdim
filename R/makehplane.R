makehplane <- function(NumN=100, DimM=3, Longit=100)
{
	
	if (DimM<3)
		{
		print("Error!!, DimM has to be >= 3 ...");
		return(0);
		}


	Vector1 <- runif(DimM)*Longit
	Vector2 <- runif(DimM)*Longit
	Vector2[DimM]<- -sum(Vector1[1:DimM-1]*Vector2[1:DimM-1])+Vector1[DimM]

	ModVect <- sqrt(sum(Vector1*Vector1))
	Vector1 <- Vector1*Longit/ModVect	

	ModVect <- sqrt(sum(Vector2*Vector2))
	Vector2 <- Vector2*Longit/ModVect	

		


	K1 <- runif(NumN);
	K2 <- runif(NumN);
	MatD <- t(Vector1*t(matrix(rep(K1,DimM),NumN,DimM)))+t(Vector2*t(matrix(rep(K2,DimM),NumN,DimM)))
	return(MatD)
}
