# slopeopt <- find optimal slope when (Ej-EM)*(1/ESS)*(Ej-EM) <= Chi2

slopeopt <- function (X,Alpha=0.5,Debug=FALSE)
{
	IsOK <- FALSE
	Chi2 <- qchisq(1-Alpha,1)
	while (!IsOK)
	{
		
		# NumP=Number of Points
		NumP <- nrow(X)
		if (NumP<3) break
		# LineP=Line
		LX <- data.frame(X)
		LineP <- lm(X2 ~ X1,data=LX)
		
		ResidualL <- abs(as.double(LineP$residual))
	 		
		EM <- median(ResidualL)
		ESS <- var(ResidualL)
		
		
		Comp <- (ResidualL-EM)*(1/ESS)*(ResidualL-EM)
		
		MaxDiff <- max(Comp)
		
		if (Debug)
		{
			print("###############################")	
			print("X=")
			print(X)
			print("Chi2=")
			print(Chi2)
			print("Residual (Ej)=")
			print(ResidualL)
			print("(Ej-EM)*(1/ESS)*(Ej-EM)=")
			print(Comp)
			print("MaxDiff=")
			print (MaxDiff)
		}
		# if MaxDiff > Chi2 then remove this point else 'IsOK=TRUE' (END)

		if (MaxDiff>Chi2)
			{
			NewX <- matrix(rep(0,(NumP-1)*2),NumP-1,2)
			NewPoint <- 1
			FindP <- FALSE
			for (h in 1:NumP)
				{
				if (Comp[h]!=MaxDiff || FindP)
					{
					NewX[NewPoint,] <- X[h,]
					NewPoint <- NewPoint+1
					}
				else
					{
					FindP <- TRUE
					}
				}
			X <- NewX
			}
		else
			{
			IsOK <- TRUE
			}
	}
LineP$IsOK <- IsOK
LineP$points <- data.frame(X)

return(LineP)
}
	
