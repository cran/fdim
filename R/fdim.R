	#########################################
	# fdim.R (v1.1) 12-12-2000  	 	#
	# University of La Rioja (SPAIN) 	#
	# Authors: 			 	#
	# Joaquin Ordieres Mere		 	#
	# mail:joaquin.ordieres@dim.unirioja.es	#
	# Fco. Javier Mtnez de Pisón	 	#
	# mail: fjmartin@die.unirioja.es	#
	# Manuel Castejon Limas			#
	# mail: manuel.castejon@dim.unirioja.es	#
	# Fco Javier de Cos Juez		#
	# mail: frde-cos@dim.unirioja.es	# 
	#########################################
#
# This function provides informations about the 
# Dq=(1/(q-1))*log2(sum(Pj^q))/log2(r) when (r->0)
# dimension of a matrix of points

# where:
#		D0=Fractal Dimension		(q=0)
#		D1=Information Dimension	(q->1)
#		D2=Correlation Dimension	(q=2)

fdim <- function (X, BaseR = 2, Mnmax = TRUE, nMax = 9, NumMinP = 1,
	q = 0, Alpha=0.2,PlotF = FALSE) 
{

    # if q=1 exit

    if (q==1)
	{
	print("Error q==1 is a singular point")
	return(0)
	}

    # Normalise the cloud of points in the range [0-1.000.000.000]
    NumCol <- ncol(X)
    NumRow <- nrow(X)
    VectMin <- rep(0, NumCol)
    for (i in 1:NumCol) {
        VectMin[i] <- min(X[, i])
    }
    for (i in 1:NumRow) {
        X[i, ] <- X[i, ] - VectMin
    }
    MaxX <- max(X)
    MulTF <- (999999999/MaxX)
    X <- t(X*MulTF)

    # Initialisate variables
    N <- 0
    r0 <- 1
    Indice <- 1
    n <- 1
    r <- 1
    M <- NumRow

    # NumpDif=Number of different points	
    NumpDif <- 0	
    DK <- .C("pointdif", as.integer(NumRow), as.integer(NumCol), 
                as.integer(X), NumpDif = as.integer(NumpDif), 
                PACKAGE = "fdim" )

    RadioN <- matrix(0, 2000, 2)
    SumFreq <- matrix(0, 2000, 2)
    SumInfo <- matrix(0, 2000, 2)	

    # The number of boxes with length side=r, which have points inside are computed	
    while ((N < (M/2)) && n < 2000 && (n<=nMax || Mnmax==FALSE))
    {
	   r <- 1/(BaseR^n)
	   r0 <- 1/r
	   NBox <- 0
	   SumSQRFreq <- 0
	   Informations <- 0
	   Dq <- 0
           DF <- .C("cboxn", as.integer(NumRow), as.integer(NumCol), 
                as.integer(NumMinP), as.integer(X), as.double(r),
		SumSQRFreq=as.double(SumSQRFreq),Informations=as.double(Informations),
                NBox = as.integer(NBox), q = as.double(q), Dq = as.double(Dq),
                PACKAGE= "fdim" )
	        
	   DF$Dq <- log2(DF$Dq)/(q-1)    
           RadioN[Indice, 1:2] <- c(log2(r), DF$Dq)
  	  
	   DF$SumSQRFreq < (DF$SumSQRFreq*(NumRow*NumRow)/(DK$NumpDif*DK$NumpDif))
           SumFreq[Indice, 1:2] <- c(r0, DF$SumSQRFreq)
            
   	   DF$Informations <- ((DF$Informations*(NumRow/DK$NumpDif))+log2(NumRow/DK$NumpDif))
           SumInfo[Indice, 1:2] <- c(log2(r0), DF$Informations)
           Indice <- Indice + 1
           n <- n + 1
           N <- DF$NBox
    }
    Indice <- Indice - 1
    
    AllPoints <- as.matrix(RadioN[1:Indice,])
    # Find optimal slope
    LineP <- slopeopt(AllPoints,Alpha)
    PuntosFreq <- log2(SumFreq[1:Indice, ])
    PuntosInfo <- SumInfo[1:Indice,]	

    FractalDim <- as.list(0)
    
    if (Indice >= 2) {
	# Dq
	FractalDim$fdim <- LineP$coefficients[2]
	# points used to calculate the slope
	FractalDim$points <- LineP$points

	# Slope is OK?
	FractalDim$slopeisOK <- LineP$IsOK
	# Coefficients of the line
	FractalDim$coefficients <- LineP$coefficients

	# Number of matrix's different points
	FractalDim$pointsdif <- DK$NumpDif

	# Residuals
	FractalDim$residual <- LineP$residual
	# Sum squared of residuals
	SumSQRResidual <- sqrt(sum(FractalDim$residual*FractalDim$residual))
	FractalDim$sumSQRresidual <- (SumSQRResidual/length(FractalDim$residual))

	# all points calculated
	FractalDim$allpoints <- data.frame(AllPoints)
	# Range of points
	FractalDim$range <- c(min(AllPoints[,1]),max(AllPoints[,1]))


	# Sum Sqr freq
	FractalDim$correlationdim <- PuntosFreq
	FractalDim$informationdim <- PuntosInfo
    	}
   
 #   if (PlotF) 
 #	{ 
 #       plot(AllPoints, type = "b", xlab = "log2(1/(BaseR^n))",
 #		ylab = "log2(sum(Pj^q)/(q-1))",
 #           	main = "Dq Dimension",
 #		col = "blue")
 #	if (FractalDim$slopeisOK)
 #		{
 #		curve(FractalDim$coefficients[1]+FractalDim$coefficients[2]*X,   # changed from x to X
 #			FractalDim$range[1],FractalDim$range[2],
 #			add=TRUE,col="red")
 #		}
 #	}

return(FractalDim)
}
