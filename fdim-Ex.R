pkgname <- "fdim"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('fdim')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("fdim")
### * fdim

flush(stderr()); flush(stdout())

### Name: fdim
### Title: Calculation of generalizated fractal dimension of Data-Sets.
### Aliases: fdim
### Keywords: robust

### ** Examples

#Example 1
library(fdim)
mydata <- makeline(10000,3,100)   	# Build a data-set if the user doesn't has it.
df <- fdim(mydata,q=0,Alpha=0.3)  	# Calculation of fractal dimension
print (df$fdim)				# Show us the q-dimension value.
print (df)				# Show us relevant information about the estimated
					# fractal dimension.

plot(df$allpoints)			# make a plot in order to see the ipoints individually
abline(coef(df),col=2)			# draw a line in order to see the correlation
					# If we think the confidence criteria must be changed ...
df2 <- slopeopt(as.matrix(df$allpoints),Alpha=0.5)
summary(df2)				# Show us the quality of the approach.

#Example 2
mydata <- makessphere(1000)		#Build a data-set with points making a Sphere Surface
if (require(xgobi)) xgobi(mydata)		#Show mydata	
df <- fdim(mydata,q=0,Alpha=0.2,PlotF=TRUE)  	# Calculation of fractal dimension
print(df$fdim)				#Fractal Dimension



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
