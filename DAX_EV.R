#Load packages
library(extRemes)

#Import DAX Data
DAX<-read.csv("C:/Users/FED/Desktop/DAX_data3.csv",stringsAsFactors = FALSE)

#Plot Data
plot(decluster(DAX$LogReturn, threshold=3.0,r = 15, clusterfun = "max"),xlab = "Day",
     ylab = "Daily (Log) Loss on DAX", main = "Chart Picture",cex.lab = 1.5,cex.axis=1.5)

#Model Fitting
D<-data.frame(L=c(decluster(DAX$LogReturn, threshold=3.0,r = 15, clusterfun = "max")),time=1:7077)

fitDC <- fevd(Data$L, data =D, 
              threshold = 3.0, type = "GP", 
              time.units = "5.19/year"
) 

plot(fitDC,cex.lab=1.5,cex.axis=1.5)


#Useful Functions
#Extremal Depenence and Large Loss Threshold
atdf(DAX$LogReturn, 0.99) 
threshrange.plot(DAX$LogReturn, r = c(1, 10), nint = 20)
mrlplot(DAX$LogReturn, xlim = c(0, 2.2))
extremalindex(DAX$LogReturn,method="runs",threshold=3.0,run.length=15)


#check on residual extremal autocorrelation
atdf(decluster(DAX$LogReturn, threshold=3.0,r = 15, clusterfun = "max"),0.99)

#Result Summary
pextRemes(fitDC, c(20, 40, 60, 100), lower.tail = FALSE)
ci(fitDC, return.period = c(2, 20, 100, 200))
ci(fitDC, type = "parameter")

#Checking
#Profile Likelihood COnfidence Interval
ci(fitDC, method = "proflik", xrange = c(4.5, 10), verbose = TRUE,alpha=0.05)

