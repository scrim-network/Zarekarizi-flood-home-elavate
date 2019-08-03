 # site number: 01554000
Stationary_GEV <- function(){
#x=read.table(file="./data/Discharge_water_level_01554000.txt",header = F,sep="\t",skip=34,
#             colClasses=c("NULL","NULL","character","NULL","numeric","NULL","numeric","NULL"),col.names=c("","","Time","","Qcfs","","WL",""))
load("./data/time_discharge_level.RData")
USGSdata=matrix(NA,nrow=length(x[,1]),ncol=3)

# separating the year from the rest of the Time column
year=as.integer(substr(x[,'Time'],1,4))

# the base of the epoc time is "1969-12-31 19:00:00"
USGSdata[,1]=as.integer(as.POSIXct(x[,"Time"]))
USGSdata[,2]=x[,2]
USGSdata[,3]=x[,3]

# Plot an initial diagram to see the timeseries
plot(USGSdata[,1],USGSdata[,3],type="l",xlab="Epoch Time",ylab="Gage Height (ft)",main="Gage Height Timeseries")

years=2007:2019
annual_maximum_WL=rep(NA,length(years))
for (i in 1:length(years)){
inds=which(grepl(toString(years[i]),x[,1]))
annual_maximum_WL[i]=max(x[inds,3],na.rm=TRUE)
}
plot(annual_maximum_WL,type="l",xlab="Year",ylab="Maximum Water Level (ft)",main="Time Series of Annual Maximum Water Levels in Selinsgrove")
hist(annual_maximum_WL)
x_ecdf=sort(annual_maximum_WL)
y_ecdf=(1:length(x_ecdf))/(length(x_ecdf)+1)
plot(x_ecdf,y_ecdf,xlab="Water Level(ft)",ylab="Cumulative Probability",main="Empirical Cumulative Distribution Function of Selinsgrove Water Level Observations")
emperical_return_period=1/(1-y_ecdf)

plot(emperical_return_period,x_ecdf,log="x",xlab="Return Period",ylab="Water Level(ft)",main="Stationary Water Level Frequency Analysis")
grid(equilogs=FALSE)

# Fit data to GEV distribution
library(ismev)
fitting_info<-gev.fit(annual_maximum_WL)
fitted_params<-fitting_info$mle
fitted_location<-fitted_params[1]
fitted_scale<-fitted_params[2]
fitted_shape<-fitted_params[3]

library(evd)
hist(rgev(1000,loc=fitted_location,scale=fitted_scale,shape=fitted_shape)) # this is just to test

# theory GEV
gev_return_periods=seq(1.00000001,500,1)
gev_exceed_prob=1/gev_return_periods
gev_nonexceed_prob=1-gev_exceed_prob
gev_quantiles<-qgev(p=gev_nonexceed_prob,loc=fitted_location,scale=fitted_scale,shape=fitted_shape,lower.tail=TRUE)

# Return Level Plot and comparision with observations
plot(emperical_return_period,x_ecdf,log="x",xlab="Return Period",ylab="Water Level(ft)",main="Stationary Water Level Frequency Analysis",
     xlim=c(1,max(gev_return_periods)),ylim=c(min(gev_quantiles),max(gev_quantiles)))
grid(equilogs=FALSE)
lines(gev_return_periods,gev_quantiles,col="blue",lwd=2)
points(emperical_return_period,x_ecdf,pch=20,col="red")

# output for the house elevation script
Selin_Depth <- gev_quantiles
Selin_Prob <- gev_exceed_prob
output_list=list("Depth"=Selin_Depth,"Prob"=Selin_Prob)
return(output_list)
}
xx=(-10:4)+24.23
np=rep(NA,length(xx))
for(h in 1:length(xx)){
  np[h]=length((USGSdata[,3]>=xx[h]))/13
}
plot(xx,np)


