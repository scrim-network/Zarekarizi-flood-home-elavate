
## Preprocessing 
# Change the directory
rm(list=ls())
setwd("~/Documents/Research/House_Elevation_Project/Source_Code")

# Load the USGS streamflow, and water level from rating curve 
load("~/Documents/Research/House_Elevation_Project/data/Selin_USGS_data.RData")

## Convert daily data to yearly
# separating the year from the rest of the Time column
year=as.integer(substr(Selin_USGS_data[,'Time'],1,4))

years=year[1]:year[length(year)]
annual_maximum_WL=rep(NA,length(years))
for (i in 1:length(years)){
  inds=which(grepl(toString(years[i]),Selin_USGS_data[,1]))
  annual_maximum_WL[i]=max(Selin_USGS_data[inds,3],na.rm=TRUE)
}

# Save Annual Maximum Water level data for use later
AnnMaxWL=cbind(years,annual_maximum_WL)
save(AnnMaxWL,file="~/Documents/Research/House_Elevation_Project/data/AnnMaxWL.RData")

## Make plots and save them
# Plot the timeseries
pdf(file="~/Documents/Research/House_Elevation_Project/Results/Gage_Height_AnnMaxRecoWL_TimSer.pdf",width=3.93, height=2.43)
par(cex=0.5)
plot(years,annual_maximum_WL,type="l",xlab="Year",ylab="Annual Maximum Water Level (ft)",col=4,lwd=2)
lin_mod=lm(annual_maximum_WL~years)
abline(lin_mod$coefficients[1],lin_mod$coefficients[2],col=2,lty=3,lwd=3)
points(years,annual_maximum_WL,pch=20,col=4,lwd=3)
legend('topright',c('Annual maximum WL','Linear regression of annual maximum WL'),col=c(4,2),lwd=c(3,3),lty=c(1,3),bty="n")
dev.off()

# Plot the histogram
pdf(file="~/Documents/Research/House_Elevation_Project/Results/Gage_Height_AnnMaxRecoWL_Hist.pdf",width=3.93, height=2.43)
par(cex=0.5)
hist(annual_maximum_WL,xlab="Annual maximum WL (ft)",col=3,main="")
dev.off()

# Plot the empirical CDF
pdf(file="~/Documents/Research/House_Elevation_Project/Results/Gage_Height_AnnMaxRecoWL_ECDF.pdf",width=3.93, height=2.43)
par(cex=0.5)
x_ecdf=sort(annual_maximum_WL)
y_ecdf=(1:length(x_ecdf))/(length(x_ecdf)+1)
plot(x_ecdf,y_ecdf,xlab="Water Level (ft)",ylab="Non-exceedance probability",pch=20,col="red")
dev.off()

