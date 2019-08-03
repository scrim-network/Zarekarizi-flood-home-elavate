rm(list=ls())
gages=c("01553500","01540500","01554000")
pdf(paste("Figures/Annual_Maximum_Floods.pdf",sep=""), width =3.94*(floor(length(gages)/2)+1), height =2.43*(floor(length(gages)/2)+1))
par(mfrow=c(floor(length(gages)/2)+1,floor(length(gages)/2)+1))
par(cex=0.5)

for(gage in 1:length(gages)){
print(gages[gage])

# find the file 
filename=list.files(path="~/Documents/Research/House_Elevation_Project/data/USGS_Raw_Data/",pattern=paste("USGS",gages[gage]))

# Test of data includes only streamflow or only water height
temp=read.table(paste("~/Documents/Research/House_Elevation_Project/data/USGS_Raw_Data/",filename,sep=""),header = F,skip=22,nrows=1,comment.char="")

################################################  
################################################  
################################################  
if(length(temp)>1){
  # Read the file downloaded from USGS
  x=read.table(paste("~/Documents/Research/House_Elevation_Project/data/USGS_Raw_Data/",filename,sep=""),header = F,sep="\t",skip=34,
               colClasses=c("NULL","NULL","character","integer","NULL","numeric","NULL","numeric","NULL","numeric","NULL"),col.names=c("","","Time","Qcfs","","WLmax","","WLmin","","WLmean",""))
  x=x[,1:2]
  
}else{

# Read the file downloaded from USGS
x=read.table(paste("~/Documents/Research/House_Elevation_Project/data/USGS_Raw_Data/",filename,sep=""),header = F,sep="\t",skip=30,colClasses=c("NULL","NULL","character","integer","NULL"),col.names=c("","","Time","Qcfs",""))
}
# Allocate the matrix
USGSdata=matrix(NA,nrow=length(x[,1]),ncol=5)

# separating the year from the rest of the Time column
years=as.integer(substr(x[,'Time'],1,4))
months=as.integer(substr(x[,'Time'],6,7))
days=as.integer(substr(x[,'Time'],9,10))

# the base of the epoc time is "1969-12-31 19:00:00"
USGSdata[,1]=as.integer(as.POSIXct(x[,"Time"]))
USGSdata[,2]=years
USGSdata[,3]=months
USGSdata[,4]=days
USGSdata[,5]=x[,2]

# Plot an initial diagram to see the timeseries
##plot(USGSdata[,1],USGSdata[,5],type="l")

# find the maximum of each year
annMax=rep(0,length(unique(years)))
for(y in min(years):max(years)){
  annMax[y-min(years)+1]=max(USGSdata[which(years==y),5])
}

# Save annual maximum flood series
AMF=cbind(min(years):max(years),annMax)
save(AMF,file=paste("~/Documents/Research/House_Elevation_Project/data/Rdata/AMF_",gages[gage],".RData",sep=""))

# plot the annual maximum flood series
plot(AMF[,1],AMF[,2],type="l",xlab="Year",ylab="Maximum Flood (cfs)",main=paste("Annual Maximum Flood for USGS ",gages[gage],sep=""))
mtext(filename,line=-1,cex=0.5)

linM=lm(AMF[,2]~AMF[,1])
abline(linM$coefficients[1],linM$coefficients[2],col="red")

# check the trend at this level
library("Kendall")
MannKendall(AMF[,2])
}
dev.off()

