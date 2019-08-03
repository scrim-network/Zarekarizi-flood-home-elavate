
# Read the file downloaded from USGS
x=read.table("~/Documents/Research/House_Elevation_Project/data/USGS_Susquehannah_River_At_Lewistown.txt",header = F,sep="\t",skip=30,colClasses=c("NULL","NULL","character","integer","NULL"),col.names=c("","","Time","Qcfs",""))
USGSdata=matrix(NA,nrow=length(x[,1]),ncol=2)

# separating the year from the rest of the Time column
year=as.integer(substr(x[,'Time'],1,4))

# the base of the epoc time is "1969-12-31 19:00:00"
USGSdata[,1]=as.integer(as.POSIXct(x[,"Time"]))
USGSdata[,2]=x[,2]

# Plot an initial diagram to see the timeseries
plot(USGSdata[,1],USGSdata[,2],type="l")

# check the trend at this level
library("Kendall")
MannKendall(USGSdata[,2])

# find the maximum of each year
AMF=rep(0,length(unique(year)))
for(y in min(year):max(year)){
  print(y)
  AMF[y-min(year)+1]=max(USGSdata[which(year==y),2])
}

# plot the annual maximum flood series
plot(min(year):(max(year)-1),AMF[1:length(AMF)-1],type="l",xlab="Year",ylab="Maximum Flood (cfs)",main="Annual Maximum Flood for USGS 01554000 Susquehanna River at Sun")

# check the trend at this level
library("Kendall")
MannKendall(AMF[1:(length(AMF)-1)])

