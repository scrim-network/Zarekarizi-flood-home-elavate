
# Read the file downloaded from USGS
x=read.table("~/Documents/Research/House_Elevation_Project/data/USGS_Raw_Data/USGS_Susquehannah_River_At_Lewistown.txt",header = F,sep="\t",skip=30,colClasses=c("NULL","NULL","character","integer","NULL"),col.names=c("","","Time","Qcfs",""))
USGSdata=matrix(NA,nrow=length(x[,1]),ncol=3)

# separating the year from the rest of the Time column
year=as.integer(substr(x[,'Time'],1,4))

# the base of the epoc time is "1969-12-31 19:00:00"
USGSdata[,1]=as.integer(as.POSIXct(x[,"Time"]))
USGSdata[,2]=x[,2]
USGSdata=USGSdata[year>=(min(year)+1),]
year=year[year>=min(year)+1]

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
AnnMaxWL=cbind(min(year):max(year),AMF)
save(AnnMaxWL,file="~/Documents/Research/House_Elevation_Project/data/Rdata/AnnMaxWL_Lewistown.RData")

stop('dd')
# plot the annual maximum flood series
plot(min(year):(max(year)-1),AMF[1:length(AMF)-1],type="l",xlab="Year",ylab="Maximum Flood (cfs)",main="Annual Maximum Flood for USGS 01554000 Susquehanna River at Sun")

# check the trend at this level
library("Kendall")
MannKendall(AMF[1:(length(AMF)-1)])


# find a reference threshold
referece_threshold=quantile(USGSdata[year>=min(year) & year<=(min(year)+30),2],0.99)
plot(USGSdata[,1],USGSdata[,2],type="l")
abline(h=referece_threshold)

# Number of floody days
n_floody_days=rep(NA,length(unique(year)))
for(i in 1:length(unique(year))){
  n_floody_days[i]=sum(USGSdata[year==(min(year)+i),2]>referece_threshold)
}
plot(n_floody_days,type="l",xlab="Year",ylab="Number of floody days")
MannKendall(n_floody_days)

# Average magnitude of floody days
avg_q_floody_days=rep(0,length(unique(year)))
for(i in 1:length(unique(year))){
  avg_q_floody_days[i]=mean(USGSdata[year==(min(year)+i) & USGSdata[,2]>referece_threshold,2])
}
avg_q_floody_days[n_floody_days==0]=0
plot(avg_q_floody_days,type="l",xlab="Year",ylab="Average discharge in floody days")
MannKendall(avg_q_floody_days)

# Number of flash floods
n_flash_floods<-rep(NA,length(unique(year)))
count<-rep(0,length(unique(year)))
USGSdata[,3]=0
for(i in 1:length(unique(year))){
  inds=which(USGSdata[,2]>referece_threshold & year==min(year)+i)
  if(length(inds)>=3){
  for(j in 2:(length(inds)-1)){
    if(inds[j-1]!=inds[j]-1 & inds[j+1]!=inds[j]+1){
      count[i]=count[i]+1
      USGSdata[inds,3]=1
      }    
  }
  }
}
pdf(paste("Figures/N_Flash_Floods.pdf",sep=""), width =3.94, height =2.43)
par(cex=0.5)
plot(count,type="l",xlab="year",ylab="Number of flash floods")
points(count,cex=2,col="red",pch=20)
dev.off()
MannKendall(count)

# Average magnitude of flash floods
avg_q_ff=rep(0,length(unique(year)))
for(i in 1:length(unique(year))){
  avg_q_ff[i]=mean(USGSdata[year==(min(year)+i) & USGSdata[,3]==1,2])
}
avg_q_ff[count==0]=0
plot(avg_q_ff,type="l",xlab="Year",ylab="Average discharge in flash floods")
MannKendall(avg_q_ff)

# Duration of independent floods (7 days of distance)


# Number of independent floods


# Frequency of floods


# Frequency of flash floods







