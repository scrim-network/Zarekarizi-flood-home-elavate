# The goal of this script is to discover the trend in 100-yr water level, if any.

# Preprocessing
rm(list=ls())

# Load the libraries
library(evdbayes)
library(ismev)
library(evir)
# Read the maximum water level data
load("~/Documents/Research/House_Elevation_Project/data/Rdata/AnnMaxWL.RData")
#load("~/Documents/Research/House_Elevation_Project/data/Rdata/AnnMaxWL_Lewistown.RData")

WL100<-matrix(NA,10000,length(AnnMaxWL[,1])-31+1)

for(j in 1:length(WL100[1,])){
  print(j)
# Flood frequency analysis with 30 years of data
mydata=AnnMaxWL[1:(30+j),2]

# Estimate the posterior distribution of parameters
mat <- diag(c(10000, 10000, 100))
pn <- prior.norm(mean = c(0,0,0), cov = mat)
pos<-posterior(10000, init = c(5,1,0.1), prior = pn, lh = "gev",data =mydata, psd = c(.02,.1,.1))
mu_no_burn=(pos[,1])
logsigma_no_burn=(pos[,2])
xi_no_burn=(pos[,3])

burn.in<-10000
mu <-mu_no_burn[(length(xi_no_burn)-10000+1):length(mu_no_burn)]
xi <- xi_no_burn[(length(xi_no_burn)-10000+1):length(xi_no_burn)]
sigma <- logsigma_no_burn[(length(logsigma_no_burn)-10000+1):length(logsigma_no_burn)]


for(i in 1:10000){
WL100[i,j]=qgev(p=(1-0.01),xi=xi[i],mu=mu[i],sigma=sigma[i])
}
}

expected_WL100<-lb<-ub<-rep(NA,length(AnnMaxWL[,1])-31+1)
for(i in 1:length(AnnMaxWL[,1])-31+1){
  expected_WL100[i]=quantile(WL100[,i],0.5)
}
for(i in 1:length(AnnMaxWL[,1])-31+1){
lb[i]=quantile(WL100[,i],0.05)
ub[i]=quantile(WL100[,i],0.95)
}
years=(AnnMaxWL[length(AnnMaxWL[,1]),1]-(length(expected_WL100)-1)):AnnMaxWL[length(AnnMaxWL[,1]),1]

pdf(paste("Figures/Subery_100yrflood_trend.pdf",sep=""), width =3.94, height =2.43)
par(cex=0.5)
plot(years,expected_WL100,type="n",col="blue",lwd=2,xlab="Year",ylab="Base Flood Elevation (ft)",ylim=c(min(lb),max(ub)))
polygon(x=c(years,rev(years)),y=c(ub,rev(lb)),col="pink",border=NA)
lines(years,expected_WL100,col="black",lwd=2)
mtext("Changes in the Base Flood Elevation" ,line=-1,cex=0.5)
mtext("At USGS 01553500 West Branch Susquehanna River at Lewisburg, PA",line=-2,cex=0.5)
dev.off()
#WL100[WL100<quantile(WL100,0.05,na.rm = TRUE)]=NA
#WL100[WL100>quantile(WL100,0.95,na.rm = TRUE)]=NA
#par(mfrow=c(1,3))
#hist(mu)
#hist(sigma)
#hist(xi)

#hist(WL100,breaks=100)
