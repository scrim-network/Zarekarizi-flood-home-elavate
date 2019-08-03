# site number: 01554000
NStationary_GEV_MCMC <- function(){
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
  library(evdbayes)
  library(ismev)
  mat <- diag(c(10000, 10000, 100))
  pn <- prior.norm(mean = c(0,0,0), cov = mat,trendsd=0.5)
  pos<-posterior(1000000, init = c(5,1,0.1,0.1), prior = pn, lh = "gev",data = annual_maximum_WL, trend=rep(1,length(annual_maximum_WL)),psd = c(.02,.1,.1,0.1))
  
  # pdf('./figure/parameter_evolution_GEV_MCMC_Stationary.pdf', width = 8.5, height = 11)
  # par(mfrow=c(3,1))
  # plot(pos[,1],type="l",main="Location Parameter",ylab="Mu")
  # plot(pos[,2],type="l",main="Scale Parameter",ylab="Sigma")
  # plot(pos[,3],type="l",main="Shape Parameter",ylab="Xi")
  # dev.off()
  # 
  fitted_location=mean(pos[,1])
  fitted_scale=mean(pos[,2])
  fitted_shape=mean(pos[,3])
  
  # pdf('./figure/parameter_hists_comparison_w_MLE.pdf', width = 8.5, height = 11)
  # par(mfrow=c(3,1))
  # hist(pos[,1],type="l",main="Location Parameter",xlab="")
  # abline(v=18.212,col="red")
  # legend('topleft', c('MLE'), 
  #        col = c('red'), lwd = 1, bty = 'n')
  # hist(pos[,2],type="l",main="Scale Parameter",xlab="")
  # abline(v=1.7786,col="red")
  # hist(pos[,3],type="l",main="Shape Parameter",xlab="")
  # abline(v=0.4212,col="red")
  # dev.off()
  # 
  #rl.pred(pos, qlim=c(30,100),1, lh = c("gev"), period = 1, lty = 1, col = 1,
  #        xlab = "return period", ylab = "return level")
  
  # theory GEV
  library(evd)
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





