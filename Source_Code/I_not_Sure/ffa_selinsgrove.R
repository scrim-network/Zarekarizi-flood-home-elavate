##==============================================================================
##
## Script plots the cost of elevating a house with regards to House size and the added elevation
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
##
## Last changes: August 4, 2019
##==============================================================================
## Copyright 2019 Mahkameh Zarekarizi
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, see <http://www.gnu.org/licenses/>.
##==============================================================================
# Global variables
rm(list=ls())
main_path="~/Documents/Research/House_Elevation_Project/GitHub/Zarekarizi-flood-home-elavate/"

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Add the script "Cost_Damage_Calculator.R" to the directory. The cost-damage 
# function exist in that script 
source(paste(main_path,'Source_Code/Damages/Cost_Damage_Calculator.R',sep=""))

# Add the script "Empirical_probability_calculator_JOEL.R" to the directory. 
source(paste(main_path,'Source_Code/Empirical_probability_calculator.R',sep=""))

# 1. Get the USGS data
load(paste(main_path,"Data/time_discharge_level.RData",sep=""))
USGSdata=matrix(NA,nrow=length(x[,1]),ncol=3)

## separating the year from the rest of the Time column
year=as.integer(substr(x[,'Time'],1,4))

## the base of the epoc time is "1969-12-31 19:00:00"
USGSdata[,1]=as.integer(as.POSIXct(x[,"Time"]))
USGSdata[,2]=x[,2]
USGSdata[,3]=x[,3]

## Plot an initial diagram to see the timeseries
#plot(USGSdata[,1],USGSdata[,3],type="l",xlab="Epoch Time",ylab="Gage Height (ft)",main="Gage Height Timeseries")

## Extract the annual maximum floods
years=2007:2019
annual_maximum_WL=rep(NA,length(years))
for (i in 1:length(years)){
  inds=which(grepl(toString(years[i]),x[,1]))
  annual_maximum_WL[i]=max(x[inds,3],na.rm=TRUE)
}

## Plot the AMFs
#plot(annual_maximum_WL,type="l",xlab="Year",ylab="Maximum Water Level (ft)",main="Time Series of Annual Maximum Water Levels in Selinsgrove")
#hist(annual_maximum_WL)

## Plot the empirical distribution of AMFs
#x_ecdf=sort(annual_maximum_WL)
#y_ecdf=(1:length(x_ecdf))/(length(x_ecdf)+1)
#plot(x_ecdf,y_ecdf,xlab="Water Level(ft)",ylab="Cumulative Probability",main="Empirical Cumulative Distribution Function of Selinsgrove Water Level Observations")

## Make the empirical return level plot
#emperical_return_period=1/(1-y_ecdf)
#plot(emperical_return_period,x_ecdf,log="x",xlab="Return Period",ylab="Water Level(ft)",main="Stationary Water Level Frequency Analysis")
#grid(equilogs=FALSE)

# Make the empirical retunrn level plot via JOEL's function
rp = median.rt(sort(annual_maximum_WL))
plot(rp,sort(annual_maximum_WL), pch = 19, col='black',log="x",xlab="Return Period",ylab="Water Level(ft)",xlim=c(1,500),ylim=c(0,100))
grid(equilogs=FALSE)

##########################################################
# FEMA RETURN LEVEL DATA##################################
##########################################################

FEMA_rp=c(2,5,10,25,50,100,500)
FEMA_rl=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
lines(FEMA_rp,FEMA_rl,pch=20,col="blue",type="l",lwd=2)

##########################################################
# GEV MLE RETURN LEVEL DATA ##############################
##########################################################

# Fitting data to GEV distribution using MLE 
library(ismev)
library(extRemes)
fitting_info<-fevd(annual_maximum_WL,type="GEV")
fitted_params<-fitting_info$results$par
fitted_location<-fitted_params[1]
fitted_scale<-fitted_params[2]
fitted_shape<-fitted_params[3]

# theory GEV
gev_return_periods=seq(1.00000001,500,1)
gev_exceed_prob=1/gev_return_periods
gev_nonexceed_prob=1-gev_exceed_prob
gev_quantiles<-qgev(p=gev_nonexceed_prob,mu=fitted_location,sigma=fitted_scale,xi=fitted_shape)

# Return Level Plot and comparision with observations
lines(gev_return_periods,gev_quantiles,col="orange",lwd=2)

##########################################################
# GEV MCMC RETURN LEVEL DATA #############################
##########################################################

# Load the packages 
library(evdbayes)
library(ismev)

# Estimate GEV parameters
mat <- diag(c(10000, 10000, 100))
pn <- prior.norm(mean = c(0,0,0), cov = mat)
pos<-posterior(1000000, init = c(5,1,0.1), prior = pn, lh = "gev",data = annual_maximum_WL, psd = c(.02,.1,.1))
fitted_locationMCMC=mean(pos[,1])
fitted_scaleMCMC=mean(pos[,2])
fitted_shapeMCMC=mean(pos[,3])

# Estimate theoretical GEV quantiles
library(evir)
gev_return_periods=seq(1.00000001,500,1)
gev_exceed_prob=1/gev_return_periods
gev_nonexceed_prob=1-gev_exceed_prob
gev_quantiles<-qgev(p=gev_nonexceed_prob,mu=fitted_locationMCMC,sigma=fitted_scaleMCMC,xi=fitted_shapeMCMC)

# Draw theoretical GEV line and compare with observations
lines(gev_return_periods,gev_quantiles,col="red",lwd=2)

##########################################################
# GEV MCMC NON-STATIONARY RETURN LEVEL DATA ##############
##########################################################

# # Fitting data to GEV distribution using MCMC considering non-stationarity 
# library(evdbayes)
# library(ismev)
# mat <- diag(c(10000, 10000, 100))
# pn <- prior.norm(mean = c(0,0,0), cov = mat,trendsd=0.5)
# pos<-posterior(1000000, init = c(5,1,0.1,0.1), prior = pn, lh = "gev",data = annual_maximum_WL, trend=rep(1,length(annual_maximum_WL)),psd = c(.02,.1,.1,0.1))
# 
# fitted_location=mean(pos[,1])
# fitted_scale=mean(pos[,2])
# fitted_shape=mean(pos[,3])
# 
# library(evd)
# gev_return_periods=seq(1.00000001,500,1)
# gev_exceed_prob=1/gev_return_periods
# gev_nonexceed_prob=1-gev_exceed_prob
# gev_quantiles<-qgev(p=gev_nonexceed_prob,loc=fitted_location,scale=fitted_scale,shape=fitted_shape,lower.tail=TRUE)
# 
# # Return Level Plot and comparision with observations
# lines(gev_return_periods,gev_quantiles,col="blue",lwd=2)

##########################################################
# PLOT APPEARANCE ########################################
##########################################################
legend('topleft',c("With uncertainty","Without uncertainty","FEMA","Observation"),lty=c(1,1,1,0),pch=c(-1,-1,-1,20),col=c("red","orange","blue","black"))
