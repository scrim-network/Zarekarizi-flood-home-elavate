##==============================================================================
##
## Script plots streamflow timeseries directly from USGS file 
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

# Read the file downloaded from USGS
x=read.table(paste(main_path,"Data/Selinsgrove_full_daily_streamflow_data_01554000.txt",sep=""),
             header = F,sep="\t",skip=34,
             colClasses=c("NULL","NULL","character","numeric","NULL"),
             col.names=c("","","Time","Qcfs",""))
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
plot(min(year):(max(year)-1),AMF[1:length(AMF)-1],
     type="l",xlab="Year",ylab="Maximum Flood (cfs)",
     main="Annual Maximum Flood for USGS 01554000 Susquehanna River at Sun")

# check the trend at this level
library("Kendall")
MannKendall(AMF[1:(length(AMF)-1)])
