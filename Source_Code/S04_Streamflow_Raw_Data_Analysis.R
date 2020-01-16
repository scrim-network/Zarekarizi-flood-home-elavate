##==============================================================================
##
## Script prepares a platform for the user to analyze the streamflow data 
## Goals: (1) Analyze trend in daily streamflow and annaul maximum flood
##        (2) Plot annual maximum flood timeseries
##        (3) Plot daily streamflow data
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##
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
## Instructions to run:
## 1. If you have not already done so, change the working directory to the main 
##    folder (Zarekarizi-Home-Elevation)
##    To do so:
##      1. If on RStudio, open the README.md file. Then on the menu bar, go to 
##         Session-->Set Working Directory-->To Source File Location
##      2. If on RStudio, on the lower right box, open "Zarekarizi-Home-Elevation"
##         Then, click on More --> Set as Working Directory
##      3. On console, type the following command: 
##         setwd("~/.../../../../Zarekarizi-Home-Elevation") 
## 2. To Run:
##      1. Click on Source button or, on console, type: Source("../../.. .R")
## 3. Outputs:
##      1. A pdf or jpeg file in "Figures" directory that contains two plots
##==============================================================================

# Global variables
main_path=getwd()

# Load the libraries needed in this script 
library("Kendall") # For trend analysis 

# Read the file downloaded from USGS
# Youc an download this file from USGS website or you can use our downloaded dile 
# stored in ./Input_Data
# The following line reads the data from .txt file 
x=read.table(paste(main_path,"/Input_Data/Daily_Streamflow_USGS01554000.txt",sep=""),
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
pdf(paste(main_path,"/Figures/S4_Streamflow_Timeseries.pdf",sep=""),width = 3.94,height=2.43)
#jpeg(paste(main_path,"/Figures/S4_Streamflow_Timeseries.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5) # Chnage the size of the plot 
plot(USGSdata[,1],USGSdata[,2],type="l",xlab='Epoch Time relative to 1969-12-31',ylab="Streamflow (cfs)")
dev.off()

# check the trend
# Results will be printed on the Console
MannKendall(USGSdata[,2])

# find the maximum of each year
# Extract the Annual Maximum Timeseries
AMF=rep(0,length(unique(year)))
for(y in min(year):max(year)){
  AMF[y-min(year)+1]=max(USGSdata[which(year==y),2])
}

# plot the annual maximum flood series
pdf(paste(main_path,"/Figures/S4_Streamflow_Annual_Maximum_Flood.pdf",sep=""),width = 3.94,height=2.43)
#jpeg(paste(main_path,"/Figures/S4_Streamflow_Annual_Maximum_Flood.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5) # Chnage the size of the plot 
plot(min(year):(max(year)-1),AMF[1:length(AMF)-1],
     type="l",xlab="Year",ylab="Maximum Flood (cfs)",
     main="Annual Maximum Flood for USGS 01554000 Susquehanna River at Sunbery")
dev.off()

# check the trend in annual maximum floods
# Trend results will be printed on screen 
MannKendall(AMF[1:(length(AMF)-1)])
