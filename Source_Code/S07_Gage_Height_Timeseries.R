##==============================================================================
##
## Script plots the timseries of water levels. 
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
##      1. Click on Source button or, on console, type: Source("../../....R")
## 3. Outputs:
##      1. Output is a timeseries plot with added FEMA return level information
##         The figure is saved in Figures folder.
##==============================================================================

# Global variables
main_path=getwd()

# Load the USGS streamflow, and water level from rating curve
# This is stored in a .RData file in Raw_Data folder 
# Time_Q_WL.RData should have been created before running this code
# Time_Q_WL.RData should have been created by S6_Convert_Streamflow_To_Gage_Height.R
load(paste(main_path,"/",load_path,"/Time_Q_WL.RData",sep=""))

# Start plotting
pdf(file=paste(main_path,"/Figures/S7_Gage_Height_Timeseries.pdf",sep=""),width=3.94, height=2.43)
#jpeg(paste(main_path,"/Figures/S7_Gage_Height_Timeseries.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5) #Adjust the size

# Convert the format of time in Time_Q_WL.RData  
timestamp = as.Date(strptime(Time_Q_WL$Time,"%Y-%m-%d"))

# Plot the timeseries
plot(timestamp,Time_Q_WL$estimated_gage_height,xaxt="n",type="l",col=4,xlab='',ylab="Gage Height (ft)",ylim=c(5,50))
title(xlab="Date", line=4)

att=timestamp[seq(1,length(timestamp),length.out=10)]
axis(1, at=att, labels = FALSE)
labels=format(att,"%b %d,%Y")
#axis.Date(1,at=att,labels=format(att,"%b %d,%Y"),las=2)
text(att-1000, par("usr")[3] -3, labels = labels, srt = 20, pos = 1, xpd = TRUE)

dev.off()
