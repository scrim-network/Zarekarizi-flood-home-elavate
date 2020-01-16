##==============================================================================
##
## Script analyzes the annual maximum water level
## Goals: (1) Extract annual maximum water level from daily data 
##        (2) Save annual maximum water level data
##        (3) Plot timeseries of annual maximum water level
##        (4) Plot histogram of annual maximum water level
##        (5) Plot empirical CDF of anual maximum water level 
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
##      1. output includes a data file (AnnMaxWL.RData) saved in /Raw_Data 
##      and three plots saved in Figures directory 
##==============================================================================

# Global variables
main_path=getwd()

# Load the USGS streamflow, and water level from rating curve
# This is stored in a .RData file in Raw_Data folder 
# Time_Q_WL.RData should have been created before running this code
# Time_Q_WL.RData should have been created by S6_Convert_Streamflow_To_Gage_Height.R
load(paste(main_path,"/",load_path,"/Time_Q_WL.RData",sep=""))

## Convert daily data to yearly
# separating the year from the rest of the Time column
year=as.integer(substr(Time_Q_WL[,'Time'],1,4))
years=year[1]:year[length(year)]
annual_maximum_WL=rep(NA,length(years))
for (i in 1:length(years)){
  inds=which(grepl(toString(years[i]),Time_Q_WL[,1]))
  annual_maximum_WL[i]=max(Time_Q_WL[inds,3],na.rm=TRUE)
}

# Save Annual Maximum Water level data for use later
AnnMaxWL=cbind(years,annual_maximum_WL)
save(AnnMaxWL,file=paste(main_path,"/Output_Data/AnnMaxWL.RData",sep=""))

## Make plots and save them
# Plot the timeseries
pdf(file=paste(main_path,"/Figures/S8_Annual_Maximum_Gage_Height.pdf",sep=""),width=3.94, height=2.43)
#jpeg(paste(main_path,"/Figures/S8_Annual_Maximum_Gage_Height.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5)
plot(years,annual_maximum_WL,type="l",xlab="Year",ylab="Annual Maximum Water Level (ft)",col=4,lwd=2)
lin_mod=lm(annual_maximum_WL~years)
abline(lin_mod$coefficients[1],lin_mod$coefficients[2],col=2,lty=3,lwd=3)
points(years,annual_maximum_WL,pch=20,col=4,lwd=3)
legend('topright',c('Annual maximum WL','Linear regression of annual maximum WL'),col=c(4,2),lwd=c(3,3),lty=c(1,3),bty="n")
dev.off()

