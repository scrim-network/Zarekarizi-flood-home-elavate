##==============================================================================
##
## Script for converting streamflow data to Gage Height using 
## the USGS rating curve
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
##
## Last changes: August 3, 2019
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
##      1. Gage Height data will be saved in the Raw_Data folder as Time_Q_WL.RData
##         This file contains a dataframe with these columns: (1)Time (2)Qcfs 
##         and (3)estimated_gage_height 
##==============================================================================

# Global variables
main_path=getwd()

# read the text file (rating curve data) downloaded from USGS 
rating_curve=read.table(file=paste(main_path,"/Input_Data/Rating_Curve_USGS01554000.txt",sep=""),header = F,sep="\t",skip=44,
                        colClasses=c("numeric","NULL","numeric","NULL"),col.names=c("Height","","Discharge",""))

# Read streamflow
streamflow=read.table(file=paste(main_path,"/Input_Data/Daily_Streamflow_USGS01554000.txt",sep=""),header = F,sep="\t",skip=34,
                      colClasses=c("NULL","NULL","character","numeric","NULL"),col.names=c("","","Time","Qcfs",""))

# Convert the streamflow data to gage height according to the rating curve
estimated_gage_height=approx(rating_curve$Discharge,rating_curve$Height,xout=streamflow$Qcfs)$y

# Save in format of .RData 
Time_Q_WL=cbind(streamflow,estimated_gage_height)
save(Time_Q_WL,file=paste(main_path,"/Output_Data/Time_Q_WL.RData",sep=""))
