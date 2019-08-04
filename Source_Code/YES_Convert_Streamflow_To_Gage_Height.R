##==============================================================================
##
## Script for converting streamflow data in Selinsgrove to Gage Height using 
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
# Global variables
rm(list=ls())
main_path="~/Documents/Research/House_Elevation_Project/GitHub/Zarekarizi-flood-home-elavate/"

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# read the text file (rating curve data) downloaded from USGS 
rating_curve=read.table(file=paste(main_path,"Data/rating_curves_01554000.txt",sep=""),header = F,sep="\t",skip=44,
                        colClasses=c("numeric","NULL","numeric","NULL"),col.names=c("Height","","Discharge",""))

# Read streamflow
streamflow=read.table(file=paste(main_path,"Data/Selinsgrove_full_daily_streamflow_data_01554000.txt",sep=""),header = F,sep="\t",skip=34,
                      colClasses=c("NULL","NULL","character","numeric","NULL"),col.names=c("","","Time","Qcfs",""))

# Convert the streamflow data to gage height according to the rating curve
estimated_gage_height=approx(rating_curve$Discharge,rating_curve$Height,xout=streamflow$Qcfs)$y

# Save in format of .RData 
Selin_USGS_data=cbind(streamflow,estimated_gage_height)
save(Selin_USGS_data,file=paste(main_path,"Data/Selin_USGS_data.RData",sep=""))
