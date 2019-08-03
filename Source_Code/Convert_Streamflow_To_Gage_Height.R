
# This script is written by Mahkameh Zarekarizi in hope of converting streamflow data in Selinsgrove to Gage Height using the USGS rating curve

# Change the directory
rm(list=ls())
setwd("~/Documents/Research/House_Elevation_Project/Source_Code")

# read the text file (rating curve data) downloaded from USGS 
rating_curve=read.table(file="~/Documents/Research/House_Elevation_Project/data/rating_curves_01554000.txt",header = F,sep="\t",skip=44,
                        colClasses=c("numeric","NULL","numeric","NULL"),col.names=c("Height","","Discharge",""))

# Read the full range of streamflow
streamflow=read.table(file="~/Documents/Research/House_Elevation_Project/data/Selinsgrove_full_daily_streamflow_data_01554000.txt",header = F,sep="\t",skip=34,
                      colClasses=c("NULL","NULL","character","numeric","NULL"),col.names=c("","","Time","Qcfs",""))

# Convert the streamflow data to gage height according to the rating curve
estimated_gage_height=approx(rating_curve$Discharge,rating_curve$Height,xout=streamflow$Qcfs)$y

Selin_USGS_data=cbind(streamflow,estimated_gage_height)
save(Selin_USGS_data,file="~/Documents/Research/House_Elevation_Project/data/Selin_USGS_data.RData")
