##==============================================================================
##
## Script for drawing the rating curve downloaded from USGS and compare it with 
## observed gage height and streamflow data available. 
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
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
main_path="~/Documents/Research/House_Elevation_Project/GitHub/Zarekarizi-flood-home-elavate/"

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# read the text file (rating curve data) downloaded from USGS 
rating_curve=read.table(file=paste(main_path,"data/rating_curves_01554000.txt",sep=""),header = F,sep="\t",skip=44,
             colClasses=c("numeric","NULL","numeric","NULL"),col.names=c("Height","","Discharge",""))

# Pot the gage and streamflow 
pdf(file=paste(main_path,"Figures/USGS_Rating_CUrve_Plot.pdf",sep=""),width=3.93, height=2.43)
par(cex=0.5)
plot(rating_curve$Discharge/1000,rating_curve$Height,log="x",type="l",lwd=2,col=4,xlab='Discharge (1,000 cfs)',ylab='Gage Height (ft)')

# Load the gage height and streamflow data for the period that Gage height is available
load(paste(main_path,"Data/time_discharge_level.RData",sep=""))

# Add the observations to the plot as points (This line is commented because there are too many points)
points(x$Qcfs/1000,x$WL,pch=20,col= rgb(red = 0, green = 1, blue = 0, alpha = 0.2))

# Add legend
legend('topleft',c('USGS Rating Curve'),lwd=c(2),col=c(4),lty=c(1),pch=c(-1),bty="n")
dev.off()
