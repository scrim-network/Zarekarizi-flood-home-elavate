##==============================================================================
##
## Script for drawing the rating curve downloaded from USGS  
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
##      1. A pdf or jpeg file in "Figures" directory that contains a plot
##==============================================================================

# Global variables
main_path=getwd()

# read the text file (rating curve data) downloaded from USGS 
rating_curve=read.table(file=paste(main_path,"/Input_Data/Rating_Curve_USGS01554000.txt",sep=""),header = F,sep="\t",skip=44,
             colClasses=c("numeric","NULL","numeric","NULL"),col.names=c("Height","","Discharge",""))

# Pot the gage and streamflow 
pdf(file=paste(main_path,"/Figures/S5_Rating_Curve.pdf",sep=""),width=3.94, height=2.43)
#jpeg(paste(main_path,"/Figures/S5_Rating_Curve.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5)
plot(rating_curve$Discharge/1000,rating_curve$Height,log="x",type="l",
     lwd=2,col=4,xlab='Discharge (1,000 cfs)',ylab='Gage Height (ft)')
dev.off()

