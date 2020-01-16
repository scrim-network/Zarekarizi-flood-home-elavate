##==============================================================================
##
## Script plots the costruction cost plot 
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
##      1. A pdf or jpeg file in "Figures" directory that contains the plot
##==============================================================================

# Global variables
main_path=getwd()

# Source other scripts 
# We need a function from Cost_Damage_Calculator called Rising 
source(paste(main_path,'/Source_Code/Functions/Cost_Damage_Calculator.R',sep=""))

# This plot is construction cost on the y-axis and added elevation on the x-axis 
# Since construction cost is dependent on house size, we choose three sizes (1,000 sqft, 2,000 sqft, and 3,000 sqft) as examples
heights=seq(0,14,length.out = 1000) # X-axis which is height changes from 0 to 14
costs<-matrix(NA,3,length(heights)) # Define the cost matrix for all three houses

for(i in 1:length(heights)){
costs[1,i]=Raising_Cost(heights[i],1000) 
costs[2,i]=Raising_Cost(heights[i],2000) 
costs[3,i]=Raising_Cost(heights[i],3000)
}
# Now the "cost" matrix is allocated

# Change units to 1,00 US$
costs=costs/1000

# Start the plot
pdf(paste(main_path,"/Figures/S3_Construction_Cost.pdf",sep=""),width = 3.94,height=2.43)
#jpeg(paste(main_path,"/Figures/S3_Construction_Cost.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
#png(paste(main_path,"/Figures/S3_Construction_Cost.png",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5) # Chnage the size of the plot 
plot(heights,costs[1,],type="n",col="darkgreen",lty=1,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",ylim=c(0,500))
#grid() # Add grids
#lines(c(),c())
# Add a polygon to the left of the plot to show elevations of less than 3 feet
polygon(x=c(0,3,3,0),y=c(0,0,500,500),border=NA,col="lightgray")

# Adjst the axes 
axis(1,pos=0)
axis(2,pos=0,at=seq(0,500,length.out = 5),labels =seq(0,500,length.out = 5))

lines(heights,costs[1,],col="darkgreen",lty=1)
text(11,330,'3,000 sqft house',srt=14)

lines(heights,costs[2,],col="darkgreen",lty=1)
text(11,230,'2,000 sqft house',srt=10)

lines(heights,costs[3,],col="darkgreen",lty=1)
text(11,130,'1,000 sqft house',srt=5)

lines(x=c(0,14),y=c(500,500))
lines(x=c(14,14),y=c(0,500))

mtext("Heightening [ft]",side=1,line=1.2,cex=0.5)
mtext("Construction cost [1,000 US$]",side=2,line=1.2,cex=0.5)

text(1.5,250,'It is not practical to \nraise a house by less than 3 feet',srt=90)
dev.off()

