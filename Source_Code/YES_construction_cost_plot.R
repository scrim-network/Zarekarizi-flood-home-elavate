##==============================================================================
##
## Script plots the costruction cost 
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
rm(list=ls())
main_path="~/Documents/Research/House_Elevation_Project/GitHub/Zarekarizi-flood-home-elavate/"

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Source other scripts 
source(paste(main_path,'Source_Code/Cost_Damage_Calculator.R',sep=""))


heights=seq(0,14,length.out = 1000)
costs<-matrix(NA,3,length(heights))

for(i in 1:length(heights)){
costs[1,i]=Rising_Cost(1000,heights[i]) 
costs[2,i]=Rising_Cost(2000,heights[i]) 
costs[3,i]=Rising_Cost(3000,heights[i])
}

costs=costs/1000

pdf(paste(main_path,"Figures/construction_cost_plot.pdf",sep=""),width = 3.94,height=2.83)
par(cex=0.5)

plot(heights,costs[1,],type="n",col="darkgreen",lty=1,xaxt="n",yaxt="n",bty="n",xlab="",ylab="",ylim=c(0,500))

grid()

polygon(x=c(0,3,3,0),y=c(0,0,500,500),border=NA,col="lightgray")

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

mtext("Added Height [ft]",side=1,line=1.2,cex=0.5)
mtext("Construction cost [1,000 US$]",side=2,line=1.2,cex=0.5)

text(1.5,250,'It is not practical to \nraise a house by less than 3 feet',srt=90)
dev.off()
