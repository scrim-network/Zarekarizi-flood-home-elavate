##==============================================================================
##
## Script extracts, saves, and plots annual maximum water level. Water levels 
## are estimated from the rating curve
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
##
## Last changes: August 4, 2019
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

# Load the USGS streamflow, and water level from rating curve 
load(paste(main_path,"Data/Selin_USGS_data.RData",sep=""))

FEMA_return_period=c(2,5,10,25,50,100,500)
FEMA_stage=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)

timestamp = as.Date(strptime(Selin_USGS_data$Time,"%Y-%m-%d"))
pdf(file=paste(main_path,"Figures/Gage_Height_Time_Series_With_FEMA_RLs.pdf",sep=""),width=3.93, height=2.43)
par(cex=0.5)


plot(timestamp,Selin_USGS_data$estimated_gage_height,xaxt="n",type="l",col=4,xlab='',ylab="Gage Height (ft)",ylim=c(5,50))
title(xlab="Date", line=4)

att=timestamp[seq(1,length(timestamp),length.out=10)]
axis(1, at=att, labels = FALSE)
labels=format(att,"%b %d,%Y")
#axis.Date(1,at=att,labels=format(att,"%b %d,%Y"),las=2)
text(att-1000, par("usr")[3] -3, labels = labels, srt = 20, pos = 1, xpd = TRUE)

abline(h=c(FEMA_stage),col="springgreen4",lwd=2)

adj_y=1
adj_x=0
cx=0.6
text(timestamp[1]+adj_x,FEMA_stage[1]+adj_y,"2-yr",col="springgreen4",font=2,cex=cx)
text(timestamp[1]+adj_x,FEMA_stage[2]+adj_y,"5-yr",col="springgreen4",font=2,cex=cx)
text(timestamp[1]+adj_x,FEMA_stage[3]+adj_y,"10-yr",col="springgreen4",font=2,cex=cx)
text(timestamp[1]+adj_x,FEMA_stage[4]+adj_y,"25-yr",col="springgreen4",font=2,cex=cx)
text(timestamp[1]+adj_x,FEMA_stage[5]+adj_y,"50-yr",col="springgreen4",font=2,cex=cx)
text(timestamp[1]+adj_x,FEMA_stage[6]+adj_y,"100-yr",col="springgreen4",font=2,cex=cx)
text(timestamp[1]+adj_x,FEMA_stage[7]+adj_y,"500-yr",col="springgreen4",font=2,cex=cx)

legend("topright",c("Gage Height (estimated from USGS rating curve)","FEMA return levels"),col=c("blue","springgreen4"),bty="n",lty=c(1,1))
dev.off()

