##==============================================================================
##
## Script plots the XLRM diagram for the home elevation problem
##
## Author: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
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
##    folder (Zarekarizi-Home-Elevation). You can do so by setwd("~/.../../../../Zarekarizi-Home-Elevation") 
## 2. To Run:
##      1. Click on Source button or, on console, type: Source("../../....R")
## 3. Outputs:
##      1. A pdf or jpeg file in "Figures" directory that contains the plot
##==============================================================================

# Global variables
main_path=getwd()
myblue <- rgb(0/255, 128/255, 1, 0.7)
myred <- "red"

# Plot XLRM 
# Dimensions are a standard golden ratio plot with 10 cm of width
# If you would like to save the plot as pdf, substitute next line with: pdf("/Figure/xlrm.pdf", width =3.94, height =2.43)
#jpeg(paste(main_path,"/Figures/S1_XLRM.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
#pdf(paste(main_path,"/Figures/S1_XLRM.pdf",sep=""),width =3.94, height =2.43)
#png(paste(main_path,"/Figures/S1_XLRM.png",sep=""),width =3.5, height =2.5,units="in",res=300)
pdf(paste(main_path,"/Figures/S1_XLRM.pdf",sep=""),width =3.5, height =2.5)

par(cex=0.35,mai=c(0,0,0,0),fig=c(0,1,0,1)) # Adjust the size
plot(NA,NA,type="n",xlim=c(0,10),ylim=c(0,11),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")

# System relationsips
polygon(x=c(4,6,6,4),y=c(2,2,7,7),border=NA,col="gray")
text(5,5,"System \nrelationships\n (R)",cex=1.75)

# Objectives 
text(8,10.75,'Objectives \n(M)',xpd=T,cex=2,col=myblue,srt=0)

polygon(x=c(6.9,9,9,6.9),y=c(8.25,8.25,9.75,9.75),border=NA,col=myblue)
polygon(x=c(6.9,9,9,6.9),y=c(6.25,6.25,7.75,7.75),border=NA,col=myblue)
polygon(x=c(6.9,9,9,6.9),y=c(4.25,4.25,5.75,5.75),border=NA,col=myblue)
polygon(x=c(6.9,9,9,6.9),y=c(2.25,2.25,3.75,3.75),border=NA,col=myblue)
polygon(x=c(6.9,9,9,6.9),y=c(0.25,0.25,1.75,1.75),border=NA,col=myblue)

arrows(x0=6,y0=3,x1=6.9,y1=1,col="black",length = 0.05)
arrows(x0=6,y0=4,x1=6.9,y1=3,col="black",length = 0.05)
arrows(x0=6,y0=5,x1=6.9,y1=5,col="black",length = 0.05)
arrows(x0=6,y0=6,x1=6.9,y1=7,col="black",length = 0.05)
arrows(x0=6,y0=7,x1=6.9,y1=9,col="black",length = 0.05)


text(8,9,"Total cost",cex=2)
text(8,7,"Benefit/cost",cex=2)
text(8,5,"Upfront cost/ \nhouse value",cex=2)
text(8,3,"Reliability",cex=2)
text(8,1,"Robustness",cex=2)

# Levers 
text(5,10.75,'Levers \n(L)',xpd=T,cex=2,col="orange")
polygon(x=c(4,6,6,4),y=c(8,8,10,10),border=NA,col="orange")
text(5,9,"How high \nto elevate?",cex=2)
arrows(x0=5,y0=8,x1=5,y1=7,col="black",length = 0.05)

# Uncertainties 
text(2,10.75,'Uncertainties \n(X)',xpd=T,cex=2,col=myred,srt=0)
polygon(x=c(1,3,3,1),y=c(8,8,10,10),border=NA,col=myred,xpd=T)
polygon(x=c(1,3,3,1),y=c(5.5,5.5,7.5,7.5),border=NA,col=myred,xpd=T)
polygon(x=c(1,3,3,1),y=c(3,3,5,5),border=NA,col=myred,xpd=T)
polygon(x=c(1,3,3,1),y=c(0.5,0.5,2.5,2.5),border=NA,col=myred,xpd=T)

text(2,6.5,"Discount\n rate",cex=2)
text(2,4,"Damage \ncurve",cex=2)
text(2,1.5,"House \nlifetime",cex=2)
text(2,9,"Flooding \nfrequency",cex=2)

arrows(x0=3,y0=9,x1=4,y1=6,col="black",length = 0.05)
arrows(x0=3,y0=6.5,x1=4,y1=5,col="black",length = 0.05)
arrows(x0=3,y0=4,x1=4,y1=4,col="black",length = 0.05)
arrows(x0=3,y0=1.5,x1=4,y1=3,col="black",length = 0.05)

# Close the plot
dev.off()

