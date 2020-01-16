##==============================================================================
##
## Script plots depth-damage function
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
##      1. Click on Source button or, on console, type: Source("../../...R")
## 3. Outputs:
##      1. A pdf or jpeg file in "Figures" directory that contains the plot
##==============================================================================

# Global variables
main_path=getwd()
myred <- rgb(1, 102/255, 102/255, 0.5)
mygreenFade <- rgb(44/255, 185/255, 95/255, 0.5) 
mygreen <- rgb(44/255, 185/255, 95/255, 1) 
myblue <- rgb(0/255, 128/255, 1, 0.5)

# Depth and damage information 
## The following depth-damage function is from the European Commission’s science and knowledge service. Please find more information in the paper 
## Link:https://ec.europa.eu/jrc/en/publication/global-flood-depth-damage-functions-methodology-and-database-guidelines
Depth_1 <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
Damage_Factors_1 <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)*100
Damage_Factors_ub_1<-Damage_Factors_1+Damage_Factors_1*0.3
Damage_Factors_lb_1<-Damage_Factors_1-Damage_Factors_1*0.3
Damage_Factors_ub_1[Damage_Factors_ub_1>100]=100

# Another source for Depth-Damage function is HAZUS:
Depth<-c(-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
Damage_Factors<-c(0,0,4,8,12,15,20,23,28,33,37,43,48,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81)
Damage_Factors_ub<-Damage_Factors+Damage_Factors*0.3
Damage_Factors_lb<-Damage_Factors-Damage_Factors*0.3
Damage_Factors_ub[Damage_Factors_ub>100]=100

Xlim=c(min(min(Depth),min(Depth_1)),max(max(Depth),max(Depth_1)))
Ylim=c(0,max(max(Damage_Factors_ub_1),max(Damage_Factors_ub)))

# Plot the depth-damage function
pdf(paste(main_path,"/Figures/S2_Depth_Damage.pdf",sep=""),width = 3.94,height=2.43)
#jpeg(paste(main_path,"/Figures/S2_Depth_Damage_certainty_HAZUS.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5)
plot(Depth,Damage_Factors,type="l",lwd=2,col="darkgreen",xlab="Water depth in house [ft]",ylab="Damage [% of house value including contents]")
grid()
dev.off()

# Plot depth-damage under uncertainty 
#jpeg(paste(main_path,"/Figures/S2_Depth_Damage_Uncertainty.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
pdf(paste(main_path,"/Figures/S2_Depth_Damage_Uncertainty.pdf",sep=""),width =3.94, height =2.43)

par(cex=0.5)
plot(Depth,Damage_Factors,type="n",lwd=2,col="darkgreen",xlab="Water depth in house [ft]",ylab="Damage [% of house value including contents]",
     ylim=c(0,100),xlim=Xlim,axes=F)
axis(1,pos=0)
axis(2,pos=-5)
lines(c(-5,25),c(100,100))
#grid()
polygon(x=c(Depth,rev(Depth)),y=c(Damage_Factors_lb,rev(Damage_Factors_ub)),border = NA,col=myblue)
lines(Depth,Damage_Factors,lwd=2,col="blue")
lines(c(25,25),c(0,100))
polygon(x=c(Depth_1,rev(Depth_1)),y=c(Damage_Factors_lb_1,rev(Damage_Factors_ub_1)),border = NA,col=mygreenFade)
lines(Depth_1,Damage_Factors_1,lwd=2,col=mygreen)

legend(9,35,c("Source: Global Flood Depth-Damage Functions \n(Huizinga et al., 2017)","Source: HAZUS (FEMA)"),lty=c(1,1),
       col=c("darkgreen","blue"),cex=0.8,bty="n")
dev.off()
