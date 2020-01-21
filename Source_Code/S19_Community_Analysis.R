##==============================================================================
##
## Script creates plots with the goal of comparing FEMA, considering-uncertainty, 
# and ignoring-uncertainty strategies for a community of houses. 
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
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
##      1. output includes multiple plots in Figures directory 
##==============================================================================

# Global variables
main_path=getwd()

# Load the results for the community created in S16_Many_Houses_CBA.R 
load(paste(main_path,"/",load_path,"/House_case_objectives/Hypothetical_houses.RData",sep='')) 
n_houses=length(opt_bc)

# Plot scatter plot of FEMA recommended height versus considering-uncertainty height 
pdf(paste(main_path,"/Figures/S19_scatter_Recommended_strategy_FEMA_vs_optimal.pdf",sep=''), width =3.94, height =3.94)
#jpeg(paste(main_path,"/Figures/S19_scatter_Recommended_strategy_FEMA_vs_optimal.jpeg",sep=""),width =3.94, height =3.94,units="in",res=300)

par(cex=0.5)
inds<-which(SOWs[,3]<(-1*3))
inds2<-which(SOWs[,3]<(-1*3) & fema_bc<1)
plot(fema_height[inds],optunc_height[inds],pch=20,col=4,xlim=c(0,14),ylim=c(0,14),xlab="FEMA recommended added height [ft]",ylab="Economic optimal heightening policy considering uncertainty [ft]")
abline(0,1,col="darkgreen",lwd=2)
text(7,1,paste("In",floor(100*sum(fema_height>0 & optunc_height==0)/n_houses),"% of the houses FEMA recommeds \nelevating but it is not cost optimal"))
text(2.5,7.5,paste("In",floor(100*sum(optunc_height>0 & fema_height<optunc_height)/n_houses),"% of the houses \nFEMA recommeded height \nis lower than cost optimal height"))
text(11,7,paste("In",floor(100*sum(optunc_height>0 & fema_height>optunc_height)/n_houses),"% of the houses \nFEMA recommeded height \nis higher than cost optimal height"),xpd=T)
points(fema_height[inds2],optunc_height[inds2],pch=20,col="red")
legend(8.5,5,c("FEMA does not pass B/C test",'FEMA passes B/C test'),pch=c(20,20),bty="o",col=c("red",'blue'))
dev.off()

# Plots optimal height ignoring uncertainty versus optimal height considering uncertainty 
pdf(paste(main_path,"/Figures/S19_scatter_Recommended_strategy_neglectingUnc_vs_optimal.pdf",sep=''), width =3.94, height =3.94)
#jpeg(paste(main_path,"/Figures/S19_scatter_Recommended_strategy_neglectingUnc_vs_optimal.jpeg",sep=""),width =3.94, height =3.94,units="in",res=300)

par(cex=0.5)
plot(opt_height,optunc_height,pch=1,type="n",col=4,xlim=c(0,14),ylim=c(0,14),xlab="Optimal added height neglecting uncertainty [ft]",ylab="Optimal added height considering uncertainty [ft]")
points(opt_height[opt_const_frac<=1],optunc_height[opt_const_frac<=1],pch=1,col="blue")
abline(0,1,col="darkgreen",lwd=2)
points(opt_height[opt_const_frac>1],optunc_height[opt_const_frac>1],pch=2,col="red")
points(opt_height[optunc_const_frac>1],optunc_height[optunc_const_frac>1],pch=6,col="red")
legend(6,4,c('The cost of optimal elevation ignoring \nuncertainty is more than house value','A sample house',
             'The cost of optimal elevation considering\n uncertainty is more than house value'),
       col=c('red','blue','red'),pch=c(2,1,6),bty="n")
dev.off()
