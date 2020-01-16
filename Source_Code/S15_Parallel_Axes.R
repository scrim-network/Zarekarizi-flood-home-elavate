##==============================================================================
##
## Script for parallel axes plot of all objectives for one house
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
##      1. Click on Source button or, on console, type: Source("../../....R")
## 3. Outputs:
##      1. output includes a plot in Figures directory 
##==============================================================================
  
# Global variables
main_path=getwd()
set.seed(0)
mygreen <- rgb(44/255, 185/255, 95/255, 1) #Colors we will use in the plot
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 0.4)
run_function=0 # In case you later want to run the script but not run the optimization function, turn this off

# Load libraries, data, and codes 
library(evd) # We would use pgev, qgev from this package
library(fields) #We need this library later when we need to add colorbar 
source(paste(main_path,'/Source_Code/Functions/Cost_Damage_Calculator.R',sep=''))
load(paste(main_path,"/",load_path,"/GEV_Parameters_MCMC.RData",sep=''))
discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))

# Functions.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate GEV parameters under certainty (choosing the mode; the most probable prediction)
mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

# Given the above parameters, calculate the base flood elevation
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) 

source(paste(main_path,"/Source_Code/Functions/House_chars.R",sep=""))

sqft=house_charactersitics()[1,'sqft']
Struc_Value=house_charactersitics()[1,'Struc_Value']
del=house_charactersitics()[1,'del']
life_span=house_charactersitics()[1,'life_span']
disc_rate=house_charactersitics()[1,'disc_rate']

naxes=4 #Number of objectives (i.e. number of axes)

# House initial stage 
House_Initial_Stage=BFE+del

# If you already have the results (optimal elevation, cost-benefit analysis, etc. for this specific house, set run_function=0)
if(run_function==1){ # Run the findopt_UNC function stored in Cost_Damage_Calculator file
  returned_data=findopt_UNC(sqft,
                            Struc_Value,
                            del,
                            House_Initial_Stage,
                            life_span,
                            disc_rate,
                            mu,sigma,xi,mu_chain,sigma_chain,xi_chain,
                            nsow=length(xi_chain),
                            discount,
                            save_return=1,
                            n_strategy=100,
                            verbose=TRUE,
                            ddUnc='deep',
                            lifeUnc='weibull',
                            drUnc='deep',
                            threshold_totalcost=0.75,
                            test=FALSE
                            )
  print(returned_data)
}

# The file has been saved with this name convention
filename=paste(main_path,"/",load_path,"/House_case_objectives/Decision_Vars_V",toString(trunc(Struc_Value/1000)),"_Sq",toString(trunc(sqft)),"_I",toString(del),".RData",sep="")

# Load the file
load(filename)

# Start plotting 
pdf(paste(main_path,"/Figures/S15_Parallel_Axes_Plot.pdf",sep=''), width =3.94, height =2.43)
#jpeg(paste(main_path,"/Figures/S15_Parallel_Axes_Plot.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
#png(paste(main_path,"/Figures/S15_Parallel_Axes_Plot.png",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5,mai=c(0.4,0.2,0.1,0.1)) #Margins for the entire figure
par(cex=0.45,fig=c(0,0.9,0,1)) #Position of the first panel 

plot(NA,NA,type="n",xlim=c(1,naxes),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="") 
indexs=(1:length(delta_h_seq))

for (i in 1:length(indexs)){ #Each iteration of this for loop plots a line on the parallel axes plot 
  lines(1:naxes,c(
                  punif(bcr_unc_mean[i],min=min(bcr_unc_mean,na.rm = T),max=max(bcr_unc_mean,na.rm = T)),
                  1-punif(construction_cost_seq[i]/Struc_Value,min=min(construction_cost_seq/Struc_Value),max=max(construction_cost_seq/Struc_Value)),
                  1-punif(damages_unc_mean[i],min=min(damages_unc_mean),max=max(damages_unc_mean)),
                  punif(safety_unc_mean[i],min=min(safety_unc_mean),max=max(safety_unc_mean))
  ),
  col=rgb(0,0.5,punif(delta_h_seq[i],min=min(delta_h_seq),max=max(delta_h_seq))), #Color of each line depends on the heightening strategy 
  lwd=.5)
}
if (signif(min(bcr_unc_mean,na.rm=T),2)>1){
one_bcr=punif(1,min=min(bcr_unc_mean,na.rm = T),max=max(bcr_unc_mean,na.rm = T)) #This is to indicate 1 on the B/C axis 
lines(x=c(1-0.05,1+0.05),y=c(one_bcr,one_bcr),xpd=T)
text(1-0.14,one_bcr,'1',xpd=T)
}

abline(v=c(1:naxes),col="black",lwd=2)
lines(x=c(1,naxes),y=c(-.01,-0.01),lwd=2)
lines(x=c(1,naxes),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(bcr_unc_mean,na.rm=T),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(bcr_unc_mean,na.rm=T),2),xpd=TRUE)
text(1,adj_y,paste(expression("Benefit-to-cost")),xpd=TRUE)
points(1.05,1.02,pch=8,cex=1,xpd=T,col="darkgoldenrod2")

text(2-adj_x,-adj_y_num,signif(max(construction_cost_seq/Struc_Value),2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(min(construction_cost_seq/Struc_Value),2),xpd=TRUE)
text(2,adj_y,paste(expression("Ratio of upfront construction\n cost to house value")),xpd=TRUE)
points(2.05,1.02,pch=8,cex=1,xpd=T,col="darkgoldenrod2")

text(3-adj_x,-adj_y_num,signif(max(damages_unc_mean)/1000,2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(min(damages_unc_mean)/1000,2),xpd=TRUE)
text(3,adj_y,paste(expression("Expected damages\n[1,000 USD]")),xpd=TRUE)
points(3.05,1.02,pch=8,cex=1,xpd=T,col="darkgoldenrod2")

text(4-adj_x,-adj_y_num,signif(min(safety_unc_mean),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(safety_unc_mean),2),xpd=TRUE)
text(4,adj_y,paste(expression("Reliability")),xpd=TRUE)
points(4.05,1.02,pch=8,cex=1,xpd=T,col="darkgoldenrod2")
arrows(x0=1-0.1,y0=0.5,x1=1-0.1,y1=1,col="darkgoldenrod2",xpd=T,length = 0.1,lwd=2)
text(0.85,0.7,'Preferred direction',srt=90,xpd=T,col="darkgoldenrod2")
par(fig=c(0.5,1,0,1),new=T)

plot(NA,NA,type="n",xlim=c(1,naxes),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
image.plot(legend.only=TRUE, zlim=c(0,14), 
           col =rgb(0,0.5,punif(delta_h_seq,min=min(delta_h_seq),max=max(delta_h_seq))), 
            horizontal = F,legend.cex=0.6,cex.axis=0.6,legend.lab="Heightening [ft]",
           legend.shrink=0.75) 
dev.off()

