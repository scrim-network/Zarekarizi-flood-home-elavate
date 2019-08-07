##==============================================================================
##
## Script aims at calculating the expected damages to a hypothetical house 
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
mygreen <- rgb(44/255, 185/255, 95/255, 1) 
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 1)
save_mode=0 # If set as 1, the model will be actually run and saves the results. 
# Running the model for all houses takes a significantly large amount of time. 
# Therefore, in cases that you have already run the model and care about just the plots, you can set this to 0  
disc_rate=0.04 # discount rate is costant for all houses 
n_houses=10000

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Load libraries, data, and functions 
library(evd) # We would use pgev, qgev from this package
source(paste(main_path,'Source_Code/Cost_Damage_Calculator.R',sep=""))
load(paste(main_path,"Results_RData/GEV/GEV_Parameter_Chains.RData",sep=""))

# Define functions 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Based on the chain, estimate the best-guess parameters 
mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

# Estimate the base flood elevation based on the chain 
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

if(save_mode==1){
require(lhs)
z<- randomLHS(n_houses, 4)
SOWs=matrix(NA,n_houses,4)
SOWs[,1] <- qunif(z[,1],100,5000) #sqft
SOWs[,2] <- qunif(z[,2],10000,1000000) # value 
SOWs[,3] <- qunif(z[,3],-10,0) # del
SOWs[,4] <- qunif(z[,4],5,100) # life span
#SOWs[,5] <- qunif(z[,5],0.01, 0.09) 
}

if(save_mode==1){
fema_height   <- fema_totcost   <- fema_safety   <- fema_bc   <- fema_totcost_frac   <- fema_const_frac   <-
opt_height    <- opt_totcost    <- opt_safety    <- opt_bc    <- opt_totcost_frac    <- opt_const_frac    <-
optunc_height <- optunc_totcost <- optunc_safety <- optunc_bc <- optunc_totcost_frac <- optunc_const_frac <- rep(NA,n_houses)

for(i in 1:n_houses){
  print(i)
  tmp=findopt_UNC(SOWs[i,1],SOWs[i,2],SOWs[i,3],(SOWs[i,3]+BFE),SOWs[i,4],disc_rate,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,10000,safety_level=0)

  fema_height[i]=tmp[2,1]
  fema_totcost[i]=tmp[2,2]
  fema_safety[i]=tmp[2,3]
  fema_bc[i]=tmp[2,4]
  fema_totcost_frac[i]=tmp[2,5]
  fema_const_frac[i]=tmp[2,6]
  
  opt_height[i]=tmp[3,1]
  opt_totcost[i]=tmp[3,2]
  opt_safety[i]=tmp[3,3]
  opt_bc[i]=tmp[3,4]
  opt_totcost_frac[i]=tmp[3,5]
  opt_const_frac[i]=tmp[3,6]
  
  optunc_height[i]=tmp[4,1]
  optunc_totcost[i]=tmp[4,2]
  optunc_safety[i]=tmp[4,3]
  optunc_bc[i]=tmp[4,4]
  optunc_totcost_frac[i]=tmp[4,5]
  optunc_const_frac[i]=tmp[4,6]
  
}
save(SOWs,
     fema_height,fema_totcost,fema_safety,fema_bc,fema_totcost_frac,fema_const_frac,
     opt_height,opt_totcost,opt_safety,opt_bc,opt_totcost_frac,opt_const_frac,
     optunc_height,optunc_totcost,optunc_safety,optunc_bc,optunc_totcost_frac,optunc_const_frac,
     file=paste(main_path,"Results_RData/Hypothetical_community_CBA_Unc_",Sys.Date(),".RData",sep=""))
}

load(paste(main_path,"Results_RData/cases_objectives/Hypothetical_community_objectives.RData",sep='')) # I changes the name here otherwise it should be same line above 

########################################################################
########################################################################
########################################################################
pdf(paste(main_path,"Figures/COMMUNITY_optcost_vs_FEMAcost.pdf",sep=''), width =3.94, height =2.43)
par(cex=0.5)
plot(opt_totcost/1000,fema_totcost/1000,pch=20,col="blue",ylab="FEMA recommendation total cost [1,000 US$]",xlab="Optimal policy total cost [1,000 US$]",cex=0.8)
abline(0,1,col="gray",lwd=3)
legend("right","1:1 line",bty="n",col="gray",lty=1,lwd=3)
dev.off()
########################################################################
########################################################################
########################################################################
pdf(paste(main_path,"Figures/COMMUNITY_scatter_optimal_cost.pdf",sep=''), width =3.94, height =2.43)
par(cex=0.5)
plot(opt_totcost/1000,optunc_totcost/1000,pch=20,col="red",ylab="With uncertainty optimal cost [1,000 US$]",xlab="Ignoring uncertainty optimal cost [1,000 US$]",cex=0.8)
abline(0,1,col="gray",lwd=3)
dev.off()
########################################################################
########################################################################
########################################################################
pdf(paste(main_path,"Figures/COMMUNITY_hist_differences.pdf",sep=''), width =3.94, height =2.43)
par(cex=0.5)
hist(optunc_height-opt_height,main="",
     xlab="optimal height under uncertainty - optimal height ignoring uncertainty",col="orange")
dev.off()
########################################################################
########################################################################
########################################################################
pdf(paste(main_path,"Figures/COMMUNITY_boxplot_differences.pdf",sep=''), width =3.94, height =2.43)
par(cex=0.5)
boxplot((fema_totcost-opt_totcost)/1000,horizontal = TRUE,xlab="Extra expenses [1,000 US$]",col="orange")
dev.off()
########################################################################
########################################################################
########################################################################
pdf(paste(main_path,"Figures/COMMUNITY_safety_histogram_differences.pdf",sep=''), width =3.94, height =2.43)
par(cex=0.5)
hist(opt_height-fema_height,col="orange",freq=FALSE,xlab="Optimal Height - FEMA recommendation",ylab="Density",main="")
par(new=TRUE)
hist(optunc_height-fema_height,col=rgb(1,0,0,alpha=0.5),freq=FALSE,xlab="Optimal Height - FEMA recommendation",ylab="Density",main="")
abline(v=0,lwd=3,col="blue")
dev.off()
########################################################################
########################################################################
########################################################################
pdf(paste(main_path,"Figures/COMMUNITY_fema_height_vs_optunc_height.pdf",sep=''), width =3.94, height =3.94)
par(cex=0.5)
plot(fema_height,optunc_height,pch=20,col=4,xlim=c(0,14),ylim=c(0,14),xlab="FEMA recommended added height [ft]",ylab="Economic optimal heightening policy considering uncertainty [ft]")
abline(0,1,col="darkgreen",lwd=2)
text(7,1.5,paste("In",floor(100*sum(fema_height>0 & optunc_height==0)/n_houses),"% of the houses FEMA recommeds \nelevating but it is not cost optimal"))
text(2.5,7.5,paste("In",floor(100*sum(optunc_height>0 & fema_height<optunc_height)/n_houses),"% of the houses \nFEMA recommeded height \nis lower than cost optimal height"))
text(11,7.5,paste("In",floor(100*sum(optunc_height>0 & fema_height>optunc_height)/n_houses),"% of the houses \nFEMA recommeded height \nis higher than cost optimal height"),xpd=T)
points(fema_height[fema_bc<1],optunc_height[fema_bc<1],pch=20,col="red")
legend(8.5,5,c("FEMA does not pass B/C test",'FEMA passes B/C test'),pch=c(20,20),bty="o",col=c("red",'blue'))
dev.off()
########################################################################
########################################################################
########################################################################
pdf(paste(main_path,"Figures/COMMUNITY_opt_height_vs_optunc_height.pdf",sep=''), width =3.94, height =3.94)
par(cex=0.5)
plot(opt_height,optunc_height,pch=1,type="n",col=4,xlim=c(0,14),ylim=c(0,14),xlab="Optimal added height neglecting uncertainty [ft]",ylab="Optimal added height considering uncertainty [ft]")
points(opt_height[opt_const_frac<=1],optunc_height[opt_const_frac<=1],pch=1,col="blue")

abline(0,1,col="darkgreen",lwd=2)
#text(7,1.5,paste("In",floor(100*sum(fema_height>0 & optunc_height==0)/n_houses),"% of the houses FEMA recommeds \nelevating but it is not cost optimal"))
#text(2.5,7.5,paste("In",floor(100*sum(optunc_height>0 & fema_height<optunc_height)/n_houses),"% of the houses \nFEMA recommeded height \nis lower than cost optimal height"))
#text(11,7.5,paste("In",floor(100*sum(optunc_height>0 & fema_height>optunc_height)/n_houses),"% of the houses \nFEMA recommeded height \nis higher than cost optimal height"),xpd=T)
points(opt_height[opt_const_frac>1],optunc_height[opt_const_frac>1],pch=2,col="red")
points(opt_height[optunc_const_frac>1],optunc_height[optunc_const_frac>1],pch=6,col="red")
legend(6,4,c('The cost of optimal elevation ignoring \nuncertainty is more than house value','A sample house',
                 'The cost of optimal elevation considering\n uncertainty is more than house value'),
       col=c('red','blue','red'),pch=c(2,1,6),bty="n")
#legend(8.5,5,c("FEMA does not pass B/C test",'FEMA passes B/C test'),pch=c(20,20),bty="o",col=c("red",'blue'))
dev.off()


