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
save_plot=1 # Set this to 1 if you want to save the plots 
plot_cb=1   # Set this to 1 if you want to plot the B/C
plot_safety=1 # Set this to 1 if you want to plot safety across strategies

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Load the necessary libraries, functions, and data 
library(evir) # We would use pgev, qgev from this package
source(paste(main_path,'Source_Code/Cost_Damage_Calculator.R',sep=""))
load(paste(main_path,"Results_RData/GEV_MCMC_Mean_Params.RData",sep=''))

# Specific variables to this house case
Struc_Value=350000 #Building value before elevating in USD
sqft=1500 # Building square footage
del=-9 # the initial difference between house elevation and the base flood elevation 
BFE=qgev(p=0.99,xi=Mean_xi,sigma=Mean_sigma,mu=Mean_mu) # Base flood elevation 
House_Initial_Stage=BFE+del # The initial stage of the building. Note: Stage is the difference between an elevation and the USGS benchmark on the river bed.
life_span=30 # building lifetime in years 
disc_rate=0.04 # Discount rate 

# From "Cost_Damage_Calculator.R", run findopt
findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,Mean_mu,Mean_sigma,Mean_xi)
filename=paste("RData/V",toString(trunc(Struc_Value/1000)),"I",toString(del),"S",toString(life_span),"R",toString(disc_rate*100),".RData",sep="")
load(filename)


if(save_plot==1){
pdf(paste("Figure/CBA2_house",del,".pdf",sep=""), width =3.94, height =6.375)
par(cex=0.5,mai=c(0.1,0.1,0.1,0.1))
}

par(cex=0.5,fig=c(0.07,0.93,0.7,0.95))
plot(delta_h_seq,total_cost_GEVMCMC/1000,type="n",lwd=1,col="black",
     ylim=c(0,max(max(total_cost_GEVMCMC/1000),Struc_Value*1.01/1000)),
     xlab="Added Height [ft]",ylab="Cost [1,000 US$]",xlim=c(0,max(delta_h_seq)))
polygon(x=c(-1,30,30,-1),y=c(Struc_Value/1000,Struc_Value/1000,Struc_Value*10/1000,Struc_Value*10/1000),col="lightgray",border=NA)
# total cost line
lines(delta_h_seq[2:length(delta_h_seq)],total_cost_GEVMCMC[2:length(delta_h_seq)]/1000,lwd=1,col="black")
points(delta_h_seq[1],total_cost_GEVMCMC[1]/1000,col="black",pch=0,cex=3)

# optimal policy point
points(delta_h_seq[which.min(total_cost_GEVMCMC)],min(total_cost_GEVMCMC)/1000,pch=20,col="blue",cex=2)

# Expected damages line
lines(delta_h_seq[1:length(delta_h_seq)],expected_damages_GEVMCMC_seq[1:length(delta_h_seq)]/1000,lwd=1,col="slateblue4")
#lines(delta_h_seq[1:2],expected_damages_GEVMCMC_seq[1:2]/1000,lwd=1,col="slateblue4",lty=2)

# construction cost line
lines(delta_h_seq[2:length(delta_h_seq)],construction_cost_seq[2:length(delta_h_seq)]/1000,lwd=1,col="turquoise4")
#lines(delta_h_seq[1:2],construction_cost_seq[1:2]/1000,lwd=1,col="turquoise4",lty=2)

abline(h=Struc_Value)
abline(v=-del+1,col="black",lwd=1,lty=2)
abline(v=-del,col="black",lwd=1,lty=3)
abline(v=0,col="black",lwd=1,lty=4)

axis(3,at=0:14,labels=signif((0:14)+House_Initial_Stage,3),main="Stage [ft]")
axis(4,at=seq(0,max(max(construction_cost_seq/1000),max(expected_damages_GEVMCMC_seq/1000),max(total_cost_GEVMCMC/1000)),length.out = 10)
     ,labels=round(100*seq(0,max(max(construction_cost_seq/1000),max(expected_damages_GEVMCMC_seq/1000),max(total_cost_GEVMCMC/1000)),length.out = 10)/(Struc_Value/1000)))

mtext("Stage [ft]",side=3,line=2.5,cex=0.5)
mtext("Fraction of house value",side=4,line=2.5,cex=0.5)
mtext("Added Height [ft]",side=1,line=2.5,cex=0.5)
mtext("Cost [1,000 US$]",side=2,line=2.5,cex=0.5)

legend("topright",c("Total cost","Construction cost","Expected damages","Cost-benefit optimal"),
#legend(7.5,145,c("Total cost","Construction cost","Expected damages in 30 years","Cost-benefit optimal elevation"),bg="white",
       col=c("black","turquoise4","slateblue4","blue"),
       lwd=c(1,1,1,NA),pch=c(NA,NA,NA,20),bty="n",box.col="black",pt.cex=c(NA,NA,NA,2))

text(-0.1,0,"Initial house elevation",adj=c(0,0),srt=90,cex=1)
text(-del+1-0.1,0,"FEMA",adj=c(0,0),srt=90,cex=1)
text(-del-0.1,0,"BFE",adj=c(0,0),srt=90,cex=1)


if(plot_cb==1){
  par(fig=c(0.07,0.93,0.40,0.65), new=TRUE)
  cb_cost=construction_cost_seq
  #cb_benefit=abs(total_cost_GEVMCMC-total_cost_GEVMCMC[1])
  cb_benefit=(expected_damages_GEVMCMC_seq[1]-expected_damages_GEVMCMC_seq)
  cb=cb_benefit/cb_cost
  #cb[1]=NA
  plot(delta_h_seq,cb,xlab="Added Height [ft]",ylab="Cost-benefit ratio",type="n",ylim=c(0,5),xlim=c(0,max(delta_h_seq)))
  polygon(x=c(-1,30,30,-1),y=c(1,1,-1,-1),col="lightgray",border=NA)
  lines(delta_h_seq,cb,xlab="Added Height [ft]",ylab="Cost-benefit ratio",type="l",ylim=c(0,5))
  
  #abline(h=1,col="blue",lwd=1)
  abline(v=-del+1,col="black",lwd=1,lty=2)
  abline(v=-del,col="black",lwd=1,lty=3)
  abline(v=0,col="black",lwd=1,lty=2)
  legend("topright",c("Cost-benefit ratio=1"),col=c("blue"),lty=c(1),lwd=c(1),bty="n")
  mtext("Benefit-cost ratio",side=2,line=2.5,cex=0.5)
  mtext("Added Height [ft]",side=1,line=2.5,cex=0.5)
  
}

if(plot_safety==1){
  par(fig=c(0.07,0.93,0.1,0.35), new=TRUE)
  plot(delta_h_seq,safety_seq,xlab="Added Height [ft]",ylab="Safety [%]",type="n",ylim=c(0,1),xlim=c(0,max(delta_h_seq)))
  polygon(x=c(-1,30,30,-1),y=c(.5,.5,-1,-1),col="lightgray",border=NA)
  lines(delta_h_seq[2:length(delta_h_seq)],safety_seq[2:length(delta_h_seq)])
  points(delta_h_seq[1],safety_seq[1],pch=0,cex=3)
  
  abline(v=-del+1,col="black",lwd=1,lty=2)
  abline(v=-del,col="black",lwd=1,lty=3)
  abline(v=0,col="black",lwd=1,lty=4)
  mtext("Safety",side=2,line=2.5,cex=0.5)
  mtext("Added Height [ft]",side=1,line=2.5,cex=0.5)
  #abline(h=.5,col="blue",lwd=1)
  
}

if(save_plot==1){
  dev.off()
}

