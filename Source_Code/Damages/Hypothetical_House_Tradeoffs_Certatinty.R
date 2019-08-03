# This script is written by Mahkameh Zarekarizi to calculate the expected damages to a hypothetical house 
# This house worth 350,000 USD
# This house is located 1 feet below the base flood elevation 
# We assume it is located right next to our USGS gage
# BFE here is 449.2 ft
# House level is 439.2 ft
# The house is 1500 square feet

# EAD is the expected annual damage
# EAD is the probability of a certain flood times its associated damages
rm(list=ls())
# Global variables
Struc_Value=350000 #USD
sqft=1500
del=-4
House_Initial_Stage=(35.3)+del
life_span=30
disc_rate=0.04
FEMA_Return_periods=c(2,5,10,25,50,100,500)
FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_MCMC_Mean_Params.RData")
mu=Mean_mu
xi=Mean_xi
sigma=Mean_sigma 

source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Certainty_Calculator.R')

# load(file="Bayesian_GEV_return_levels.RData")
# GEVMCMC_Return_periods=1:10000
# GEVMCMC_Return_levels=MC_rl_mean2

findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi)

filename=paste("RData/V",toString(trunc(Struc_Value/1000)),"I",toString(del),"S",toString(life_span),"R",toString(disc_rate*100),".RData",sep="")
load(filename)

#source("../Figures/mycolors.R")

#pdf("Figure/CBA_house-6.pdf", width =3.94, height =2.43)
#par(cex=0.5)


#pdf("Figure/tradeoffs_certainty-4.pdf", width =3.94, height =2.43)
#par(cex=0.5)

fema_recom_damages=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,abs(del)+1,life_span,disc_rate,mu,sigma,xi)
fema_recom_cost=Rising_Cost(sqft,abs(del)+1)

plot(construction_cost_seq/1000,expected_damages_seq/1000,type="l",lwd=4,
     xlab="Construction cost [1,000 US$]",ylab="Expected damages [1,000 US$]")
points(construction_cost_seq[which.min(total_cost)]/1000,
       expected_damages_seq[which.min(total_cost)]/1000,pch=15,col=4,cex=3)
points(fema_recom_cost/1000,fema_recom_damages/1000,pch=15,col="orange",cex=2)
points(construction_cost_seq[1]/1000,
       expected_damages_seq[1]/1000,pch=15,col=3,cex=2)
#axis(3,at=construction_cost_seq/1000,labels=delta_h_seq+House_Initial_Stage)
#mtext("Stage [ft]",side=3,line=2.5,cex=0.5)
legend(0,80,c("No action","Cost effective policy","FEMA recommendation","Total costs under certainty"),
       pch=c(15,15,15,NA),col=c(3,4,"orange",1),lty=c(NA,NA,NA,1),lwd=c(NA,NA,NA,4),bty="n",pt.cex=c(2,2,2,NA))
#dev.off()

 # source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Certainty_Calculator.R')
 # disc_rates=seq(0.01,0.09,length.out=10)
 # life_spans=seq(5,100,length.out=10)
 # Struc_Values=seq(60000,1000000,length.out=10)
 # dels=seq(-10,0,length.out=10)
 # 
 # x=rep(NA,length(disc_rates))
 # for (i in 1:length(disc_rates)){
 # xx=findopt(Struc_Value,dels[i],House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,GEVMCMC_Return_periods,GEVMCMC_Return_levels)
 # x[i]=xx[1,1]
 # #print(xx)
 # }
 # plot(x,type="l",col="red",lwd=2,xlab="Discounting Rate",ylab="Optimal Elevation")
