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
plot_without_reliability=FALSE
plot_with_reliability=TRUE
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Certainty_Calculator.R')

# load(file="Bayesian_GEV_return_levels.RData")
# GEVMCMC_Return_periods=1:10000
# GEVMCMC_Return_levels=MC_rl_mean2

findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi,safety_level=0.7)
filename=paste("RData/V",toString(trunc(Struc_Value/1000)),"I",toString(del),"S",toString(life_span),"R",toString(disc_rate*100),".RData",sep="")
load(filename)

########################################################
if(plot_with_reliability==TRUE){
  print(lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,abs(del)+1))
  print(opt_height_safety)
  
  pdf("Figure/CBA_house_w_safetyObj-4.pdf", width =3.94, height =2.43)
  par(cex=0.5,mar=c(4,4,4,4))
  plot(delta_h_seq,total_cost_GEVMCMC/1000,type="l",lwd=2,col="black",
       ylim=c(0,max(total_cost_GEVMCMC/1000,na.rm=TRUE)),
       xlab="Added Height [ft]",ylab="Cost [1,000 US$]")
  points(opt_height,opt_height_total_cost/1000,pch=20,col="blue",cex=2)
  lines(delta_h_seq,expected_damages_GEVMCMC_seq/1000,type="l",lwd=1,col="slateblue4")
  lines(delta_h_seq,construction_cost_seq/1000,type="l",lwd=1,col="turquoise4")
  axis(3,at=0:14,labels=(0:14)+House_Initial_Stage,main="Stage [ft]")
  axis(4,at=seq(0,max(total_cost_GEVMCMC/1000,na.rm=TRUE),length.out = 11),labels=seq(0,100,length.out = 11),main="Safety [%]")
  
  abline(v=-del+1,col="black",lwd=1,lty=2)
  abline(v=-del,col="black",lwd=1,lty=3)
  abline(v=0,col="black",lwd=1,lty=4)
  text(-0.1,0,"Initial house elevation",adj=c(0,0),srt=90,cex=0.7)
  text(-del-0.1,0,"Base flood elevation",adj=c(0,0),srt=90,cex=0.7)
  text(-del+1-0.1,0,"FEMA recommendation",adj=c(0,0),srt=90,cex=0.7)
  #legend(3,500,c("Total cost","Construction cost","Expected damages in 30 years","Cost-benefit optimal elevation","Safety"),
  #       col=c("black","turquoise4","slateblue4","blue","red"),
  #       lwd=c(2,1,1,NA,2),pch=c(NA,NA,NA,20,NA),bty="n",cex=0.8)
  
  par(new=T)
  plot(delta_h_seq,100*safety_seq,col="red",xaxt="n",xlab="",ylab="",yaxt="n",type="l",lwd=2,ylim=c(0,100))
  
  mtext("Stage [ft]",side=3,line=2.5,cex=0.5)
  mtext("Safety [%]",side=4,line=2.5,cex=0.5)
  
  
  dev.off()
}
