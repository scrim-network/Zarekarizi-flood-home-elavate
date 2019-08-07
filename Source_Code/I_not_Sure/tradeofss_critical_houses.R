
rm(list=ls())
library(evir) # We would use pgev, qgev from this package
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')
source("../Figures/mycolors.R")

# Global variables
Struc_Value=350000 #USD
sqft=1500
del=-6
run_function=0
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_MCMC_Mean_Params.RData")
BFE=qgev(p=0.99,xi=Mean_xi,sigma=Mean_sigma,mu=Mean_mu) # FEMA BFE=35.3
# FEMA_Return_periods=c(2,5,10,25,50,100,500)
# FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
House_Initial_Stage=BFE+del
life_span=30
disc_rate=0.04

load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")
mu=quantile(mu_chain,0.5)
xi=quantile(xi_chain,0.5)
sigma=quantile(sigma_chain,0.5) 

# Run the cost benefit analysis under uncertainty
if(run_function==1){
findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,10000,safety_level=0)
}

filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/UNC-SR_V",toString(trunc(Struc_Value/1000)),"Sq",toString(trunc(sqft)),"I",toString(del),".RData",sep="")
load(filename)
cb_cost=construction_cost_seq
cb_benefit=(expected_damages_GEVMCMC_seq[1]-expected_damages_GEVMCMC_seq)
cb=cb_benefit/cb_cost


pdf(paste("Figure/Tradeoffs",del,".pdf",sep=""), width =3.94, height =2.34)
par(cex=0.5)
# Start plotting
plot(expected_damages_GEVMCMC_seq/1000,construction_cost_seq/1000,type="n",col="blue",
     xlim=c(min(damages_unc_min/1000),max(damages_unc_max/1000)),
     ylab="Upfront construction cost [1,000 US$]",
     xlab="Expected damages [1,000 US$]")

# Add the area where the cost is as much as house value
polygon(y=c(-100,300,300,-100),x=c(Struc_Value/1000,Struc_Value/1000,500,500),col="gray",border=NA)

# Add the uncertainty bounds
polygon(y=c(construction_cost_seq/1000,rev(construction_cost_seq/1000)),x=c(damages_unc_max/1000,rev(damages_unc_min/1000)),col = "#FF666640",border=NA)

# Add the expected value line under uncertainty
lines(y=construction_cost_seq[2:length(delta_h_seq)]/1000,x=damages_unc_mean[2:length(delta_h_seq)]/1000,col="red")
lines(y=construction_cost_seq[1:2]/1000,x=damages_unc_mean[1:2]/1000,col="red",lty=2)

# Add the tradeoffs under certainty
lines(y=construction_cost_seq[2:length(delta_h_seq)]/1000,x=expected_damages_GEVMCMC_seq[2:length(delta_h_seq)]/1000,col="blue")
lines(y=construction_cost_seq[1:2]/1000,x=expected_damages_GEVMCMC_seq[1:2]/1000,col="blue",lty=2)

# Add points on the lines size changes with safety
inds=intersect(which(safety_seq>=0.5),c(1,2,10,12,20,21,31))
points(y=construction_cost_seq[inds]/1000,x=expected_damages_GEVMCMC_seq[inds]/1000,pch=16,cex=1*punif(q=safety_unc_mean[inds],min=0,max=1),col="blue")

inds=intersect(which(safety_unc_mean>=0.5),c(1,2,10,12,20,21,31))
points(y=construction_cost_seq[inds]/1000,x=damages_unc_mean[inds]/1000,pch=16,cex=1*punif(q=safety_unc_mean[inds],min=0,max=1),col="red")

# Add another set of points to indicate benefit to cost ratio
inds=intersect(which(cb>=1),c(1,2,10,12,20,21,31))
points(y=construction_cost_seq[inds]/1000,x=expected_damages_GEVMCMC_seq[inds]/1000,pch=16,cex=1*punif(q=safety_unc_mean[inds],min=0,max=1),col="blue")

inds=intersect(which(cb_unc_mean>=1),c(1,2,10,12,20,21,31))
points(y=construction_cost_seq[inds]/1000,x=damages_unc_mean[inds]/1000,pch=3,cex=1*punif(q=safety_unc_mean[inds],min=0,max=1),col="black")

# Add points for optimal policies
points(y=construction_cost_seq[which.min(total_cost_GEVMCMC)]/1000,x=expected_damages_GEVMCMC_seq[which.min(total_cost_GEVMCMC)]/1000,pch=0,cex=1.5,bg=myblue,col=myblue)
points(y=construction_cost_seq[which.min(totcost_unc_mean)]/1000,x=damages_unc_mean[which.min(totcost_unc_mean)]/1000,pch=0,cex=1.5,col=myred)

# Add FEMA's recommendation
  # First, run findopt under certainty to obtain FEMA_Recomm_cost and FEMA_Recomm_damage
  findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,Mean_mu,Mean_sigma,Mean_xi)
  # Then add the points
  points(y=FEMA_Recomm_cost/1000,x=FEMA_Recomm_damage/1000,pch=0,col=mygreen,bg=mygreen,cex=1.5)

# Add legend
legend("topright",c("Tradeoffs under uncertainty","Tradeoffs ignoring uncertainty","B/C>1","","Safety","0.5","1"),
       lty=c(1,1,NA,NA,NA,NA,NA),pch=c(NA,NA,3,NA,NA,16,16),col=c("red","blue","black",NA,NA,"black","black"),bty="n",pt.cex=c(NA,NA,1,NA,NA,0.5,1))
  
text(Struc_Value/1000,25,"House value")
arrows(Struc_Value/1000,15, x1 = Struc_Value/1000, y1 =-5, length = 0.1, angle = 30)

arrows(200,130, x1 =200, y1 =0, length = 0.1, angle = 30,code=3)
text(215,70,"Infeasible zone (h<3 feet)",srt=90)

text(150,170,"Optimal policy under uncertainty")
arrows(60,170, x1 =30, y1 =155, length = 0.05, angle = 30)

text(30,100,"FEMA \n Recommendation")
arrows(18,120, x1 =18, y1 =140, length = 0.05, angle = 30)

text(60,10,"Optimal policy \n ignoring uncertainty")
arrows(115,0, x1 =130, y1 =0, length = 0.05, angle = 30)



dev.off()





