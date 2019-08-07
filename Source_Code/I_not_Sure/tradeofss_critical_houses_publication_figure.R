
rm(list=ls())
library(evir) # We would use pgev, qgev from this package
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')
source("../Figures/mycolors.R")

# Global variables
# Struc_Value=350000 #USD
# sqft=1500
# del=-6
Struc_Value=277000 #USD
sqft=1000
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


pdf(paste("Figure/Tradeoffs_pub",del,".pdf",sep=""), width =3.94, height =2.34)
par(cex=0.5)
# Start plotting
plot(y=expected_damages_GEVMCMC_seq/1000,x=construction_cost_seq/1000,type="n",col="blue",
     ylim=c(min(expected_damages_GEVMCMC_seq)/1000,max(damages_unc_mean/1000)),
     xlab="Upfront investment [1,000 US$]",
     ylab="Expected damages [1,000 US$]")
library(plotrix)
gap.plot(construction_cost_seq/1000, expected_damages_GEVMCMC_seq/1000, gap.axis="x",gap=c(1,99), type="b", xlab="index", ylab="value")
axis.break(2, from, breakcol="snow", style="gap")
axis.break(2, from*(1+0.02), breakcol="black", style="slash")
axis.break(4, from*(1+0.02), breakcol="black", style="slash")
axis(2, at=from)

#axis(3,at=construction_cost_seq[c(1,2,11,31)]/1000,labels=signif(delta_h_seq[c(1,2,11,31)],3))
#mtext("Added elevation [ft]",side=3,line=2.5,cex=0.5)
#abline(v=construction_cost_seq[which(delta_h_seq==abs(del)+1)]/1000,col="lightgray")
lines(x=c(0,0),y=c(0,155),col="lightgray",xpd=TRUE)

# Add the area where the cost is as much as house value
#polygon(x=c(-100,300,300,-100),y=c(Struc_Value/1000,Struc_Value/1000,500,500),col="gray",border=NA)

# Add the uncertainty bounds
#polygon(x=c(construction_cost_seq/1000,rev(construction_cost_seq/1000)),y=c(damages_unc_max/1000,rev(damages_unc_min/1000)),col = "#FF666640",border=NA)

# Add the expected value line under uncertainty
lines(x=construction_cost_seq[2:length(delta_h_seq)]/1000,y=damages_unc_mean[2:length(delta_h_seq)]/1000,col="red",lty=2)
lines(x=construction_cost_seq[which(cb_unc_mean>=0.95)]/1000,y=damages_unc_mean[which(cb_unc_mean>=0.95)]/1000,col="red",lwd=1)
lines(x=construction_cost_seq[1:2]/1000,y=damages_unc_mean[1:2]/1000,col="red",lty=2)

# Add the tradeoffs under certainty
lines(x=construction_cost_seq[2:length(delta_h_seq)]/1000,y=expected_damages_GEVMCMC_seq[2:length(delta_h_seq)]/1000,col="blue",lty=2)
lines(x=construction_cost_seq[1:2]/1000,y=expected_damages_GEVMCMC_seq[1:2]/1000,col="blue",lty=2)

# Add points on the lines size changes with safety
inds=intersect(which(safety_seq>=0.5),c(1,2,10,11,19,20,29))
points(x=construction_cost_seq[inds]/1000,y=expected_damages_GEVMCMC_seq[inds]/1000,pch=16,cex=1.5*safety_seq[inds],col="blue")

inds=intersect(which(safety_unc_mean>=0.5),c(1,2,10,11,19,20,29))
points(x=construction_cost_seq[inds]/1000,y=damages_unc_mean[inds]/1000,pch=16,cex=1.5*safety_unc_mean[inds],col="red")

# Add another set of points to indicate benefit to cost ratio
#inds=intersect(which(cb>=1),c(1,2,10,12,20,21,31))
#points(x=construction_cost_seq[inds]/1000,y=expected_damages_GEVMCMC_seq[inds]/1000,pch=3,cex=1*punif(q=safety_unc_mean[inds],min=0,max=1),col="blue")

#inds=intersect(which(cb_unc_mean>=1),c(1,2,10,12,20,21,31))
#points(x=construction_cost_seq[inds]/1000,y=damages_unc_mean[inds]/1000,pch=3,cex=1*punif(q=safety_unc_mean[inds],min=0,max=1),col="black")

# Add points for optimal policies
points(x=construction_cost_seq[which.min(total_cost_GEVMCMC)]/1000,y=expected_damages_GEVMCMC_seq[which.min(total_cost_GEVMCMC)]/1000,pch=1,cex=1.5,col="black")
points(x=construction_cost_seq[which.min(totcost_unc_mean)]/1000,y=damages_unc_mean[which.min(totcost_unc_mean)]/1000,pch=1,cex=1.5,col="black")

# Add FEMA's recommendation
# First, run findopt under certainty to obtain FEMA_Recomm_cost and FEMA_Recomm_damage
#findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,Mean_mu,Mean_sigma,Mean_xi)
# Then add the points
#points(x=FEMA_Recomm_cost/1000,y=FEMA_Recomm_damage/1000,pch=0,col=mygreen,bg=mygreen,cex=1.5)

# Add legend
polygon(x=c(0,45,45,0),y=c(0,0,50,50),border ="black",col=myredalpha05)
par(new=TRUE)
legend(18,45,c("B/C>=1","B/C<1"),lty=c(1,2),col=c("black","black"),bty="n")

#text(y=Struc_Value/1000,x=25,"House value")
#arrows(y0=Struc_Value/1000,x0=15, y1 = Struc_Value/1000, x1 =-5, length = 0.1, angle = 30)

#arrows(y0=200,x0=130, y1 =200, x1 =0, length = 0.1, angle = 30,code=3)
#text(215,70,"Infeasible zone (h<3 feet)",srt=90)

text(117,70,"Optimal policy \n under \n uncertainty",col="red")
arrows(117,50, x1 =108, y1 =24, length = 0.05, angle = 30,col="red")

text(130,170,"FEMA \n Recommendation",col="gray")
arrows(130,170, x1 =150, y1 =170, length = 0.05, angle = 30,col="gray")

text(13,85,"Optimal policy \n ignoring uncertainty",col="blue")
arrows(0,85, x1 =0, y1 =110, length = 0.05, angle = 30,col="blue")

text(65,105,"Trade-off under uncertainty",col="red",srt=-14)
text(65,80,"Trade-off ignoring uncertainty",col="blue",srt=-14)
legend(3,35,c("0.5","1"),pch=c(16,16),col=c("black","black"),bty="n",pt.cex=c(0.5,1))
text(10,42,"Safety")

mtext(side=3,at=30,line=1,"Initial house elevation",col="gray",cex=0.5)
arrows(30,155, x1 =3, y1 =155, length = 0.05, angle = 30,col="gray",xpd=TRUE)

text(80,32,"FEMA recommendation \n under uncertainty",col="gray")
arrows(90,31, x1 =106, y1 =31, length = 0.05, angle = 20,col="gray",xpd=TRUE)

text(73,10,"FEMA recommendation \n ignoring uncertainty",col="gray")
arrows(90,12, x1 =106, y1 =14, length = 0.05, angle = 20,col="gray",xpd=TRUE)

dev.off()





