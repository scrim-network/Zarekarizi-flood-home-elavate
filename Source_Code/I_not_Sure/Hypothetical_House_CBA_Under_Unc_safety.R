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
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_MCMC_Mean_Params.RData")

mu=Mean_mu
xi=Mean_xi
sigma=Mean_sigma 

source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Uncertainty_Calculator2.R')

findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,10000,safety_level=0)
#findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,mu_chain,sigma_chain,xi_chain,n)

filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/UNC-SR_V",toString(trunc(Struc_Value/1000)),"Sq",toString(trunc(sqft)),"I",toString(del),".RData",sep="")
load(filename)

source("../Figures/mycolors.R")

pdf("Figure/CBA_Unc_safety_house-4.pdf", width =3.94, height =2.43)
par(cex=0.5)
plot(delta_h_seq,total_cost_GEVMCMC/1000,type="l",lwd=2,col="black",
     ylim=c(0,max(totcost_unc_max/1000)),
     xlab="Added elevation [ft]",ylab="Cost [1,000 US$]")
#points(delta_h_opt-1,opt/1000,pch=16,col="#0080FF40",cex=1)
#points(round(mean(delta_h_opt-1)),mean(opt[delta_h_opt-1==round(mean(delta_h_opt-1))]/1000),pch=16,col=1,cex=2)
lines(delta_h_seq,totcost_unc_mean/1000,col=myred,lwd=2)
points(opt_height,opt_height_total_cost/1000,col="black",cex=1.5,pch=16)
points(opt_height_unc,opt_height_unc_total_cost/1000,col=myred,cex=1.5,pch=16)

polygon(x=c(delta_h_seq,rev(delta_h_seq)),y=c(totcost_unc_max/1000,rev(totcost_unc_min/1000)),
        col = "#FF666640",border=NA)

axis(3,at=0:14,labels=(0:14)+House_Initial_Stage,main="Stage [ft]")
axis(4,at=seq(0,max(totcost_unc_max/1000,na.rm=TRUE),length.out = 11),labels=seq(0,100,length.out = 11),main="Safety [%]")


abline(v=-del+1,col="black",lwd=1,lty=2)
abline(v=-del,col="black",lwd=1,lty=3)
abline(v=0,col="black",lwd=1,lty=4)
text(-0.1,0,"Initial house elevation",col=1,adj=c(0,0),srt=90,cex=0.7)
text(-del-0.1,0,"Base flood elevation",col=1,adj=c(0,0),srt=90,cex=0.7)
text(-del+1-0.1,0,"FEMA recommendation",col=1,adj=c(0,0),srt=90,cex=0.7)

# legend("topright",c("Total cost neglecting uncertainty","Expected total cost under uncertainty","Total cost 90% credible intervals",
#                     "Minimum cost scenario neglecting uncertainty","Minimum cost scenario under uncertainty"),
#        pch=c(NA,NA,22,16,16),
#        col=c("black",myred,"#FF666640","black",myred),lwd=c(2,2,NA,NA,NA),bty="n",
#        pt.bg=c(NA,NA,"#FF666640",NA,NA),pt.cex=c(NA,NA,3,1,1))
mtext("Stage [ft]",side=3,line=2.5,cex=0.5)
mtext("Safety [%]",side=4,line=2.5,cex=0.5)

par(new=T)
plot(delta_h_seq,100*safety_seq,col=myblue,xaxt="n",xlab="",ylab="",yaxt="n",type="l",lwd=2,ylim=c(0,100))
polygon(x=c(delta_h_seq,rev(delta_h_seq)),y=c(100*safety_unc_max,rev(100*safety_unc_min)),
        col = "#0080FF40",border=NA)


dev.off()

