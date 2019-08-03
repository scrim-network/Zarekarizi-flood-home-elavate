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
plot_hypothetical_community_safety_histogram=TRUE
plot_chain_of_hypothetical_houses_CBA=TRUE
plot_parallel_axis=FALSE

n_sow=10000
require(lhs)
#z<- randomLHS(n_sow, 3)
#SOWs=matrix(NA,n_sow,3)
#SOWs[,1] <- qunif(z[,1],100,5000) 
#SOWs[,2] <- qunif(z[,2],10000,1000000)
#SOWs[,3] <- qunif(z[,3],-10,0) 
#SOWs[,4] <- qunif(z[,4],5,100) 
#SOWs[,5] <- qunif(z[,5],0.01, 0.09) 

# Global variables
Struc_Value=350000 #USD
sqft=1500
del=-6
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
#findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi)

#fema_recom_height <- fema_recom_Totcost <- cba_opt_height <- cba_opt_Totcost <- cba_opt_safety <- fema_recom_safety <- rep(NA,n_sow)

# for(i in 1:n_sow){
#   print(i)
#   tmp=findopt(SOWs[i,1],SOWs[i,2],SOWs[i,3],(SOWs[i,3]+35.3),life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi,0.7)
#   fema_recom_height[i]=tmp[3,1]
#   fema_recom_Totcost[i]=tmp[3,2]
#   fema_recom_safety[i]=tmp[3,3]
#   
#   cba_opt_height[i]=tmp[2,1]
#   cba_opt_Totcost[i]=tmp[2,2]
#   cba_opt_safety[i]=tmp[2,3]
#}
#save(SOWs,fema_recom_height,fema_recom_Totcost,fema_recom_safety,cba_opt_height,cba_opt_Totcost,cba_opt_safety,file="RData/Hypothetical_community_CBA_safety_Obj.RData")

load(file="RData/Hypothetical_community_CBA_safety_Obj.RData")
color_safety=rep(1,n_sow)
color_safety[cba_opt_safety>=fema_recom_safety]=2
color_safety[cba_opt_safety<=fema_recom_safety]=3
#save(SOWs,fema_recom_height,fema_recom_Totcost,cba_opt_height,cba_opt_Totcost,file="RData/chain_of_hypothetical_houses_CBA.RData")


# fema_recom_height <- fema_recom_Totcost <- cba_opt_height <- cba_opt_Totcost <- rep(NA,n_sow)
if(plot_chain_of_hypothetical_houses_CBA==TRUE){
  pdf("Figure/Hypothetical_community_total_cost_comparison_with_FEMA_w_safetyObj.pdf", width =3.94, height =2.43)
  par(cex=0.5)
  plot(cba_opt_Totcost/1000,fema_recom_Totcost/1000,pch=20,col=color_safety,ylab="FEMA recommendation total cost [1,000 US$]",xlab="Optimal cost [1,000 US$]",cex=0.8)
  abline(1,1,col="gray",lwd=3)
  legend("bottomright",c("FEMA policy safety = Optimal cost policy safety","FEMA policy safety < Optimal cost policy safety",
                         "FEMA policy safety > Optimal cost policy safety"),pch=c(20,20,20),col=c(1,2,3),bty="n")
  #legend("right","1:1 line",bty="n",col="gray",lty=1,lwd=3)
  dev.off()
}

if(plot_hypothetical_community_safety_histogram==TRUE){
  pdf("Figure/hypothetical_community_safetyObj_histogram.pdf", width =3.94, height =2.43)
  par(cex=0.5)
  hist(color_safety,breaks=c(0.5,1.5,2.5,3.5),col=c(1,2,3),freq=FALSE,xlab="",ylab="Frequency [%]",main="",xaxt="n")
  axis(1,at=c(1,2,3),labels = c("Similar safty","FEMA lower safety","FEMA higher safety"))
  dev.off()
  #100*sum(abs(fema_recom_Totcost-cba_opt_Totcost)<100)/length(cba_opt_Totcost)
  #print()
  #plot(cba_opt_height,fema_recom_height)
}

if(plot_parallel_axis==TRUE){
  plot(NA,NA,type="n",xlim=c(1,3),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
  abline(v=c(1:3))
  axis(1,at=1:3,labels = c("Optimal total cost","FEMA total cost","safety"),line=2,padj=0.5)
  
  for (i in 1:n_sow){
    if(cba_opt_safety[i]>=0){
      lines(1:3,c(punif(cba_opt_Totcost[i],min=min(cba_opt_Totcost),max=max(cba_opt_Totcost)),
                  punif(fema_recom_Totcost[i],min=min(fema_recom_Totcost),max=max(fema_recom_Totcost)),
                  punif(cba_opt_safety[i],min=min(cba_opt_safety),max=max(cba_opt_safety))))
    }
  }
}

pdf("Figure/hypothetical_community_safetyObj_histogram_differences.pdf", width =3.94, height =2.43)
par(cex=0.5)
hist(cba_opt_height-fema_recom_height,col="orange",freq=FALSE,xlab="Optimal Height - FEMA recommendation",ylab="Density",main="")
abline(v=0,lwd=3,col="blue")
dev.off()
