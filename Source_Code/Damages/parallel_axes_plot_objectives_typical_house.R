
############################################################
############################################################
# Requirements
############################################################
rm(list=ls())
library(evd) # We would use pgev, qgev from this package
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")
source("../Figures/mycolors.R")

############################################################
############################################################
# Global variables
############################################################
save_figures=1
plot_cb=1
plot_safety=1
run_function=0


############################################################
############################################################
# Start the main program
############################################################
# Calculate GEV parameters (choosing the mode; the most probable prediction)
mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

# Given the above parameters, calculate the base flood elevation
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

sqft=1500
Struc_Value=350000 #USD
del=-5
life_span=30
disc_rate=0.04
# House charachteristics
House_Initial_Stage=BFE+del

# If you already have the results (optimal elevation, cost-benefit analysis, etc. for this specific house, set run_function=0)
if(run_function==1){ # Run the findopt_UNC function stored in Cost_Damage_Calculator file
  returned_data=findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,
                            FEMA_Return_levels,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,10000,safety_level=0,save_return=1)
  print(returned_data)
}

# The file has been saved with this name convention
filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/UNC-SR_V",toString(trunc(Struc_Value/1000)),"Sq",toString(trunc(sqft)),"I",toString(del),".RData",sep="")
load(filename)
# Calculate the benefit-to-cost ratio for ignoring uncertainty case. This is not automatically calculated in the findopt_UNC function.
cb_cost=construction_cost_seq
cb_benefit=(expected_damages_GEVMCMC_seq[1]-expected_damages_GEVMCMC_seq)
cb=cb_benefit/cb_cost
cb_unc_mean[1]=0
########################################################################
indexs=(1:length(delta_h_seq))
if(save_figures==1){
  pdf("Figure/parallel_plots/objective_tradeoffs_typical_parallel.pdf", width =3.94, height =2.43)
  par(cex=0.5,mai=c(0.4,0.2,0.1,0.1))
  par(cex=0.45,fig=c(0,0.9,0,1))

}
# objectives are bc-upfront investiment-expected damages 
naxes=4
plot(NA,NA,type="n",xlim=c(1,naxes),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
#title('Parallel plot of objective trade-offs for a typical house in Selinsgrove, PA')
for (i in 1:length(indexs)){
  lines(1:naxes,c(
                  punif(cb_unc_mean[i],min=min(cb_unc_mean,na.rm = T),max=max(cb_unc_mean,na.rm = T)),
                  punif(construction_cost_seq[i]/Struc_Value,min=min(construction_cost_seq/Struc_Value),max=max(construction_cost_seq/Struc_Value)),
                  punif(damages_unc_mean[i],min=min(damages_unc_mean),max=max(damages_unc_mean)),
                  punif(safety_unc_mean[i],min=min(safety_unc_mean),max=max(safety_unc_mean))
  ),
  col=rgb(0,0.5,punif(delta_h_seq[i],min=min(delta_h_seq),max=max(delta_h_seq))),
  lwd=.5)
}
abline(v=c(1:naxes),col="black",lwd=2)

lines(x=c(1,naxes),y=c(-.01,-0.01),lwd=2)
lines(x=c(1,naxes),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

#text(1-adj_x,-adj_y_num,signif(min(delta_h_seq),2),xpd=TRUE)
#text(1-adj_x,1+adj_y_num,signif(max(delta_h_seq),2),xpd=TRUE)
#text(1,adj_y,paste(expression("Added elevation\n[ft]")),xpd=TRUE)

text(1-adj_x,-adj_y_num,signif(min(cb_unc_mean,na.rm=T),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(cb_unc_mean,na.rm=T),2),xpd=TRUE)
text(1,adj_y,paste(expression("Benefit-to-cost")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(construction_cost_seq/Struc_Value),2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(construction_cost_seq/Struc_Value),2),xpd=TRUE)
text(2,adj_y,paste(expression("Ratio of upfront construction\n cost to house value")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(damages_unc_mean)/1000,2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(damages_unc_mean)/1000,2),xpd=TRUE)
text(3,adj_y,paste(expression("Expected damages\n[1,000 USD]")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(safety_unc_mean)*100,2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(safety_unc_mean)*100,2),xpd=TRUE)
text(4,adj_y,paste(expression("Probability of \nno floods in\n 30 years [%]")),xpd=TRUE)
#text(4.6,0.5,"Added height [ft]",srt=90,xpd=T)

par(fig=c(0.5,1,0,1),new=T)
library(fields)
plot(NA,NA,type="n",xlim=c(1,naxes),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
image.plot(legend.only=TRUE, zlim=c(0,14), 
           col =rgb(0,0.5,punif(delta_h_seq,min=min(delta_h_seq),max=max(delta_h_seq))), 
            horizontal = F,legend.cex=0.6,cex.axis=0.6,legend.lab="Added height [ft]",
           legend.shrink=0.75) 
if(save_figures==1){
  
  dev.off()
}
