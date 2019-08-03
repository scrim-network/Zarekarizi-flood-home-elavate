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
library(evd) # We would use pgev, qgev from this package

source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')

save_mode=0
load_mode=0
save_plots=0

n_houses=1000
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
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

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

# load(file="Bayesian_GEV_return_levels.RData")
# GEVMCMC_Return_periods=1:10000
# GEVMCMC_Return_levels=MC_rl_mean2
# findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi)

if(save_mode==1){
fema_height   <- fema_totcost   <- fema_safety   <- fema_bc   <- fema_totcost_frac   <- fema_const_frac   <-
opt_height    <- opt_totcost    <- opt_safety    <- opt_bc    <- opt_totcost_frac    <- opt_const_frac    <-
optunc_height <- optunc_totcost <- optunc_safety <- optunc_bc <- optunc_totcost_frac <- optunc_const_frac <- rep(NA,n_houses)

for(i in 1:n_houses){
  print(i)
  tmp=findopt_UNC(SOWs[i,1],SOWs[i,2],SOWs[i,3],(SOWs[i,3]+BFE),SOWs[i,4],disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,10000,safety_level=0)

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
     file=paste("RData/Hypothetical_community_CBA_Unc_",Sys.Date(),".RData",sep=""))
}

load("RData/Hypothetical_community_CBA_Unc_2019-05-31.RData")

if (save_plots==1){
pdf("Figure/chain_of_hypothetical_houses_total_cost_comparison_with_FEMA.pdf", width =3.94, height =2.43)
par(cex=0.5)
}
plot(opt_totcost/1000,fema_totcost/1000,pch=20,col="blue",ylab="FEMA recommendation total cost [1,000 US$]",xlab="Optimal policy total cost [1,000 US$]",cex=0.8)
abline(0,1,col="gray",lwd=3)
legend("right","1:1 line",bty="n",col="gray",lty=1,lwd=3)
if(save_plots==1){
dev.off()
}

plot(opt_totcost/1000,optunc_totcost/1000,pch=20,col="red",ylab="With uncertainty optimal cost [1,000 US$]",xlab="Ignoring uncertainty optimal cost [1,000 US$]",cex=0.8)
abline(0,1,col="gray",lwd=3)


hist(optunc_height-opt_height,main="",
     xlab="optimal height under uncertainty - optimal height ignoring uncertainty",col="orange")

plot(optunc_height,opt_height,xlab="Optimal height under uncertainty",ylab="Optimal height ignoring uncertainty",pch=20,col="blue")
abline(0,1,col="darkgreen",lwd=2)

barplot(c(100*sum(optunc_height==opt_height)/length(opt_height),
          100*sum(optunc_height>opt_height)/length(opt_height),
          100*sum(optunc_height<opt_height)/length(opt_height)),col=c("orange","red","blue"))
axis(1,at=c(1,2,3),labels=c("UQ Does not impact the optimal height","UQ increases the optimal height","UQ decreases the optimal height"))
cbind(optunc_height[optunc_height<opt_height],opt_height[optunc_height<opt_height])
cbind(optunc_height,opt_height)

if(save_plots==1){
pdf("Figure/chain_of_hypothetical_houses_boxplot_differences.pdf", width =3.94, height =2.43)
par(cex=0.5)
}
boxplot((fema_totcost-opt_totcost)/1000,horizontal = TRUE,xlab="Extra expenses [1,000 US$]",col="orange")
if(save_plots==1){
dev.off()
}

100*sum(abs(fema_totcost-opt_totcost)<100)/length(opt_totcost)
#plot(cba_opt_height,fema_recom_height)

if(save_plots==1){
pdf("Figure/hypothetical_community_safety_histogram_differences.pdf", width =3.94, height =2.43)
par(cex=0.5)
}
hist(opt_height-fema_height,col="orange",freq=FALSE,xlab="Optimal Height - FEMA recommendation",ylab="Density",main="")
par(new=TRUE)
hist(optunc_height-fema_height,col=rgb(1,0,0,alpha=0.5),freq=FALSE,xlab="Optimal Height - FEMA recommendation",ylab="Density",main="")

abline(v=0,lwd=3,col="blue")
if(save_plots==1){
dev.off()
}


plot(fema_bc,optunc_bc,xlim=c(0,40),ylim=c(0,40))
abline(0,1)
abline(v=1,h=1)
plot(fema_bc)
abline(h=1)

plot(opt_bc)
abline(h=1)

plot(optunc_bc)
abline(h=1)

plot(fema_const_frac,optunc_const_frac,xlim=c(0,1),ylim=c(0,1))
abline(0,1)

plot(fema_totcost_frac,optunc_totcost_frac,xlim=c(0,1),ylim=c(0,1))
abline(0,1)


########################################################################
########################################################################
########################################################################
pdf("Figure/fema_height_vs_optunc_height.pdf", width =3.94, height =3.94)
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
pdf("Figure/opt_height_vs_optunc_height.pdf", width =3.94, height =3.94)
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

########################################################################
########################################################################
########################################################################
par(mfrow=c(2,2))
indexes=which(optunc_height>fema_height)
plot(fema_bc[indexes],optunc_bc[indexes])
abline(0,1)
plot(fema_totcost_frac[indexes],optunc_totcost_frac[indexes])
abline(0,1)
plot(fema_const_frac[indexes],optunc_const_frac[indexes])
abline(0,1)
plot(fema_safety[indexes],optunc_safety[indexes])
abline(0,1)

par(mfrow=c(2,2))
indexes=which(optunc_height<fema_height & optunc_height>0)
plot(fema_bc[indexes],optunc_bc[indexes])
abline(0,1)
plot(fema_totcost_frac[indexes],optunc_totcost_frac[indexes])
abline(0,1)
plot(fema_const_frac[indexes],optunc_const_frac[indexes])
abline(0,1)
plot(fema_safety[indexes],optunc_safety[indexes])
abline(0,1)

plot(fema_const_frac,optunc_const_frac,ylim=c(0,30))
abline(0,1)

plot(fema_safety, optunc_safety,xlim=c(0,1),ylim=c(0,1))
abline(0,1)


plot(fema_bc);abline(h=1)
plot(fema_safety)
plot(fema_const_frac)
plot(fema_totcost_frac)
plot(fema_safety,fema_const_frac)


