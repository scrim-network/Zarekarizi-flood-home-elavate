############################################################
############################################################
# Requirements
############################################################
rm(list=ls())
set.seed(1)
library(evd) # We would use pgev, qgev from this package
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")
source("../Figures/mycolors.R")

################################################################################
################################################################################
################################################################################
# This function calculates the expected damages for the lifetime of the house 
# under certainty. It needs three expected values for GEV parameters.
# Expected shape, location, and scale parameters could come from MCMC chains or just MLE estimates
expected_damages <- function(Struc_Value,House_Initial_Stage,
                             delta_h,life_span,disc_rate,
                             mu,sigma,xi){
  # Arguments:
  # Struc_Value: House value; the price of the house
  # House_Initial_Stage: Elevation of the house with respect to gage datum (stage) before being raised.
  # delta_h: How much is the house elevated from the initial stage above? This is the amount of heightening; could be zero
  # life_span: The expected lifespan of the house; usually 30 years 
  # disc_rate: The expected discount rate; usually 0.04 (4%)
  # mu: The expected location parameter of the GEV function.
  # sigma: The expected scale parameter of the GEV function.
  # xi: The expected shape parameter of the GEV function.
  
  
  House_Current_Stage=House_Initial_Stage+delta_h # The stage of the house after being elevaed. reminder: Stage the elevation of the house with respect to the gage datum in feet 
  
  # Establish the damage-depth relationship 
  EU_Depth <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  RES_Damage_Factors <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
  
  # how much is lost (in USD) at each depth. This depends on the value of the house 
  damage_vals=RES_Damage_Factors*Struc_Value
  
  # critical depths are depths where the damage factor changes. 
  Critical_Depths=EU_Depth+House_Current_Stage # Calculates the stage of critical depths
  #print("Critical_Depths")
  #print(Critical_Depths)
  # What is the probability that water level exceeds each critical depth?
  Critical_Probs=1-pgev(q=Critical_Depths, shape=xi, loc=mu, scale=sigma)
  
  # The following block is just to avoild NaNs and NAs
  if(sum(is.nan(Critical_Probs))>0){
    test_x=rgev(10^6, shape=xi, loc=mu, scale=sigma)
    Critical_Probs[Critical_Depths<min(test_x)]=0
    Critical_Probs[Critical_Depths>max(test_x)]=1
  }
  if(sum(is.nan(Critical_Probs))>0){
    stop("I dont know what to do....")
  }
  #print(Critical_Probs)
  
  # Calculate expected annual damages (EAD)
  EADfrac=rep(NA,length(Critical_Depths))
  for(i in 1:length(Critical_Depths)){
    if(i==length(Critical_Depths)){
      EADfrac[i]=Critical_Probs[i]*damage_vals[i]
    }else{
      EADfrac[i]=(Critical_Probs[i]-Critical_Probs[i+1])*damage_vals[i]
    }
  }
  EAD=sum(EADfrac)
  
  # Expected damages is the summation of EAD over lifespan times discount factor for that year.
  disc_fac <- rep(NA,floor(life_span))
  for (i in 1:floor(life_span)){
    disc_fac[i]=1/(1+disc_rate)^(i-1)
  }
  disc_sum=sum(disc_fac)
  expected_damages=EAD*disc_sum
  return(c(expected_damages,EAD))
}


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
nsow=100
z<- randomLHS(nsow, 1)
SOWs=matrix(NA,nsow,1)
SOWs[,1] <- floor(qunif(z[,1],1,length(mu_chain))) 
dxt<-matrix(NA,nsow,nsow)


require(lhs)
pdf("Figure/value_uncertainty_v3.pdf", width =1*3.94, height =4*2.43)




par(cex=0.5,fig=c(0,1,.75,1))
#hs<-hist(temp/1000,plot = T,xlab="Lifetime Expected Damages [1,000 USD]",ylab='Density',main="",col="gray",border = NA,breaks = 20)
#plot(hs$mids,hs$density,col="black",type="l",xlab="Lifetime Expected Damages [1,000 USD]",ylab='Density')
#abline(v=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu,sigma,xi)[1]/1000)
#abline(v=mean(temp)/1000,col="red")
#legend('topright',c('Considering uncertainty','Neglecting uncertainty'),col=c('red','black'),lty=c(1,1))

plot(NA,NA,xlim=c(10,60),ylim=c(0,.15),xlab="Annual Maximum Flood Level [ft]",ylab="Probability density")
#mtext('',side=3,cex=0.5,line=1)
xt<-seq(10,60,length.out = nsow)
for(j in 1:nsow){
  dxt[j,]=dgev(xt,loc=mu_chain[SOWs[j,1]],scale=sigma_chain[SOWs[j,1]],shape=xi_chain[SOWs[j,1]])
  lines(xt,dxt[j,],col="lightgray")
}
#x=sort(rgev(nsow,shape=xi,scale=sigma,loc=mu))
dx=dgev(xt,shape=xi,scale=sigma,loc=mu)
lines(xt,dx,lty=1,lwd=1)
lines(xt,colMeans(dxt),lty=1,lwd=1,col="red")
legend('topright',c('Expected value considering uncertainty','Maximum likelihood estimate \nneglecting uncertainty','Initial house level'),
       col=c('red','black','darkgreen'),lty=c(1,1,1),bty="n",lwd=c(1,1,2))

abline(v=House_Initial_Stage,col="darkgreen",lwd=2)



ss=0.04
#par(mai=c(.4,.4,.4,.4))
par(cex=0.5,fig=c(0,1,.5,.75),new=T)
plot(NA,NA,xlim=c(House_Initial_Stage,60),ylim=c(0,.15),xlab="Annual Maximum Flood Level [ft]",ylab="Probability that water level exceeds the house lowest level")
#mtext('',side=3,cex=0.5,line=1)
xt<-seq(House_Initial_Stage,60,length.out = nsow)
for(j in 1:nsow){
  dxt[j,]=1-pgev(xt,loc=mu_chain[SOWs[j,1]],scale=sigma_chain[SOWs[j,1]],shape=xi_chain[SOWs[j,1]])
  lines(xt,dxt[j,],col="lightgray")
}
#x=sort(rgev(nsow,shape=xi,scale=sigma,loc=mu))
dx=1-pgev(xt,shape=xi,scale=sigma,loc=mu)
lines(xt,dx,lty=1,lwd=1)


lines(xt,colMeans(dxt),lty=1,lwd=1,col="red")

#abline(v=BFE,col="blue")
abline(v=House_Initial_Stage,col="darkgreen",lwd=2)
legend('topright',c('Expected value considering uncertainty','Maximum likelihood estimate neglecting uncertainty','Samples from the joint parameter distribution','Initial house level'),
       lty=c(1,1,1,1),lwd=c(1,1,1,2),col=c('red','black','lightgray','darkgreen'),bty="n")

par(fig=c(0,1,.25,.5),new=T)
# Establish the damage-depth relationship 
EU_Depth <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
depth=EU_Depth+House_Initial_Stage
RES_Damage_Factors <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
Damages=RES_Damage_Factors*350
#plot(xt,approx(depth,Damages,xt)$y,type="l",ylab="Damahes [1,000 US$]",xlab="Depth [ft]")
plot(c(depth,60),100*c(Damages,max(Damages))/(Struc_Value/1000),type="l",ylab="Damages [% of house value]",xlab="Water depth [ft]",xlim=c(House_Initial_Stage,60))

#abline(v=depth,col="lightgray")
#abline(v=BFE,col="blue")
abline(v=House_Initial_Stage,col="darkgreen",lwd=2)
legend('right',c('Initial house level'),col='darkgreen',lwd=2,bty="n")
#plot(xt,dx*approx(depth,Damages,xt)$y,type="l")
#for(i in 1:nsow){
#  lines(xt,dxt[j,]*approx(depth,Damages,xt)$y,col="lightgray")
#}
temp<-temp2<-rep(NA,nsow)
for(j in 1:nsow){
  temp[j]=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]])[1]
  temp2[j]=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]])[2]
}
par(fig=c(0,1,0,.25),new=T)
hs<-hist(temp2/1000,breaks = 20,plot = F)#,xlab="Annual Expected Damages (EAD) [1,000 US$]",ylab='Density',main="",col="white",border = NA)
#plot(hs$mids,hs$density,col="black",type="l",xlab="Annual Expected Damages [1,000 US$]",ylab='Density')
s <- ksmooth(hs$mids,hs$density, kernel = "normal",bandwidth =2)
plot(s,lwd=1,type="l",xlab="Annual Expected Damages (EAD) [1,000 US$]",ylab='Probability density',main="")
abline(v=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu,sigma,xi)[2]/1000)
abline(v=mean(temp2)/1000,col="red")
legend('topright',c('Mode','Expected value considering uncertainty','Maximum likelihood estimate \nneglecting uncertainty'),col=c('gray','red','black'),lty=c(1,1,1),bty="n")
abline(v=hs$mid[which.max(hs$counts)],col="gray")




s=.37
par(fig=c(0,1,0,1),cex=0.5,new=T)
plot(NA,NA,type="n",xlim=c(0,1),ylim=c(0,1),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
#polygon(x=c(0.4,0.6,0.6,0.4),y=c(0.4,0.4,0.45,0.45)+0.25-s,border=NA,col="gray")
#arrows(x0=0.455,y0=0.79-s,x1=0.49,y1=.7-s,length=0.05,col="blue",lwd=2)
#arrows(x0=0.5+0.045,y0=0.79-s,x1=0.51,y1=.7-s,length=0.05,col="blue",lwd=2)
#text(0.5,0.425+0.25-s,'Multiply',col="blue",cex=2)
#arrows(x0=0.5,y0=0.65-s,x1=0.5,y1=.585-s,length=0.05,col="blue",lwd=2)
#arrows(x0=0.4,y0=0.77,x1=0.4,y1=.67,length=0.05,col="blue",lwd=2)

#arrows(x0=0.5,y0=0.25,x1=0.5,y1=.15,length=0.05,col="blue",lwd=2)
text(-0.03,1.06,'a)',bty="n",xpd=T,cex=2)
text(-0.03,.76,'b)',bty="n",xpd=T,cex=2)
text(-0.03,.465,'c)',bty="n",xpd=T,cex=2)
text(-0.03,0.165,'d)',bty="n",xpd=T,cex=2)

dev.off()


