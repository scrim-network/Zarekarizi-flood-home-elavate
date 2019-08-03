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
del=-6
House_Initial_Stage=(35.3)+del
life_span=30
disc_rate=0.04
FEMA_Return_periods=c(2,5,10,25,50,100,500)
FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
delta_h=0
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_MCMC_Mean_Params.RData")
mu=Mean_mu
xi=Mean_xi
sigma=Mean_sigma 

source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Certainty_Calculator.R')

pdf("Figure/variation_of_expected_damages.pdf", width =3.94, height =2*2.43)
par(mfrow=c(4,2),cex=0.3)
#par()

struc_value_seq=seq(50000,1000000,length.out = 1000)
LED <- rep(NA,length(struc_value_seq))
for(i in 1:length(struc_value_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(struc_value_seq[i],House_Initial_Stage,delta_h,life_span,disc_rate,mu,sigma,xi)
}
plot(struc_value_seq/1000,LED/1000,type="l",col="blue",lwd=2,xlab="House Value [1,000 US$]",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))


init_stage_seq=seq(20,50,length.out = 1000)
LED <- rep(NA,length(init_stage_seq))
for(i in 1:length(init_stage_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,init_stage_seq[i],delta_h,life_span,disc_rate,mu,sigma,xi)
}
plot(init_stage_seq,LED/1000,type="l",col="blue",lwd=2,xlab="Initial House Stage [ft]",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))


delta_h_seq=seq(1,14,length.out = 1000)
LED <- rep(NA,length(delta_h_seq))
for(i in 1:length(delta_h_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,mu,sigma,xi)
}
plot(delta_h_seq,LED/1000,type="l",col="blue",lwd=2,xlab="Added Height [ft]",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))


lifespan_seq=seq(5,100,length.out = 1000)
LED <- rep(NA,length(lifespan_seq))
for(i in 1:length(lifespan_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h,lifespan_seq[i],disc_rate,mu,sigma,xi)
}
plot(lifespan_seq,LED/1000,type="l",col="blue",lwd=2,xlab="House Lifespan [years]",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))


dis_rate_seq=seq(0.01,0.9,length.out = 1000)
LED <- rep(NA,length(dis_rate_seq))
for(i in 1:length(dis_rate_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h,life_span,dis_rate_seq[i],mu,sigma,xi)
}
plot(dis_rate_seq,LED/1000,type="l",col="blue",lwd=2,xlab="Discount Rate [%]",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))


mu_seq=seq(10,30,length.out = 1000)
LED <- rep(NA,length(mu_seq))
for(i in 1:length(mu_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h,life_span,disc_rate,mu_seq[i],sigma,xi)
}
plot(mu_seq,LED/1000,type="l",col="blue",lwd=2,xlab="GEV Location parameter",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))

sigma_seq=seq(2,5,length.out = 1000)
LED <- rep(NA,length(sigma_seq))
for(i in 1:length(sigma_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h,life_span,disc_rate,mu,sigma_seq[i],xi)
}
plot(sigma_seq,LED/1000,type="l",col="blue",lwd=2,xlab="GEV Scale parameter",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))


xi_seq=seq(0.000001,0.04,length.out = 1000)
LED <- rep(NA,length(xi_seq))
for(i in 1:length(xi_seq)){
  LED[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h,life_span,disc_rate,mu,sigma,xi_seq[i])
}
plot(xi_seq,LED/1000,type="l",col="blue",lwd=2,xlab="GEV Shape parameter",ylab="Lifetime Expected Damages [1,000 US$]",ylim=c(0,500))




dev.off()










