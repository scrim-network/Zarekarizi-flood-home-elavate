# This script is written by Mahkameh Zarekarizi to calculate the expected damages to a hypothetical house 
# This house worth 350,000 USD
# This house is located 1 feet below the base flood elevation 
# We assume it is located right next to our USGS gage
# BFE here is 449.2 ft
# House level is 439.2 ft
# The house is 1500 square feet

# EAD is the expected annual damage
# EAD is the probability of a certain flood times its associated damages
# rm(list=ls())
# 
# # Global variables
# Struc_Value=350000 #USD
# sqft=1500
# del=-6
# House_Initial_Stage=(35.3)+del
# life_span=30
# disc_rate=0.04
# FEMA_Return_periods=c(2,5,10,25,50,100,500)
# FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
# load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")
# source("../Figures/mycolors.R")
# n=100

findopt_UNC <- function(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,mu_chain,sigma_chain,xi_chain,n){
  
# Functions ##########################################################################################
######################################################################################################
######################################################################################################

lifetime_expected_damages_GEV_MCMC <- function(Struc_Value,House_Initial_Stage,
                                               delta_h,life_span,disc_rate,
                                               mu,sigma,xi){
  
  House_Current_Stage=House_Initial_Stage+delta_h
  
  # Establish the damage-depth relationship 
  EU_Depth <-           c(-100,-1,0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  RES_Damage_Factors <- c(0,0,0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
  damage_vals=RES_Damage_Factors*Struc_Value
  #plot(EU_Depth,damage_vals/1000,xlab="Depth of water in the structure (ft)",ylab="Damages [1,000 US$]")
  
  # Flood chances based on FEMA 
  Return_periods=1:1000
  No_Exceed_Chance=1-(1/(Return_periods))
  Exceed_Chance=1/(Return_periods)
  Return_levels=qgev(p=No_Exceed_Chance,mu=mu,sigma=sigma,xi=xi)
  
  #plot(FEMA_Return_levels,FEMA_No_Exceed_Chance)
  #plot(FEMA_Exceed_Chance,FEMA_Return_levels,xlab="Exceedence Probability",ylab="Return Level")
  
  # Interpolate FEMA data
  flood_chance_seq=seq(min(Exceed_Chance),max(Exceed_Chance),by=0.001)
  flood_rp_seq=1/(flood_chance_seq)
  flood_level_seq=approx(Exceed_Chance,Return_levels,xout=flood_chance_seq)$y
  #plot(flood_rp_seq,flood_level_seq,log="x",xlab="Seq of return periods",ylab="seq of return levels")
  
  InHouse_Water_Depth=flood_level_seq-House_Current_Stage
  #InHouse_Water_Depth[InHouse_Water_Depth<0]=0
  InHouse_Damage=approx(EU_Depth,damage_vals,xout=InHouse_Water_Depth)$y
  #plot(InHouse_Water_Depth,InHouse_Damage,xlab="Seq Depths",ylab="Seq Damages")
  
  wid <- pol_area <- rep(NA,length(flood_chance_seq))
  #plot(flood_chance_seq,InHouse_Damage,xlab="Flooding chance",ylab="Damage")
  for(i in 2:length(flood_chance_seq)){
    wid[i]=flood_chance_seq[i]-flood_chance_seq[i-1]
    pol_area[i]=0.5*wid[i]*(InHouse_Damage[i]+InHouse_Damage[i+1])
  }
  
  EAD=sum(pol_area,na.rm=TRUE)
  
  disc_fac <- rep(NA,life_span)
  for (i in 0:(life_span-1)){
    disc_fac[i+1]=1/(1+disc_rate)^i
  }
  #plot(disc_fac)
  disc_sum=sum(disc_fac)
  expected_damages=EAD*disc_sum
  #print(c(EAD,expected_damages))
  return(expected_damages)
}



Rising_Cost <- function(sqft,delta_h){
  # Calculate cost of elevating according to CLARA 
  # 82.5/sqft (3 to 7)
  # 86.25/sqft (7 to 10)
  # 103.75/sqft (10 to 14)
  Base_cost= 10000 + 300 + 470 + 4300 + 2175 + 3500
  #Base_cost= 0
  
  if(delta_h>=3 && delta_h<7){
    rate=82.5
  }else if(delta_h>=7 && delta_h<10){
    rate=86.25
  }else if(delta_h>=10 && delta_h<=14){
    rate=103.75
  }else if(delta_h<3){
    rate=80 # i made that up for now
  }else{
    rate=NA
    print('Sorry, your Delta_H is not in the acceptable range')
  }
  rise_cost=Base_cost+rate*sqft
  if(delta_h==0){rise_cost=0}
  
  #x=0:14
  #y=c(0.0,5717.5,10820.0  ,16845.0 , 23865.0  ,31960.0  ,41292.5  ,51662.5,
  #  63422.5 , 76557.5 , 91140.0 ,107245.0 ,124952.5, 144330.0, 163707.5)
  #rise_cost=approx(x,y,xout=delta_h)$y
  return(rise_cost)
}
######################################################################################################
######################################################################################################
######################################################################################################



construction_cost_seq <-expected_damages_seq <- rep(NA,15)
expected_damages_seq_max <- expected_damages_seq_min<- mean_expected_damage <- rep(NA,15)
total_cost_seq_max <- total_cost_seq_min <- mean_total_cost <- rep(NA,15)
temp <- temp2 <- matrix(NA,15,n)

for (i in 0:14){
  print(i)
  construction_cost_seq[i+1]=Rising_Cost(sqft,i)
  #expected_damages_seq[i+1]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,i,30,0.04,FEMA_Return_levels,FEMA_Return_periods)
  expected_damages_seq[i+1]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,i,30,0.04,mean(mu_chain),mean(sigma_chain),mean(xi_chain))
  
  require(lhs)
  z<- randomLHS(n, 3)
  SOWs=matrix(NA,n,3)
  SOWs[,1] <- qunif(z[,1],10,200)
  SOWs[,2] <- qunif(z[,2],0.01, 0.09) 
  SOWs[,3] <- qunif(z[,3],1,length(mu_chain)) 
  
  for(j in 1:n){
    #temp[i+1,j]=lifetime_expected_damages(Struc_Value,House_Initial_Stage,i,SOWs[j,1],SOWs[j,2],FEMA_Return_levels,FEMA_Return_periods)
    temp[i+1,j]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,i,SOWs[j,1],SOWs[j,2],mu_chain[SOWs[j,3]],sigma_chain[SOWs[j,3]],xi_chain[SOWs[j,3]])
    
    temp2[i+1,j]=temp[i+1,j]+construction_cost_seq[i+1]
  }
  expected_damages_seq_max[i+1]=quantile(temp[i+1,],0.95)
  expected_damages_seq_min[i+1]=quantile(temp[i+1,],0.05)
  mean_expected_damage[i+1]=mean(temp[i+1,])
  
  total_cost_seq_max[i+1]=quantile(temp2[i+1,],0.95)
  total_cost_seq_min[i+1]=quantile(temp2[i+1,],0.05)
  mean_total_cost[i+1]=mean(temp2[i+1,])
  
}
opt <- delta_h_opt <- rep(NA,n)
for(j in 1:n){
  delta_h_opt[j]=which.min(temp2[,j])
  opt[j]=min(temp2[,j])
}

# cost benefit analysis
total_cost=construction_cost_seq+expected_damages_seq

filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/UNC-SR_V",toString(trunc(Struc_Value/1000)),"Sq",toString(trunc(sqft)),"I",toString(del),".RData",sep="")
print(paste("Saving to ...",filename))
save(mean_expected_damage,mean_total_cost,total_cost,expected_damages_seq,construction_cost_seq,total_cost_seq_min,total_cost_seq_max,opt,
     expected_damages_seq_max,expected_damages_seq_min,delta_h_opt,SOWs,file=filename)
myreturn=cbind(delta_h_opt,opt)
print(myreturn)
return(myreturn)
}
#x=findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,mu_chain,sigma_chain,xi_chain)
  
