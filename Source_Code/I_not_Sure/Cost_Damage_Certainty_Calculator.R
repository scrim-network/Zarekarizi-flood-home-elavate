# This script is written by Mahkameh Zarekarizi to calculate the expected damages to a hypothetical house 
# This house worth 350,000 USD
# This house is located 1 feet below the base flood elevation 
# We assume it is located right next to our USGS gage
# BFE here is 449.2 ft
# House level is 439.2 ft
# The house is 1500 square feet

# EAD is the expected annual damage
# EAD is the probability of a certain flood times its associated damages

# Global variables
# Struc_Value=350000 #USD
# del=-6
# House_Initial_Stage=(35.3)+del
# life_span=30
# disc_rate=0.04
# sqft=1500
# mu=19.85
# sigma=3.24
# xi=0.02
# disc_rate=0.04
# life_span=30
# delta_h=1
# House_Initial_Stage=35.3-1
# Struc_Value=350000
# 
# lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,
#                                    delta_h,life_span,disc_rate,
#                                    mu,sigma,xi)
# 
# 
# 


library(evir)

lifetime_safety <- function(life_span,mu,sigma,xi,House_Initial_Stage,delta_h){
  House_Current_Stage=House_Initial_Stage+delta_h
  safety=pgev(House_Current_Stage,xi=xi,sigma=sigma,mu=mu)**life_span
  return(safety)
}

lifetime_expected_damages_GEV_MCMC <- function(Struc_Value,House_Initial_Stage,
                                               delta_h,life_span,disc_rate,
                                               mu,sigma,xi){
  
  House_Current_Stage=House_Initial_Stage+delta_h
  
  # Establish the damage-depth relationship 
  EU_Depth <-           c(-100,-1,0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  RES_Damage_Factors <- c(0,0,0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
  damage_vals=RES_Damage_Factors*Struc_Value
  #plot(EU_Depth,damage_vals/1000,xlab="Depth of water in the structure (ft)",ylab="Damages [1,000 US$]")
  # 
  Critical_Depths=EU_Depth+House_Current_Stage
  Critical_Probs=1-pgev(q=Critical_Depths, xi=xi, mu=mu, sigma=sigma)
  EADfrac=rep(NA,length(Critical_Depths))
  for(i in 1:length(Critical_Depths)){
    if(i==length(Critical_Depths)){
      EADfrac[i]=Critical_Probs[i]*damage_vals[i]
    }else{
      EADfrac[i]=(Critical_Probs[i]-Critical_Probs[i+1])*damage_vals[i]
    }
  }
  EAD=sum(EADfrac)
  disc_fac <- rep(NA,floor(life_span))
  for (i in 1:floor(life_span)){
    disc_fac[i]=1/(1+disc_rate)^(i-1)
  }
  disc_sum=sum(disc_fac)
  expected_damages=EAD*disc_sum
  return(expected_damages)
}

lifetime_expected_damages <- function(Struc_Value,House_Initial_Stage,
                                      delta_h,life_span,disc_rate,
                                      FEMA_Return_levels,FEMA_Return_periods){
  House_Current_Stage=House_Initial_Stage+delta_h
  
  # Establish the damage-depth relationship 
  EU_Depth <-           c(-100,-1,0   , 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  RES_Damage_Factors <- c(  0,  0,0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
  damage_vals=RES_Damage_Factors*Struc_Value

  # Flood chances based on FEMA 
  FEMA_No_Exceed_Chance=1-(1/(FEMA_Return_periods))
  FEMA_Exceed_Chance=1/(FEMA_Return_periods)

  # Interpolate FEMA data
  flood_chance_seq=seq(min(FEMA_Exceed_Chance),max(FEMA_Exceed_Chance),by=0.001)
  flood_rp_seq=1/(flood_chance_seq)
  flood_level_seq=approx(FEMA_Exceed_Chance,FEMA_Return_levels,xout=flood_chance_seq,rule=2)$y

  InHouse_Water_Depth=flood_level_seq-House_Current_Stage
  InHouse_Damage=approx(EU_Depth,damage_vals,xout=InHouse_Water_Depth,rule=2)$y

  wid <- pol_area <- rep(NA,length(flood_chance_seq))
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
  
  return(rise_cost)
}
######################################################################################################
######################################################################################################


findopt <- function(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi,safety_level=0){

  BFE=House_Initial_Stage-del
  FEMA_Recomm_cost=Rising_Cost(sqft,abs(del)+1)
  FEMA_Recomm_damage=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,abs(del)+1,life_span,disc_rate,mu,sigma,xi)
  FEMA_safety=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,abs(del)+1)
    
  FEMA_Return_periods=c(2,5,10,25,50,100,500)
  FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
  # load(file="Bayesian_GEV_return_levels.RData")
  # GEVMCMC_Return_periods=1:10000
  # GEVMCMC_Return_levels=MC_rl_mean2

  construction_cost_seq <-expected_damages_seq <- expected_damages_GEVMCMC_seq <- delta_h_seq <- safety_seq <- rep(NA,100)
  delta_h_seq <- seq(0,14,length.out=100)

  for (i in 1:length(delta_h_seq)){
    construction_cost_seq[i]=Rising_Cost(sqft,delta_h_seq[i])
    expected_damages_seq[i]=lifetime_expected_damages(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,FEMA_Return_levels,FEMA_Return_periods)
    expected_damages_GEVMCMC_seq[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,mu,sigma,xi)
    safety_seq[i]=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,delta_h_seq[i])
  }

  total_cost=construction_cost_seq+expected_damages_seq
  total_cost_GEVMCMC=construction_cost_seq+expected_damages_GEVMCMC_seq
  total_cost_GEVMCMC[safety_seq<safety_level]=NA
  #print(total_cost_GEVMCMC)
  opt_height=delta_h_seq[which.min(total_cost_GEVMCMC)]
  #print(safety_seq)
  opt_height_total_cost=min(total_cost_GEVMCMC,na.rm = TRUE)
  opt_height_safety=safety_seq[which.min(total_cost_GEVMCMC)]
  total_cost_GEVMCMC=construction_cost_seq+expected_damages_GEVMCMC_seq # repeat this code so NA values disapear 
  #filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/V",toString(trunc(Struc_Value/1000)),"I",toString(del),"S",toString(life_span),"R",toString(disc_rate*100),".RData",sep="")
  #print(filename)
  #save(opt_height_safety,opt_height,opt_height_total_cost,safety_seq,FEMA_Recomm_cost,FEMA_Recomm_damage,delta_h_seq,total_cost,expected_damages_seq,construction_cost_seq,expected_damages_GEVMCMC_seq,total_cost_GEVMCMC,file=filename)
  myreturn=rbind(cbind(delta_h_seq[which.min(total_cost)],min(total_cost),0),
                 cbind(opt_height,opt_height_total_cost,opt_height_safety),
                 cbind(abs(del)+1,FEMA_Recomm_cost+FEMA_Recomm_damage,FEMA_safety))

  #plot(delta_h_seq,total_cost_GEVMCMC)
  #print(myreturn)
  return(myreturn)
}

#findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi)
  

