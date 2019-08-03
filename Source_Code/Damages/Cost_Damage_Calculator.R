# This script is written by Mahkameh Zarekarizi with 3 purposes:
#   1. Calculating the costs of elevating a house
#   2. Calculating the expected damages under certainty and uncertainty
#   3. Finding the optimal heightening level under certatinty and uncertainty

# Global requirements for all fnctions:
library(evd) # We would use pgev, qgev from this package

################################################################################
################################################################################
################################################################################
# This function calculates the safety of a house. Safety is defined as the 
# Probability of not being flooded at all during the lifetime of the house
lifetime_safety <- function(life_span,mu,sigma,xi,House_Initial_Stage,delta_h){
  # Arguments:
  # life_span: the expected lifespan of the house; usually 30 years
  # mu: the location parameter of the GEV distribution 
  # sigma: the scale parameter of the GEV distribution 
  # xi: the shape parameter of teh GEV distribution 
  # House_Initial_Stage: The initial stage (in feet) of the house before being raised
  # delta_h: If the house is raised, this arguments indicates how much it is raised. Otherwise it is 0.
  
  # The current stage of the house after being raised (if it is raised)
  House_Current_Stage=House_Initial_Stage+delta_h
  
  # Safety is probability of zero floods during the next n years where n is the expected lifetime of the house
  safety=pgev(House_Current_Stage,shape=xi,scale=sigma,loc=mu)**floor(life_span)
#  if(sum(is.nan(safety) || is.na(safety))>0){
#    test_x=rgev(10^6, xi=xi, mu=mu, sigma=sigma)
#      if(House_Current_Stage<min(test_x)){
#      safety=0
#      }else if(House_Current_Stage>max(test_x)){
#      safety=1
#      }}
  #print(safety)
  if(sum(is.nan(safety) || is.na(safety))>0){
    stop("I dont know what to do....")
  }
  
  return(safety)
}

################################################################################
################################################################################
################################################################################
# This function calculates the expected damages for the lifetime of the house 
# under certainty. It needs three expected values for GEV parameters.
# Expected shape, location, and scale parameters could come from MCMC chains or just MLE estimates
lifetime_expected_damages_GEV_MCMC <- function(Struc_Value,House_Initial_Stage,
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
  return(expected_damages)
}

################################################################################
################################################################################
################################################################################
# lifetime_expected_damages <- function(Struc_Value,House_Initial_Stage,
#                                       delta_h,life_span,disc_rate,
#                                       FEMA_Return_levels,FEMA_Return_periods){
#   House_Current_Stage=House_Initial_Stage+delta_h
#   
#   # Establish the damage-depth relationship 
#   EU_Depth <-           c(0   , 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
#   RES_Damage_Factors <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
#   damage_vals=RES_Damage_Factors*Struc_Value
#   
#   # Flood chances based on FEMA 
#   FEMA_No_Exceed_Chance=1-(1/(FEMA_Return_periods))
#   FEMA_Exceed_Chance=1/(FEMA_Return_periods)
#   
#   # Interpolate FEMA data
#   flood_chance_seq=seq(min(FEMA_Exceed_Chance),max(FEMA_Exceed_Chance),by=0.001)
#   flood_rp_seq=1/(flood_chance_seq)
#   flood_level_seq=approx(FEMA_Exceed_Chance,FEMA_Return_levels,xout=flood_chance_seq,rule=2)$y
#   
#   InHouse_Water_Depth=flood_level_seq-House_Current_Stage
#   InHouse_Damage=approx(EU_Depth,damage_vals,xout=InHouse_Water_Depth,rule=2)$y
#   
#   wid <- pol_area <- rep(NA,length(flood_chance_seq))
#   for(i in 2:length(flood_chance_seq)){
#     wid[i]=flood_chance_seq[i]-flood_chance_seq[i-1]
#     pol_area[i]=0.5*wid[i]*(InHouse_Damage[i]+InHouse_Damage[i+1])
#   }
#   
#   EAD=sum(pol_area,na.rm=TRUE)
#   
#   disc_fac <- rep(NA,life_span)
#   for (i in 0:(life_span-1)){
#     disc_fac[i+1]=1/(1+disc_rate)^i
#   }
#   #plot(disc_fac)
#   disc_sum=sum(disc_fac)
#   expected_damages=EAD*disc_sum
#   #print(c(EAD,expected_damages))
#   return(expected_damages)
# }
################################################################################
################################################################################
################################################################################
# This function calculates the cost of elevating the house using CLARA model 
# For the source, see appendix A of CLARA model at this webpage:
# http://coastal.la.gov/our-plan/2017-coastal-master-plan/ 
Rising_Cost_discrete <- function(sqft,delta_h){
  # Arguments:
  # sqft: size of the house in square feet 
  # delta_h: amount of heightening in feet 
  
  # Cost of elevating according to CLARA are as the following:
  # 82.5/sqft (3 to 7)
  # 86.25/sqft (7 to 10)
  # 103.75/sqft (10 to 14)
  
  # There is a base cost for elevating any house as the following. For more information, see appendix A of CLARA model through the link above 
  Base_cost= 10000 + 300 + 470 + 4300 + 2175 + 3500
  
  # The cost of elevating the house after the base cost depends on the size of the house
  if(delta_h>=3 && delta_h<7){
    rate=82.5
  }else if(delta_h>=7 && delta_h<10){
    rate=86.25
  }else if(delta_h>=10 && delta_h<=14){
    rate=103.75
  }else{     
    rate=NA
    if(delta_h!=0){
    print('Sorry, your Delta_H is not in the acceptable range')}
  }
  
  # total cost of elevating the house:
  rise_cost=Base_cost+rate*sqft
  
  # There is no cost for not elevating the house
  if(delta_h==0){rise_cost=0}
  
  return(rise_cost)
}



################################################################################
################################################################################
################################################################################
# This function calculates the cost of elevating the house using CLARA model 
# For the source, see appendix A of CLARA model at this webpage:
# http://coastal.la.gov/our-plan/2017-coastal-master-plan/ 
#Rising_Cost_interpolated <- function(sqft,delta_h){
  Rising_Cost<- function(sqft,delta_h){
    
  # Arguments:
  # sqft: size of the house in square feet 
  # delta_h: amount of heightening in feet 
  
  # Cost of elevating according to CLARA are as the following:
  # 82.5/sqft (3 to 7)
  # 86.25/sqft (7 to 10)
  # 103.75/sqft (10 to 14)
  
  # There is a base cost for elevating any house as the following. For more information, see appendix A of CLARA model through the link above 
  Base_cost= 10000 + 300 + 470 + 4300 + 2175 + 3500
  Hs=c(3,5,8.5,12,14)
  Rates=c(80.36,82.5,86.25,103.75,113.75)
  #print(delta_h)
  # The cost of elevating the house after the base cost depends on the size of the house
  if(delta_h>=3 && delta_h<=14){
    rate=approx(Hs,Rates,delta_h)$y
  }else{     
    rate=NA
    if(delta_h!=0){
      #print('Sorry, your Delta_H is not in the acceptable range')
      #print(delta_h)
      }
  }
  
  # total cost of elevating the house:
  rise_cost=Base_cost+rate*sqft
  
  # There is no cost for not elevating the house
  if(delta_h==0){rise_cost=0}
  
  return(rise_cost)
}

#(103.75-approx(Hs,Rates,10)$y)+103.75=113.75
######################################################################################################
######################################################################################################
######################################################################################################
findopt_UNC <- function(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,
                        FEMA_Return_levels,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,nsow,safety_level=0,
                        save_return=0){
  
  BFE=House_Initial_Stage-del
  FEMA_Recomm_cost=Rising_Cost(sqft,abs(del)+1)
  FEMA_Recomm_damage=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,abs(del)+1,life_span,disc_rate,mu,sigma,xi)
  FEMA_safety=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,abs(del)+1)
  
  FEMA_Return_periods=c(2,5,10,25,50,100,500)
  FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
  # load(file="Bayesian_GEV_return_levels.RData")
  # GEVMCMC_Return_periods=1:10000
  # GEVMCMC_Return_levels=MC_rl_mean2
  #delta_h_seq <- c(0,seq(3,7,length.out=10)[1:9],seq(7,10,length.out=10)[1:9],seq(10,14,length.out=10))
  delta_h_seq <- c(0,seq(3,14,length.out=100))
  
  height_incr=length(delta_h_seq)
  
  construction_cost_seq <-expected_damages_seq <- expected_damages_GEVMCMC_seq <- safety_seq <- rep(NA,height_incr)
  damages_unc_max <- damages_unc_min <- damages_unc_mean <- 
  totcost_unc_max <- totcost_unc_min <- totcost_unc_mean <- 
  safety_unc_max  <- safety_unc_min  <- safety_unc_mean  <- 
  cb_unc_max      <- cb_unc_min      <- cb_unc_mean      <- rep(NA,height_incr)
    
  require(lhs)
  require(truncnorm)
  for (i in 1:length(delta_h_seq)){
    #print(100*i/length(delta_h_seq))
    construction_cost_seq[i]=Rising_Cost(sqft,delta_h_seq[i])
    #expected_damages_seq[i]=lifetime_expected_damages(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,FEMA_Return_levels,FEMA_Return_periods)
    expected_damages_GEVMCMC_seq[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,mu,sigma,xi)
    safety_seq[i]=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,delta_h_seq[i])
    
    
    z<- randomLHS(nsow, 1)
    SOWs=matrix(NA,nsow,1)
#    SOWs[,1] <- qtruncnorm(z[,1],mean=30,a=5,b=100,sd=25) #5,100)
#    SOWs[,2] <- qtruncnorm(z[,2],mean=0.04,a=0.01,b=0.09,sd=0.03) #0.01, 0.09) 
#    SOWs[,3] <- qunif(z[,3],1,length(mu_chain)) 
#    SOWs[,4] <- qunif(z[,3],1,length(mu_chain)) 
    SOWs[,1] <- floor(qunif(z[,1],1,length(mu_chain))) 
    #mu_chain=sort(mu_chain)
    #sigma_chain=sort(sigma_chain)
    #xi_chain=sort(xi_chain)
    temp <- tmp2 <- tmp3 <- rep(NA,nsow)
    #print(z)
    for(j in 1:nsow){
      #print(c(Struc_Value,House_Initial_Stage,delta_h_seq[i],SOWs[j,1],SOWs[j,2],mu_chain[SOWs[j,3]],sigma_chain[SOWs[j,3]],xi_chain[SOWs[j,3]]))
      #temp[j]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],SOWs[j,1],SOWs[j,2],mu_chain[SOWs[j,3]],sigma_chain[SOWs[j,3]],xi_chain[SOWs[j,3]])
 #     temp[j]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],30,0.04,mu_chain[SOWs[j,3]],sigma_chain[SOWs[j,3]],xi_chain[SOWs[j,3]])
#      tmp2[j]=lifetime_safety(30,mu_chain[SOWs[j,3]],sigma_chain[SOWs[j,3]],xi_chain[SOWs[j,3]],House_Initial_Stage,delta_h_seq[i])

      
      temp[j]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]])
      #tmp2[j]=lifetime_safety(30,mu_chain[j],sigma_chain[j],xi_chain[j],House_Initial_Stage,delta_h_seq[i])
      tmp2[j]=lifetime_safety(life_span,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]],House_Initial_Stage,delta_h_seq[i])
            
    }

    #abline(v=safety_seq[i])
    #abline(v=mean(tmp2),col="red",lwd=2)
    # print(sum(is.na(temp)))
    # print(sum(is.nan(temp)))
    # print(sum(is.na(tmp2)))
    # print(sum(is.nan(tmp2)))
    
    damages_unc_max[i]=quantile(temp[],0.95)
    damages_unc_min[i]=quantile(temp[],0.05)
    damages_unc_mean[i]=mean(temp)
    #damages_unc_mean[i]=quantile(temp,0.5,na.rm=TRUE)
    
    tmp3=temp+construction_cost_seq[i]
    totcost_unc_max[i]=quantile(tmp3[],0.95)
    totcost_unc_min[i]=quantile(tmp3[],0.05)
    totcost_unc_mean[i]=mean(tmp3)
    #totcost_unc_mean[i]=quantile(tmp3,0.5,na.rm=TRUE)
    
    if(i==1){
      damages_initial<-temp
    }
    
    if(i>1){
    tmp4<-(damages_initial-temp)/construction_cost_seq[i] # vector of benefits 
    #print(tmp4)
    tmp4[tmp4<0]=0
    cb_unc_max[i]=quantile(tmp4,0.95)
    cb_unc_min[i]=quantile(tmp4,0.05)
    cb_unc_mean[i]=mean(tmp4)
    #cb_unc_mean[i]=quantile(tmp4,.5,na.rm=TRUE)
    }
    #print(sum(is.na(tmp2)))
    safety_unc_max[i]=quantile(tmp2[],0.95)
    safety_unc_min[i]=quantile(tmp2[],0.05)
    safety_unc_mean[i]=mean(tmp2)
    #safety_unc_mean[i]=quantile(tmp2,.5,na.rm=TRUE)
    
  }
  
  #total_cost=construction_cost_seq+expected_damages_seq
  #print(total_cost_GEVMCMC)
  
  # Find optimal height neglecting uncertainty
  total_cost_GEVMCMC=construction_cost_seq+expected_damages_GEVMCMC_seq
  #total_cost_GEVMCMC[safety_seq<safety_level]=NA
  opt_height=delta_h_seq[which.min(total_cost_GEVMCMC)]
  opt_height_construc_cost=construction_cost_seq[which.min(total_cost_GEVMCMC)]
  opt_height_total_cost=min(total_cost_GEVMCMC)
  opt_height_safety=safety_seq[which.min(total_cost_GEVMCMC)]
  opt_height_damage=expected_damages_GEVMCMC_seq[which.min(total_cost_GEVMCMC)]
  
  temmp=totcost_unc_mean
  totcost_unc_mean[safety_unc_min<safety_level]=NA
  opt_height_unc=delta_h_seq[which.min(totcost_unc_mean)]
  opt_height_unc_total_cost=totcost_unc_mean[which.min(totcost_unc_mean)]
  opt_height_unc_safety=safety_unc_mean[which.min(totcost_unc_mean)]
  opt_height_unc_damage=damages_unc_mean[which.min(totcost_unc_mean)]
  opt_height_unc_bc=cb_unc_mean[which.min(totcost_unc_mean)]
  opt_height_unc_construc_cost=construction_cost_seq[which.min(totcost_unc_mean)]
  
  totcost_unc_mean=temmp
  total_cost_GEVMCMC=construction_cost_seq+expected_damages_GEVMCMC_seq # repeat this code so NA values disapear 
  #print(paste("Saving to ...",filename))
  #save(damages_unc_max,damages_unc_mean,damages_unc_min,cb_unc_min,cb_unc_mean,cb_unc_max,safety_unc_max,safety_unc_min,safety_unc_mean,delta_h_seq,totcost_unc_max,totcost_unc_min,totcost_unc_mean,opt_height_unc_total_cost,opt_height_unc,
  #     opt_height_safety,opt_height,opt_height_total_cost,safety_seq,FEMA_Recomm_cost,FEMA_Recomm_damage,
  #    delta_h_seq,expected_damages_seq,construction_cost_seq,expected_damages_GEVMCMC_seq,total_cost_GEVMCMC,file=filename)
  
#  myreturn=rbind(cbind(opt_height,opt_height_total_cost,opt_height_safety),
#                 cbind(opt_height_unc,opt_height_unc_total_cost,NA),
#                 cbind(abs(del)+1,FEMA_Recomm_cost+FEMA_Recomm_damage,FEMA_safety))
  myreturn=rbind(Not_elevating=cbind(        opt_h=0,               total_cost=total_cost_GEVMCMC[1],              safety=safety_seq[1],        bc=0,                                                                           totcost_houseval=total_cost_GEVMCMC[1]/Struc_Value,                 construc_cost_houselval=0,                                        pass_safety=sum(safety_seq[1]>=0.5),         pass_bc=0,                                                                                       pass_upfront=1                                                 ),                
                 fema_elevating=cbind(       opt_h=abs(del)+1,      total_cost=FEMA_Recomm_cost+FEMA_Recomm_damage,safety=FEMA_safety,          bc=(expected_damages_GEVMCMC_seq[1]-FEMA_Recomm_damage)/FEMA_Recomm_cost,       totcost_houseval=(FEMA_Recomm_cost+FEMA_Recomm_damage)/Struc_Value, construc_cost_houselval=FEMA_Recomm_cost/Struc_Value,             pass_safety=sum(FEMA_safety>=0.5),           pass_bc=sum(((expected_damages_GEVMCMC_seq[1]-FEMA_Recomm_damage)/FEMA_Recomm_cost)>=1),         pass_upfront=sum((FEMA_Recomm_cost/Struc_Value)<1)             ),                   
                 opt_elevating=cbind(        opt_h=opt_height,      total_cost=opt_height_total_cost,              safety=opt_height_safety,    bc=(expected_damages_GEVMCMC_seq[1]-opt_height_damage)/opt_height_construc_cost,totcost_houseval=opt_height_total_cost/Struc_Value,                 construc_cost_houselval=opt_height_construc_cost/Struc_Value,     pass_safety=sum(opt_height_safety>=0.5),     pass_bc=sum(((expected_damages_GEVMCMC_seq[1]-opt_height_damage)/opt_height_construc_cost)>=1),  pass_upfront=sum((opt_height_construc_cost/Struc_Value)<1)     ),
                 optunc_elevating=cbind(     opt_h=opt_height_unc,  total_cost=opt_height_unc_total_cost,          safety=opt_height_unc_safety,bc=opt_height_unc_bc                                                           ,totcost_houseval=opt_height_unc_total_cost/Struc_Value,             construc_cost_houselval=opt_height_unc_construc_cost/Struc_Value, pass_safety=sum(opt_height_unc_safety>=0.5), pass_bc=sum((opt_height_unc_bc)>=1),                                                             pass_upfront=sum((opt_height_unc_construc_cost/Struc_Value)<1) )
                 ) 
  row.names(myreturn) <- c("Not_elevating","fema_elevating","opt_elevating","optunc_elevating") 

    if(save_return==1){
    filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/UNC-SR_V",toString(trunc(Struc_Value/1000)),"Sq",toString(trunc(sqft)),"I",toString(del),".RData",sep="")
    save(damages_unc_max,damages_unc_mean,damages_unc_min,cb_unc_min,cb_unc_mean,cb_unc_max,safety_unc_max,safety_unc_min,safety_unc_mean,
         delta_h_seq,totcost_unc_max,totcost_unc_min,totcost_unc_mean,opt_height_unc_total_cost,opt_height_unc,
         opt_height_safety,opt_height,opt_height_total_cost,safety_seq,FEMA_Recomm_cost,FEMA_Recomm_damage,
         delta_h_seq,expected_damages_seq,construction_cost_seq,expected_damages_GEVMCMC_seq,total_cost_GEVMCMC,file=filename)}
  
  #plot(delta_h_seq,total_cost_GEVMCMC)
  #print(myreturn)
  return(myreturn)
}

######################################################################################################
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
  delta_h_seq <- c(0,seq(3,14,length.out=30))
  
  construction_cost_seq <- expected_damages_GEVMCMC_seq  <- safety_seq <- rep(NA,length(delta_h_seq))
  
  for (i in 1:length(delta_h_seq)){
    #print(i)
    #print(delta_h_seq)
    construction_cost_seq[i]=Rising_Cost(sqft,delta_h_seq[i])
    #expected_damages_seq[i]=lifetime_expected_damages(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,FEMA_Return_levels,FEMA_Return_periods)
    expected_damages_GEVMCMC_seq[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,mu,sigma,xi)
    
    safety_seq[i]=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,delta_h_seq[i])
  }
  print(construction_cost_seq)
  #total_cost=construction_cost_seq+expected_damages_seq
  total_cost_GEVMCMC=construction_cost_seq+expected_damages_GEVMCMC_seq
  #total_cost_GEVMCMC[safety_seq<safety_level]=NA
  #print(total_cost_GEVMCMC)
  opt_height=delta_h_seq[which.min(total_cost_GEVMCMC)]
  #print(safety_seq)
  opt_height_total_cost=min(total_cost_GEVMCMC,na.rm = TRUE)
  opt_height_safety=safety_seq[which.min(total_cost_GEVMCMC)]
  opt_height_damages=expected_damages_GEVMCMC_seq[which.min(total_cost_GEVMCMC)]
  #total_cost_GEVMCMC=construction_cost_seq+expected_damages_GEVMCMC_seq # repeat this code so NA values disapear 
  opt_height_damage=expected_damages_GEVMCMC_seq[which.min(total_cost_GEVMCMC)]
  filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/V",toString(trunc(Struc_Value/1000)),"I",toString(del),"S",toString(life_span),"R",toString(disc_rate*100),".RData",sep="")
  print(filename)
  save(opt_height_damages,opt_height_safety,opt_height,opt_height_total_cost,safety_seq,FEMA_Recomm_cost,FEMA_Recomm_damage,delta_h_seq,construction_cost_seq,expected_damages_GEVMCMC_seq,total_cost_GEVMCMC,file=filename)
  
  myreturn=rbind(Not_elevating=cbind(    opt_h=0,          total_cost=total_cost_GEVMCMC[1],              safety=safety_seq[1],    bc=0,                                                                        totcost_houseval=total_cost_GEVMCMC[1]/Struc_Value),
                fema_elevating=cbind(    opt_h=abs(del)+1, total_cost=FEMA_Recomm_cost+FEMA_Recomm_damage,safety=FEMA_safety,      bc=(expected_damages_GEVMCMC_seq[1]-FEMA_Recomm_damage)/FEMA_Recomm_cost,    totcost_houseval=FEMA_Recomm_cost/Struc_Value),
                opt_elevating=cbind(     opt_h=opt_height, total_cost=opt_height_total_cost,              safety=opt_height_safety,bc=(expected_damages_GEVMCMC_seq[1]-opt_height_damage)/opt_height_total_cost,totcost_houseval=opt_height_total_cost/Struc_Value)) 
  row.names(myreturn) <- c("Not_elevating","fema_elevating","opt_elevating") 
  #plot(delta_h_seq,total_cost_GEVMCMC)
  #print(myreturn)
  return(myreturn)
}


  
