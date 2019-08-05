##==============================================================================
##
## Script contains the series of functions for 
##   1. Calculating the costs of elevating a house
##   2. Calculating the expected damages under certainty and uncertainty
##   3. Finding the optimal heightening level under certatinty and uncertainty
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
##==============================================================================
## Copyright 2019 Mahkameh Zarekarizi
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, see <http://www.gnu.org/licenses/>.
##==============================================================================
# Global variables
rm(list=ls())
main_path="~/Documents/Research/House_Elevation_Project/GitHub/Zarekarizi-flood-home-elavate/"

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Load the required libraries
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
  if(sum(is.nan(safety) || is.na(safety))>0){
    stop("NaNs cannot be handled")
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
  DD_Depth <-  c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  DD_Damage <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)

  # how much is lost (in USD) at each depth. This depends on the value of the house 
  damage_vals=DD_Damage*Struc_Value
  
  # critical depths are depths where the damage factor changes. 
  Critical_Depths=DD_Depth+House_Current_Stage # Calculates the stage of critical depths
  
  # What is the probability that water level exceeds each critical depth?
  Critical_Probs=1-pgev(q=Critical_Depths, shape=xi, loc=mu, scale=sigma)
  
  # The following block is just to avoid NaNs and NAs
  if(sum(is.nan(Critical_Probs))>0){
    test_x=rgev(10^6, shape=xi, loc=mu, scale=sigma)
    Critical_Probs[Critical_Depths<min(test_x)]=0
    Critical_Probs[Critical_Depths>max(test_x)]=1
  }
  if(sum(is.nan(Critical_Probs))>0){
    stop("I dont know what to do....")
  }

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

######################################################################################################
######################################################################################################
######################################################################################################
findopt_UNC <- function(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,mu,sigma,xi,
                        mu_chain,sigma_chain,xi_chain,nsow,safety_level=0,
                        save_return=0){
  ## ARGUMENTS:
  ##sqft=square footage of the buildig 
  ##Struc_Value=building value in US dollars 
  ##del=the added height to the building 
  ##House_Initial_Stage= the initial elevation of the building before elevating 
  ##life_span=life span in years 
  ##disc_rate=discount rate
  ##mu=the location parameter of GEV under certainty 
  ##sigma=the scale parameter of GEV under certainty 
  ##xi=the shape parameter of GEV under certainty 
  ##mu_chain=The MCMC chain of GEV location parameter 
  ##sigma_chain=The MCMC chain of GEV scale parameter 
  ##xi_chain=The MCMC chain of GEV shape parameter 
  ##nsow=number of state of the worlds, or scenarions
  ##safety_level=The minimum safety to be considered 
  ##save_return=If set as 1, all the returned variables will be saved and also be printed; otherwise it will only be printed  
  
  # Base Flood Elevation calculation 
  BFE=House_Initial_Stage-del
  
  # Cost, expected damages and safety of when the building is elevated to FEMA's recommendation  
  FEMA_Recomm_cost=Rising_Cost(sqft,abs(del)+1)
  FEMA_Recomm_damage=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,abs(del)+1,life_span,disc_rate,mu,sigma,xi)
  FEMA_safety=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,abs(del)+1)
  

  delta_h_seq <- c(0,seq(3,14,length.out=100))
  
  height_incr=length(delta_h_seq)
  
  construction_cost_seq <-expected_damages_seq <- expected_damages_GEVMCMC_seq <- safety_seq <- rep(NA,height_incr)
  damages_unc_max <- damages_unc_min <- damages_unc_mean <- 
  totcost_unc_max <- totcost_unc_min <- totcost_unc_mean <- 
  safety_unc_max  <- safety_unc_min  <- safety_unc_mean  <- 
  cb_unc_max      <- cb_unc_min      <- cb_unc_mean      <- rep(NA,height_incr)
  
  require(lhs)
  require(truncnorm)
  
  # Start iterating over various strategies (elevation policy)
  for (i in 1:length(delta_h_seq)){
    
    # Construction cost for ith elevation strategy
    construction_cost_seq[i]=Rising_Cost(sqft,delta_h_seq[i]) 
    
    ## IGNORING UNCERTAINTY--------------------------------
    ### LED (lifetime expected damages ignoring uncertainty)
    expected_damages_GEVMCMC_seq[i]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,mu,sigma,xi)
    ### LS (Lifetime Safety ignoring uncertainty)
    safety_seq[i]=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,delta_h_seq[i])
    ### Total cost vector for ignoring uncertainty approach 
    total_cost_GEVMCMC=construction_cost_seq+expected_damages_GEVMCMC_seq
    ### Optimal height for ignoring uncertainty approach
    opt_height=delta_h_seq[which.min(total_cost_GEVMCMC)]
    ### Construction cost of eleveating to ignoring-uncertainty optimal elevation 
    opt_height_construc_cost=construction_cost_seq[which.min(total_cost_GEVMCMC)]
    ### Total cost of elevating to optimal height for ignoring uncertainty approach 
    opt_height_total_cost=min(total_cost_GEVMCMC)
    ### The safety of the building after being elevated to optimal elevation ignoring uncertainty 
    opt_height_safety=safety_seq[which.min(total_cost_GEVMCMC)]
    ### The expected damages after elevating to optimal elevation of ignoring-uncertainty approach 
    opt_height_damage=expected_damages_GEVMCMC_seq[which.min(total_cost_GEVMCMC)]
    
    ## CONSIDERING UNCERTAINTY --------------------------
    # There are two ways to create the ensemble; 
    # 1. You can calculate the expected damages for every parameter set for MCMC chains 
    # 2. You can calculate the expected damages for random parameter sets chosen by Latin Hypercube Sampling (LHS)
    # Below, we have chosen the second option 
    z<- randomLHS(nsow, 1)
    SOWs=matrix(NA,nsow,1)
    SOWs[,1] <- floor(qunif(z[,1],1,length(mu_chain))) 
    led_ens <- safety_ens <- totcost_ens <- rep(NA,nsow)
    
    for(j in 1:nsow){
      led_ens[j]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,delta_h_seq[i],life_span,disc_rate,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]])
      #led_ens[j]=lifetime_safety(30,mu_chain[j],sigma_chain[j],xi_chain[j],House_Initial_Stage,delta_h_seq[i]) # This line is for the first option; if you are willing to use the 
      # first option, uncomment this line and comment the line before  
      safety_ens[j]=lifetime_safety(life_span,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]],House_Initial_Stage,delta_h_seq[i])
    }
    # Now that we have the ensemble, we can estimate the upper and lower bounds 
    damages_unc_max[i]=quantile(led_ens[],0.95)
    damages_unc_min[i]=quantile(led_ens[],0.05)
    damages_unc_mean[i]=mean(led_ens)

    # Create the ensemble of the total cost 
    totcost_ens=led_ens+construction_cost_seq[i]
    # Estimate the upper and lower bounds of the total cost 
    totcost_unc_max[i]=quantile(totcost_ens[],0.95)
    totcost_unc_min[i]=quantile(totcost_ens[],0.05)
    totcost_unc_mean[i]=mean(totcost_ens)
    
    # To calculate the benefits later, we will need the ensemble of expected damages for "not elevating" (or delta_h=0) strategy
    # Save the ensemble of expected damages for i==0
    if(i==1){
      damages_initial<-led_ens
    }
    # Calculate the ensemble of benefit/cost ratio 
    if(i>1){
    bc_ens<-(damages_initial-led_ens)/construction_cost_seq[i]  
    bc_ens[bc_ens<0]=0
    # Estimate the lower and upper bounds of Benefit/Cost ratio
    cb_unc_max[i]=quantile(bc_ens,0.95)
    cb_unc_min[i]=quantile(bc_ens,0.05)
    cb_unc_mean[i]=mean(bc_ens)
    }
    # Calculate the upper and lower bounds of safety ensemble 
    safety_unc_max[i]=quantile(safety_ens[],0.95)
    safety_unc_min[i]=quantile(safety_ens[],0.05)
    safety_unc_mean[i]=mean(safety_ens)
  } #End of loop for elevatio strategies
  
  # Save this for later use   
  temmp=totcost_unc_mean
  # Exclude the ensemble members that have safeties less than your desired safety threshold 
  totcost_unc_mean[safety_unc_min<safety_level]=NA
  # Optimal height under uncertainty 
  opt_height_unc=delta_h_seq[which.min(totcost_unc_mean)]
  # The total cost associated with elevating a building to optimal elevation under uncertainty 
  opt_height_unc_total_cost=totcost_unc_mean[which.min(totcost_unc_mean)]
  # The safety associated with elevating the building to optimal elevation under uncertainty 
  opt_height_unc_safety=safety_unc_mean[which.min(totcost_unc_mean)]
  # The damages associated with optimal elevation under uncertainty 
  opt_height_unc_damage=damages_unc_mean[which.min(totcost_unc_mean)]
  # The benefit-to-cost ratio associated with optimal elevation under uncertainty 
  opt_height_unc_bc=cb_unc_mean[which.min(totcost_unc_mean)]
  # The costruction cost associated with optimal elevation under uncertainty 
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


  
