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
lifetime_expected_damages <- function(Struc_Value,House_Initial_Stage,
                                               delta_h,life_span,disc_fac,
                                               mu,sigma,xi,DD_Depth,DD_Damage){
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
  
  # how much is lost (in USD) at each depth. This depends on the value of the house 
  DD_Damage=DD_Damage/100
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
  #print(paste("EAD=",EAD))
  if(length(disc_fac)!=round(life_span)){
    print(length(disc_fac))
    print(life_span)
    stop('Discount factor problem')
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
Raising_Cost_discrete <- function(sqft,delta_h){
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
  raise_cost=Base_cost+rate*sqft
  
  # There is no cost for not elevating the house
  if(delta_h==0){raise_cost=0}
  
  return(raise_cost)
}

################################################################################
################################################################################
################################################################################
# This function calculates the cost of elevating the house using CLARA model 
# For the source, see appendix A of CLARA model at this webpage:
# http://coastal.la.gov/our-plan/2017-coastal-master-plan/ 
#Raising_Cost_interpolated <- function(sqft,delta_h){
Raising_Cost<- function(delta_h,sqft){
  
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
  raise_cost=Base_cost+rate*sqft
  
  # There is no cost for not elevating the house
  if(delta_h==0){raise_cost=0}
  
  return(raise_cost)
}

#------------------------------------------
#------------------------------------------
#   HOUSE LIFETIME UNCERTAINTY
#------------------------------------------
#------------------------------------------
# Deep uncertainty in lifetime
lifetime_uncertainty <- function(nsow,lifetime_function="weibull"){
  require(truncnorm)
  a=1;b=100
  m1=30;m2=50
  sd1=m1/5
  sd2=m2/5
  lifetime_unc<-matrix(NA,nsow,2)
  lifetime_unc[,1]=(rtruncnorm(nsow, a, b, mean =m1, sd = sd1))
  lifetime_unc[,2]=(rtruncnorm(nsow, a, b, mean =m2, sd =sd2))
  life_deep_unc_switch=sample(c(1,2),nsow,replace = TRUE)
  Life_DeepUnc<-rep(NA,nsow)
  if(lifetime_function=="30yr"){
    for(i in 1:nsow){Life_DeepUnc[i]=lifetime_unc[i,1]}
  }else if(lifetime_function=="50yr"){
    for(i in 1:nsow){Life_DeepUnc[i]=lifetime_unc[i,2]}
  }else if(lifetime_function=="deep"){
    for(i in 1:nsow){Life_DeepUnc[i]=lifetime_unc[i,life_deep_unc_switch[i]]}
  }else if(lifetime_function=="weibull"){
    Life_DeepUnc=(rweibull(nsow,shape=2.8,scale=73.5))
    Life_DeepUnc[Life_DeepUnc<2]=2
  }else{
    stop('Sorry---lifetime function type is not known. Options are "30yr", "50yr", or "deep". Please try again. Thanks')
  }
  return(Life_DeepUnc)
}
#------------------------------------------
#------------------------------------------
#   END---HOUSE LIFETIME UNCERTAINTY
#------------------------------------------
#------------------------------------------


#------------------------------------------
#------------------------------------------
#   DISCOUNT RATE UNCERTAINTY 
#------------------------------------------
#------------------------------------------
discount_rate_uncertainty <- function(obs_discount,nsow,dr_function="deep",life_span=30,disc_rate=0.04){
  #set.seed(1000)
  # Deep uncertainty in discount rate 
  # This matrix is the nsow*101 matrix of uncertain discount factors. 
  # Discount factors in this matrix are a combination of all three discount rate models (random walk, mean reverting, and drift)
  # First, we will allocate this matrix 
  discFac_DeepUnc<-matrix(NA,nsow,201) 
  
  # The following array is a 3-dimensional array that contains discount factors from each model in a separate dimension
  # First, let's create this array 
  disc_factor_unc=array(rep(NA,(3*nsow*101)),dim=c(nsow,201,3))
  
  # Then, let's assign discount rates of each model to a separate dimension
  # We start with random walk model 
  disc_factor_unc[,,1] <- compute_dfactors(rw_discount(log(obs_discount[,2]),200,nsow))
  
  # Next, mean reverting model 
  disc_factor_unc[,,2] <- compute_dfactors(mrv_discount(log(obs_discount[,2]),200,nsow))
  
  # Finally, the drift model 
  disc_factor_unc[,,3] <- compute_dfactors(drift_discount(log(obs_discount[,2]),200,nsow))
  
  # The following variable is just a variable that determines switch between models
  deep_unc_switch=sample(c(1,2,3),nsow,replace = TRUE)
  
  # Then, let's use the switch and choose discount rate from a model for each State of the world (SOW)
  if(dr_function=="rw"){
    for(i in 1:nsow){discFac_DeepUnc[i,]=disc_factor_unc[i,,1]}
  }else if(dr_function=="mrv"){
    for(i in 1:nsow){discFac_DeepUnc[i,]=disc_factor_unc[i,,2]}
  }else if(dr_function=="drift"){
    for(i in 1:nsow){discFac_DeepUnc[i,]=disc_factor_unc[i,,3]}
  }else if(dr_function=="deep"){
    for(i in 1:nsow){discFac_DeepUnc[i,]=disc_factor_unc[i,,deep_unc_switch[i]]}
  }else if(dr_function=="cert-4%"){
  }else{
    stop('Sorry---discount rate function type is not known. Options are "rw", "mrv", "drift", or "deep". Please try again. Thanks')
  }
  
  if(dr_function=="cert-4%"){
    ## fixed (4%) discount rate
    disc_factor_cer=rep(1,201)
    for(i in 1:201){
      disc_factor_cer[i]=exp(-1*(0.04*(i-1)))
    }
    discFac_DeepUnc=disc_factor_cer
  }
  return(discFac_DeepUnc)
}
#------------------------------------------
#------------------------------------------
#   END---DISCOUNT RATE UNCERTAINTY 
#------------------------------------------
#------------------------------------------



#------------------------------------------
#------------------------------------------
#   DEPTH-DAMAGE FUNCTION UNCERTAINTY
#------------------------------------------
#------------------------------------------
depth_damage_uncertainty <- function(nsow,dd_function="deep"){
  Depth_1 <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  Damage_Factors_1 <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)*100
  
  Depth_2<-c(-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
  Damage_Factors_2<-c(0,0,4,8,12,15,20,23,28,33,37,43,48,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81)
  
  depths <- seq(min(min(Depth_1),min(Depth_2)),max(max(Depth_1),max(Depth_2)),length.out = 100)
  
  Damage_Factors_1 <- approx(Depth_1,Damage_Factors_1,xout = depths,rule = 2,yleft=0)$y
  Damage_Factors_2 <- approx(Depth_2,Damage_Factors_2,xout = depths,rule = 2,yleft=0)$y
  
  damage_factors_DeepUnc <- matrix(NA,nrow=nsow,ncol=length(depths))
  damage_factors <- array(rep(NA,(2*length(depths)*nsow)),dim=c(length(depths),nsow,2))
  
  error <- runif(nsow,-0.3,0.3)
  for (i in 1:nsow){
    damage_factors[,i,1] <- Damage_Factors_1+error[i]*Damage_Factors_1
  }
  error <- runif(nsow,-0.3,0.3)
  for (i in 1:nsow){
    damage_factors[,i,2] <- Damage_Factors_2+error[i]*Damage_Factors_2
  }
  
  switch_deep_unc <- sample(c(1,2),nsow,replace = TRUE)
  if(dd_function=="eu"){
    for(i in 1:nsow){damage_factors_DeepUnc[i,]=damage_factors[,i,1]}
  }else if(dd_function=="hazus"){
    for(i in 1:nsow){damage_factors_DeepUnc[i,]=damage_factors[,i,2]}
  }else if(dd_function=="deep"){
    for(i in 1:nsow){damage_factors_DeepUnc[i,]=damage_factors[,i,switch_deep_unc[i]]}
  }else if(dd_function=="cert-hazus"){
    output=rbind(depths,Damage_Factors_2)
  }else{
    stop('Sorry---depth-damage function type is not known. Options are "eu", "hazus", or "deep". Please try again. Thanks')
  }
  
  if(dd_function!="cert-hazus"){
    damage_factors_DeepUnc[damage_factors_DeepUnc>100]=100
    damage_factors_DeepUnc[damage_factors_DeepUnc<0]=0
    output=rbind(depths,damage_factors_DeepUnc)
  }
  return(output)
  
  # The following code is to validate the code by plotting the depth-damage functions 
  #plot(depths,Damage_Factors_1,ylim=c(0,100))
  #lines(depths,Damage_Factors_2)
  #for(i in 1:100){
  #  lines(depths,damage_factors_DeepUnc[,i])
  #}
}
#------------------------------------------
#------------------------------------------
#   END----DEPTH-DAMAGE FUNCTION UNCERTAINTY
#------------------------------------------
#------------------------------------------



#------------------------------------------
#------------------------------------------
#   GEV PARAMETERS UNCERTAINTY
#------------------------------------------
#------------------------------------------
gev_uncertainty <- function(nsow,mu_chain,sigma_chain,xi_chain){
  samples <- runif(nsow,1,length(mu_chain))
  params_unc <- matrix(NA,nsow,3)
  params_unc[,1] <- mu_chain[samples]
  params_unc[,2] <- sigma_chain[samples]
  params_unc[,3] <- xi_chain[samples]
  return(params_unc)
}
#------------------------------------------
#------------------------------------------
#   END------GEV PARAMETERS UNCERTAINTY
#------------------------------------------
#------------------------------------------

######################################################################################################
######################################################################################################
######################################################################################################
findopt_UNC <- function(sqft, # square footage of the buildig
                        Struc_Value, #Struc_Value=building value in US dollars 
                        del, #initial elevation of the house minus BFE 
                        House_Initial_Stage, #House_Initial_Stage= the initial elevation of the building before elevating
                        life_span, ##life_span=life span in years 
                        disc_rate, ##disc_rate=discount rate
                        mu,sigma,xi, ##location, scale, and shape parameters of GEV under certainty 
                        mu_chain,sigma_chain,xi_chain, ##mu_chain=The MCMC chain of GEV location parameter 
                        nsow, ##nsow=number of state of the worlds, or scenarions
                        obs_discount,
                        save_return=0, ##save_return=If set as 1, all the returned variables will be saved and also be printed; otherwise it will only be printed
                        n_strategy=100, #number os elevation strategies to be explored 
                        verbose=TRUE, #set is T if you would like to print the progress of the program
                        ddUnc="deep", #Uncertainty type in depth-damage function. Options= "deep", "eu", and "hazus" 
                        lifeUnc="weibull", # Type of uncertainty in the house lifetime. options: "30yr", "50yr", and "deep"
                        drUnc="deep", # Type of discount rate uncertainty options: "rw", "mrv", "drift", and "deep"
                        threshold_totalcost=1,
                        test=FALSE
                        ){
 # For debuging purposes - please ignore 
  if(test){
  sqft=1500
  Struc_Value=320000
  del=-6
  House_Initial_Stage=27.88917
  life_span=30
  disc_rate=0.04
  mu=20.45111
  sigma=3.314449
  xi=-0.0561129
  mu_chain=mu_chain
  sigma_chain=sigma_chain
  xi_chain=xi_chain
  nsow=10000
  obs_discount=discount
  save_return=0 
  n_strategy=10 
  verbose=TRUE
  ddUnc="deep" 
  lifeUnc="weibull" 
  drUnc="deep"
 }
  #------------------------------------------
  #------------------------------------------
  #      GENERATE STATE OF THE WORLDS
  #------------------------------------------
  #------------------------------------------
  require(lhs)
  gevparams <- gev_uncertainty(nsow,mu_chain,sigma_chain,xi_chain) # shape: nsow*3
  drs <- discount_rate_uncertainty(obs_discount,nsow,drUnc) # shape: nsow*101
  lts <- lifetime_uncertainty(nsow,lifeUnc) # shape: nsow
  ddf <- depth_damage_uncertainty(nsow,ddUnc) # shape: nsow*100
  set.seed(0)
  z <- randomLHS(nsow, 4)
  i_sow <- qunif(z,1,nsow)
  i_sow[,4]=i_sow[,4]+1
  
  SOWs=matrix(NA,nsow,305)
  SOWs[,1:3] <- gevparams[i_sow[,1],1:3] #GEV paramters 
  SOWs[,4:204] <- drs[i_sow[,2],] # Discount rates 
  SOWs[,205] <- lts[i_sow[,3]] # Lifetime
  SOWs[,206:305] <- ddf[i_sow[,4],] # Depth-Damage function
  #------------------------------------------
  #------------------------------------------
  #  END------GENERATE STATE OF THE WORLDS
  #------------------------------------------
  #------------------------------------------
  
  
  
  #------------------------------------
  #------------------------------------
  # CALCULATE OBJECTIVE I: TOTAL COST  
  #------------------------------------
  #------------------------------------
  delta_h_seq <- c(0,seq(3,14,length.out=n_strategy))
  totcost_unc_max <- totcost_unc_min <- totcost_unc_mean <- construction_cost_seq <- rep(NA,length(delta_h_seq))
  damages_unc_max <- damages_unc_min <- damages_unc_mean <- construction_cost_seq <- rep(NA,length(delta_h_seq))
  
  led_ens <- totcost_ens <- matrix(NA,length(delta_h_seq),nsow)
  for (i in 1:length(delta_h_seq)){
      # STEP 1: For each strategy, calculate the total cost
      if(verbose){
      print(paste('Evaluating objective I (total cost) for strategy #',toString(i),'out of',toString(length(delta_h_seq)),'strategies'))
      }
      for(j in 1:nsow){
                        led_ens[i,j]=lifetime_expected_damages(Struc_Value,
                                                    House_Initial_Stage,
                                                    delta_h_seq[i],
                                                    floor(SOWs[j,205]), #Lifetime
                                                    SOWs[j,4:(4-1+floor(SOWs[j,205]))], #Discount factor from year 0 to whatever the lifetime is 
                                                    SOWs[j,1],SOWs[j,2],SOWs[j,3], #GEV parameters 
                                                    ddf[1,], #Depths from depth-damage function
                                                    SOWs[j,206:305] # Damage fractions from depth-damage function
                                                    )
      }
      # STEP 2: For each strategy, calculate the Construction cost
      construction_cost_seq[i] <- Raising_Cost(delta_h_seq[i],sqft)
  
      # STEP 3: For each strategy, calculate total cost ensemble (construction cost + lifetime expected damages) 
      totcost_ens[i,]=led_ens[i,]+construction_cost_seq[i]
  
      # STEP 4: Estimate the upper and lower bounds of the total cost 
      damages_unc_max[i]=quantile(led_ens[i,],0.95)
      damages_unc_min[i]=quantile(led_ens[i,],0.05)
      damages_unc_mean[i]=mean(led_ens[i,])
      
      # STEP 5: Estimate the upper and lower bounds of the damages 
      
      #print(mean(led_ens[i,])/Struc_Value)
      totcost_unc_max[i]=quantile(totcost_ens[i,],0.95)
      totcost_unc_min[i]=quantile(totcost_ens[i,],0.05)
      totcost_unc_mean[i]=mean(totcost_ens[i,])
      #plot(totcost_unc_mean/Struc_Value,type="l")
  }
  #------------------------------------
  #------------------------------------
  # END ---- OBJECTIVE I: TOTAL COST  
  #------------------------------------
  #------------------------------------
      
  
  
   #------------------------------------
   #------------------------------------
   # CALCULATE OBJECTIVE II: SAFETY  
   #------------------------------------
   #------------------------------------
    safety_unc_max  <- safety_unc_min  <- safety_unc_mean  <- rep(NA,length(delta_h_seq))
    safety_ens  <-  matrix(NA,length(delta_h_seq),nsow)
    
    for (i in 1:length(delta_h_seq)){
        if(verbose){
        print(paste('Evaluating objective II (safety) for strategy #',toString(i),'out of',toString(length(delta_h_seq)),'strategies'))
        }
        for(j in 1:nsow){
                         safety_ens[i,j]=lifetime_safety(life_span,
                                                       SOWs[j,1],SOWs[j,2],SOWs[j,3],
                                                       House_Initial_Stage,
                                                       delta_h_seq[i]
                                                       )
        }
      # Calculate the upper and lower bounds of safety ensemble 
      safety_unc_max[i]=quantile(safety_ens[i,],0.95)
      safety_unc_min[i]=quantile(safety_ens[i,],0.05)
      safety_unc_mean[i]=mean(safety_ens[i,])
  }
  #------------------------------------
  #------------------------------------
  # END --- OBJECTIVE II: SAFETY  
  #------------------------------------
  #------------------------------------

  
  
  #------------------------------------
  #------------------------------------
  # OBJECTIVE III: B/C  
  #------------------------------------
  #------------------------------------
  bcr_unc_max <- bcr_unc_min <- bcr_unc_mean <- rep(NA,length(delta_h_seq))
  bcr_ens <- matrix(NA,length(delta_h_seq),nsow)
  for (i in 1:length(delta_h_seq)){
      if(verbose){
      print(paste('Evaluating objective III (B/C) for strategy #',toString(i),'out of',toString(length(delta_h_seq)),'strategies'))
      }
      # To calculate the benefits we will need the ensemble of expected damages for "not elevating" 
      # (or delta_h=0) strategy
      # Save the ensemble of expected damages for i==0
      if(i==1){damages_initial<-led_ens}
  
    # Calculate the ensemble of benefit/cost ratio 
      if(i>1){
          bcr_ens[i,]<-(led_ens[1,]-led_ens[i,])/construction_cost_seq[i]  
          if(sum(bcr_ens[i,]<0)>1){
            stop('DANGER...NEGATIVE BENEFIT')
          }
          # Estimate the lower and upper bounds of Benefit/Cost ratio
          bcr_unc_max[i]=quantile(bcr_ens[i,],0.95)
          bcr_unc_min[i]=quantile(bcr_ens[i,],0.05)
          bcr_unc_mean[i]=mean(bcr_ens[i,])
      }
  }
  bcr_unc_max[1]=0
  bcr_unc_min[1]=0
  bcr_unc_mean[1]=0
  bcr_ens[1,]=0
  #------------------------------------
  #------------------------------------
  # END--------OBJECTIVE III: B/C  
  #------------------------------------
  #------------------------------------
 
  
  
  
  
  #------------------------------------
  #------------------------------------
  # OBJECTIVE IV: Satisficing   
  #------------------------------------
  #------------------------------------
  satisficing_all <- satisficing_safety <- 
  satisficing_bcr <- satisficing_totcost <- rep(NA,length(delta_h_seq))
  
  for(i in 1:length(delta_h_seq)){
    satisficing_all[i]=100*sum(bcr_ens[i,]>1 & 
                           safety_ens[i,] >0.5 &
                          (totcost_ens[i,]/Struc_Value)<threshold_totalcost 
                           )/nsow 
    satisficing_bcr[i]=100*sum(bcr_ens[i,]>1)/nsow 
    satisficing_safety[i]=100*sum(safety_ens[i,] >0.5)/nsow 
    satisficing_totcost[i]=100*sum((totcost_ens[i,]/Struc_Value)<threshold_totalcost)/nsow 
  }
  #------------------------------------
  #------------------------------------
  # END ----- OBJECTIVE IV: Satisficing   
  #------------------------------------
  #------------------------------------
  
  
  
  
  #------------------------------------
  #------------------------------------
  # Objectives+optimal elevation best gues State of the world   
  #------------------------------------
  #------------------------------------
  damages_bg <- safety_bg <- rep(NA,length(delta_h_seq))
  
  ddf_bg <- depth_damage_uncertainty(nsow,'cert-hazus') # shape: nsow*100
  dr_bg <- discount_rate_uncertainty(obs_discount,nsow,dr_function="cert-4%",life_span,disc_rate)
  
  # Start iterating over various strategies (elevation policy)
  for (i in 1:length(delta_h_seq)){
    if(verbose){
    print(paste('Evaluating objectives for best guess (ignoring uncertainty) strategy #',toString(i),'out of',toString(length(delta_h_seq)),'strategies'))
    }
    ### LED (lifetime expected damages ignoring uncertainty)
    damages_bg[i]=lifetime_expected_damages(Struc_Value,
                                                     House_Initial_Stage,
                                                     delta_h_seq[i],
                                                     life_span,
                                                     dr_bg[1:floor(life_span)],
                                                     mu,sigma,xi,
                                                     ddf_bg[1,],
                                                     ddf_bg[2,])
    ### LS (Lifetime Safety ignoring uncertainty)
    safety_bg[i]=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,delta_h_seq[i])
  }
  # Calculating benefits and costs
  bcr_cost_bg=construction_cost_seq
  bcr_benefit_bg=damages_bg[1]-damages_bg
  bcr_bg=bcr_benefit_bg/bcr_cost_bg
  bcr_bg[1]=0
  ### Total cost vector for ignoring uncertainty approach 
  total_cost_bg=construction_cost_seq+damages_bg
  
  ### Optimal height for ignoring uncertainty approach
  opt_height_bg=delta_h_seq[which.min(total_cost_bg)]
  
  ### Construction cost of eleveating to ignoring-uncertainty optimal elevation 
  opt_height_bg_construc_cost=construction_cost_seq[which.min(total_cost_bg)]

  ### Construction cost of eleveating to ignoring-uncertainty optimal elevation 
  opt_height_bg_damages=damages_bg[which.min(total_cost_bg)]
  
  ### Total cost of elevating to optimal height for ignoring uncertainty approach 
  opt_height_bg_total_cost=min(total_cost_bg)
  
  ### The safety of the building after being elevated to optimal elevation ignoring uncertainty 
  opt_height_bg_safety=safety_bg[which.min(total_cost_bg)]
  
  ### The B/C of the building after being elevated to optimal elevation ignoring uncertainty 
  opt_height_bg_bcr=bcr_bg[which.min(total_cost_bg)]
  
  opt_height_bg_satis_all <- satisficing_all[which.min(total_cost_bg)]
  opt_height_bg_satis_bcr <- satisficing_bcr[which.min(total_cost_bg)]
  opt_height_bg_satis_totcost <- satisficing_totcost[which.min(total_cost_bg)]
  opt_height_bg_satis_safety <- satisficing_safety[which.min(total_cost_bg)]
  
  #------------------------------------
  #------------------------------------
  # Objectives+optimal elevation best gues State of the world   
  #------------------------------------
  #------------------------------------
  
  
  
  
  
  #------------------------------------
  #------------------------------------
  # Objectives I, II, and III for FEMA's strategy  
  #------------------------------------
  #------------------------------------
  # Cost, expected damages and safety of when the building is elevated to FEMA's recommendation 
  if(verbose){
  print(paste('Evaluating objectives for FEMA strategy'))
  }
  FEMA_Recomm_cost=Raising_Cost(abs(del)+1.5,sqft)
  FEMA_Recomm_damage=lifetime_expected_damages(Struc_Value,
                                                        House_Initial_Stage,
                                                        abs(del)+1.5,
                                                        life_span,
                                                        dr_bg[1:life_span],
                                                        mu,sigma,xi,
                                                        ddf_bg[1,],
                                                        ddf_bg[2,])
  FEMA_Recomm_safety=lifetime_safety(life_span,mu,sigma,xi,House_Initial_Stage,abs(del)+1.5)
  FEMA_Recomm_tot_cost=FEMA_Recomm_damage+FEMA_Recomm_cost
  FEMA_Recomm_bcr=abs(FEMA_Recomm_damage-damages_bg[1])/FEMA_Recomm_cost
  
  FEMA_Recomm_satis_all <- satisficing_all[which.min(abs((abs(del)+1.5)-delta_h_seq))]
  FEMA_Recomm_satis_bcr <- satisficing_bcr[which.min(abs((abs(del)+1.5)-delta_h_seq))]
  FEMA_Recomm_satis_totcost <- satisficing_totcost[which.min(abs((abs(del)+1.5)-delta_h_seq))]
  FEMA_Recomm_satis_safety <- satisficing_safety[which.min(abs((abs(del)+1.5)-delta_h_seq))]
  
  #------------------------------------
  #------------------------------------
  # END-----Objectives for FEMA's strategy  
  #------------------------------------
  #------------------------------------
  
  
  
 
  
  #------------------------------------
  #------------------------------------
  # Optimal elevation if we consider all the uncertainty sources   
  #------------------------------------
  #------------------------------------
  if(verbose){print(paste('Evaluating optimal height under uncertainty'))}
  
  # Optimal height under uncertainty 
  opt_height_unc=delta_h_seq[which.min(totcost_unc_mean)]
  
  # The total cost associated with elevating a building to optimal elevation under uncertainty 
  opt_height_unc_total_cost=totcost_unc_mean[which.min(totcost_unc_mean)]
  
  # The safety associated with elevating the building to optimal elevation under uncertainty 
  opt_height_unc_safety=safety_unc_mean[which.min(totcost_unc_mean)]
  
  # The benefit-to-cost ratio associated with optimal elevation under uncertainty 
  opt_height_unc_bcr=bcr_unc_mean[which.min(totcost_unc_mean)]
  
  # The costruction cost associated with optimal elevation under uncertainty 
  opt_height_unc_construc_cost=construction_cost_seq[which.min(totcost_unc_mean)]

  # The costruction cost associated with optimal elevation under uncertainty 
  opt_height_unc_damages=led_ens[which.min(totcost_unc_mean),]
  
  opt_height_unc_satis_all <- satisficing_all[which.min(totcost_unc_mean)]
  opt_height_unc_satis_bcr <- satisficing_bcr[which.min(totcost_unc_mean)]
  opt_height_unc_satis_totcost <- satisficing_totcost[which.min(totcost_unc_mean)]
  opt_height_unc_satis_safety <- satisficing_safety[which.min(totcost_unc_mean)]
  #------------------------------------
  #------------------------------------
  # END-----Optimal elevation if we consider all the uncertainty sources   
  #------------------------------------
  #------------------------------------
  
  #------------------------------------
  #------------------------------------
  # Money lost if gone with ignoring uncertainty optimal point 
  #------------------------------------
  #------------------------------------
  money_lost_damages=led_ens[which.min(total_cost_bg),]-led_ens[which.min(totcost_unc_mean),]
  money_saved_construction=construction_cost_seq[which.min(totcost_unc_mean)]-construction_cost_seq[which.min(total_cost_bg)]
  net_lost=money_lost_damages-money_saved_construction
  #------------------------------------
  #------------------------------------
  # END-----Money lost if gone with ignoring uncertainty optimal point    
  #------------------------------------
  #------------------------------------
  
  
  #------------------------------------
  #------------------------------------
  #            SAVE DATA    
  #------------------------------------
  #------------------------------------
  if(verbose){print(paste('Saving the data'))}
  
  if(save_return==1){ # If the user sets the save_return argument as 1, data will be saved. 
    filename=paste(getwd(),"/Output_Data/House_case_objectives/Decision_Vars_V",toString(trunc(Struc_Value/1000)),
                   "_Sq",toString(trunc(sqft)),
                   "_I",toString(del),
                   ".RData",sep="")
    save(delta_h_seq,
         construction_cost_seq,
         
         bcr_unc_min,
         bcr_unc_mean,
         bcr_unc_max,
         
         safety_unc_max,
         safety_unc_min,
         safety_unc_mean,
         
         totcost_unc_max,
         totcost_unc_min,
         totcost_unc_mean,
 
         damages_unc_max,
         damages_unc_mean,
         damages_unc_min,
         
         opt_height_unc,
         opt_height_unc_total_cost,
         opt_height_unc_safety,
         opt_height_unc_construc_cost,
         opt_height_unc_bcr,
         opt_height_unc_damages,
         opt_height_unc_satis_all,
         opt_height_unc_satis_bcr,
         opt_height_unc_satis_safety,
         opt_height_unc_satis_totcost,
         
         bcr_bg,
         safety_bg,
         total_cost_bg,
         damages_bg,
         
         opt_height_bg,
         opt_height_bg_total_cost,
         opt_height_bg_safety,
         opt_height_bg_construc_cost,
         opt_height_bg_bcr,
         opt_height_bg_damages,
         
         FEMA_Recomm_cost,
         FEMA_Recomm_tot_cost,
         FEMA_Recomm_safety,
         FEMA_Recomm_bcr,
         FEMA_Recomm_damage,
         
         net_lost,
         money_lost_damages,
         money_saved_construction,
         
         satisficing_all,
         satisficing_totcost,
         satisficing_safety,
         satisficing_bcr,
         
         file=filename)}
  #------------------------------------
  #------------------------------------
  #            SAVE DATA    
  #------------------------------------
  #------------------------------------
  
  
  
  #------------------------------------
  #------------------------------------
  #            PRINT DATA    
  #------------------------------------
  #------------------------------------
  # The following is the table that is shown upon running the function. It is not saved. It will be shown on display only
  if(verbose){print(paste('Printing the data'))}
  myreturn=rbind(Not_elevating=cbind(    opt_h=0,             total_cost=total_cost_bg[1],              safety=safety_bg[1],            bc=0,                     totcost_houseval=total_cost_bg[1]/Struc_Value,          construc_cost_houselval=0,                                       satisficing_all=satisficing_all[1],      satisficing_reliability=satisficing_safety[1],       satisficing_bcr=satisficing_bcr[1],      satisficing_totcost=satisficing_totcost[1]),             
                 fema_elevating=cbind(   opt_h=abs(del)+1.5,  total_cost=FEMA_Recomm_tot_cost,          safety=FEMA_Recomm_safety,      bc=FEMA_Recomm_bcr,       totcost_houseval=FEMA_Recomm_tot_cost/Struc_Value,      construc_cost_houselval=FEMA_Recomm_cost/Struc_Value,            satisficing_all=FEMA_Recomm_satis_all,   satisficing_reliability=FEMA_Recomm_satis_safety,    satisficing_bcr=FEMA_Recomm_satis_bcr,   satisficing_totcost=FEMA_Recomm_satis_totcost),        
                 opt_elevating=cbind(    opt_h=opt_height_bg, total_cost=opt_height_bg_total_cost,      safety=opt_height_bg_safety,    bc=opt_height_bg_bcr,     totcost_houseval=opt_height_bg_total_cost/Struc_Value,  construc_cost_houselval=opt_height_bg_construc_cost/Struc_Value, satisficing_all=opt_height_bg_satis_all, satisficing_reliability=opt_height_bg_satis_safety,  satisficing_bcr=opt_height_bg_satis_bcr, satisficing_totcost=opt_height_bg_satis_totcost),
                 optunc_elevating=cbind( opt_h=opt_height_unc,total_cost=opt_height_unc_total_cost,     safety=opt_height_unc_safety,   bc=opt_height_unc_bcr,    totcost_houseval=opt_height_unc_total_cost/Struc_Value, construc_cost_houselval=opt_height_unc_construc_cost/Struc_Value,satisficing_all=opt_height_unc_satis_all,satisficing_reliability=opt_height_unc_satis_safety, satisficing_bcr=opt_height_unc_satis_bcr,satisficing_totcost=opt_height_unc_satis_totcost)
  ) 
  row.names(myreturn) <- c("Not_elevating","fema_elevating","opt_elevating","optunc_elevating") 
  if(verbose){print(myreturn)}
  #------------------------------------
  #------------------------------------
  #       END----PRINT DATA    
  #------------------------------------
  #------------------------------------
  
  return(myreturn)
}
