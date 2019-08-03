###################################
# file: Sobol_SLR_SALib.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
# 
# Distributed under GNU general public license
# No warranty
#
# Adapted from: cost-benefit-slr.R
# Authored by Ryan Sriver and Klaus Keller
# Pennsylvania State University
#
# Based on: van Dantzig D (1956). Economic Decision Problems for flood prevention. 
# Econometrica 24:276-287
###################################
# Last modified: 30 August, 2015
###################################

# Compile
rm(list = ls())
graphics.off()

library(compiler)
library(evir) # We would use pgev, qgev from this package

enableJIT(3)

# Set seed for reproducibility 
set.seed(1)

# Number of observations
n_obs = 10^4

# Read in Sobol Samples
Sobol <- read.table("sobolParameterSets.txt", sep = ' ', header = FALSE)
#names(Sobol) <- c("Struc_Value","del","life_span","disc_rate","mu","sigma","xi")
names(Sobol) <- c("mu","sigma","xi")

n_Sobol = length(Sobol[,1])


# Source baseline model priors
source("../../../Scripts/priors.R")

###################################
###  van Dantzig problem setup  ###
###################################
# 

lifetime_expected_damages_GEV_MCMC <- function(Struc_Value,del,
                                               life_span,disc_rate,
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
  Struc_Value=350000
  del=-5
  life_span=30
  disc_rate=0.04
  BFE=35.65
  House_Initial_Stage=BFE+del
House_Current_Stage=House_Initial_Stage # The stage of the house after being elevaed. reminder: Stage the elevation of the house with respect to the gage datum in feet 

# Establish the damage-depth relationship 
EU_Depth <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
RES_Damage_Factors <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)

# how much is lost (in USD) at each depth. This depends on the value of the house 
damage_vals=RES_Damage_Factors*Struc_Value

# critical depths are depths where the damage factor changes. 
Critical_Depths=EU_Depth+House_Current_Stage # Calculates the stage of critical depths

# What is the probability that water level exceeds each critical depth?

Critical_Probs=1-pgev(q=Critical_Depths, xi=xi, mu=mu, sigma=sigma)
if(sum(is.na(Critical_Probs))>0){
  test_x=rgev(10^6, xi=xi, mu=mu, sigma=sigma)
  Critical_Probs[Critical_Depths<min(test_x)]=0
  Critical_Probs[Critical_Depths>max(test_x)]=1
}
if(sum(is.na(Critical_Probs))>0){
  stop("I dont know what to do....")
}
#Critical_Probs=1-pgev(q=Critical_Depths, xi=-0.27, mu=Mean_mu, sigma=Mean_sigma)
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

ed<-rep(NA,length(Sobol$del))
for(i in 1:length(Sobol$del)){
  ed[i]=lifetime_expected_damages_GEV_MCMC(Sobol$Struc_Value[i],Sobol$del[i],Sobol$life_span[i],Sobol$disc_rate[i],Sobol$mu[i],Sobol$sigma[i],Sobol$xi[i])
}
ed=ed[!is.na(ed)]
# Create data frame for mean (expected) objective values
exp_objectives <- data.frame(ed)
# Write table to text file for Sobol Sensitivity Analysis
write.table(exp_objectives, file = "objectiveValues.txt", sep = ' ', col.names = FALSE, row.names = FALSE)

# Save Global Environment
save.image("Output/SALib_SLR.RData")