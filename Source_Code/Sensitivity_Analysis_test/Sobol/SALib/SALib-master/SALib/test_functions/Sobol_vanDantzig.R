###################################
# file: Sobol_vanDantzig.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Sobol Analysis for parameter sensitivity
# Based on Java MOEAFramework, as described in:
# https://waterprogramming.wordpress.com/2012/08/13/running-sobol-sensitivity-analysis-using-moeaframework/
#################################### 

# Compile
rm(list = ls())
graphics.off()

library(compiler)
enableJIT(3)

# Set prior parameter values - Section 6 "The Doubtful Constants," van Dantzig (1956)
source("Scripts/priors.R")

priors <- data.frame(p_zero_p, alpha_p, V_p, delta_prime_p, k_p, subs_rate, sea_level_rate)

# Read in Sobol Samples
Sobol <- read.table("sobolParameterSets.txt", sep = ' ', header = FALSE)
names(Sobol) <- names(priors)

n_Sobol = length(Sobol[,1])

# van Dantzig problem setup:
# Time horizon to the next revision of the dike (in years), from paper 
T = 75

# time scale
beta_p = alpha_p*subs_rate

# considered range of Deike heigtening in meters
X = seq(0,10,by=0.05)

# considered time horizon, in annual increments
time = seq(0,T,by=1)

# Define variables
p_exceed = array(NA, dim = c(length(X), n_Sobol))
costs = array(NA, dim = c(length(X), n_Sobol))
NPV_expected_losses = array(NA, dim = c(length(X), n_Sobol))
EV_p_exceed_transient = array(NA, dim = c(length(X), n_Sobol))
Total_flood_frequency = array(NA, dim = c(length(X), n_Sobol))
total_costs = array(NA, dim = c(length(X), n_Sobol))

discount_factor = array(NA, dim = c(length(time), n_Sobol))
effective_height = array(NA, dim = c(length(X), length(time), n_Sobol))
p_exceed_transient = array(NA, dim = c(length(X),length(time), n_Sobol))
NPV_costs_flooding = array(NA, dim = c(length(X),length(time), n_Sobol))

subsidence = array(NA, dim = c(length(time), n_Sobol))
sea_level_rise = array(NA, dim = c(length(time), n_Sobol)) 

# Run model for 10,000 SOW
for(i in 1:length(X)) {
  # analyze for each considered Deike heightening (eq. 1 in paper)
  p_exceed[i,]= Sobol$p_zero_p*exp(-(Sobol$alpha_p)*X[i])
  
  for (j in 1:length(time)) {  
 
    # subsidence rate over time horizon
    subsidence[j,] = Sobol$subs_rate*time[j]
    
    #sea level rise over time horizon
    sea_level_rise[j,] = Sobol$sea_level_rate*time[j]
    
    # analze for each year of the initial time period
    year =j-1
    
    #losses in the future are discounted at the annual net discount rate
    discount_factor[j,] = 1/(1+Sobol$delta_prime_p)^year
    
    #The effective deike height decreases with subsidence and sea-level rise.
    effective_height[i,j,] = X[i] - subsidence[j,] - sea_level_rise[j,]
    
    # For a stationary flooding frequency, we can evaluate the annual flooding
    #frequency with the old observeations and the new effective height.
    p_exceed_transient[i,j,] = Sobol$p_zero_p*exp(-(Sobol$alpha_p)*effective_height[i,j,])
    
    #The net present value of the losses per year are the product of the
    #frequency of flooding per year, the damages per flood, and the discount factor.
    NPV_costs_flooding[i,j,] = p_exceed_transient[i,j,]*Sobol$V_p*discount_factor[j,]
  }
  
  #The costs of building the dike increase linearly with respect to height.
  costs[i,] = Sobol$k_p*X[i]
  
}

#The total discounted expected losses are the sum of the discounted expected annual losses.
#Apply sum function over Sobol sets
NPV_expected_losses = apply(NPV_costs_flooding, c(1,3), sum) 
 
#The average annual flood frequency is the mean the annual flood frequencies.
#EV_p_exceed_transient[i,] = mean(p_exceed_transient[i,,])
EV_p_exceed_transient = apply(p_exceed_transient, c(1,3), mean)

#The total flood frequency over the life-time of the project is the sum of the flood frequencies,
#assuming independence, as in the original paper
Total_flood_frequency = apply(p_exceed_transient, c(1,3), sum)

for(i in 1:length(X)){
  
  #The total costs that depend on the deike height. Note that the fixed
  #costs of setting up the deike heightening as well as the effects of
  #deike height on costs beyond the time horizon are neglected
  total_costs[i,] = costs[i,] + NPV_expected_losses[i,]
  
}

# Baseline model data frame for 4 Objectives
# Load baseline model
source(file = "Scripts/Baseline_curves.R")

Objectives.base <- data.frame(total_costs.base, costs.base, NPV_expected_losses.base, EV_p_exceed_transient.base)

# Load curves averaged for each dike height, X:
source("Scripts/Mean_curves.R")

# Create data frame for mean (expected) objective values
exp_objectives <- data.frame(total_costs[min_ind_mean,], costs[min_ind_mean,], NPV_expected_losses[min_ind_mean,], EV_p_exceed_transient[min_ind_mean,])

# Write table to text file for Sobol Sensitivity Analysis
write.table(exp_objectives, file = "objectiveValues.txt", sep = ' ', col.names = FALSE, row.names = FALSE)
