###################################
# file: Baseline_curves.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Generates baseline model curves for model objectives
# Used to plot best guess curves against uncertainty
#################################### 

# Define variables
p_exceed.base = array(NA,dim=c(length(X)))
costs.base = array(NA,dim=c(length(X)))
NPV_expected_losses.base = array(NA,dim=c(length(X)))
EV_p_exceed_transient.base = array(NA,dim=c(length(X)))
Total_flood_frequency.base = array(NA,dim=c(length(X)))
total_costs.base = array(NA,dim=c(length(X)))

discount_factor.base = array(NA, dim=c(length(time)))
effective_height.base = array(NA, dim=c(length(X),length(time)))
p_exceed_transient.base = array(NA, dim=c(length(X),length(time)))
NPV_costs_flooding.base = array(NA, dim=c(length(X),length(time)))

subsidence.base = subs_rate*time
sea_level_rise.base = sea_level_rate*time

for(i in 1:length(X)) {
  # analyze for each considered Deike heightening (eq. 1 in paper)
  p_exceed.base[i]= p_zero_p*exp(-alpha_p*X[i])
  
  for (j in 1:length(time)) {
    
    # analze for each year of the initial time period
    year =j-1
    
    #losses in the future are discounted at the annual net discount rate
    discount_factor.base[j]=1/(1+delta_prime_p)^year
    
    #The effective deike height decreases with subsidence and sea-level rise.
    effective_height.base[i,j]=X[i]-subsidence.base[j]-sea_level_rise.base[j]
    
    
    # For a stationary flooding frequency, we can evaluate the annual flooding
    #frequency with the old observeations and the new effective height.
    p_exceed_transient.base[i,j]= p_zero_p*exp(-alpha_p*effective_height.base[i,j])
    
    
    #The net present value of the losses per year are the product of the
    #frequency of flooding per year, the damages per flood, and the discount factor.
    NPV_costs_flooding.base[i,j]=p_exceed_transient.base[i,j]*V_p*discount_factor.base[j]
  }
  
  #The costs of building the dike increase linearly with respect to height.
  costs.base[i]=k_p*X[i]
  
  #The total discounted expected losses are the sum of the discounted expected annual losses.
  NPV_expected_losses.base[i]=sum(NPV_costs_flooding.base[i,])
  
  #The average annual flood frequency is the mean the annual flood frequencies.
  EV_p_exceed_transient.base[i]=mean(p_exceed_transient.base[i,])
  
  #The total flood frequency over the life-time of the project is the sum of the flood frequencies,
  #assuminG independence, as in the original paper
  Total_flood_frequency.base[i]=sum(p_exceed_transient.base[i,])
  
  #The total costs that depend on the deike height. Note that the fixed
  #costs of setting up the deike heightening as well as the effects of
  #deike height on costs beyond the time horizon are neglected
  total_costs.base[i]=costs.base[i]+NPV_expected_losses.base[i]
  
}

min_ind=seq(along=total_costs.base)[total_costs.base == min(total_costs.base)]
min_cost_X=X[min_ind]
