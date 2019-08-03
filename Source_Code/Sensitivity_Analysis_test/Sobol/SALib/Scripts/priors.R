###################################
# file: priors.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Prior parameter values for van Dantzig (1956) model
#################################### 
Struc_Value=350000
del=-5
life_span=30
disc_rate=0.04
mu=19.85
sigma=3.24
xi=0.024
# Create data frame of prior values
priors = data.frame(Struc_Value,del,life_span,disc_rate,mu,sigma,xi)#, V_p, delta_prime_p, k_p, subs_rate, sea_level_rate)