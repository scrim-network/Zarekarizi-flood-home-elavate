###################################
# file: slr_vanDantzig.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Sea level rise module for van Dantzig analysis
# Based on rejection sampling framework:
# Lempert et al. 2012 - Characterizing Uncertain Sea Level Rise Projection
#################################### 

# Define function for global sea level rise
sea_level_global <- function(a,      # sea level anomaly [m] at t_0
                             b,      # initial rate      [m/a]
                             c,      # accelartion       [m/a^2]
                             c_star, # abrupt increase of rate [m/a]
                             t_star, # timing of abrupt rate increase
                             t) {
  sea_level_global <- a + b*t + c*(t^2) + c_star * ( (sign(t - t_star) + 1) / 2 ) * (t - t_star)
  
  return(sea_level_global)  
}

