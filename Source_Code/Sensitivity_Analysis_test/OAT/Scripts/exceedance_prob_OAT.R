###################################
# file: exceedance_prob_OAT.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Generates survival function curves
# for exceedance probability estimations
# for use in OAT analysis
#################################### 

library(fExtremes)

# Determine exceedance probabilities by generating survival curves for each GEV parameter set
q = seq(0,1, length.out = 10^4+2)
length_param = length(Parameters[,1])

gev <- sapply(1:length_param, function(x){
  qgev(q, Parameters$xi[x], Parameters$mu[x], Parameters$sigma[x])
})

gev <- gev[2:(length(gev[,1])-1),]
q <- seq(0,1, length.out = length(gev[,1]))

min_es <- sapply(1:length_param, function(x){
  min(gev[,x]/100)
})

max_es <- sapply(1:length_param, function(x){
  max(gev[,x]/100)
})

p_exceed <- mat.or.vec(length_param, 1)
p_index <- mat.or.vec(length_param, 1)

# Function for Initial Exceedance Frequency
exceedance_prob <- function(X){

    p_exceed <- mat.or.vec(length_param, 1)

    p_exceed <- sapply(1:length_param, function(i) {
      if(X <= min_es[i]) {
        return(1)
      } 
      
      if(X >= max_es[i]) {
        return(0)
      } 
      
      if(X > min_es[i] && X < max_es[i]) {
        return(1-q[which.min(abs(gev[,i]/100 - X))])
      }
  })
    
    return(p_exceed)
    
}


# Function for Exceedance Probability at Effective Dike Heights
exceedance_effective <- function(X){
  
  p_effective <- mat.or.vec(length_param, 1)
  
  p_effective <- sapply(1:length_param, function(i) {
    if(X <= min_es[i]) {
      return(1)
    } 
    
    if(X >= max_es[i]) {
      return(0)
    } 
    
    if(X > min_es[i] && X < max_es[i]) {
      return(1-q[which.min(abs(gev[,i]/100 - X))])
    }
  })
  
  return(p_exceed)
  
}