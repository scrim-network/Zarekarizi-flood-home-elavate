###################################
# file: upper.diag.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Function to convert second-order output of SALib analysis
# into upper-triangular matrix for radial convergence plotting
#################################### 

upper.diag<-function(x){
  m<-(-1+sqrt(1+8*length(x)))/2
  X<-lower.tri(matrix(NA,m,m),diag=TRUE)
  X[X==TRUE]<-x
  X[upper.tri(X, diag = FALSE)] <- NaN
  t(X)
}