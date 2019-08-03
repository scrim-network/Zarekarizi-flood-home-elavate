###################################################
# file: Estimate_GEV_MLE_RecoWL.R
#
# - estimates the parameters of a stationary GEV 
#   model using regular MLE method.
###################################################
# Authored by: Mahkameh Zarekarizi and Klaus Keller
# mahkameh.zare@gmail.com, klaus@psu.edu
# Pennsylvania State University
#
# Distributed under the GNU general public license
# No warranty
#
###################################################
# Last changes: August 3, 2019
###################################################



# This script is written by Mahkameh Zarekarizi for 

# Change the directory
rm(list=ls())
setwd("~/Documents/Research/House_Elevation_Project/Source_Code")

# Load the USGS streamflow, and water level from rating curve 
load("~/Documents/Research/House_Elevation_Project/data/AnnMaxWL.RData")

# First, let's use ismev package. Later, I will add other packages too
library(ismev)
fitting_info<-gev.fit(AnnMaxWL[,2])
fitted_params<-fitting_info$mle

# Location parameter
fitted_location<-fitted_params[1]
# Scale parameter 
fitted_scale<-fitted_params[2]
# Shape parameter 
fitted_shape<-fitted_params[3]

### Fit GEV of residuals ###
library(extRemes)
library(fExtremes)
library(ismev)

year.res.max.fit <- fevd(AnnMaxWL[,2])   # extRemes package
year.res.max.fit2 <- gevFit(AnnMaxWL[,2])   # fExtremes package
year.res.max.fit3 <- gev.fit(AnnMaxWL[,2], show = FALSE)   # ismev package

year.return <- return.level(year.res.max.fit, return.period = c(2:10000), alpha = 0.1, do.ci = TRUE)



# Print the results
print(rbind(c("ISMEV Location","ISMEV scale","ISMEV shape"),c(fitted_location,fitted_scale,fitted_shape)))
save(list=ls(),file="~/Documents/Research/House_Elevation_Project/data/GEV_MLE_Parameters.RData")
