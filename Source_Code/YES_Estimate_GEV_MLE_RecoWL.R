##==============================================================================
##
## Script estimates the parameters of a stationary GEV 
#   model using regular MLE method.
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
##
## Last changes: August 4, 2019
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

# Load the USGS streamflow, and water level from rating curve 
load(paste(main_path,"Data/AnnMaxWL.RData",sep=""))

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
stationary_MLE_params=rbind(c("ISMEV Location","ISMEV scale","ISMEV shape"),c(fitted_location,fitted_scale,fitted_shape))
save(stationary_MLE_params,file=paste(main_path,"Results_RData/GEV_MLE_Parameters.RData",sep=""))
