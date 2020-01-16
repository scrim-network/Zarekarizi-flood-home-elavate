##==============================================================================
##
## Script estimates the parameters of a stationary GEV 
#   model using regular MLE method.
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##
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
## Instructions to run:
## 1. If you have not already done so, change the working directory to the main 
##    folder (Zarekarizi-Home-Elevation)
##    To do so:
##      1. If on RStudio, open the README.md file. Then on the menu bar, go to 
##         Session-->Set Working Directory-->To Source File Location
##      2. If on RStudio, on the lower right box, open "Zarekarizi-Home-Elevation"
##         Then, click on More --> Set as Working Directory
##      3. On console, type the following command: 
##         setwd("~/.../../../../Zarekarizi-Home-Elevation") 
## 2. To Run:
##      1. Click on Source button or, on console, type: Source("../../....R")
## 3. Outputs:
##      1. output includes a data file GEV_MLE_Parameters.RData in /Results_RData  
##==============================================================================

# Global variables
main_path=getwd()

# Load annual maximum water level data  
# it is saved in Raw_Data folder. You must have run S8_Gage_Height_Annual_Maximum.R before this script
load(paste(main_path,"/",load_path,"/AnnMaxWL.RData",sep=""))

# We use ismev package. 
library(ismev)
fitting_info<-gev.fit(AnnMaxWL[,2])
fitted_params<-fitting_info$mle

# Location parameter
fitted_location<-fitted_params[1]
# Scale parameter 
fitted_scale<-fitted_params[2]
# Shape parameter 
fitted_shape<-fitted_params[3]

# Print the results
MLE_params=rbind(c("ISMEV Location","ISMEV scale","ISMEV shape"),c(fitted_location,fitted_scale,fitted_shape))
print(MLE_params)

# Save the parameters
save(MLE_params,file=paste(main_path,"/Output_Data/GEV_Parameters_MLE.RData",sep=""))
