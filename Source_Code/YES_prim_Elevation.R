##==============================================================================
##
## Script plots the XLRM diagram for the home elevation problem
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
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

# Load data and libraries
load(paste(main_path,"Results_RData/cases_objectives/Hypothetical_community_objectives.RData",sep=""))
source(paste(main_path,"Source_Code/Prim_functions.R",sep=""))

# Ientify houses for which elevating is recommended
## Find houses in which recommeded elevation under uncertainty is greater than zero
## Define a vector (bool_elevate) that assigns YES (1) or NO (0)
## to each house. If 1, it means that elevating is recommended (i.e. the optimal elevation is greater than 0).
bool_elevate<-rep(NA,length(optunc_height))
for(i in 1:length(optunc_height)){
  if     (optunc_height[i]>0.0001){bool_elevate[i]=1}
  else                       {bool_elevate[i]=0}
}

SOWss=SOWs # State of the worlds (house characteristics)
SOWss[,2]=SOWs[,2]/1000 # Divide house values by 1,000 for simplicity
colnames(SOWss)<-c('size[sqft]','value[1,000 US$]','elevation wrt BFE[ft]','lifespan[yrs]') # Define column names

# Plot and save 
pdf(paste(main_path,"Figures/PRIM_elevation.pdf",sep=""),width = 3.94,height=2.83)
par(cex=0.5)
analyze.prim(SOWss[,],bool_elevate,threshold.type=-1,threshold=0.5)
dev.off()

