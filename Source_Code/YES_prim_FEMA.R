##==============================================================================
##
## Script for conducting PRIM analysis and plotting the results. PRIM analysis
## is based on MORDM functions. Functions are stored in a separate file and they
## are called here. 
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
## Load damages, b/c, ..., and optimal elevations of all the houses
load(paste(main_path,"Results_RData/cases_objectives/Hypothetical_community_objectives.RData",sep=""))
## Load all the functions needed for PRIM
source(paste(main_path,"Source_Code/Prim_functions.R",sep=""))

# Start the program 
## Exclude houses that have NaN 
inds=which(    (!is.nan(fema_bc) & !is.na(fema_bc)) & 
               (!is.nan(fema_const_frac) & !is.na(fema_const_frac)) & 
               (!is.nan(fema_safety) & !is.na(fema_safety)))

# Ientify houses in which FEMA recommendation passes the test
## Find houses in which fema_const_frac (cost of elevating to FEMA divided by house value)
## is less than 1 and the B/C ratio is greated than 1. Define a vector that assigns YES (1) or NO (0)
## to each house. If 1, it means that FEMA pases the test for that house.
fema_pass<-rep(NA,length(inds))
for(j in 1:length(inds)){
  i=inds[j]
  if     (fema_bc[i]<1 & fema_const_frac[i]< 1){fema_pass[j]=0}
  else if(fema_bc[i]>1 & fema_const_frac[i]>=1){fema_pass[j]=0}
  else if(fema_bc[i]<1 & fema_const_frac[i]>=1){fema_pass[j]=0}
  else                                         {fema_pass[j]=1}
}

# 
SOWss=SOWs # State of the worlds (house characteristics)
SOWss[,2]=SOWs[,2]/1000 # Divide house values by 1,000 for simplicity
colnames(SOWss)<-c('size[sqft]','value[1,000 US$]','elevation wrt BFE[ft]','lifespan[yrs]') # Define column names

# Plot and save 
pdf(paste(main_path,"Figures/PRIM_FEMA_PASS_NO_PASS.pdf",sep=""),width = 3.94,height=2.83)
par(cex=0.5)
analyze.prim(SOWss[inds,],fema_pass,threshold.type=-1,threshold=0.5)
dev.off()

