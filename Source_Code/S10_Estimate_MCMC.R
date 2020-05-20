##==============================================================================
##
## Script estimates GEV parameters using MCMC 
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
##      1. output includes a data file GEV_Parameter_Chains.RData in /Results_RData  
##==============================================================================

# Global variables
main_path=getwd()

# Load the libraries required in this script  
library(evdbayes)
library(ismev)


#--------------------------------------------------------------
# Functions----------------------------------------------------
#--------------------------------------------------------------
## functions
source(paste(main_path,'/Source_Code/Functions/MAP_function.R',sep=""))

# Load Annual Maximum Data  
load(paste(main_path,"/",load_path,"/AnnMaxWL.RData",sep=""))

# Estimate parameters 
set.seed(1)

mat <- diag(c(1000,100,1))
pn <- prior.norm(mean = c(0,0,0), cov = mat)
pos<-posterior(50000, init = c(5,1,0.1), prior = pn, lh = "gev",data = AnnMaxWL[,2],psd = c(0.2,.1,.1))

mu=(pos[,1])
logsigma=(pos[,2])
xi=(pos[,3])

# Choose the last 10,000 iterations
mu_chain <-mu[(length(xi)-10000+1):length(xi)]
xi_chain <- xi[(length(xi)-10000+1):length(xi)]
sigma_chain <- logsigma[(length(logsigma)-10000+1):length(logsigma)]

# print MAP estimates 
par_hats = find_MAP(mu_chain, sigma_chain, xi_chain)
mu_hat = par_hats[1]
sigma_hat = par_hats[2]
xi_hat = par_hats[3]
print(paste('MAP estimated parameters are: Mu=',mu_hat,'sigma=',sigma_hat,'xi=',xi_hat))

# Save MCMC chains 
save(mu_chain,xi_chain,sigma_chain,file=paste(main_path,"/Output_Data/GEV_Parameters_MCMC.RData",sep=""))
