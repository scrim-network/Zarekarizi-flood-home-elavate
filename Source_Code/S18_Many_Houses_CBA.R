##==============================================================================
##
## Script generates many houses and analyzes objectives for each of those houses. 
## Running the model for all houses takes a significantly large amount of time. 
## Therefore, in cases that you have already run the model and care about just 
## the plots, you can set this to 0. To decrease the computational time, you can
## also consider decreasing number of houses (default is 10000) and the number of 
## strategies (default is 100). 
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
##      1. output includes a data file in "Results_RData/cases_objectives" folder
##==============================================================================
 
# Global variables
main_path=getwd()
set.seed(0)
mygreen <- rgb(44/255, 185/255, 95/255, 1) 
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 1)
disc_rate=0.04 # discount rate is costant for all houses 
life_span=30
discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))
source(paste(main_path,"/Source_Code/Functions/random_discount.R",sep=""))

n_houses=1000
n_strategy=10

# Load libraries, data, and functions 
library(evd) # We would use pgev, qgev from this package
require(lhs)
source(paste(main_path,'/Source_Code/Functions/Cost_Damage_Calculator.R',sep=""))
load(paste(main_path,"/",load_path,"/GEV_Parameters_MCMC.RData",sep=""))
source(paste(main_path,'/Source_Code/Functions/MAP_function.R',sep=""))

# Calculate GEV parameters (choosing the MAP; the best-guess)
pars_hat = find_MAP(mu_chain,sigma_chain,xi_chain)
mu = pars_hat[1] # Location parameter used for ignoring-uncertainty scenario
xi = pars_hat[3] # Shape parameter used for ignoring-uncertainty scenario
sigma = pars_hat[2] # Scale parameter used for ignoring-uncertainty scenario

# Estimate the base flood elevation based on the chain 
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

set.seed(0)

z<- randomLHS(n_houses, 3)
SOWs=matrix(NA,n_houses,3)
SOWs[,1] <- qunif(z[,1],100,5000) #sqft
SOWs[,2] <- qunif(z[,2],10000,1000000) # value 
SOWs[,3] <- qunif(z[,3],-10,-3) # del

fema_height   <- fema_totcost   <- fema_safety   <- fema_bc   <- fema_totcost_frac   <- fema_const_frac   <-
opt_height    <- opt_totcost    <- opt_safety    <- opt_bc    <- opt_totcost_frac    <- opt_const_frac    <-
optunc_height <- optunc_totcost <- optunc_safety <- optunc_bc <- optunc_totcost_frac <- optunc_const_frac <- rep(NA,n_houses)

for(i in 1:n_houses){
  print(paste(100*i/n_houses,'% completed',sep="")) #This would show the progress of the code 
  tmp=findopt_UNC(SOWs[i,1], #sqft
                  SOWs[i,2], #value 
                  SOWs[i,3], #del 
                  (SOWs[i,3]+BFE),
                  life_span,
                  disc_rate,
                  mu,sigma,xi,
                  mu_chain,sigma_chain,xi_chain,
                  length(mu_chain),
                  discount,
                  save_return=0, ##save_return=If set as 1, all the returned variables will be saved and also be printed; otherwise it will only be printed
                  n_strategy, #number os elevation strategies to be explored 
                  verbose=FALSE, #set is T if you would like to print the progress of the program
                  ddUnc="deep", #Uncertainty type in depth-damage function. Options= "deep", "eu", and "hazus" 
                  lifeUnc="weibull", # Type of uncertainty in the house lifetime. options: "30yr", "50yr", and "deep"
                  drUnc="deep", # Type of discount rate uncertainty options: "rw", "mrv", "drift", and "deep"
                  threshold_totalcost=0.75,
                  test=FALSE)
  
  
  fema_height[i]=tmp[2,1]
  fema_totcost[i]=tmp[2,2]
  fema_safety[i]=tmp[2,3]
  fema_bc[i]=tmp[2,4]
  fema_totcost_frac[i]=tmp[2,5]
  fema_const_frac[i]=tmp[2,6]
  
  opt_height[i]=tmp[3,1]
  opt_totcost[i]=tmp[3,2]
  opt_safety[i]=tmp[3,3]
  opt_bc[i]=tmp[3,4]
  opt_totcost_frac[i]=tmp[3,5]
  opt_const_frac[i]=tmp[3,6]
  
  optunc_height[i]=tmp[4,1]
  optunc_totcost[i]=tmp[4,2]
  optunc_safety[i]=tmp[4,3]
  optunc_bc[i]=tmp[4,4]
  optunc_totcost_frac[i]=tmp[4,5]
  optunc_const_frac[i]=tmp[4,6]
}

# Save the results
save(SOWs, #All state of the worlds (size, value, elevation wrt BFE, and lifespan)
     fema_height, #FEMA's recommended added height 
     fema_totcost, #Total cost if elevated to FEMA's recommendation 
     fema_safety, #Safety of the house if elevated to FEMA's recommendation
     fema_bc, #B/C of FEMA's recommendation
     fema_totcost_frac, #Total cost (expected damages + cost of elevating) of FEMA's recommendation 
     fema_const_frac, #The construction cost over house value if elevated to FEMA's recommendation
     opt_height, #Optimal height ignoring uncertainty
     opt_totcost, #Total cost if elevated to ignoring-uncertainty elevation 
     opt_safety, #Safety if elevated to ignoring-uncertainty elevation 
     opt_bc, #Benefit to cost ratio if elevated to ignoring-uncertainty elevation 
     opt_totcost_frac, #Total cost as a fraction of house value if elevated to ignoring-uncertainty elevation 
     opt_const_frac, #Construction cost as a fraction of house value if elevated to ignoring-uncertainty elevation 
     optunc_height, #Optimal height under uncertainty
     optunc_totcost, #Total cost if elevated to under-uncertainty recommended elevation 
     optunc_safety, #Safety if elevated to under-uncertainty recommended elevation 
     optunc_bc, #B/C of under-uncertainty strategy 
     optunc_totcost_frac, #Total cost as a fraction of house value if elevated to under-uncertainty recommended elevation 
     optunc_const_frac, #Construction cost as a fraction of house value if elevated to under-uncertainty recommended elevation
     file=paste(main_path,"/Output_Data/House_case_objectives/Hypothetical_houses.RData",sep=""))
