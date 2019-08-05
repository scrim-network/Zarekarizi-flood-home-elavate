##==============================================================================
##
## Script plots the cost of elevating a house with regards to House size and the added elevation
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

# Add the script "Cost_Damage_Calculator.R" to the directory. The cost-damage 
# function exist in that script 
source(paste(main_path,'Source_Code/Damages/Cost_Damage_Calculator.R',sep=""))

# Create a vector of possible house size values 
sqft_seq=seq(50,50000,length.out=100)

# create a vector of plausible added elevatuion values
delta_seq=seq(0.00000001,14,length.out=100)

# Calculate the cost of elevating a house 
rise_cost=matrix(NA,length(sqft_seq),length(delta_seq))

for(i in 1:length(sqft_seq)){
  for(j in 1:length(delta_seq)){
    rise_cost[i,j]=Rising_Cost(sqft_seq[i],delta_seq[j])  
}}

## simple plot of raising cost!
rise_cost_rate=rep(NA,length(delta_seq))
for(j in 1:length(delta_seq)){
  rise_cost_rate[j]=Rising_Cost(1500,delta_seq[j])  
}
pdf(paste(main_path,"Figures/Cost_of_elevating.pdf",sep=""), width =3.94, height =2.43)
par(cex=0.5) 
plot(delta_seq,rise_cost_rate/1000,type="l",xlab="Added height [ft]",ylab=expression("Elevating cost rate[1,000 US$]"))
dev.off()
