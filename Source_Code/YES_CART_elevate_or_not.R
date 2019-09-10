##==============================================================================
##
## Script plots 
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

# Load libraries, data, and functions 
library(rpart)
library(rpart.plot)
load(paste(main_path,"Results_RData/cases_objectives/Hypothetical_community_objectives.RData",sep=""))

SOWss=SOWs
SOWss[,2]=SOWs[,2]/1000
elevate<-elevate_char<-rep(NA,length(SOWs[,1]))
for(i in 1:length(SOWss[,1])){
  if(optunc_height[i]>0){elevate[i]=1}
  else if(optunc_height[i]==0){elevate[i]=0}
}
for(i in 1:length(SOWss[,2])){
  if(elevate[i]==1){elevate_char[i]="RIASE"}
  else if(elevate[i]==0){elevate_char[i]="NO RAISE"}
}

mydf=cbind.data.frame(SOWss,elevate_char)
colnames(mydf)<-c('size[sqft]','value[1,000 US$]','elevation wrt BFE[ft]','lifespan[yrs]','Elevate')

z.auto <- rpart(Elevate ~ .,mydf)
z.auto
post(z.auto,file="",title. = "",horizontal = F,bg="red",fg="blue",colormodel="gray")

pdf(paste(main_path,"Figures/CART_house_characteristics_elevate.pdf",sep=""),width = 3.94,height=2.83)
par(cex=0.7)
rpart.plot(z.auto)
dev.off()
