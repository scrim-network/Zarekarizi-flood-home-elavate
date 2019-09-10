##==============================================================================
##
## Script plots the 
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
library(rpart)
library(rpart.plot)

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Load libraries, data, and functions 
## Load optimal elevations, damages and costs for all the sample houses
load(paste(main_path,"Results_RData/cases_objectives/Hypothetical_community_objectives.RData",sep=""))
mymat=cbind(SOWs,(fema_bc>1))

inds=which(    (!is.nan(fema_bc) & !is.na(fema_bc)) & 
               (!is.nan(fema_const_frac) & !is.na(fema_const_frac)) & 
                 (!is.nan(fema_safety) & !is.na(fema_safety)) 
               )

fema_pass<-fema_pass_char<-rep(NA,length(inds))
for(j in 1:length(inds)){
  i=inds[j]
  if(fema_bc[i]<1 & fema_const_frac[i]< 1){fema_pass[j]=1}
  else if(fema_bc[i]>1 & fema_const_frac[i]>=1){fema_pass[j]=2}
  else if(fema_bc[i]<1 & fema_const_frac[i]>=1){fema_pass[j]=3}
  else{fema_pass[j]=4}
}

for(i in 1:length(inds)){
  if(fema_pass[i]==1){fema_pass_char[i]="NO PASS"}
  else if(fema_pass[i]==2){fema_pass_char[i]="NO PASS"}
  else if(fema_pass[i]==3){fema_pass_char[i]="NO PASS"}
  else if(fema_pass[i]==4){fema_pass_char[i]="PASS"}
}

# fema_pass[i]=toString(sum(fema_bc[inds[i]]>1))
SOWss=SOWs
SOWss[,2]=SOWs[,2]/1000
mydf=cbind.data.frame(SOWss[inds,],fema_pass_char)
colnames(mydf)<-c('size[sqft]','value[1,000 US$]','elevation wrt BFE[ft]','lifespan[yrs]','pass')

z.auto <- rpart(pass ~ .,mydf)
z.auto
post(z.auto,file="",title. = "",horizontal = F,bg="red",fg="blue",colormodel="gray")

pdf(paste(main_path,"Figures/houses_fema_no_pass.pdf",sep=""),width = 3.94,height=2.83)
par(cex=0.7)
rpart.plot(z.auto)
dev.off()
