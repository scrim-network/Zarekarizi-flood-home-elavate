##==============================================================================
##
## Script aims at classifying houses in which FEMA's recommendation does not pass 
## the benefit-to-cost test. For this, we use CART analysis  
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
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
##      1. output includes one plot in Figures directory 
##==============================================================================

# Global variables
main_path=getwd()
set.seed(0)
# Load libraries needed in this script 
library(rpart)
library(rpart.plot)
## Load optimal elevations, damages and costs for all the sample houses
load(paste(main_path,"/",load_path,"/House_case_objectives/Hypothetical_houses.RData",sep=""))

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
colnames(mydf)<-c('size[sqft]','value[1,000 US$]','elevation wrt BFE[ft]','pass')

z.auto <- rpart(pass ~ .,mydf)
z.auto
post(z.auto,file="",title. = "",horizontal = F,bg="red",fg="blue",colormodel="gray")

# Start the plot 
pdf(paste(main_path,"/Figures/S20_CART_FEMA.pdf",sep=""),width = 3.94,height=2.43)
#jpeg(paste(main_path,"/Figures/S20_CART_FEMA.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.7)
rpart.plot(z.auto)
dev.off()
