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
mygreen <- rgb(44/255, 185/255, 95/255, 1) 
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 0.4)
save_plot=1
plot_cb=1
plot_safety=1
run_function=0

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Required libraries, data, and functions 
library(evd) # We would use pgev, qgev from this package
library(plotrix) # For axis.break 
load(paste(main_path,"Results_RData/GEV/GEV_Parameter_Chains.RData",sep=""))
source(paste(main_path,'Source_Code/Cost_Damage_Calculator.R',sep=""))

# Define new functions 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate GEV parameters (choosing the mode; the most probable prediction)
mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

# Given the above parameters, calculate the base flood elevation
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

# Other parameters and house characteristics 
sqft=1500
Struc_Value=350000 #USD
del=-5
life_span=30
disc_rate=0.04

# House initial stage 
House_Initial_Stage=BFE+del

# If you already have the results (optimal elevation, cost-benefit analysis, etc. for this specific house, set run_function=0)
if(run_function==1){ # Run the findopt_UNC function stored in Cost_Damage_Calculator file
  returned_data=findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,10000,safety_level=0,save_return=1)
  print(returned_data)
}

# The file has been saved with this name convention
filename=paste(main_path,"Results_RData/cases_objectives/UNC-SR_V",toString(trunc(Struc_Value/1000)),"Sq",toString(trunc(sqft)),"I",toString(del),".RData",sep="")
# Load the file
load(filename)

# Calculate the benefit-to-cost ratio for ignoring uncertainty case. This is not automatically calculated in the findopt_UNC function.
cb_cost=construction_cost_seq
cb_benefit=(expected_damages_GEVMCMC_seq[1]-expected_damages_GEVMCMC_seq)
cb=cb_benefit/cb_cost


if(save_plot==1){
pdf(paste(main_path,"Figures/Tradeoffs_D",del,'_Sq',sqft,'_V',Struc_Value,'_LS',life_span,".pdf",sep=""), width =3.94, height =2.34)
par(cex=0.5)
}

# xrange is defined here for simplicity od the rest of the code. This is to exclude elevations less than 3 feet 
xrange=2:length(delta_h_seq)

# x1 is the point on x axis that is indicated as '0' (but it is not zero and its real value is x1)
x1=(min(construction_cost_seq[xrange])-0.05*(min(construction_cost_seq[xrange])))/1000

# x3 is where the trade-off plots start
x3=min(construction_cost_seq[xrange])/1000

# x2 is the point where the axis break will be
x2=(x1+x3)/2

# x4 is the last point on x-axis
x4=max(construction_cost_seq)/1000

# minimum and maximum points on y-axis
y1=floor(min(expected_damages_GEVMCMC_seq)/1000)
y2=(max(damages_unc_mean[xrange])/1000)
y3=y2+0.1*y2
y4=y3+0.1*y3
yp1=(y2+y3)/2
yp2=(y3+y4)/2

# Start plotting
plot(y=expected_damages_GEVMCMC_seq[xrange]/1000,
     x=construction_cost_seq[xrange]/1000,
     type="n",xaxt="n",yaxt="n",bty="n",
     xlim=c(x1,x4),ylim=c(y1,y4),
     xlab='',ylab='')

#axis(3,at=construction_cost_seq[c(1,2,11,31)]/1000,labels=signif(delta_h_seq[c(1,2,11,31)],3))
mtext("Expected damages [1,000 US$]",side=2,line=1.5,cex=0.5)
mtext("Upfront investment [1,000 US$]",side=1,line=1.5,cex=0.5)

# Indicate the 0-3 feet area that is inpractical 
polygon(x=c(x1,x3,x3,x1),y=c(y1,y1,y4,y4),border = NA,col="gray")


# Add axes
axis(1,at=c(x1,seq(x3,x4,length.out = 5)),labels=signif(c(0,seq(x3,x4,length.out = 5)),2),pos=0)
axis.break(1,pos=0,x2,bgcol=rgb(0,0,0,alpha = 0),style = 'slash',brw=0.02)
axis(2,pos=x1,at=c(seq(y1,y2,length.out = 5),y3,y4),labels = signif(c(seq(y1,y2,length.out = 5),damages_unc_mean[1]/1000,expected_damages_GEVMCMC_seq[1]/1000),2))
lines(x=c(x4,x4),y=c(y1,y4))
lines(x=c(x1,x4),y=c(y4,y4))
axis.break(2,pos=x1,yp1,bgcol=rgb(0,0,0,alpha=0),style = 'slash',brw=0.01)
axis.break(2,pos=x1,yp2,bgcol=rgb(0,0,0,alpha=0),style = 'slash',brw=.01)

# Draw the trade-off line (with uncertainty) and make it dashed whenever it does not pass the cost-benefit analysis
v=1*(cb_unc_mean>1)

if(is.na(cb_unc_mean[1]) | is.nan(cb_unc_mean[1])){
inds=c(2,1+which(diff(v)!=0),length(v))
}else{
inds=c(1,1+which(diff(v)!=0),length(v))
}

for(i in 1:(length(inds)-1)){
  if(v[inds[i]]==0){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,damages_unc_mean[inds[i]:inds[i+1]]/1000,lty=2,col="red")
  }else if(v[inds[i]]==1){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,damages_unc_mean[inds[i]:inds[i+1]]/1000,lty=1,col="red")
  }
}
    # Add the point that indicates the expected damages without raising the house
    points(x=x1,y=y4,col="red",xpd=T,pch=20)


# Draw the trade-off line (with uncertainty) and make it dashed whenever it does not pass the cost-benefit analysis
v=1*(cb>1)
if(is.na(cb[1]) | is.nan(cb[1])){
  inds=c(2,1+which(diff(v)!=0),length(v))
}else{
  inds=c(1,1+which(diff(v)!=0),length(v))
}

for(i in 1:(length(inds)-1)){
  if(v[inds[i]]==0){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,expected_damages_GEVMCMC_seq[inds[i]:inds[i+1]]/1000,lty=2,col="blue")
  }else if(v[inds[i]]==1){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,expected_damages_GEVMCMC_seq[inds[i]:inds[i+1]]/1000,lty=1,col="blue")
  }
}
    # Add the point that indicates the expected damages without raising the house
    points(x=x1,y=y3,col="blue",pch=20)

# Add other texts to the plot
text(x2,(y1+y4)/2,'It is not practical to \nraise a house by less than 3 feet',srt=90,cex=0.6)



# Add points for optimal policies without uncertainty 
mymin=which.min(total_cost_GEVMCMC)
if(mymin==1){
  points(x=x1,y=y3,pch=0,cex=2,col="blue")
}else{
  points(x=construction_cost_seq[mymin]/1000,y=expected_damages_GEVMCMC_seq[mymin]/1000,pch=0,cex=2,col="blue")
}


# Add points for optimal policies with uncertainty 
mymin=which.min(totcost_unc_mean)
if(mymin==1){
  points(x=x1,y=y4,pch=0,cex=2,col="red")
}else{
  points(x=construction_cost_seq[mymin]/1000,y=damages_unc_mean[mymin]/1000,pch=0,cex=2,col="red")
}

# Add legend 
legend(x4-0.14*x4,y4-0.05*y4,c('Considering uncertainty','Neglecting uncertainty','Passes B/C test','Does not pass B/C test','Economic optimal policy','"No action" expected damages'),
       col=c('red','blue','black','black','black','black'),lty=c(1,1,1,2,NA,NA),pch=c(NA,NA,NA,NA,0,20),bty="n")

dev.off()


