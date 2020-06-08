##==============================================================================
##
## Script for a 2D plot of trade-offs between upfront investment and expected damages
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
##      1. output includes a plot in Figures directory 
##==============================================================================

# Global variables
main_path=getwd()
mygreen <- rgb(44/255, 185/255, 95/255, 1) #Colors we will use
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 0.4)
mygray <- rgb(190/255, 190/255, 190/255, 1)
mygray="gray"
run_function=1

# Required libraries, data, and functions 
library(evd) # We would use pgev, qgev from this package
library(plotrix) # For axis.break 
load(paste(main_path,"/",load_path,"/GEV_Parameters_MCMC.RData",sep="")) #Load GEV parameters 
source(paste(main_path,'/Source_Code/Functions/Cost_Damage_Calculator.R',sep=""))
discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))
source(paste(main_path,'/Source_Code/Functions/MAP_function.R',sep=""))

# Calculate GEV parameters (choosing the MAP; the best-guess)
pars_hat = find_MAP(mu_chain,sigma_chain,xi_chain)
mu = pars_hat[1] # Location parameter used for ignoring-uncertainty scenario
xi = pars_hat[3] # Shape parameter used for ignoring-uncertainty scenario
sigma = pars_hat[2] # Scale parameter used for ignoring-uncertainty scenario

# Given the above parameters, calculate the base flood elevation
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

source(paste(main_path,"/Source_Code/Functions/House_chars.R",sep=""))

sqft=house_charactersitics()[1,'sqft']
Struc_Value=house_charactersitics()[1,'Struc_Value']
del=house_charactersitics()[1,'del']
life_span=house_charactersitics()[1,'life_span']
disc_rate=house_charactersitics()[1,'disc_rate']

# House initial stage 
House_Initial_Stage=BFE+del

# If you already have the results (optimal elevation, cost-benefit analysis, etc. for this specific house, set run_function=0)
if(run_function==1){ # Run the findopt_UNC function stored in Cost_Damage_Calculator file
  returned_data=findopt_UNC(sqft,
                            Struc_Value,
                            del,
                            House_Initial_Stage,
                            life_span,
                            disc_rate,
                            mu,sigma,xi,mu_chain,sigma_chain,xi_chain,
                            nsow=length(xi_chain),
                            discount,
                            save_return=1,
                            n_strategy=100,
                            verbose=TRUE,
                            ddUnc='deep',
                            lifeUnc='weibull',
                            drUnc='deep',
                            threshold_totalcost=0.75,
                            test=FALSE
  )  
}

# The file has been saved with this name convention
filename=paste(main_path,"/",load_path,"/House_case_objectives/Decision_Vars_V",toString(trunc(Struc_Value/1000)),"_Sq",toString(trunc(sqft)),"_I",toString(del),".RData",sep="")

# Load the file
load(filename)

# Start the plot
pdf(paste(main_path,"/Figures/S16_Tradeoffs_damages_investment.pdf",sep=""), width =2.43, height =2.43)
png(paste(main_path,"/Figures/S16_Tradeoffs_damages_investment.png",sep=""), width =2.43, height =2.43,units="in",res=300)

par(cex=0.5) #Adjust the size 

# xrange is defined here for simplicity of the rest of the code. This is to exclude elevations less than 3 feet 
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
y1=floor(min(damages_bg)/1000)
y2=(max(damages_unc_mean[xrange])/1000)
y3=y2+0.1*y2
y4=y3+0.1*y3
yp1=(y2+y3)/2
yp2=(y3+y4)/2

# Start plotting
plot(y=damages_bg[xrange]/1000,
     x=construction_cost_seq[xrange]/1000,
     type="n",xaxt="n",yaxt="n",bty="n",
     xlim=c(x1,x4),ylim=c(y1,y4),
     xlab='',ylab='',yaxs="i",xaxs="i")

mtext("Expected damages [1,000 US$]",side=2,line=2,cex=0.5)
mtext("Upfront investment [1,000 US$]",side=1,line=2,cex=0.5)

# Draw the trade-off line (with uncertainty) and make it dashed whenever it does not pass the cost-benefit analysis
v=1*(bcr_unc_mean>1)

if(is.na(bcr_unc_mean[1]) | is.nan(bcr_unc_mean[1])){
  inds=c(2,1+which(diff(v)!=0),length(v))
}else{
  inds=c(1,1+which(diff(v)!=0),length(v))
}

for(i in 1:(length(inds)-1)){
  if(v[inds[i]]==0){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,damages_unc_mean[inds[i]:inds[i+1]]/1000,lty=3,col="red")
  }else if(v[inds[i]]==1){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,damages_unc_mean[inds[i]:inds[i+1]]/1000,lty=1,col="red")
  }
}

# Draw the trade-off line (with uncertainty) and make it dashed whenever it does not pass the cost-benefit analysis
v=1*(bcr_bg>1)
if(is.na(bcr_bg[1]) | is.nan(bcr_bg[1])){
  inds=c(2,1+which(diff(v)!=0),length(v))
}else{
  inds=c(1,1+which(diff(v)!=0),length(v))
}

for(i in 1:(length(inds)-1)){
  if(v[inds[i]]==0){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,damages_bg[inds[i]:inds[i+1]]/1000,lty=3,col="blue")
  }else if(v[inds[i]]==1){
    lines(construction_cost_seq[inds[i]:inds[i+1]]/1000,damages_bg[inds[i]:inds[i+1]]/1000,lty=1,col="blue")
  }
}
# Indicate the 0-3 feet area that is inpractical 
polygon(x=c(x1,x3,x3,x1),y=c(y1,y1,y4,y4),border = NA,col="gray")

# Add axes
axis(1,at=c(x1,seq(x3,x4,length.out = 5)),labels=signif(c(0,seq(x3,x4,length.out = 5)),2),pos=0)
axis.break(1,pos=0,x2,bgcol=rgb(0,0,0,alpha = 0),style = 'slash',brw=0.02)
axis(2,pos=x1,at=c(seq(y1,y2,length.out = 5),y3,y4),labels = signif(c(seq(y1,y2,length.out = 5),damages_unc_mean[1]/1000,damages_bg[1]/1000),2))
lines(x=c(x4,x4),y=c(y1,y4))
lines(x=c(x1,x4),y=c(y4,y4))
axis.break(2,pos=x1,yp1,bgcol=rgb(0,0,0,alpha=0),style = 'slash',brw=0.01)
axis.break(2,pos=x1,yp2,bgcol=rgb(0,0,0,alpha=0),style = 'slash',brw=.01)

# Add the point that indicates the expected damages without raising the house
points(x=x1,y=y4,col="red",xpd=T,pch=20)

# Add the point that indicates the expected damages without raising the house
points(x=x1,y=y3,col="blue",pch=20,xpd=T)

# Add other texts to the plot
text(x2,(y1+y4)/2,'It is not practical to \nraise a house by less than 3 feet',srt=90,cex=0.6)

# Add points for optimal policies without uncertainty 
mymin=which.min(total_cost_bg)
if(mymin==1){
  points(x=x1,y=y3,pch=0,cex=2,col="blue",xpd=T)
}else{
  points(x=construction_cost_seq[mymin]/1000,y=damages_bg[mymin]/1000,pch=0,cex=2,col="blue",xpd=T)
}

# Add points for optimal policies with uncertainty 
mymin=which.min(totcost_unc_mean)
if(mymin==1){
  points(x=x1,y=y4,pch=0,cex=2,col="red")
}else{
  points(x=construction_cost_seq[mymin]/1000,y=damages_unc_mean[mymin]/1000,pch=0,cex=2,col="red")
}

points(FEMA_Recomm_cost/1000,FEMA_Recomm_damage/1000,pch=9,col="darkorchid3",cex=2)
points(x1,0,pch=8,cex=2,xpd=T,col="darkgoldenrod2")

# Add legend 
legend(x4-0.15*x4,y4-0.02*y4,c('Considering uncertainty','Neglecting uncertainty','Passes B/C test','Does not pass B/C test',
                              'Economic optimal policy','"No action" expected damages','Infeasible ideal point','FEMA Recommendation'),
       col=c('red','blue','black','black','black','black','darkgoldenrod2','darkorchid3'),
       lty=c(1,1,1,3,NA,NA,NA,NA),pch=c(NA,NA,NA,NA,0,20,8,9),bty="l",box.lwd=0.5,box.col="gray",cex=0.6,pt.cex = c(NA,NA,NA,NA,1,1,1,1),pt.lwd = c(NA,NA,NA,NA,1,1,0.5,0.5))


dev.off()
dev.off()

