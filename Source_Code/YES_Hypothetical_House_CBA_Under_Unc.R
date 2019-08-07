##==============================================================================
##
## Script aims at calculating the expected damages to a hypothetical house 
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
myred <- rgb(1, 102/255, 102/255, 1)

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Switches for this script 
save_plot=1 # Set this to 1 if you would like to save the plots.
run_function=1 # set to 1 if you want to run the "Cost_Damage_Calculator.R"; 
               # otherwise, if you already have the outputs for this specific house and would like to just plot, set to 0


# Load libraries, code, and data required to run this script 
library(evd) # We would use pgev, qgev from this package
source(paste(main_path,'Source_Code/Cost_Damage_Calculator.R',sep=""))
load(paste(main_path,"Results_RData/GEV/GEV_Parameter_Chains.RData",sep=""))

# Functions
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate GEV parameters (choosing the mode; the most probable prediction)
mu=getmode(mu_chain) # Location parameter used for ignoring-uncertainty scenario
xi=getmode(xi_chain) # Shape parameter used for ignoring-uncertainty scenario
sigma=getmode(sigma_chain) # Scale parameter used for ignoring-uncertainty scenario

# Given the above parameters, calculate the base flood elevation. Please note that we calculate the BFE using GEV and we do not use USGS data (in order to be consistent)
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

# In the paper, we are focusing on three specific hypothetical houses. 
# Below is the information of these houses:
houses=rbind(
  c(1500,350000,-5,30), # i=1: Typical house
  c(3708.575257,316329.770178,-6.789922,95.430767), # i=2: Considering uncertainty increases the optimal elevation but then it doesnt pass the B-C test
  c(1000,277000,-4.7,30) # i=3: Considering uncertainty increases the optimal elevation and it totally makes sense.
)

i=3 # Set i to the number of the house you would like to run the program for.
sqft=houses[i,1]
Struc_Value=houses[i,2]
del=houses[i,3]
life_span=houses[i,4] 
disc_rate=0.04

# House initial stage (elevation difference with the benchmark of the river bed)
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

############################################################
############################################################
# PLOTS
############################################################
  if(save_plot==1){ # if save_plot=0, the plots will not be saved and they will pop up in Rstudio or R
    pdf(paste(main_path,"Figures/UNC_SMOOTH_CBA_house_D",del,'_Sq',sqft,'_V',Struc_Value,'_LS',life_span,".pdf",sep=""), width =3.94, height =6.375)
    par(cex=0.5,mai=c(0.09,0.1,0.3,0.1))
  }

  par(cex=0.5,fig=c(0.07,0.93,0.6,0.95))
  
  # The upper limit of the y-axis
  upper_ylim=max(totcost_unc_max/1000)
  
  # Plot and main lines
  plot(delta_h_seq,total_cost_GEVMCMC/1000,type="n",lwd=1,col="black",
       ylim=c(0,upper_ylim),xlim=c(0,14),
       yaxt="n",xaxt="n",bty="n",xlab="",ylab="")
  
  # Ploygon to show the structure value
  polygon(x=c(0,14,14,0),y=c(Struc_Value/1000,Struc_Value/1000,upper_ylim,upper_ylim),col="lightgray",border=NA)
  
  # Polygon to show the infeasable elevation area
  polygon(x=c(0,3,3,0),y=c(0,0,upper_ylim,upper_ylim),col="darkgray",border = NA,density =30,angle=-45)
  polygon(x=c(0,3,3,0),y=c(0,0,upper_ylim,upper_ylim),col="darkgray",border = NA,density =30,angle=45)
  
  # Add axes and their labels
  axis(2,at=seq(0,upper_ylim,length.out = 5),labels=signif(seq(0,upper_ylim,length.out = 5),2),pos=0)
  axis(1,pos=0)
  mtext("Added Height [ft]",side=1,line=1.2,cex=0.5)
  mtext("Cost [1,000 US$]",side=2,line=1.5,cex=0.5)
  
  # If you want to add an axis on top to show the stage
  axis_stage=0
  if(axis_stage==1){
    axis(3,at=0:14,labels=signif((0:14)+House_Initial_Stage,3),main="Stage [ft]")
    mtext("Stage [ft]",side=3,line=2.5,cex=0.5)
  }else{
    lines(x=c(0,14),y=c(upper_ylim,upper_ylim))
  }
  
  # If you want to add an axis on the right to show the percent of house value
  axis_percent_right=1
  if(axis_percent_right==1){
    axis(4,pos=14,at<-seq(0,upper_ylim,length.out = 10),
         labels=round(100*at/(Struc_Value/1000)))
    mtext("Fraction of house value",side=4,line=1.5,cex=0.5)
  }
  
  # Add polygon for uncertainty in total cost 
  polygon(x=c(delta_h_seq[1:length(delta_h_seq)],rev(delta_h_seq[1:length(delta_h_seq)])),y=c(totcost_unc_max[1:length(delta_h_seq)]/1000,rev(totcost_unc_min[1:length(delta_h_seq)]/1000)),
          col = "#FF666640",border=NA)
  
  # add the line for total cost under certainty
  lines(delta_h_seq[2:length(delta_h_seq)],total_cost_GEVMCMC[2:length(delta_h_seq)]/1000,lty=2,lwd=1,col="red")
  lines(delta_h_seq[1:2],total_cost_GEVMCMC[1:2]/1000,col="red",lty=2) # This is separate because someone might 
  # want to exclude elevations between 0 and 3 becuse this range is infeasable. It does not make sense to elevate a house to 1 feet
  
  # add line for total cost under uncertainty 
  lines(delta_h_seq[2:length(delta_h_seq)],totcost_unc_mean[2:length(delta_h_seq)]/1000,lty=1,lwd=1,col="red")
  lines(delta_h_seq[1:2],totcost_unc_mean[1:2]/1000,lty=1,col="red")
  
  # add points indicating minimum total cost
  points(delta_h_seq[which.min(total_cost_GEVMCMC)],min(total_cost_GEVMCMC)/1000,pch=1,col="red",cex=3)
  points(opt_height_unc,opt_height_unc_total_cost/1000,pch=20,col="red",cex=4)
  
  # addd damages lines
  lines(delta_h_seq,damages_unc_mean/1000,lty=1,lwd=1,col="darkgreen")
  lines(delta_h_seq,expected_damages_GEVMCMC_seq/1000,lty=2,lwd=1,col="darkgreen")
  
  # add line for constructio cost
  lines(delta_h_seq[2:length(delta_h_seq)],construction_cost_seq[2:length(delta_h_seq)]/1000,type="l",lwd=1,col="black")
  #lines(delta_h_seq[1:2],construction_cost_seq[1:2]/1000,col="black",lty=3)
  
  
  # If you want to add lines to indicate FEMA recommendation, current location of the house
  indicator_lines=1
  if(indicator_lines==1){
  lines(x=c(0,14),y=c(Struc_Value/1000,Struc_Value/1000),col="gray",lty=2)
    
  # FEMA's recommendation 
  lines(x=c(-del+1,-del+1),y=c(0,upper_ylim),col="blue",lwd=1,lty=2)
  text(-del+1-0.1,upper_ylim-0.15*upper_ylim,"FEMA",adj=c(0,0),srt=90,cex=1)
  
  # BFE
  lines(x=c(-del,-del),y=c(0,upper_ylim),col="blue",lwd=1,lty=2)
  text(-del-0.1,upper_ylim-0.15*upper_ylim,"BFE",adj=c(0,0),srt=90,cex=1)
  }
  
legend_type=2
if(legend_type==1){
         legend("topright",c("Total cost under uncertainty",'Total cost neglecting uncertainty',
                             "Construction cost",
                             "Expected damages under uncertainty",'Expected damages neglecting uncertainty',
                             "Optimal elevation under uncertainty",'Optimal elevation neglecting uncertainty','90% C.I.'),bg="white",
         col=c("black","turquoise4","slateblue4","blue"),
         lwd=c(2,1,1,NA),pch=c(NA,NA,NA,20),bty="o",box.col="black",cex=0.75)
}else if(legend_type==2){
         legend(0,upper_ylim+0.23*upper_ylim,c("Total cost under uncertainty",'Total cost neglecting uncertainty',
                             "Construction cost",
                             "Expected damages neglecting uncertainty",'Expected damages under uncertainty',
                             "Optimal elevation under uncertainty",'Optimal elevation neglecting uncertainty','90% C.I.'),
         col=c("red","red",'black',"darkgreen","darkgreen","red","red",myred),
         lty=c(1,2,1,2,1,NA,NA,NA),
         lwd=c(1,1,1,1,1,NA,NA,NA)*0.5,
         pch=c(NA,NA,NA,NA,NA,20,1,22),
         #pt.cex=c(NA,NA,NA,NA,NA,1,1,2),
         bty="n",ncol=2,
         pt.bg=c(NA,NA,NA,NA,NA,NA,NA,myred),cex=0.8,xpd=T)
}
text(0.5,upper_ylim-0.05*upper_ylim,"a)")
text(1.5,upper_ylim-0.15*upper_ylim,'It is not practical\n to elevate a house\n by less than 3 feet',xpd=T,cex=0.7)
#arrows(1.5,upper_ylim+0.1*upper_ylim,1.5,upper_ylim-0.1*upper_ylim,length = 0.1)
############################################################
############################################################
# Benefit to cost plot
############################################################

  par(fig=c(0.07,0.93,0.35,.6), new=TRUE)
  
  # Calculating benefits and costs
  cb_cost=construction_cost_seq
  cb_benefit=abs(expected_damages_GEVMCMC_seq-expected_damages_GEVMCMC_seq[1])
  cb=cb_benefit/cb_cost
  cb[1]=NA
  ymax=2.5
  cb[1]=0
  cb_unc_mean[1]=0
  cb_unc_max[1]=0
  cb_unc_min[1]=0
  # prepare the main plot and add axes but dont draw anything
  plot(delta_h_seq,cb,type="n",ylim=c(0,ymax),xlim=c(0,14),xaxt="n",bty="n",yaxt="n")
  
  # Add a polygon to show the area where benefits are lower than costs and we want to avoid that area
  polygon(x=c(0,14,14,0),y=c(1,1,0,0),col="lightgray",border=NA)
  
  # Polygon to show the infeasable elevation area
  polygon(x=c(0,3,3,0),y=c(0,0,ymax,ymax),col="darkgray",border = NA,density =30,angle=-45)
  polygon(x=c(0,3,3,0),y=c(0,0,ymax,ymax),col="darkgray",border = NA,density =30,angle=45)
  
  # Add axes 
  axis(2,at=seq(0,ymax,length.out = 5),labels=signif(seq(0,2,length.out = 5),2),pos=0)
  axis(1,pos=0)
  lines(y=c(ymax,ymax),x=c(0,14))  
  lines(y=c(0,ymax),x=c(14,14))  
  mtext("Benefit-cost ratio",side=2,line=1.5,cex=0.5)
  mtext("Added Height [ft]",side=1,line=1.5,cex=0.5)
  
  
  # The a line for b/c with and without uncertainty
  lines(delta_h_seq[2:length(delta_h_seq)],cb[2:length(delta_h_seq)],col="red",lty=2)
  lines(delta_h_seq[1:2],cb[1:2],lty=2) # This is separated in case someone wants to remove it
  lines(delta_h_seq,cb_unc_mean,type="l",col="red")
  lines(x=c(0,14),y=c(1,1),col="gray",lwd=1,lty=2)
  
  # Show the uncertainty bounds via a polygon
    polygon(x=c(delta_h_seq,rev(delta_h_seq)),y=c(cb_unc_max,rev(cb_unc_min)),
          col = "#FF666640",border=NA)
  
  # FEMA's recommendation 
  lines(x=c(-del+1,-del+1),y=c(0,upper_ylim),col="blue",lwd=1,lty=2)
  text(-del+1-0.1,0+0.01*upper_ylim,"FEMA",adj=c(0,0),srt=90,cex=1)
  
  # BFE
  lines(x=c(-del,-del),y=c(0,upper_ylim),col="blue",lwd=1,lty=2)
  text(-del-0.1,0+0.01*upper_ylim,"BFE",adj=c(0,0),srt=90,cex=1)
  
  # Add legend
  #legend(6,1.98,c("Expected B/C under uncertainty","B/C ignoring uncertainty","90% C.I."),col=c("red","black",myred),
  #       lty=c(1,1,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="n",bg="white",box.col="black",pt.bg=c(NA,NA,myred),cex=0.5)
  legend(8,2.5,c("Expected B/C considering\n uncertainty","B/C neglecting uncertainty","90% C.I."),col=c("red","red",myred),
         lty=c(1,2,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="n",bg="white",box.col="black",pt.bg=c(NA,NA,myred),cex=0.8)
  
  text(0.5,ymax-ymax*0.1,"b)")



############################################################
############################################################
# Safety plot
############################################################


  par(fig=c(0.07,0.93,0.1,0.35), new=TRUE)
  
  # Prepare the main plot, the axes and labels but dont plot anything yet
  plot(delta_h_seq,safety_seq,type="n",ylim=c(0,1),xlim=c(0,14),xaxt="n",bty="n",yaxt="n")
  
  # Polygon to indicate areas where safety is less than 50%
  polygon(x=c(0,14,14,0),y=c(.5,.5,0,0),col="gray",border=NA)
  
  # Polygon to show the infeasable elevation area
  polygon(x=c(0,3,3,0),y=c(0,0,1,1),col="darkgray",border = NA,density =30,angle=-45)
  polygon(x=c(0,3,3,0),y=c(0,0,1,1),col="darkgray",border = NA,density =30,angle=45)
  
  # Add axes and their labels
  axis(1,pos=0)
  axis(2,pos=0)
  lines(x=c(0,14),y=c(1,1))
  lines(x=c(14,14),y=c(0,1))
  mtext("Probability of no floods over a 30 year mortgage",side=2,line=1.5,cex=0.5)
  mtext("Added Height [ft]",side=1,line=1.5,cex=0.5)
  
  # Add the main lines
  lines(delta_h_seq[2:length(delta_h_seq)],safety_seq[2:length(delta_h_seq)],col="red",lty=2)
  lines(delta_h_seq[1:2],safety_seq[1:2],lty=2,col="red") # This is separated just incase someone needs to remove the first part
  # This option is provided because elevating a house to less than 3 feet does not make sense
  # Show the uncertainty bounds via a polygon
  polygon(x=c(delta_h_seq[1:length(delta_h_seq)],rev(delta_h_seq[1:length(delta_h_seq)])),
          y=c(safety_unc_max[1:length(delta_h_seq)],rev(safety_unc_min[1:length(delta_h_seq)])),
          col = "#FF666640",border=NA)
  lines(delta_h_seq[2:length(delta_h_seq)],safety_unc_mean[2:length(delta_h_seq)],type="l",col="red")
  lines(delta_h_seq[1:2],safety_unc_mean[1:2],lty=1,col="red")
  
  # FEMA's recommendation 
  lines(x=c(-del+1,-del+1),y=c(0,upper_ylim),col="blue",lwd=1,lty=2)
  text(-del+1-0.1,0+0.01*upper_ylim,"FEMA",adj=c(0,0),srt=90,cex=1)
  
  # BFE
  lines(x=c(-del,-del),y=c(0,upper_ylim),col="blue",lwd=1,lty=2)
  text(-del-0.1,0+0.01*upper_ylim,"BFE",adj=c(0,0),srt=90,cex=1)
  
  # Add a horizontal line to indicate a safety of 0.5
  lines(x=c(0,14),y=c(0.5,0.5),col="lightgray",lwd=1,lty=2)
  
 # Add legend
# legend(6,0.98,c("Expected safety under uncertainty","Safety under certainty","90% C.I."),col=c("red","black",myred),
#        lty=c(1,1,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="n",box.col="black",pt.bg=c(NA,NA,myred),cex=0.5)
 legend(8,.5,c("Expected safety considering\n uncertainty","Safety neglecting uncertainty","90% C.I."),col=c("red","red",myred),
        lty=c(1,2,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="n",box.col="black",pt.bg=c(NA,NA,myred),cex=0.8)
 
 # Add panel number
 text(0.5,.9,"c)")
 
if(save_plot==1){
  dev.off()
}
