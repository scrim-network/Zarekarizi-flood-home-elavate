##==============================================================================
##
## Script calculates objectives for one house
## Goals include: (1) Calculating all the objectives for the house including
##                    B/C, total cost, EAD, lifetime expected damages and
##                    costruction cost for all heightening strategies.
##                (2) Plots a three-panel diagram. first diagram is the total cost
##                    Second diagram is the benefit to cost ratio and the third
##                    diagram is safety for a range of heightening strategies.
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
##      1. output includes a plot in Figures directory and saves
##         data (if the user chooses) in /Results_RData/House_case_objectives/
##==============================================================================

# Global variables
main_path=getwd()
set.seed(0)
mygreen <- rgb(44/255, 185/255, 95/255, 1)
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 1)
mygray <- rgb(190/255, 190/255, 190/255, 0.2)
run_function=1 # In case you later want to run the script but not run the optimization function, turn this off
# Load libraries, code, and data required to run this script
library(evd) # We would use pgev, qgev from this package
source(paste(main_path,'/Source_Code/Functions/Cost_Damage_Calculator.R',sep=""))
load(paste(main_path,"/",load_path,"/GEV_Parameters_MCMC.RData",sep=""))
source(paste(main_path,"/Source_Code/Functions/random_discount.R",sep=""))
discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))
source(paste(main_path,"/Source_Code/Functions/House_chars.R",sep=""))

sqft=house_charactersitics()[1,'sqft']
Struc_Value=house_charactersitics()[1,'Struc_Value']
del=house_charactersitics()[1,'del']
life_span=house_charactersitics()[1,'life_span']
disc_rate=house_charactersitics()[1,'disc_rate']

# Functions
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate GEV parameters (choosing the mode; the most probable prediction)
mu=getmode(mu_chain) # Location parameter used for ignoring-uncertainty scenario
xi=getmode(xi_chain) # Shape parameter used for ignoring-uncertainty scenario
sigma=getmode(sigma_chain) # Scale parameter used for ignoring-uncertainty scenario

# Given the above parameters, calculate the base flood elevation. Please note that we calculate the BFE using GEV and we do not use USGS data (in order to be consistent)
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) 


# House initial stage (elevation difference with the benchmark of the river bed)
House_Initial_Stage=BFE+del

# If you already have the results (optimal elevation, cost-benefit analysis, etc. for this specific house, set run_function=0)
if(run_function==1){ # Run the findopt_UNC function stored in Cost_Damage_Calculator file
  returned_data=findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,
                            disc_rate,
                            mu,sigma,xi,mu_chain,sigma_chain,xi_chain,
                            nsow=length(xi_chain),
                            discount,
                            save_return=1,
                            n_strategy=100,
                            verbose=TRUE,
                            ddUnc='deep', #   'eu',
                            lifeUnc='weibull', #   'weibull',
                            drUnc='deep', #    'drift',
                            threshold_totalcost=0.75,
                            test=FALSE)
}


# The file has been saved with this name convention
filename=paste(main_path,"/",load_path,"/House_case_objectives/Decision_Vars_V",toString(trunc(Struc_Value/1000)),"_Sq",toString(trunc(sqft)),"_I",toString(del),".RData",sep="")

# Load the file
load(filename)

############################################################
# Total Cost Plot (Panel A)
############################################################
pdf(paste(main_path,"/Figures/S14_House_Objectives_P1.pdf",sep=""), width =3.94, height =2.43)
#jpeg(paste(main_path,"/Figures/S14_House_Objectives_P1.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
#png(paste(main_path,"/Figures/S14_House_Objectives_P1.png",sep=""),width =3.94, height =2.43,units="in",res=300)
#pdf(paste(main_path,"/Figures/S14_House_Objectivess_P1_v2.pdf",sep=""), width =3.5, height =2.5)

par(cex=0.5)
par(cex=0.5,mai=c(0.09,0.1,0.3,0.1))
par(cex=0.5,fig=c(0.07,0.93,0.07,0.97))

# The upper limit of the y-axis
upper_ylim=max(totcost_unc_max/1000)

# Plot and main lines
plot(delta_h_seq,total_cost_bg/1000,type="n",lwd=1,col="black",
     ylim=c(0,upper_ylim),xlim=c(0,14),
     yaxt="n",xaxt="n",bty="n",xlab="",ylab="")

# Ploygon to show the structure value
if(Struc_Value/1000<upper_ylim){
  #polygon(x=c(0,14,14,0),y=c(Struc_Value/1000,Struc_Value/1000,upper_ylim,upper_ylim),col=mygray,border=NA)
  lines(c(0,14),c(Struc_Value/1000,Struc_Value/1000),col="black",lty=3)
  text(13,Struc_Value/1000+0.05*Struc_Value/1000,"100%",xpd=T)
}
# Polygon to show the infeasable elevation area
polygon(x=c(0,3,3,0),y=c(0,0,upper_ylim,upper_ylim),col=mygray,border = NA,density =30,angle=-45)
polygon(x=c(0,3,3,0),y=c(0,0,upper_ylim,upper_ylim),col=mygray,border = NA,density =30,angle=45)

# Add axes and their labels
axis(2,at=seq(0,upper_ylim,length.out = 5),labels=signif(seq(0,upper_ylim,length.out = 5),2),pos=0)
axis(1,pos=0)
mtext("Heightening [ft]",side=1,line=1.2,cex=0.5)
mtext("Cost [1,000 US$]",side=2,line=1.5,cex=0.5)

# Add an axis on top to show the stage
lines(x=c(0,14),y=c(upper_ylim,upper_ylim))

# Add an axis on the right to show the percent of house value
axis(4,pos=14,at<-seq(0,upper_ylim,length.out = 5),
     labels=round(100*at/(Struc_Value/1000)))
mtext("Total cost as a fraction of house value [%]",side=4,line=1.5,cex=0.5)

# Add polygon for uncertainty in total cost
totcost_unc_max[totcost_unc_max>upper_ylim*1000]=upper_ylim*1000
polygon(x=c(delta_h_seq[1:length(delta_h_seq)],rev(delta_h_seq[1:length(delta_h_seq)])),
        y=c(totcost_unc_max[1:length(delta_h_seq)]/1000,rev(totcost_unc_min[1:length(delta_h_seq)]/1000)),
        col = "#FF666640",border=NA)

# add line for total cost under uncertainty
lines(delta_h_seq[2:length(delta_h_seq)],totcost_unc_mean[2:length(delta_h_seq)]/1000,lty=1,lwd=1,col="red")
lines(delta_h_seq[1:2],totcost_unc_mean[1:2]/1000,lty=1,col="red")

# add the line for total cost under certainty
lines(delta_h_seq[2:length(delta_h_seq)],total_cost_bg[2:length(delta_h_seq)]/1000,lty=2,lwd=1,col="red")
lines(delta_h_seq[1:2],total_cost_bg[1:2]/1000,col="red",lty=2) # This is separate because someone might
# want to exclude elevations between 0 and 3 becuse this range is infeasable. It does not make sense to elevate a house to 1 feet

# add points indicating minimum total cost
points(delta_h_seq[which.min(total_cost_bg)],min(total_cost_bg)/1000,pch=1,col="red",cex=3)
points(opt_height_unc,opt_height_unc_total_cost/1000,pch=20,col="red",cex=4)

# addd damages lines
lines(delta_h_seq,damages_unc_mean/1000,lty=1,lwd=1,col="darkgreen")
lines(delta_h_seq,damages_bg/1000,lty=2,lwd=1,col="darkgreen")

# add line for constructio cost
lines(delta_h_seq[2:length(delta_h_seq)],construction_cost_seq[2:length(delta_h_seq)]/1000,type="l",lwd=1,col="black")

# Add lines to indicate FEMA recommendation, current location of the house
indicator_lines=1
if(indicator_lines==1 & Struc_Value/1000<upper_ylim){
  #lines(x=c(0,14),y=c(Struc_Value/1000,Struc_Value/1000),col="gray",lty=2)
}
if(indicator_lines==1){
  # FEMA's recommendation
  lines(x=c(-del+1.5,-del+1.5),y=c(0,upper_ylim),col="blue",lwd=1,lty=3)
  text(-del+1.5-0.1,upper_ylim-0.15*upper_ylim,"FEMA",adj=c(0,0),srt=90,cex=1)
  
  # BFE
  #lines(x=c(-del,-del),y=c(0,upper_ylim),col="blue",lwd=1,lty=2)
  #text(-del-0.1,upper_ylim-0.15*upper_ylim,"BFE",adj=c(0,0),srt=90,cex=1)
}

legend(0,upper_ylim+0.23*upper_ylim,c("Total cost under uncertainty",'Total cost neglecting uncertainty',
                                      "Construction cost",
                                      "Expected damages neglecting uncertainty",'Expected damages under uncertainty',
                                      "Optimal elevation under uncertainty",'Optimal elevation neglecting uncertainty','90% C.I.'),
       col=c("red","red",'black',"darkgreen","darkgreen","red","red",myred),
       lty=c(1,3,1,3,1,NA,NA,NA),
       lwd=c(1,1,1,1,1,NA,NA,NA)*0.5,
       pch=c(NA,NA,NA,NA,NA,20,1,22),
       #pt.cex=c(NA,NA,NA,NA,NA,1,1,2),
       bty="n",ncol=2,
       pt.bg=c(NA,NA,NA,NA,NA,NA,NA,myred),cex=0.8,xpd=T,pt.lwd =c(NA,NA,NA,NA,NA,1,0.5,1))
text(1.5,upper_ylim-0.15*upper_ylim,'It is not practical\n to elevate a house\n by less than 3 feet',xpd=T,cex=0.7)
text(-0.5,upper_ylim+0.2*upper_ylim,'a)',xpd=T)

dev.off()
############################################################
# Benefit to cost plot (Panel B)
############################################################
pdf(paste(main_path,"/Figures/S14_House_Objectives_P2.pdf",sep=""), width =3.94, height =2.43)
#jpeg(paste(main_path,"/Figures/S14_House_Objectives_P2.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
#png(paste(main_path,"/Figures/S14_House_Objectives_P2.png",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5)
ymax=max(bcr_unc_mean)+0*max(bcr_unc_mean)
ymax=floor(ymax)+1
bcr_unc_max[bcr_unc_max>ymax]=ymax

# prepare the main plot and add axes but dont draw anything
plot(delta_h_seq,bcr_bg,type="n",ylim=c(0,ymax),xlim=c(0,14),xaxt="n",bty="n",yaxt="n",xlab="",ylab="")

# Add a polygon to show the area where benefits are lower than costs and we want to avoid that area
#polygon(x=c(0,14,14,0),y=c(1,1,0,0),col=mygray,border=NA)

# Polygon to show the infeasable elevation area
polygon(x=c(0,3,3,0),y=c(0,0,ymax,ymax),col=mygray,border = NA,density =30,angle=-45)
polygon(x=c(0,3,3,0),y=c(0,0,ymax,ymax),col=mygray,border = NA,density =30,angle=45)

# Show the uncertainty bounds via a polygon
polygon(x=c(delta_h_seq,rev(delta_h_seq)),y=c(bcr_unc_max,rev(bcr_unc_min)),
        col = "#FF666640",border=NA)

# Add axes
axis(2,at=seq(0,ymax,length.out = 5),labels=signif(seq(0,ymax,length.out = 5),2),pos=0)
axis(1,pos=0)
lines(y=c(ymax,ymax),x=c(0,14))
lines(y=c(0,ymax),x=c(14,14))
mtext("Benefit-to-cost ratio",side=2,line=1.5,cex=0.5)
mtext("Heightening [ft]",side=1,line=1.5,cex=0.5)

# FEMA's recommendation
lines(x=c(-del+1.5,-del+1.5),y=c(0,ymax),col="blue",lwd=1,lty=3)
text(-del+1.5-0.1,ymax-0.18*ymax,"FEMA",adj=c(0,0),srt=90,cex=1,col="blue")
lines(x=c(opt_height_unc,opt_height_unc),y=c(0,ymax),lwd=1,lty=3,col="darkgreen")
text(opt_height_unc-0.1,ymax-0.13*ymax,"OPT",adj=c(0,0),srt=90,cex=1,col="darkgreen")

# BFE
#lines(x=c(-del,-del),y=c(0,ymax),col="blue",lwd=1,lty=2)
#text(-del-0.1,ymax-0.15*ymax,"BFE",adj=c(0,0),srt=90,cex=1)

# The a line for b/c with and without uncertainty
lines(delta_h_seq[2:length(delta_h_seq)],bcr_bg[2:length(delta_h_seq)],col="red",lty=2)
lines(delta_h_seq[1:2],bcr_bg[1:2],lty=2,col="red") # This is separated in case someone wants to remove it
lines(delta_h_seq,bcr_unc_mean,type="l",col="red")
lines(x=c(0,14),y=c(1,1),col="black",lwd=1,lty=3)

# Add legend
#legend(6,1.98,c("Expected B/C under uncertainty","B/C ignoring uncertainty","90% C.I."),col=c("red","black",myred),
#       lty=c(1,1,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="n",bg="white",box.col="black",pt.bg=c(NA,NA,myred),cex=0.5)
legend(0.5,2,c("Expected B/C considering\n uncertainty","B/C neglecting uncertainty","90% credible intervals"),
       col=c("red","red",myred),
       lty=c(1,3,NA),
       lwd=c(1,1,NA),
       pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="n",bg="white",box.col="black",pt.bg=c(NA,NA,myred),cex=0.8,ncol=1,xpd=T)
#text(0,ymax+0.05*ymax,paste("House size:",sqft,"[sqft], House initial elevation:",abs(del),"below BFE, House value:",Struc_Value,"U.S.$ ,Best guess lifetime:",life_span,"[years], Best guess discount rate:",disc_rate,'[%/year]'),
#        col="black",cex=0.5,xpd=T,pos=4)
dev.off()
############################################################
# Safety plot (Panel C)
############################################################
pdf(paste(main_path,"/Figures/S14_House_Objectives_P3.pdf",sep=""), width =3.94, height =2.43)
#jpeg(paste(main_path,"/Figures/S14_House_Objectives_P3.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
#png(paste(main_path,"/Figures/S14_House_Objectives_P3.png",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5)
ymax=1

# Prepare the main plot, the axes and labels but dont plot anything yet
plot(delta_h_seq,safety_bg,type="n",ylim=c(0,ymax),xlim=c(0,14),xaxt="n",bty="n",yaxt="n",xlab="",ylab="")

# Polygon to indicate areas where safety is less than 50%
#polygon(x=c(0,14,14,0),y=c(.5,.5,0,0),col="gray",border=NA)

# Polygon to show the infeasable elevation area
polygon(x=c(0,3,3,0),y=c(0,0,1,1),col=mygray,border = NA,density =30,angle=-45)
polygon(x=c(0,3,3,0),y=c(0,0,1,1),col=mygray,border = NA,density =30,angle=45)

# Add axes and their labels
axis(1,pos=0)
axis(2,pos=0,at=seq(0,1,by=0.2),labels = c("0",seq(0.2,1,by=0.2)))
lines(x=c(0,14),y=c(1,1))
lines(x=c(14,14),y=c(0,1))
mtext("Reliability",side=2,line=1.5,cex=0.5)
mtext("Heightening [ft]",side=1,line=1.5,cex=0.5)

# Add the main lines
lines(delta_h_seq[2:length(delta_h_seq)],safety_bg[2:length(delta_h_seq)],col="red",lty=2)
lines(delta_h_seq[1:2],safety_bg[1:2],lty=2,col="red") # This is separated just incase someone needs to remove the first part
# This option is provided because elevating a house to less than 3 feet does not make sense
# Show the uncertainty bounds via a polygon
polygon(x=c(delta_h_seq[1:length(delta_h_seq)],rev(delta_h_seq[1:length(delta_h_seq)])),
        y=c(safety_unc_max[1:length(delta_h_seq)],rev(safety_unc_min[1:length(delta_h_seq)])),
        col = "#FF666640",border=NA)
lines(delta_h_seq[2:length(delta_h_seq)],safety_unc_mean[2:length(delta_h_seq)],type="l",col="red")
lines(delta_h_seq[1:2],safety_unc_mean[1:2],lty=1,col="red")

# FEMA's recommendation
lines(x=c(-del+1.5,-del+1.5),y=c(0,ymax),col="blue",lwd=1,lty=3)
text(-del+1.5-0.1,0+0.01*ymax,"FEMA",adj=c(0,0),srt=90,cex=1,col="blue")
lines(x=c(opt_height_unc,opt_height_unc),y=c(0,ymax),lwd=1,lty=3,col="darkgreen")
text(opt_height_unc-0.1,0+0.01*ymax,"OPT",adj=c(0,0),srt=90,cex=1,col="darkgreen")
# BFE
#lines(x=c(-del,-del),y=c(0,ymax),col="blue",lwd=1,lty=2)
#text(-del-0.1,0+0.01*ymax,"BFE",adj=c(0,0),srt=90,cex=1)

# Add a horizontal line to indicate a safety of 0.5
lines(x=c(0,14),y=c(0.5,0.5),col="black",lwd=1,lty=3)
text(0.5,0.54,'0.5')
# Add legend
legend(9.5,.4,c("Considering\n uncertainty","Neglecting uncertainty","90% credible intervals"),
       col=c("red","red",myred),
       lty=c(1,3,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="n",box.col="black",pt.bg=c(NA,NA,myred),cex=0.8)
#text(0,ymax+0.05*ymax,paste("House size:",sqft,"[sqft], House initial elevation:",abs(del),"below BFE, House value:",Struc_Value,"U.S.$ ,Best guess lifetime:",life_span,"[years], Best guess discount rate:",disc_rate,'[%/year]'),
#     col="black",cex=0.5,xpd=T,pos=4)

dev.off()

############################################################
# Satisficing plot (Panel D)
############################################################
pdf(paste(main_path,"/Figures/S14_House_Objectives_P4.pdf",sep=""), width =3.94, height =2.43)
#jpeg(paste(main_path,"/Figures/S14_House_Objectives_P4.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
#png(paste(main_path,"/Figures/S14_House_Objectives_P4.png",sep=""),width =3.94, height =2.43,units="in",res=300)
#pdf(paste(main_path,"/Figures/S14_House_Objectives_P4_v2.pdf",sep=""), width =3.5, height =2.5)

par(cex=0.5)
ymax=max(max(satisficing_all),max(satisficing_bcr),max(satisficing_safety),max(satisficing_totcost))

# Prepare the main plot, the axes and labels but dont plot anything yet
plot(delta_h_seq,satisficing_all,type="n",ylim=c(0,ymax),xlim=c(0,14),xaxt="n",bty="n",yaxt="n",xlab="",ylab="")

# Add axes and their labels
axis(1,pos=0)
axis(2,pos=0,at=seq(0,100,by=20),labels = c("0",seq(20,100,by=20)))
lines(x=c(0,14),y=c(100,100))
lines(x=c(14,14),y=c(0,100))
mtext("Satisficing [%]",side=2,line=1.5,cex=0.5)
mtext("Heightening [ft]",side=1,line=1.5,cex=0.5)

# Add the main lines
lines(delta_h_seq[1:length(delta_h_seq)],satisficing_all[1:length(delta_h_seq)],col="red",lty=1,lwd=2)
lines(delta_h_seq[1:length(delta_h_seq)],satisficing_bcr[1:length(delta_h_seq)],col="blue",lty=2)
lines(delta_h_seq[1:length(delta_h_seq)],satisficing_totcost[1:length(delta_h_seq)],col="purple",lty=2)
lines(delta_h_seq[1:length(delta_h_seq)],satisficing_safety[1:length(delta_h_seq)],col="orange",lty=2)

#points(delta_h_seq[1:length(delta_h_seq)],satisficing_all[1:length(delta_h_seq)],col="red",lty=1,pch=16)
#points(delta_h_seq[1:length(delta_h_seq)],satisficing_bcr[1:length(delta_h_seq)],col="darkblue",lty=1,pch=16)
#points(delta_h_seq[1:length(delta_h_seq)],satisficing_totcost[1:length(delta_h_seq)],col="black",lty=1,pch=16)
#points(delta_h_seq[1:length(delta_h_seq)],satisficing_safety[1:length(delta_h_seq)],col="darkgreen",lty=1,pch=16)

# FEMA's recommendation 
lines(x=c(-del+1.5,-del+1.5),y=c(0,ymax),col="black",lwd=1,lty=3)
text(-del+1.6,ymax+0.1*ymax,"FEMA's \nrecommendation",adj=c(0,0),srt=0,cex=0.7,xpd=1,col="black")
arrows(x0=-del+1.5,y0=ymax,y1=ymax+ymax*0.1,xpd=T,length = 0.05,col="black")



lines(x=c(opt_height_bg,opt_height_bg),y=c(0,ymax),col="black",lwd=1,lty=3)
text(opt_height_bg+0.1,ymax+0.1*ymax,"Optimal height \nneglecting uncertainty",adj=c(0,0),srt=0,cex=0.7,xpd=1,col="black")
arrows(x0=opt_height_bg,y0=ymax,y1=ymax+ymax*0.1,xpd=T,length = 0.05,col="black")

lines(x=c(opt_height_unc,opt_height_unc),y=c(0,ymax),col="black",lwd=1,lty=3)
text(opt_height_unc,ymax+0.1*ymax,"Optimal height \nconsidering uncertainty",adj=c(0,0),srt=0,cex=0.7,xpd=1,col="black")
arrows(x0=opt_height_unc,y0=ymax,y1=ymax+ymax*0.1,xpd=T,length = 0.05,col="black")

lines(x=c(opt_height_unc,opt_height_unc),y=c(0,ymax),col="gray",lwd=1,lty=3)

# Add legend
legend(0.2,98,c("Satisficing all objectives","Satisficing benefit-to-cost ratio",
                "Satisficing the ratio of \ntotal cost to house value",
                "Satisficing reliability"),
       col=c("red","blue","purple","orange"),
       lty=c(1,2,2,2),lwd=c(2,1,1,1),bty="n",cex=0.7)
#text(0,ymax+0.05*ymax,paste("House size:",sqft,"[sqft], House initial elevation:",abs(del),"below BFE, House value:",Struc_Value,"U.S.$ ,Best guess lifetime:",life_span,"[years], Best guess discount rate:",disc_rate,'[%/year]'),
#     col="black",cex=0.5,xpd=T,pos=4)
text(-0.5,ymax+0.2*ymax,'b)',xpd=T)
dev.off()

