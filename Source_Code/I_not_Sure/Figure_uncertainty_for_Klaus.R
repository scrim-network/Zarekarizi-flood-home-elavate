# This script is written by Mahkameh Zarekarizi to calculate the expected damages to a hypothetical house 
# This house worth 350,000 USD
# This house is located 1 feet below the base flood elevation 
# We assume it is located right next to our USGS gage
# BFE here is 449.2 ft
# House level is 439.2 ft
# The house is 1500 square feet

# EAD is the expected annual damage
# EAD is the probability of a certain flood times its associated damages
rm(list=ls())
library(evd) # We would use pgev, qgev from this package
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')

save_plot=1
plot_without_reliability=TRUE
plot_cb=1
plot_safety=1

# Global variables
#Struc_Value=277000 #USD
#sqft=1000
#del=-6
#Struc_Value=350000 #USD
#sqft=1500
#del=-6.343
#341.768978 532667.855229     -2.326931
Struc_Value=277000 #USD
sqft=1000
del=-4.7
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

run_function=0
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")
mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3
# FEMA_Return_periods=c(2,5,10,25,50,100,500)
# FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)
House_Initial_Stage=BFE+del
life_span=30
disc_rate=0.04

#mu=mean(mu_chain)
#xi=mean(xi_chain)
#sigma=mean(sigma_chain) 



if(run_function==1){
  findopt_UNC(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi,mu_chain,sigma_chain,xi_chain,10000,safety_level=0)
}
filename=paste("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/UNC-SR_V",toString(trunc(Struc_Value/1000)),"Sq",toString(trunc(sqft)),"I",toString(del),".RData",sep="")
load(filename)

source("../Figures/mycolors.R")

if(save_plot==1){
  pdf(paste("Figure/UNC_for_klaus",del,".pdf",sep=""), width =3.94, height =6.375)
  
  par(cex=0.5,mai=c(0.09,0.1,0.3,0.1))
}
par(cex=0.5,fig=c(0.07,0.93,0.6,0.95))
plot(delta_h_seq,total_cost_GEVMCMC/1000,type="n",lwd=1,col="black",
     #ylim=c(0,1.3*max(totcost_unc_max/1000)),
     ylim=c(0,200),
     xlab="Added Height [ft]",ylab="Cost [1,000 US$]",xlim=c(0,14),yaxt="n",xaxt="n",bty="n")
axis(2,line=-2,pos=0)
axis(1,line=-2,pos=0)
lines(y=c(200,200),x=c(0,14))  

polygon(x=c(delta_h_seq[2:length(delta_h_seq)],rev(delta_h_seq[2:length(delta_h_seq)])),
        y=c(construction_cost_seq[2:length(delta_h_seq)]/1000,rev(totcost_unc_mean[2:length(delta_h_seq)]/1000)),col=myred,border="NA")

polygon(x=c(delta_h_seq[2:length(delta_h_seq)],rev(delta_h_seq[2:length(delta_h_seq)])),
        y=c(construction_cost_seq[2:length(delta_h_seq)]/1000,rev(total_cost_GEVMCMC[2:length(delta_h_seq)]/1000)),col=mygreen,border="NA")


#polygon(x=c(-1,30,30,-1),y=c(Struc_Value/1000,Struc_Value/1000,Struc_Value*10/1000,Struc_Value*10/1000),col="gray",border=NA)

#polygon(x=c(delta_h_seq[1:length(delta_h_seq)],rev(delta_h_seq[1:length(delta_h_seq)])),y=c(totcost_unc_max[1:length(delta_h_seq)]/1000,rev(totcost_unc_min[1:length(delta_h_seq)]/1000)),
#        col = "#FF666640",border=NA)
# add the line for total cost under certainty
lines(delta_h_seq[2:length(delta_h_seq)],total_cost_GEVMCMC[2:length(delta_h_seq)]/1000,lty=1,lwd=1,col="darkgreen")
#lines(delta_h_seq[1:2],total_cost_GEVMCMC[1:2]/1000,col="black",lty=2)
# add line for total cost under uncertainty 
lines(delta_h_seq[2:length(delta_h_seq)],totcost_unc_mean[2:length(delta_h_seq)]/1000,lty=1,lwd=1,col="red")
#lines(delta_h_seq[1:2],totcost_unc_mean[1:2]/1000,lty=2,col="red")

# add points indicating minimum total cost
points(delta_h_seq[which.min(total_cost_GEVMCMC)],min(total_cost_GEVMCMC)/1000,pch=20,col="darkgreen",cex=3)
points(opt_height_unc,opt_height_unc_total_cost/1000,pch=20,col="red",cex=4)

#lines(delta_h_seq,damages_unc_mean/1000,lty=1,lwd=1,col="red")

#lines(delta_h_seq,expected_damages_GEVMCMC_seq/1000,lty=2,lwd=1,col="red")
lines(delta_h_seq[2:length(delta_h_seq)],construction_cost_seq[2:length(delta_h_seq)]/1000,type="l",lwd=1,col="black")
points(delta_h_seq[1],construction_cost_seq[1]/1000,col="black")

#abline(h=Struc_Value/1000,col="blue",lty=2)
abline(v=-del+1,col="blue",lwd=1,lty=2)
#abline(v=-del,col="black",lwd=1,lty=3)
#abline(v=0,col="black",lwd=1,lty=4)

#axis(3,at=0:14,labels=signif((0:14)+House_Initial_Stage,3),main="Stage [ft]")

# axis(4,pos=14,at=seq(0,max(totcost_unc_max/1000),length.out = 10)
#     ,labels=round(100*seq(0,max(max(construction_cost_seq/1000),max(expected_damages_GEVMCMC_seq/1000),max(total_cost_GEVMCMC/1000)),length.out = 10)/(Struc_Value/1000)))
axis(4,pos=14,at=seq(0,200,length.out = 10)
     ,labels=paste(round(100*seq(0,200,length.out = 10)/(Struc_Value/1000)),"%",sep=""))

#mtext("Stage [ft]",side=3,line=2.5,cex=0.5)
mtext("Fraction of house value",side=4,line=1.5,cex=0.5)
mtext("Added Height [ft]",side=1,line=1.2,cex=0.5)
mtext("Cost [1,000 US$]",side=2,line=1.5,cex=0.5)
text(10,210,"FEMA recommendation",xpd=TRUE,col="blue")
arrows(7.8,205, x1 =5.7, y1 =195, length = 0.05, angle = 30,col="blue",xpd=TRUE)

text(-del,210,"Base Flood Elevation",xpd=TRUE,col="black")
lines(c(-del,-del),c(195,205),xpd=TRUE,col="black")
arrows(5,480, x1 =6, y1 =450, length = 0.05, angle = 30,col="black",xpd=TRUE)

text(11,180,"Economically optimal \n policy considering uncertainty",xpd=TRUE,col="red")
arrows(9.5,163, x1 =8.9, y1 =140, length = 0.03, angle = 30,col="red",xpd=TRUE)

text(1.5,230,"Economically optimal \n policy neglecting uncertainty",xpd=TRUE,col="darkgreen")
arrows(2,210, x1 =0.12, y1 =122, length = 0.03, angle = 30,col="darkgreen",xpd=TRUE)






#text(2.2,105,"Risk considering uncertainty",xpd=TRUE,col="red",srt=-42)
#text(2,52,"Risk neglecting uncertainty",xpd=TRUE,col="red",srt=-42)

# text(6,52,"Risk considering uncertainty",xpd=TRUE,col="red",srt=-18)




#  legend("topright",c("Total cost","Construction cost","Expected damages in 30 years","Cost-benefit optimal elevation"),bg="white",
#         #legend(7.5,145,c("Total cost","Construction cost","Expected damages in 30 years","Cost-benefit optimal elevation"),bg="white",
#         col=c("black","turquoise4","slateblue4","blue"),
#         lwd=c(2,1,1,NA),pch=c(NA,NA,NA,20),bty="o",box.col="black")
legend(7.5,100,c("Total cost considering uncertainty","Total cost neglecting uncertainty",
                 "Construction cost"),
       col=c("red","darkgreen","black"),
       lty=c(1,1,1),lwd=c(1,1,1),pch=c(NA,NA,NA),pt.cex=c(NA,NA,NA),bty="n",pt.bg=c(NA,NA,NA),cex=0.91)
#  legend("topright",c("Expected total costs under uncertainty","Total costs ignoring uncertainty","90% C.I.","Optimal elevation under uncertainty",
#                      "Optimal elevation under certainty","Construction cost","Expected damages ignoring uncertainty"),
#         col=c("red","black",myred,"red","black","turquoise4","slateblue4"),
#         lty=c(1,1,NA,NA,NA,1,1),lwd=c(1,1,NA,NA,NA,1,1),pch=c(NA,NA,22,20,20,NA,NA),pt.cex=c(NA,NA,2,1,1,NA,NA),bty="n",pt.bg=c(NA,NA,myred,NA,NA,NA,NA))

#text(-0.1,0,"Initial house elevation",adj=c(0,0),srt=90,cex=1)
#text(-del-0.1,0,"BFE",adj=c(0,0),srt=90,cex=1)
#text(-del+1-0.1,0,"FEMA",adj=c(0,0),srt=90,cex=1)
par(new=TRUE)
#legend(0,200," ",bg="white",xpd=TRUE)
text(0.5,185,"a)")


if(plot_cb==1){
  par(fig=c(0.07,0.93,0.35,.6), new=TRUE)
  cb_cost=construction_cost_seq
  #cb_benefit=abs(total_cost_GEVMCMC-total_cost_GEVMCMC[1])
  cb_benefit=abs(expected_damages_GEVMCMC_seq-expected_damages_GEVMCMC_seq[1])
  cb=cb_benefit/cb_cost
  #cb[1]=NA
  plot(delta_h_seq,cb,type="n",ylim=c(0,2),xlim=c(0,14),xaxt="n",bty="n",yaxt="n")
  axis(2,at=seq(0,2,length.out = 5),labels=signif(seq(0,2,length.out = 5),2),pos=0)
  axis(1,pos=0)
  lines(y=c(2,2),x=c(0,14))  
  lines(y=c(0,2),x=c(14,14))  
  
  polygon(x=c(0,14,14,0),y=c(1,1,0,0),col="lightgray",border=NA)
  lines(delta_h_seq[2:length(delta_h_seq)],cb[2:length(delta_h_seq)])
  lines(delta_h_seq[1:2],cb[1:2],lty=2)
  
  #polygon(x=c(delta_h_seq,rev(delta_h_seq)),y=c(cb_unc_max,rev(cb_unc_min)),
  #        col = "#FF666640",border=NA)
  
  lines(delta_h_seq,cb_unc_mean,type="l",col="red")
  #abline(h=1,col="gray",lwd=1,lty=2)
  abline(v=-del+1,col="blue",lwd=1,lty=2)
  #abline(v=-del,col="black",lwd=1,lty=3)
  #abline(v=0,col="black",lwd=1,lty=2)
  #  legend("topleft",c("Expected B/C under uncertainty","B/C under certainty","90% C.I."),col=c("red","black",myred),
  #         lty=c(1,1,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="o",bg="white",box.col="black",pt.bg=c(NA,NA,myred))
  mtext("Benefit-cost ratio",side=2,line=1.5,cex=0.5)
  mtext("Added Height [ft]",side=1,line=1.5,cex=0.5)
  #par(new=TRUE)
  #legend(0,2," ",bg="white",xpd=TRUE)
  text(0.5,1.85,"b)")
  text(2.5,1.1,"B/C considering uncertainty",xpd=TRUE,col="red")
  text(2.5,0.5,"B/C neglecting uncertainty",xpd=TRUE,col="black")
  
  
}

if(plot_safety==1){
  par(fig=c(0.07,0.93,0.1,0.35), new=TRUE)
  plot(delta_h_seq,safety_seq,type="n",ylim=c(0,1),xlim=c(0,14),xaxt="n",bty="n",yaxt="n")
  axis(1,pos=0)
  axis(2,pos=0)
  lines(x=c(0,14),y=c(1,1))
  lines(x=c(14,14),y=c(0,1))
  #polygon(x=c(-1,30,30,-1),y=c(.5,.5,-1,-1),col="gray",border=NA)
  lines(delta_h_seq[2:length(delta_h_seq)],safety_seq[2:length(delta_h_seq)])
  lines(delta_h_seq[1:2],safety_seq[1:2],lty=1)
  
  abline(v=-del+1,col="blue",lwd=1,lty=2)
  #abline(v=-del,col="black",lwd=1,lty=3)
  #abline(v=0,col="black",lwd=1,lty=4)
  #abline(h=0.5,col="blue",lwd=1,lty=2)
  #polygon(x=c(delta_h_seq[1:length(delta_h_seq)],rev(delta_h_seq[1:length(delta_h_seq)])),
  #        y=c(safety_unc_max[1:length(delta_h_seq)],rev(safety_unc_min[1:length(delta_h_seq)])),
  #        col = "#FF666640",border=NA)
  lines(delta_h_seq[2:length(delta_h_seq)],safety_unc_mean[2:length(delta_h_seq)],type="l",col="red")
  lines(delta_h_seq[1:2],safety_unc_mean[1:2],lty=1,col="red")
  
  mtext("Probability of no floods over a 30 year mortgage",side=2,line=1.5,cex=0.5)
  mtext("Added Height [ft]",side=1,line=1.5,cex=0.5)
  #  legend("topleft",c("Expected safety under uncertainty","Safety under certainty","90% C.I."),col=c("red","black",myred),
  #         lty=c(1,1,NA),lwd=c(1,1,NA),pch=c(NA,NA,22),pt.cex=c(NA,NA,2),bty="o",bg="white",box.col="black",pt.bg=c(NA,NA,myred))
  #par(new=TRUE)
  #legend(0,1," ",bg="white",xpd=TRUE)
  text(0.5,.9,"c)")
  text(3.1,.8,"Neglecting uncertainty",xpd=TRUE,col="black")
  text(10,0.7,"Considering uncertainty",xpd=TRUE,col="red")
  
  
}
if(save_plot==1){
  dev.off()
}
# This block of code is just for a presentation figure. Nothing important
# pdf(paste("Figure/damages_uncertainty",del,".pdf",sep=""), width =3.94, height =2.43)
#   par(cex=0.5)
#   plot(delta_h_seq,damages_unc_mean/1000,type="n",
#        ylim=c(min(damages_unc_min)/1000,max(damages_unc_max)/1000),xlab="",ylab="")
#   
#   polygon(x=c(-1,30,30,-1),y=c(Struc_Value/1000,Struc_Value/1000,Struc_Value*10/1000,Struc_Value*10/1000),col="gray",border=NA)
#   polygon(x=c(delta_h_seq,rev(delta_h_seq)),y=c(damages_unc_max/1000,rev(damages_unc_min/1000)),
#           col = "#FF666640",border=NA)
#     lines(delta_h_seq,construction_cost_seq/1000,type="l",lwd=1,col="turquoise4")
#   
#   lines(delta_h_seq,damages_unc_mean/1000,col="red")
#   legend("topright",c("Construction cost","Expected damages","Damages 90% C.I."),bg="white",
#   col=c("turquoise4","red",myred),
#   lwd=c(1,1,NA),pch=c(NA,NA,22),bty="n",box.col="n",pt.bg=c(NA,NA,myred),pt.cex=c(1,1,2))
#   mtext("Added Height [ft]",side=1,line=2.5,cex=0.5)
#   mtext("Cost [1,000 US$]",side=2,line=2.5,cex=0.5)
#   
#   dev.off()

