##==============================================================================
##
## Script aims at drawing the plot for discount rate time series   
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
## 1. change the directory to the main folder and execute  
##==============================================================================

# Global variables
main_path=getwd()
set.seed(0)
mygreenFade <- rgb(44/255, 185/255, 95/255, 0.5)
mygreen <- rgb(44/255, 185/255, 95/255, 1)
myblue <- rgb(0/255, 128/255, 1, 1)
myblueFade <- rgb(0/255, 128/255, 1, 0.5)
myredFade <- rgb(1, 102/255, 102/255, 0.5)
myred <- rgb(1, 102/255, 102/255, 1)
ny=100 #Number of years

# Load all the functions needed for PRIM
source(paste(main_path,"/Source_Code/Functions/random_discount.R",sep=""))
discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))
discount[,1]=as.numeric(discount[,1])
library(pracma)

# Run discount rate functions
## mean-reverting discount rate
mrv_disc_unc <- mrv_discount(log(discount[,2]))
mrv_disc_cer <- colMeans(mrv_disc_unc)
mrv_dr_ub <- apply(mrv_disc_unc,2,quantile,1-(0.05/2))
mrv_dr_lb <- apply(mrv_disc_unc,2,quantile,(0.05/2))

## random-walk discount rate
rw_disc_unc <- rw_discount(log(discount[,2]))
rw_disc_cer <- colMeans(rw_disc_unc)
rw_dr_ub <- apply(rw_disc_unc,2,quantile,1-(0.05/2))
rw_dr_lb <- apply(rw_disc_unc,2,quantile,(0.05/2))

## drift discount rate
drft_disc_unc <- drift_discount(log(discount[,2]))
drft_disc_cer <- colMeans(drft_disc_unc)
drft_dr_ub <- apply(drft_disc_unc,2,quantile,1-(0.05/2))
drft_dr_lb <- apply(drft_disc_unc,2,quantile,(0.05/2))

# Historical discount rate from Newell and Pizer
np_data=discount[2:length(discount[,1]),]

#####################
# PLOT ##############
#####################
pdf(paste(main_path,"/Figures/S21_Discount_Rate_Timeseries.pdf",sep=""),width =3.94, height =2.43)
png(paste(main_path,"/Figures/S21_Discount_Rate_Timeseries.png",sep=""),width =3.94, height =2.43,units="in",res=300)

# Properties of the main figure
par(cex=0.5,mai=c(0.09,0.1,0.1,0.1))

# Create the first plot 
par(cex=0.5,fig=c(0.07,0.8,0.1,1))
ymax=max(max(rw_dr_ub),max(mrv_dr_ub),max(drft_dr_ub),max(np_data[,2]))
ymax=ymax+ymax*0.05
#ymin=min(min(dr_rw_lb[2:101]),min(dr_mr_lb[2:101]),min(np_data[,2]))
#ymin=min(min(rw_dr_lb),min(mrv_dr_lb),min(drft_dr_lb),min(np_data[,2]))
#ymin=ymin-ymin*0.5
ymin=0

plot(np_data[,1],np_data[,2],type="n",xlim=c(1800,2118),ylim=c(ymin,ymax),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",
     yaxs="i",xaxs="i")

# Add axes and their labels
axis(1,at=c(seq(1800,2118,by=25),2118),pos=ymin,cex.axis=0.7)
axis(2,pos=1799,at=seq(ymin,ymax,length.out = 5),labels=signif(seq(ymin,ymax,length.out = 5),2),cex.axis=0.7)
lines(x=c(1799,2118),y=c(ymax,ymax))
lines(x=c(2118,2118),y=c(0,ymax))
mtext("Year",side=1,line=2.2,cex=0.5)
mtext("Discount Rate [%/year]",side=2,line=2.2,cex=0.5)
lines(x=c(2018,2018),y=c(ymin,ymax),col="gray")

# Add historical data lines 
lines(np_data[,1],np_data[,2],col="black",lwd=1)

# Add polygons for credible intervals  
polygon(x=c(seq(2019,2118),rev(seq(2019,2118))),y=c(rw_dr_ub,rev(rw_dr_lb)),border = NA,col=myredFade)
polygon(x=c(seq(2019,2118),rev(seq(2019,2118))),y=c(mrv_dr_lb,rev(mrv_dr_ub)),border = NA,col=myblueFade)
polygon(x=c(seq(2019,2118),rev(seq(2019,2118))),y=c(drft_dr_lb,rev(drft_dr_ub)),border = NA,col=mygreenFade)

# Add lines for means 
lines(seq(2019,2118),rw_disc_cer,col="red",lwd=1)
lines(seq(2019,2118),mrv_disc_cer,col="blue",lwd=1)
lines(seq(2019,2118),drft_disc_cer,col="darkgreen",lwd=1)

# Connect 2018 to 2019
lines(x=c(2018,2019),y=c(np_data[which(np_data[,1]==2018),2],rw_disc_cer[1]),col="red")
lines(x=c(2018,2019),y=c(np_data[which(np_data[,1]==2018),2],mrv_disc_cer[1]),col="blue")
lines(x=c(2018,2019),y=c(np_data[which(np_data[,1]==2018),2],drft_disc_cer[1]),col="darkgreen")

# Add 2018
text(2000,ymin+0.3,"2018",col="gray")
text(1830,3.8,"4 [%/year]",col="gray")
text(1830,1.7,"1.5 [%/year]",col="gray")

# Show 4% and 1.5%
lines(c(1800,2118),c(4,4),col="gray",lty=3)
lines(c(1800,2118),c(1.5,1.5),col="gray",lty=3)

# Add legend 
legend(1820,ymax,c("Historical net yield U.S. government bond","Expected value of mean reverting model",
                   "Expected value of random walk model",'Expected value of mean reverting model with a background trend',
                   "90% credible intervals for mean reverting model","90% credible intervals for random walk model",
                   "90% credible intervals for mean reverting model with background trend"),
       col=c("black","blue","red","darkgreen",myblueFade,myredFade,mygreenFade),
       lty=c(1,1,1,1,NA,NA,NA),bty="n",pch=c(NA,NA,NA,NA,22,22,22),pt.cex=c(NA,NA,NA,NA,2,2,2),
       pt.bg = c(NA,NA,NA,NA,myblueFade,myredFade,mygreenFade),cex=0.8)

text(1808,ymax-ymax*0.03,"b)")

par(cex=0.5,fig=c(0.76,1,0.1,1),new=TRUE)

inds_rw=which(rw_disc_unc[,ny] > rw_dr_lb[ny] & rw_disc_unc[,ny] < rw_dr_ub[ny])
hs_rw<-hist(rw_disc_unc[inds_rw,ny],100,plot = FALSE)

inds_mr=which(mrv_disc_unc[,ny]>mrv_dr_lb[ny] & mrv_disc_unc[,ny] < mrv_dr_ub[ny])
hs_mr<-hist(mrv_disc_unc[inds_mr,ny],100,plot = FALSE)

inds_drft=which(drft_disc_unc[,ny]>drft_dr_lb[ny] & drft_disc_unc[,ny] < drft_dr_ub[ny])
hs_drft<-hist(drft_disc_unc[inds_drft,ny],100,plot = FALSE)

xmax=max(max(hs_mr$density),max(hs_rw$density),max(hs_drft$density))
xmin=min(min(hs_mr$density),min(hs_mr$density),min(hs_drft$density))

xmin=0
plot(hs_mr$density,hs_mr$mids,type="n",ylim=c(ymin,ymax),xlim=c(xmin,xmax),xlab="",ylab="",yaxs="i",bty="n",xaxt="n",yaxt="n",yaxs="i")

axis(1,pos=ymin,at=c(xmin,xmax),labels=c((xmin),signif(xmax,2)),cex.axis=0.8)

polygon(x=c(smooth(hs_rw$density,"3RS3R"),rep(0,length(smooth(hs_rw$density,"3RS3R")))),
        y=c(hs_rw$mids,rev(hs_rw$mids)),
        border = NA,col=myredFade)
polygon(x=c(smooth(hs_mr$density,"3RS3R"),rep(0,length(smooth(hs_mr$density,"3RS3R")))),
        y=c(hs_mr$mids,rev(hs_mr$mids)),
        border = NA,col=myblueFade)
polygon(x=c(smooth(hs_drft$density,"3RS3R"),rep(0,length(smooth(hs_drft$density,"3RS3R")))),
        y=c(hs_drft$mids,rev(hs_drft$mids)),
        border = NA,col=mygreenFade)

lines(x=c(xmin,xmin),y=c(ymin,ymax))
lines(x=c(xmin,xmax),y=c(ymax,ymax))
lines(x=c(xmax,xmax),y=c(ymin,ymax))

mtext("Probability Density",side=1,line=2.2,cex=0.5)
lines(x=c(xmin,xmax),y=c(rw_disc_cer[ny],rw_disc_cer[ny]),col="red")
lines(x=c(xmin,xmax),y=c(mrv_disc_cer[ny],mrv_disc_cer[ny]),col="blue")
lines(x=c(xmin,xmax),y=c(drft_disc_cer[ny],drft_disc_cer[ny]),col="darkgreen")
text(xmax-xmax*0.1,ymax-ymax*0.03,"c)")
# Show 4% and 1.5%
lines(c(xmin,xmax),c(4,4),col="gray",lty=3)
lines(c(xmin,xmax),c(1.5,1.5),col="gray",lty=3)

dev.off()
dev.off()
