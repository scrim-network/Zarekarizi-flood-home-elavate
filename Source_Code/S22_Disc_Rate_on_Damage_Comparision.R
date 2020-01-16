##==============================================================================
##
## Goal: plot the figure that compares the damages under different discount rate modles  
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
## Change the directory to the main folder and execute 
##==============================================================================

# Global variables
main_path=getwd()
set.seed(0)

mygreen <- rgb(44/255, 185/255, 95/255, 0.5)
myblue <- rgb(0/255, 128/255, 1, 0.5)
myred <- rgb(1, 102/255, 102/255, 0.5)
lifespan=30
# Load all the functions needed for PRIM
source(paste(main_path,"/Source_Code/Functions/random_discount.R",sep=""))
source(paste(main_path,"/Source_Code/Functions/EAD_Function.R",sep=""))
discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))

library(evd) # We would use pgev, qgev from this package

# Run discount rate functions
## mean-reverting discount rate
mrv_disc_unc <- mrv_discount(log(discount[,2]),lifespan)
mrv_df_unc <- compute_dfactors(mrv_disc_unc)

## random-walk discount rate
rw_disc_unc <- rw_discount(log(discount[,2]),lifespan)
rw_df_unc <- compute_dfactors(rw_disc_unc)

## drift discount rate
drft_disc_unc <- drift_discount(log(discount[,2]),lifespan)
drft_df_unc <- compute_dfactors(drft_disc_unc)

fixed_df<-c(1,exp(-1*cumsum(rep(0.04,lifespan))))
fixed_df=sum(fixed_df)

fixed_df_15<-c(1,exp(-1*cumsum(rep(0.015,lifespan))))
fixed_df_15=sum(fixed_df_15)

h1<-hist(apply(rw_df_unc,1,sum),col=myblue,30)
h2<-hist(apply(mrv_df_unc,1,sum),col=myred,30)
h3<-hist(apply(drft_df_unc,1,sum),col=mygreen,30)

#hist(100*EAD*apply(rw_df_unc,1,sum)/house_value,col=myblue,30)
# Functions
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
load(paste(main_path,"/",load_path,"/GEV_Parameters_MCMC.RData",sep=""))

mu=getmode(mu_chain) # Location parameter used for ignoring-uncertainty scenario
xi=getmode(xi_chain) # Shape parameter used for ignoring-uncertainty scenario
sigma=getmode(sigma_chain) # Scale parameter used for ignoring-uncertainty scenario

source(paste(main_path,"/Source_Code/Functions/House_chars.R",sep=""))
sqft=house_charactersitics()[1,'sqft']
house_value=house_charactersitics()[1,'Struc_Value']
del=house_charactersitics()[1,'del']

BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu)
House_Initial_Stage=BFE+del
EAD=EAD_Certainty(house_value,House_Initial_Stage,0,mu,sigma,xi)

xmax=max(max(h1$mids),max(h2$mids),max(h3$mids),fixed_df,fixed_df_15)
xmin=min(min(h1$mids),min(h2$mids),min(h3$mids),fixed_df,fixed_df_15)
xmin=xmin-0.05*xmin
ymax=max(max(h1$density),max(h2$density),max(h3$density))
ymin=0

#jpeg(paste(main_path,"/Figures/S22_DiscountRate_Impact_On_Damages.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
pdf(paste(main_path,"/Figures/S22_DiscountRate_Impact_On_Damages.pdf",sep=""),width =3.94, height =2.43)
#png(paste(main_path,"/Figures/S22_DiscountRate_Impact_On_Damages.png",sep=""),width =3.94, height =2.43,units="in",res=300)

par(cex=0.5)
par(cex=0.5,mai=c(0.09,0.09,0.09,0.09))
par(cex=0.5,fig=c(0.05,0.99,0.1,0.7))

plot(EAD*h2$mids,h2$density,type="n",xlab="",ylab="",xaxt="n",yaxt="n",bty="n",xlim=EAD*c(xmin,xmax),ylim=c(ymin,ymax))

#lines(h1$mids,h1$density,col=myred)
polygon(x=EAD*c(h1$mids,rev(h1$mids)),
        y=c(rep(0,length(h1$density)),rev(smooth(h1$density,"3",endrule="copy"))),col=myred,border=NA)

#lines(h2$mids,h2$density,col=myblue)
polygon(x=EAD*c(h2$mids,rev(h2$mids)),
        y=c(rep(0,length(h2$density)),rev(smooth(h2$density,"3",endrule="copy"))),col=myblue,border=NA)

polygon(x=EAD*c(h3$mids,rev(h3$mids)),
        y=c(rep(0,length(h3$density)),rev(smooth(h3$density,"3",endrule="copy"))),col=mygreen,border=NA)

axis(2,pos=EAD*xmin,at=seq(ymin,ymax,length.out = 5),labels = signif(seq(ymin,ymax,length.out = 5),2))
#axis(1,pos=ymin,at=signif(EAD*seq(xmin,xmax,length.out = 10),4))
axis(1,pos=ymin,at=(EAD*seq(xmin,xmax,length.out = 10)),labels = round(100*EAD*seq(xmin,xmax,length.out = 10)/house_value))

mtext("Density",side=2,line=1.2,cex=0.5)
mtext("Present value of expected damages in percent of house value [%]",side=1,line=2,cex=0.5)

lines(x=EAD*c(xmin,xmax),y=c(ymax,ymax))
lines(x=EAD*c(xmax,xmax),y=c(ymin,ymax))

lines(c(EAD*fixed_df,EAD*fixed_df),c(0,ymax),col="deeppink",lwd=2)
lines(c(EAD*fixed_df_15,EAD*fixed_df_15),c(0,ymax),col="darkgoldenrod3",lwd=2)

legend(EAD*(xmax-0.32*xmax),ymax,
       c('Random walk model','Mean reverting model','Mean reverting with background trend','Fixed 4% per year','Fixed 1.5% per year',"Mean"),
       col=c(myred,myblue,mygreen,"deeppink",'darkgoldenrod3',"red"),
       lty=c(NA,NA,NA,1,1,NA),pch=c(22,22,22,NA,NA,18),bty="n",
       pt.bg=c(myred,myblue,mygreen,NA,NA,"red"),pt.cex=c(2,2,2,NA,NA,1.5),cex=0.8)

#text(EAD*xmax-EAD*xmax*0.18,ymax-ymax*0.46,"House Value = 100,000 US$",cex=0.8,col="gray")
par(cex=0.5,fig=c(0.05,0.99,0.6,0.8),new=TRUE,bty="n")
boxplot(EAD*apply(rw_df_unc,1,sum),horizontal=TRUE,ylim=EAD*c(xmin,xmax),axes=FALSE,col=myred)
abline(v=EAD*fixed_df,col="deeppink",lwd=2)
abline(v=EAD*fixed_df_15,col="darkgoldenrod3",lwd=2)

points(mean(EAD*apply(rw_df_unc,1,sum)),1,cex=1.5,pch=18,col="red",bg="red")

par(cex=0.5,fig=c(0.05,0.99,0.7,0.9),new=TRUE,bty="n")
boxplot(EAD*apply(mrv_df_unc,1,sum),horizontal=TRUE,ylim=EAD*c(xmin,xmax),axes=FALSE,col=myblue)

abline(v=EAD*fixed_df,col="deeppink",lwd=2)
abline(v=EAD*fixed_df_15,col="darkgoldenrod3",lwd=2)

points(mean(EAD*apply(mrv_df_unc,1,sum)),1,cex=1.5,pch=18,col="red",bg="red")

par(cex=0.5,fig=c(0.05,0.99,0.8,0.999),new=TRUE,bty="n")
boxplot(EAD*apply(drft_df_unc,1,sum),horizontal=TRUE,ylim=EAD*c(xmin,xmax),axes=FALSE,col=mygreen)
abline(v=EAD*fixed_df,col="deeppink",lwd=2)
abline(v=EAD*fixed_df_15,col="darkgoldenrod3",lwd=2)

points(mean(EAD*apply(drft_df_unc,1,sum)),1,cex=1.5,pch=18,col="red",bg="red")
text(EAD*xmin,1,"a)",xpd=T)

dev.off()

