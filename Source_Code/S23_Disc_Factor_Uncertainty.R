##==============================================================================
##
## Goal: plot the figure that compares discount factors under different models  
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

# Global variables
main_path=getwd()
set.seed(0)
mygreen <- rgb(44/255, 185/255, 95/255, 0.5)
mygreenBold <- rgb(44/255, 185/255, 95/255,1)
myblue <- rgb(0/255, 128/255, 1, 0.5)
myred <- rgb(1, 102/255, 102/255, 0.5)
myredBold <- rgb(1, 102/255, 102/255,1)

# Load all the functions needed for PRIM
source(paste(main_path,"/Source_Code/Functions/random_discount.R",sep=""))
discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))

# Run discount rate functions 
## mean-reverting discount rate
mrv_disc_unc <- mrv_discount(log(discount[,2]))
mrv_disc_cer <- colMeans(mrv_disc_unc)
mrv_df_unc <- compute_dfactors(mrv_disc_unc)
mrv_df_cer <- colMeans(mrv_df_unc)

## random-walk discount rate
rw_disc_unc <- rw_discount(log(discount[,2]))
rw_disc_cer <- colMeans(rw_disc_unc)
rw_df_unc <- compute_dfactors(rw_disc_unc)
rw_df_cer <- colMeans(rw_df_unc)

## drift discount rate
drft_disc_unc <- drift_discount(log(discount[,2]))
drft_disc_cer <- colMeans(drft_disc_unc)
drft_df_unc <- compute_dfactors(drft_disc_unc)
drft_df_cer <- colMeans(drft_df_unc)

## fixed (4%) discount rate
fixed_factor=rep(1,dim(rw_df_unc)[2])
for(i in 2:length(fixed_factor)){
fixed_factor[i]=exp(-1*(0.04*i))
}

#########################
# same plot in log scale 
#########################
#jpeg(paste(main_path,"/Figures/S23_Discount_Factors_Comparision_logScale.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
pdf(paste(main_path,"/Figures/S23_Discount_Factors_Comparision_logScale.pdf",sep=""),width =3.94, height =2.43)

ymin=min(apply(rw_df_unc  ,2,quantile,(0.05/2)),
         apply(mrv_df_unc ,2,quantile,(0.05/2)),
         apply(drft_df_unc,2,quantile,(0.05/2))
         )
ymax=max(apply(rw_df_unc  ,2,quantile,(1-0.05/2)),
         apply(mrv_df_unc ,2,quantile,(1-0.05/2)),
         apply(drft_df_unc,2,quantile,(1-0.05/2))
         )
par(cex=0.5)
plot(rw_df_cer,type="n",xlab="",ylab="",log="y",xaxt="n",yaxt="n",bty="n",
     ylim=c(ymin,ymax))
ats=c(0,0.0001,0.001,0.01,0.1,1)
lbls=c("0","1e-4","1e-03","1e-02","1e-01","1")
axis(2,pos=0,at=c(ymin,ats[ats>ymin]),labels=c("",lbls[ats>ymin]),cex.axis=0.8,las=2)

axis(1,pos=ymin,at=seq(0,100,by=10),labels=seq(0,100,by=10),cex.axis=0.8)
mtext("Discount Factor",side=2,line=2,cex=0.5)
mtext("Years in future",side=1,line=2,cex=0.5)
lines(x=c(0,100),y=c(1,1))
lines(x=c(100,100),y=c(ymin,1))

polygon(x=c(0:100,rev(0:100)),y=c(apply(mrv_df_unc,2,quantile,(0.05/2)),rev(apply(mrv_df_unc,2,quantile,(1-0.05/2)))),col=myblue,border=NA)
polygon(x=c(0:100,rev(0:100)),y=c(apply(rw_df_unc,2,quantile,(0.05/2)),rev(apply(rw_df_unc,2,quantile,(1-0.05/2)))),col=myred,border=NA)
polygon(x=c(0:100,rev(0:100)),y=c(apply(drft_df_unc,2,quantile,(0.05/2)),rev(apply(drft_df_unc,2,quantile,(1-0.05/2)))),col=mygreen,border=NA)

lines(0:100,fixed_factor,type="l",col="black",lwd=0.8)
lines(0:100,rw_df_cer,type="l",col=myredBold,lwd=0.8)
lines(0:100,mrv_df_cer,type="l",col="blue",lwd=0.8)
lines(0:100,drft_df_cer,type="l",col="green",lwd=0.8)

legend(5,0.15,c("Fixed 4%/year",
                'Random walk certainty-equivalent',
                'Random walk 95% C.I.',
                'Mean reverting certainty-equivalent',
               'Mean reverting 95% C.I.',
               'Model with background trend certainty equivalent',
               'Model with background trend 95% C.I.'),
       col=c("black",myredBold,myred,"blue",myblue,mygreenBold,mygreen),lty=c(1,1,NA,1,NA,1,NA),pch=c(NA,NA,22,NA,22,NA,22),
       bty="n",pt.bg=c(NA,NA,myred,NA,myblue,NA,mygreen),pt.cex=c(NA,NA,2,NA,2,NA,2))
dev.off()

