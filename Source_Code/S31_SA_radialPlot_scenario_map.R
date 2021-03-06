##==============================================================================
##
## Goal: 
##       Reads sensitivity indices saved in last step. Then it will plots a figure with 
##       six radial plots. Each radial plot is for a possible scenario 
##       (3 discount rate models and two depth-damage functions)
##       
## Outputs:
##      A pdf or jpeg file in "Figures" directory that contains the radial plots
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
##    folder (Zarekarizi-Home-Elevation) and run the script 
##==============================================================================

main_path=getwd()
set.seed(0)
mygreen <- rgb(44/255, 185/255, 95/255, 1) 
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 1)

# Loading libraries 
library(RColorBrewer)
library(graphics)
library(plotrix)
source(paste(main_path,'/Source_Code/Functions/sobol_functions.R',sep=""))

n_params <- 6 # set number of parameters
#names=c(expression(mu),expression(sigma),expression(xi),expression(d),expression(r),expression(n))
names=c('Location','Scale','Shape','Depth-\ndamage','Discount rate','Lifetime')
cols=c('darkgreen','darkgreen','darkgreen','darkred','purple','purple')

# Make the plot 
pdf(paste(main_path,"/Figures/S31_SA_RadialPlot_Scenario_Map.pdf",sep=""),width =3.94*2, height =3*3.94)
#png(paste(main_path,"/Figures/S31_SA_RadialPlot_Scenario_Map.png",sep=""),width =3.94*2, height =3*3.94,units="in",res=300)

par(cex=0.5,mai=c(0,0.3,0.3,0))
plot(c(-1,1),c(-1,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n",type="n")

# Possible scenarions 
dr_options=c('rw_discount','mrv_discount','drift_discount')
dd_options=c('hazus','eu')

for(dr_ind in 1:3){
  for(dd_ind in 1:2){
    print(paste('Discount rate model=',dr_options[dr_ind],'and Depth-Damage is',dd_options[dd_ind]))
    sobol_file_1 <- paste0(main_path,"/",load_path,"/sobol_outputs/Sobol-1_",strsplit(dr_options[dr_ind],'_')[[1]][1],"_",dd_options[dd_ind],".txt")
    sobol_file_2 <- paste0(main_path,"/",load_path,"/sobol_outputs/Sobol-2_",strsplit(dr_options[dr_ind],'_')[[1]][1],"_",dd_options[dd_ind],".txt")
    
    ## Import data from sensitivity analysis
    # First- and total-order indices
    s1st <- read.csv(sobol_file_1,sep=' ',header=TRUE,nrows = n_params,as.is=c(TRUE,rep(FALSE,5)))
    parnames <- s1st[,1]

    # Import second-order indices
    s2_table <- read.csv(sobol_file_2,sep=' ',header=TRUE,nrows = n_params*(n_params-1)/2,as.is=c(TRUE,rep(FALSE,4)))

    # Convert second-order to upper-triangular matrix
    s2 <- matrix(nrow=n_params, ncol=n_params, byrow=FALSE)
    s2[1:(n_params-1), 2:n_params] = upper.diag(s2_table$S2)
    s2 <- as.data.frame(s2)
    colnames(s2) <- rownames(s2) <- s1st$Parameter

    # Convert confidence intervals to upper-triangular matrix
    s2_conf_low <- matrix(nrow=n_params, ncol=n_params, byrow=FALSE)
    s2_conf_high <- matrix(nrow=n_params, ncol=n_params, byrow=FALSE)
    s2_conf_low[1:(n_params-1), 2:n_params] = upper.diag(s2_table$S2_conf_low)
    s2_conf_high[1:(n_params-1), 2:n_params] = upper.diag(s2_table$S2_conf_high)

    s2_conf_low <- as.data.frame(s2_conf_low)
    s2_conf_high <- as.data.frame(s2_conf_high)
    colnames(s2_conf_low) <- rownames(s2_conf_low) <- s1st$Parameter
    colnames(s2_conf_high) <- rownames(s2_conf_high) <- s1st$Parameter

    # Determine which indices are statistically significant
    sig.cutoff <- 0

    # S1 & ST: using the confidence intervals
    s1st1 <- stat_sig_s1st(s1st,greater=sig.cutoff,method="congtr",sigCri='either')

    # S2: using the confidence intervals
    s2_sig1 <- stat_sig_s2(s2,s2_conf_low,s2_conf_high,method='congtr',greater=sig.cutoff)

    cent_x=0
    cent_y=0.2
    radi=0.6
    alph=360/(n_params)
    print(c(0.5*(dd_ind-1),0.5*dd_ind,0.2+0.25*(dr_ind-1),0.2+dr_ind*0.25))
    par(fig=c(0.5*(dd_ind-1),0.5*dd_ind,0.2+0.25*(dr_ind-1),0.2+dr_ind*0.25),new=T)

    plot(c(-1,1),c(-1,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n",type="n")
    draw.circle(0,.2,0.5,border = NA,col="gray90")


    for(j in 1:(n_params)){
      i=j-1
      cosa=cospi(alph*i/180)
      sina=sinpi(alph*i/180)
      #text(cent_x+cosa,cent_y,expression(mu))
      #text(cent_x+cosa*(radi+radi*0.3),cent_y+sina*(radi+radi*0.6),names[j],srt=0,cex=1.5,col=cols[j])
      if(j==3){
        text(cent_x+cosa*(radi+radi*0.9),cent_y+sina*(radi+radi*0.5),names[j],srt=0,cex=1.5,col=cols[j])
      }else{
        text(cent_x+cosa*(radi+radi*0.2),cent_y+sina*(radi+radi*0.5),names[j],srt=0,cex=1.5,col=cols[j])
      }
    
      myX=cent_x+cosa*(radi-0.2*radi)
      myY=cent_y+sina*(radi+0.1*radi)
      for (z in j:n_params){
        if(s2_sig1[j,z]==1){
        g=z-1
        cosaa=cospi(alph*g/180)
        sinaa=sinpi(alph*g/180)
        EndX=cent_x+cosaa*(radi-0.2*radi)
        EndY=cent_y+sinaa*(radi+0.1*radi)
        lines(c(myX,EndX),c(myY,EndY),col='darkblue',
        lwd=qunif((s2[j,z]-0.0001)/(0.1-0.0001),0.5,5))
        }
      }
  
    if(s1st1[j,9]>=1){ #Total-order nodes 
        draw.circle(cent_x+cosa*(radi-0.2*radi),cent_y+sina*(radi+0.1*radi),
        radius = qunif((s1st1[j,4]-0.01)/(0.5-0.01),0.03,0.2),
        col="black")}
  
   if(s1st1[j,8]>=1){ #First-order nodes 
        draw.circle(cent_x+cosa*(radi-0.2*radi),cent_y+sina*(radi+0.1*radi),
        radius = qunif((s1st1[j,2]-0.01)/(0.5-0.01),0.01,0.15),
        col=rgb(1, 102/255, 102/255,1),border = NA)}
  
    }
  }
}

# The legend 
par(fig=c(0.25,0.75,0.0,0.25),new=T)
plot(c(-1,1),c(-1,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="")

draw.circle(-0.8,0,0.1,border = NA,col=rgb(1, 102/255, 102/255,1))
draw.circle(-0.6,0,0.01,border = NA,col=rgb(1, 102/255, 102/255,1))
text(-0.8,0.35,'50%',cex=1.5)
text(-0.6,0.35,'1%',cex=1.5)
text(-0.7,0.5,'First-order',cex=2)

draw.circle(-0.2,0,0.15,col="black")
draw.circle(0,0,0.03,col="black")
text(-0.2,0.35,'50%',cex=1.5)
text(0,0.35,'1%',cex=1.5)
text(-0.1,0.5,'Total-order',cex=2)

lines(c(0.4,0.5),c(0,0),lwd=5,col="darkblue")
lines(c(0.6,0.7),c(0,0),lwd=0.5,col="darkblue")
text(0.4,0.35,'10%',cex=1.5)
text(0.6,0.35,'0.1%',cex=1.5)
text(0.5,0.5,'Second-order',cex=2)

par(fig=c(0,1,0,1),new=T)
plot(c(-1,1),c(-1,1),type="n",bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
text(-0.5,1.05,'Depth-Damage: HAZUS',cex=2.5,xpd=TRUE)
text(0.5,1.05,'Depth-Damage: EU',cex=2.5,xpd=TRUE)
text(-1.07,0.75,'Mean reverting with trend',cex=2.5,xpd=TRUE,srt=90)
text(-1.07,0.2,'Mean reverting',cex=2.5,xpd=TRUE,srt=90)
text(-1.07,-0.4,'Random walk',cex=2.5,xpd=TRUE,srt=90)

#dev.off()
dev.off()


