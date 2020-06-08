##==============================================================================
##
## Goal: 
##       Reads sensitivity indices saved in last step. Then it will plots the radial plots 
##       when discount rate uncertainty is quantified by sampling from a range 
## Outputs:
##      A pdf or jpeg file in "Figures" directory that contains the radial plot
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

main_path=getwd() #Gets the current directory 
set.seed(0)
mygreen <- rgb(44/255, 185/255, 95/255, 1) 
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 1)

# Loading libraries 
library(RColorBrewer)
library(graphics)
library(plotrix)
source(paste(main_path,'/Source_Code/Functions/sobol_functions.R',sep=""))

# input files that contain sobol indices
sobol_file_1 <- paste0(main_path,"/",load_path,"/sobol_outputs/Sobol-1_drRange.txt")
sobol_file_2 <- paste0(main_path,"/",load_path,"/sobol_outputs/Sobol-2_drRange.txt")

n_params <- 6 # set number of parameters
#names=c(expression(mu),expression(sigma),expression(xi),expression(d),expression(r),expression(n))
names=c('Location \nparameter','Scale \nparameter','Shape parameter','Depth-\ndamage','Discount rate','Lifetime')
cols=c('darkgreen','darkgreen','darkgreen','darkred','purple','purple')

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
sig.cutoff <- 0 #In this paper, an index is aignificant if zero is not included in the 95% confidence interval

# S1 & ST: using the confidence intervals
s1st1 <- stat_sig_s1st(s1st,greater=sig.cutoff,method="congtr",sigCri='either')

# S2: using the confidence intervals
s2_sig1 <- stat_sig_s2(s2,s2_conf_low,s2_conf_high,method='congtr',greater=sig.cutoff)


# Settings for the radial plot
cent_x=0
cent_y=0.2
radi=0.6
alph=360/(n_params)

pdf(paste(main_path,"/Figures/S33_SA_RadialPlot_dr_range.pdf",sep=""),width =3.94, height =3.94)
#png(paste(main_path,"/Figures/S33_SA_RadialPlot_dr_range.png",sep=""),width =3.94, height =3.94,units="in",res=300)

par(cex=0.5,mai=c(0.1,0.1,0.1,0.1))
plot(c(-1,1),c(-1,1),bty="n",xlab="",ylab="",xaxt="n",yaxt="n",type="n")
draw.circle(0,.2,0.5,border = NA,col="gray90")

for(j in 1:(n_params)){
  i=j-1
  cosa=cospi(alph*i/180)
  sina=sinpi(alph*i/180)
  text(cent_x+cosa*(radi+radi*0.05),cent_y+sina*(radi+radi*0.05),names[j],srt=0,cex=1,col=cols[j])
  
  myX=cent_x+cosa*(radi-0.2*radi)
  myY=cent_y+sina*(radi-0.2*radi)
  for (z in j:n_params){ #Second-order interactions 
    if(s2_sig1[j,z]==1){
      g=z-1
      cosaa=cospi(alph*g/180)
      sinaa=sinpi(alph*g/180)
      EndX=cent_x+cosaa*(radi-0.2*radi)
      EndY=cent_y+sinaa*(radi-0.2*radi)
      lines(c(myX,EndX),c(myY,EndY),col='darkblue',
            lwd=qunif((s2[j,z]*s2_sig1[j,z]-min(s2*s2_sig1,na.rm=T))/(max(s2*s2_sig1,na.rm=T)-min(s2*s2_sig1,na.rm=T)),0.5,5))
    }
  }
  
  if(s1st1[j,9]>=1){ #Total-order nodes 
    draw.circle(cent_x+cosa*(radi-0.2*radi),cent_y+sina*(radi-0.2*radi),
                radius = qunif((s1st1[j,4]-min(s1st1[,4]))/(max(s1st1[,4])-min(s1st1[,4])),0.03,0.1),
                col="black")}
  
  if(s1st1[j,8]>=1){ #First-order nodes 
    draw.circle(cent_x+cosa*(radi-0.2*radi),cent_y+sina*(radi-0.2*radi),
                radius = qunif((s1st1[j,2]-min(s1st1[,2]))/(max(s1st1[,2])-min(s1st1[,2])),0.01,0.08),
                col=rgb(1, 102/255, 102/255,1),border = NA)}
}

# Plot the box below the plot 
x1=0.3
y1=-0.01
draw.circle(x1+-0.9,y1+-0.97,0.08,border = NA,col=rgb(1, 102/255, 102/255,1))
draw.circle(x1+-0.7,y1+-0.97,0.01,border = NA,col=rgb(1, 102/255, 102/255,1))
text(x1+-0.9,y1+-0.83,paste(round(100*max(s1st1[s1st1[,"s1_sig"]>=1,2])),'%',sep=""))
text(x1+-0.7,y1+-0.83,paste(round(100*min(s1st1[s1st1[,"s1_sig"]>=1,2])),'%',sep=""))
text(x1+-0.8,y1+-0.75,'First-order')

draw.circle(x1+-0.4,y1+-0.97,0.1,col="black")
draw.circle(x1+-0.2,y1+-0.97,0.03,col="black")
text(x1+-0.4,y1+-0.83,paste(round(100*max(s1st1[s1st1[,"st_sig"]>=1,4])),'%',sep=""))
text(x1+-0.2,y1+-0.83,paste(round(100*min(s1st1[s1st1[,"st_sig"]>=1,4])),'%',sep=""))
text(x1+-0.3,y1+-0.75,'Total-order')

lines(c(x1+0.1,x1+0.2),c(y1+-0.97,y1+-0.97),lwd=5,col="darkblue")
lines(c(x1+0.3,x1+0.4),c(y1+-0.97,y1+-0.97),lwd=0.5,col="darkblue")
text(x1+0.15,y1+-0.83,paste(round(100*max(s2[s2_sig1>=1],na.rm=T)),'%',sep=""))
text(x1+0.35,y1+-0.83,paste(round(100*min(s2[s2_sig1>=1],na.rm=T)),'%',sep=""))
text(x1+0.25,y1+-0.75,'Second-order')

par(fig=c(0,1,0,1),new=T)
plot(c(0,1),c(0,1),type="n",bty="n",xaxt="n",yaxt="n")
text(0.9,0.9,'Earth sciences',cex=1.5,col="darkgreen", font=2)#,srt=-50)
text(0.5,0.24,'Social sciences',cex=1.5,col="purple", font=2)#,srt=20)
text(0.08,0.66,'Engineering',cex=1.5,col="darkred", font=2)#,srt=-90)

dev.off()
#dev.off()


