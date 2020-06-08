##==============================================================================
##
## Script to compare return levels with and without uncertainty quantification
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
##      1. output includes a plot that compares return levels with and without uncertainty
##==============================================================================

# Global variables
main_path=getwd()
set.seed(0)
myred <- rgb(1, 102/255, 102/255, 1)
if("fExtremes" %in% (.packages())){
  detach("package:fExtremes", unload=TRUE)
}
if("evd" %in% (.packages())){
  detach("package:evd", unload=TRUE)
}
library('DEoptim')
#--------------------------------------------------------------
# Functions----------------------------------------------------
#--------------------------------------------------------------
source(paste(main_path,'/Source_Code/Functions/MAP_function.R',sep=""))

## function to estimate the return level from GEV distribution
myreturnlevel <- function(t,mu,sigma,xi){
  library(evir)
  x<-qgev(p=1-(1/t),xi=xi,mu=mu,sigma=sigma)
  return(x)
}

# define emperical probability searching function
median.auxiliary.func <- function(p, e, n){
  out <- abs((1-pbinom(e-1, n, p))-0.5)
  return(out)
}

# Numerical median probability return period formula
median.rt <- function(obs){
  l <- length(obs)
  # define variables
  e <- 1:l # the ranks of the events
  n <- l # sample size for the events
  pb <- txtProgressBar(min = 0, max = l, initial = 0, char = '=', style = 1) # loading bar
  prob <- vector(mode = 'numeric', length = l)
  for (i in 1:l) {
    setTxtProgressBar(pb, i) # loading bar
    fit <- DEoptim(median.auxiliary.func, lower = 0, upper = 1, e = e[i], n = n, control = DEoptim.control(trace = FALSE))
    prob[i] <- fit$optim$bestmem
  }
  close(pb)
  out <- sort(1/prob, decreasing = FALSE)
  return(out)
}
#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------

# Load libraries and data required to run this code
load(paste(main_path,"/",load_path,"/GEV_Parameters_MCMC.RData",sep=""))
load(paste(main_path,"/",load_path,"/AnnMaxWL.RData",sep=""))

library(evir)
plot_rps <- c(seq(1,2,0.1),seq(3,9,1),seq(10,90,10),seq(100,500,100))

# Find return levels for each parameter set
MC_rl <- sapply(1:length(xi_chain), function(x) {
  myreturnlevel(plot_rps, mu_chain[x], (sigma_chain[x]), xi_chain[x])
})

# Max, Min, and Mean (expected) return level
MC_rl_mean <- apply(MC_rl, 1, mean)

# Find upper and lower limits for 90% CI bounds
lower_90 <- sapply(1:length(plot_rps), function (x){quantile(MC_rl[x,],0.05)})
upper_90 <- sapply(1:length(plot_rps), function (x) {quantile(MC_rl[x,],0.95)})

# We need a second panel to show the dnesity at return level of 500
rl_500 <- MC_rl[32,]
rl_500_lb=quantile(rl_500,0.05)
rl_500_ub=quantile(rl_500,0.95)
rl_500[rl_500<rl_500_lb]=NA
rl_500[rl_500>rl_500_ub]=NA
h500<-hist(rl_500,25,plot = F)

# Estimate the return levels ignoring uncertainty (estimated from MAP)
pars_hat = find_MAP(mu_chain,sigma_chain,xi_chain)
mu_cert = pars_hat[1]
sigma_cert = pars_hat[2]
xi_cert = pars_hat[3]

# return levels under ignoring uncertainty 
MC_rl_cert <- myreturnlevel(plot_rps,mu_cert,sigma_cert,xi_cert)

###############################
########### PLOT ##############
###############################
png(paste(main_path,"/Figures/S11_Return_Level_Plot.png",sep=""),width =3.94, height =2.43,units="in",res=300)
pdf(paste(main_path,"/Figures/S11_Return_Level_Plot.pdf",sep=""),width =3.94, height =2.43)

# plot high-level variables
par(cex=0.5,mai=c(0.3,0.4,0.1,0.1))
par(cex=0.5,fig=c(0,0.7,0.05,1))

ymin=15
ymax=60
xmin=1
xmax=500

# The base plot
plot(plot_rps,MC_rl_mean, log = "x", xlim = c(xmin,xmax),type="n",bty="n",xaxt="n",xaxs="i",yaxs="i",
     ylim = c(ymin,ymax),yaxt="n",xlab = "",ylab="")

# Axes 
axis(1,pos=ymin, at=c(1,10,100,500),cex.axis=0.8,lwd=0.5)# , label=parse(text=paste("10^", seq(-1,log10(10^4), by = 1), sep="")))
axis(2, pos=xmin,at = c(seq(ymin,ymax,by=10),ymax),lwd=0.5,cex.axis=0.8)#, labels = seq(15,80, by = 10), las = 1)

# x and y axis labels 
mtext("Return period [years]",side=1,line=2.5,cex=0.5)
mtext("Return level [feet]",side=2,line=2.5,cex=0.5)

# Box around the plot 
lines(x=c(xmin,xmax),y=c(ymax,ymax))
lines(x=c(xmax,xmax),y=c(ymin,ymax))

# With and without uncertainty lines
lines(plot_rps,MC_rl_mean,lty=1,col="red")
lines(plot_rps,MC_rl_cert,lty=1,col="blue")

# Uncertainty boundaries
polygon(x = c(plot_rps[2:length(plot_rps)],rev(plot_rps[2:length(plot_rps)])), 
        y = c(upper_90[2:length(plot_rps)], rev(lower_90[2:length(plot_rps)])), border = NA , col = "#FF666640")

# Observation points
# Traditional plotting position 
#points(1/(1-(1:length(AnnMaxWL[,2]))/(length(AnnMaxWL[,2]) + 1)), sort(AnnMaxWL[,2]), lwd = 1, cex = 0.75, pch = 20, bg = "white",col="red")
# Joel's method of calculating probabilities
points(median.rt(sort(AnnMaxWL[,2])), sort(AnnMaxWL[,2]), lwd = 1, cex = 0.75, pch = 20, bg = "white")

# Legend
legend(2,ymax-ymax*0.01,
       c("Annual maximum observations","Expected return level considering uncertainty","90% Credible Intervals","Maximum a posteriori return level"),
       col = c('black',"red", myred,'blue'),
       pt.bg = c("white", NA, "#FF666640",'blue'),
       pch = c(20, NA, 22,NA),
       lty = c(NA, 1,NA,1),
       lwd = c(1.5, 1, NA,1),
       bty = 'n',
       pt.cex = c(1, NA, 2,NA),
       inset = c(0.01, -0.01),
       cex=0.9)
# Panel indicator 
text(xmin+xmin*0.4,ymax-ymax*0.05,"d)",cex=1.5)

## Second panel 
par(cex=0.5,fig=c(0.62,1,0.05,1),new=TRUE)
xmin=0
xmax=max(h500$density)
xmax=xmax+0.1*xmax
ymean=mean(MC_rl[32,])
ymean_best_guess=MC_rl_cert[32]

plot(h500$density,h500$mids,ylim=c(ymin,ymax),xlim=c(xmin,xmax),xlab="",ylab="",xaxs="i",yaxs="i",bty="n",xaxt="n",yaxt="n",type="n")
axis(1,pos=ymin,at=c(xmin,xmax),labels=c(0,signif(xmax,2)),cex.axis=0.8,lwd=0.5)
mtext("Probability density of \n500-year return level",side=1,line=2.5,cex=0.5)

# box around the plot 
lines(x=c(xmin,xmin),y=c(ymin,ymax))
lines(x=c(xmin,xmax),y=c(ymax,ymax))
lines(x=c(xmax,xmax),y=c(ymin,ymax))

# main line
#lines(h500$density,h500$mids,type="l")
polygon(x=c(smooth(h500$density,"3R"),rep(0,length(h500$density))),y=c(h500$mids,rev(h500$mids)),col="#FF666640",border = NA)
lines(x=c(xmin,xmax),y=c(ymean,ymean),col="red")
lines(x=c(xmin,xmax),y=c(ymean_best_guess,ymean_best_guess),col="blue")
# Panel indicator 
text(xmin+0.01,ymax-ymax*0.05,"e)",cex=1.5)

dev.off()
dev.off()

print(paste('100-year return level with considering uncertainty=',ymean))
print(paste('100-year return level without considering uncertainty=',ymean_best_guess))
print(paste('Difference is =',(ymean-ymean_best_guess)))
print(paste('Percent increase =',100*(ymean-ymean_best_guess)/(ymean_best_guess)))
