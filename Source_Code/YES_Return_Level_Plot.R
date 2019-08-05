##==============================================================================
##
## Script 
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
myred <- rgb(1, 102/255, 102/255, 1)

# Functions
## function to get the mode of a distribution
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
## function to estimate the return level from GEV distribution
myreturnlevel <- function(t,mu,sigma,xi){
  x<-mu - (sigma/xi)*(1-((-log(1-(1/t)))^(-xi)))
  return(x)
}

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# Load estimated parameters
load(paste(main_path,"Results_RData/GEV_Bayes_Parameters.RData",sep=""))

# Load libraries
library(evir)

# Choose the last 10,000 iterations
mu_chain <-mu[(length(xi)-10000+1):length(xi)]
xi_chain <- xi[(length(xi)-10000+1):length(xi)]
logsigma_chain <- logsigma[(length(logsigma)-10000+1):length(logsigma)]

# Find return levels for each parameter set
MC_rl <- sapply(1:length(xi_chain), function(x) {
  myreturnlevel((1:10^4), mu_chain[x], (logsigma_chain[x]), xi_chain[x])
})

# Max, Min, and Mean (expected) return level
MC_rl_mean <- apply(MC_rl, 1, mean)

# Find upper and lower limits for 90% CI bounds
lower_90 <- sapply(1:10000, function (x) 
{  quantile(MC_rl[x,], 0.1)  })

upper_90 <- sapply(1:10000, function (x) 
{  quantile(MC_rl[x,], 0.9) })

# Estimate the return levels ignoring uncertainty (estimated from mode of the parameter chains)
MC_rl_cert <- benreturn((1:10^4),getmode(mu_chain),getmode(logsigma_chain),getmode(xi_chain))

# PLOT
pdf(paste(main_path,"Figures/Return_Level_plot.pdf",sep=""), width =3.94, height =2.43)
par(cex=0.5)

# The base plot
plot(1:10^4,MC_rl_mean, log = "x", xlim = c(0.85, 10^4),type="n",xaxt="n",ylim = c(15, 80),yaxt="n",xlab = "Return period [years]",ylab="Return level [feet]")
# Axes 
axis(1, lwd = 1.5, at=10^(seq(-1,log10(10^4), by = 1)), label=parse(text=paste("10^", seq(-1,log10(10^4), by = 1), sep="")))
axis(2, at = seq(15, 80, by=10), labels = seq(15,80, by = 10), lwd = 1.5, las = 1)
# With and without uncertainty lines
lines(1:10^4,MC_rl_mean,lty=1,col="red")
lines(1:10^4,MC_rl_cert,lty=1,col="blue")
# Uncertainty boundaries
polygon(x = c(2:10000, 10000:2), y = c(upper_90[2:10000], rev(lower_90[2:10000])), border = NA , col = "#FF666640")
# Observation points
points(1/(1-(1:length(AnnMaxWL[,2]))/(length(AnnMaxWL[,2]) + 1)), sort(AnnMaxWL[,2]), lwd = 1.5, cex = 0.75, pch = 20, bg = "white")
# Legend
legend("topleft",
       c("Annual maximum observations","Expected return level considering uncertainty","90% C.I.","Return level neglecting uncertainty"),
       col = c('black',"red", myred,'blue'),
       pt.bg = c("white", NA, "#FF666640",'blue'),
       pch = c(20, NA, 22,NA),
       lty = c(NA, 1,NA,1),
       lwd = c(1.5, 1, NA,1),
       bty = 'n',
       pt.cex = c(1, NA, 2,NA),
       inset = c(0.01, -0.01))
box(lwd = 1.5)
dev.off()
