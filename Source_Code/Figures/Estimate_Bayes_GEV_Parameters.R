# This script estimates GEV parameters using MCMC

# Preprocessing  #######################################################################################
rm(list=ls())

# global parameters #######################################################################################
plot_params_evolution=FALSE
plot_prams_density=TRUE


# Load Annual Maximum Data  #######################################################################################
load("~/Documents/Research/House_Elevation_Project/data/AnnMaxWL.RData")
source("~/Documents/Research/House_Elevation_Project/Source_Code/Freq_Analysis/GEVMCMCSource.R")

# Load the libraries #######################################################################################
library(evdbayes)
library(ismev)

# Estimate parameters #######################################################################################
mat <- diag(c(10000, 10000, 100))
pn <- prior.norm(mean = c(0,0,0), cov = mat)
pos<-posterior(1000000, init = c(5,1,0.1), prior = pn, lh = "gev",data = AnnMaxWL[,2], psd = c(.02,.1,.1))
mu=(pos[,1])
logsigma=(pos[,2])
xi=(pos[,3])

burn.in<-50000
mu.burn<-mu[burn.in:length(mu)]
xi.burn<-xi[burn.in:length(xi)]
logsigma.burn<-logsigma[burn.in:length(logsigma)]

mu.burn2 <-mu[(length(xi)-10000+1):length(mu)]
xi.burn2 <- xi[(length(xi)-10000+1):length(xi)]
logsigma.burn2 <- logsigma[(length(logsigma)-10000+1):length(logsigma)]

########HPD############### #######################################################################################
# Higest Posterior Density. This finds the shortest interval which contains 95% of the MCMC records.
mu_hpd<-hpd(mu.burn,p=0.05)
logsigma_hpd<-hpd(logsigma.burn,p=0.05)
xi_hpd<-hpd(xi.burn,p=0.05)
# Higest Posterior Density. This finds the shortest interval which contains 90% of the MCMC records.
mu_hpd16<-hpd(mu.burn,p=0.1)
logsigma_hpd16<-hpd(logsigma.burn,p=0.1)
xi_hpd16<-hpd(xi.burn,p=0.1)

###############DENSITY PLOTS########################## #######################################################################################
#Calculate the posterio Modes. This are has been sampled at the higest probability. 
mudat<-data.frame(mu.x=density(mu.burn)$x,mu.y=density(mu.burn)$y)
mumode<-mudat[mudat$mu.y==max(mudat$mu.y),1]
sigmadat<-data.frame(sigma.x=density(exp(logsigma.burn))$x,sigma.y=density(exp(logsigma.burn))$y)
sigmamode<-sigmadat[sigmadat$sigma.y==max(sigmadat$sigma.y),1]
xidat<-data.frame(xi.x=density(xi.burn)$x,xi.y=density(xi.burn)$y)
ximode<-xidat[xidat$xi.y==max(xidat$xi.y),1]

save(list=ls(),file="~/Documents/Research/House_Elevation_Project/data/GEV_Bayes_Parameters.RData")

# if plot_params_evolution is true, plot the evolution of parameters #######################################################################################
# The code inside the function is written by Perry Oddo 
if(plot_params_evolution){
  
  # Inspect for Burn-in Time
  
  # Starting the plot 
  par(mfrow=c(2,3),mar=c(2,2,2,2))
  # MCMC Plot for Mu
  plot(ts(mu),ylab= expression(paste("Estimation for ",mu)),xlab="Iteration",main="Markov Chain Results")
  abline(v=burn.in,col="red")
  # MCMC Plot for Sigma
  plot(ts(exp(logsigma)),ylab= expression(paste("Estimation for ",sigma)),xlab="Iteration",main="Markov Chain Results")
  abline(v=burn.in,col="red")
  # MCMC Plot for Xi
  plot(ts(xi),ylab= expression(paste("Estimation for ",xi)),xlab="Iteration",main="Markov Chain Results")
  abline(v=burn.in,col="red")
  # MCMC Plot for Mu taking out Burn-In records
  plot(ts(mu.burn),ylab= expression(paste("Estimation for ",mu)),xlab="Iteration",main="Markov Chain Results (Remove Burn-In)")
  # MCMC Plot for Sigma taking out Burn-In records
  plot(ts(exp(logsigma.burn)),ylab= expression(paste("Estimation for ",sigma)),xlab="Iteration",main="Markov Chain Results (Remove Burn-In)")
  # MCMC Plot for Xi taking out Burn-In records
  plot(ts(xi.burn),ylab= expression(paste("Estimation for ",xi)),xlab="Iteration",main="Markov Chain Results (Remove Burn-In)")
  
}


#######################################################################################
# If plot_prams_density is TRUE, plot the density of the parameters and compare them with MLE estimations
if(plot_prams_density){
  
  par(mfrow=c(3,1),mar=c(2,2,2,2))
  plot(density(mu.burn),main=expression(paste("Density for sampled ",mu)))
  abline(v = mumode,col="red",lwd=3)
  abline(v = (19.871),col="blue",lwd=2,lty="dashed")
  abline(v = mu_hpd[1,1],col="gray",lwd=3,lty="dotted")
  abline(v = mu_hpd[1,2],col="gray",lwd=3,lty="dotted")
  abline(v = mu_hpd16[1,1],col="green",lwd=3,lty="dotted")
  abline(v = mu_hpd16[1,2],col="green",lwd=3,lty="dotted")
  legend("topright", 
         legend = c("Posterior Mode",
                    "MLE Estimate",
                    "90% Credible Int",
                    "95% Credible Int"), 
         fill = c("red","blue","green","gray"), ncol = 1,
         cex = 1,text.width=0.5)
  
  
  plot(density(exp(logsigma.burn)),main=expression(paste("Density for sampled ",sigma)))
  abline(v = sigmamode,col="red",lwd=3)
  abline(v = exp(3.168),col="blue",lwd=2,lty="dashed")
  abline(v = exp(logsigma_hpd[1,1]),col="gray",lwd=3,lty="dotted")
  abline(v = exp(logsigma_hpd[1,2]),col="gray",lwd=3,lty="dotted")
  abline(v = exp(logsigma_hpd16[1,1]),col="green",lwd=3,lty="dotted")
  abline(v = exp(logsigma_hpd16[1,2]),col="green",lwd=3,lty="dotted")
  
  plot(density(xi.burn),main=expression(paste("Density for sampled ",xi)))
  abline(v = ximode,col="red",lwd=3)
  abline(v = (0.00515921024408503),col="blue",lwd=2,lty="dashed")
  abline(v = xi_hpd[1,1],col="gray",lwd=3,lty="dotted")
  abline(v = xi_hpd[1,2],col="gray",lwd=3,lty="dotted")
  abline(v = xi_hpd16[1,1],col="green",lwd=3,lty="dotted")
  abline(v = xi_hpd16[1,2],col="green",lwd=3,lty="dotted")
  
  
}


