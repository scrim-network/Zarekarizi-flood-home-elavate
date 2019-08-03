rm(list=ls())

load("~/Documents/Research/House_Elevation_Project/data/AnnMaxWL.RData")
MCMC_coredata=AnnMaxWL[,2]
source("~/Documents/Research/House_Elevation_Project/Source_Code/Freq_Analysis/GEVMCMCSource.R")

#Set the initial conditions for the Markov Chain
dataset<-MCMC_coredata
startparam<-c(10,10,10)
errvect<-c(20,0.2,0.2)
sdvect<-c(300,30,20)
 
# Run the MCMC run for 100k iterations
#t1<-ALLVARnsGEVmh(dataset,100000,startparam,errvect,sdvect)

# Allocate the final values of the Markov Chain into individual vectors
#mu<-t1[[1]][,1]
#logsigma<-t1[[1]][,2]
#xi<-t1[[1]][,3]

library(evdbayes)
library(ismev)
mat <- diag(c(10000, 10000, 100))
pn <- prior.norm(mean = c(0,0,0), cov = mat)
pos<-posterior(1000000, init = c(5,1,0.1), prior = pn, lh = "gev",data = MCMC_coredata, psd = c(.02,.1,.1))

# pdf('./figure/parameter_evolution_GEV_MCMC_Stationary.pdf', width = 8.5, height = 11)
# par(mfrow=c(3,1))
# plot(pos[,1],type="l",main="Location Parameter",ylab="Mu")
# plot(pos[,2],type="l",main="Scale Parameter",ylab="Sigma")
# plot(pos[,3],type="l",main="Shape Parameter",ylab="Xi")
# dev.off()
# 
mu=(pos[,1])
logsigma=(pos[,2])
xi=(pos[,3])



# Inspect for Burn-in Time
burn.in<-50000

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
mu.burn<-mu[burn.in:length(mu)]
plot(ts(mu.burn),ylab= expression(paste("Estimation for ",mu)),xlab="Iteration",main="Markov Chain Results (Remove Burn-In)")
# MCMC Plot for Sigma taking out Burn-In records
logsigma.burn<-logsigma[burn.in:length(logsigma)]
plot(ts(exp(logsigma.burn)),ylab= expression(paste("Estimation for ",sigma)),xlab="Iteration",main="Markov Chain Results (Remove Burn-In)")
# MCMC Plot for Xi taking out Burn-In records
xi.burn<-xi[burn.in:length(xi)]
plot(ts(xi.burn),ylab= expression(paste("Estimation for ",xi)),xlab="Iteration",main="Markov Chain Results (Remove Burn-In)")

# ####################SAMPLE MEANS ANALYSIS###########################
# # Calculate the Sample means up to time i for i in 1:100000. 
# # The i-th record is the sample mean for records 1:i.  
# mu_mean<-MC_mean(mu.burn)
# sigma_mean<-exp(MC_mean(logsigma.burn))
# xi_mean<-MC_mean(xi.burn)
# 
# #Plot the sample means. This is to check for burn-in times. 
# par(mfrow=c(3,1),mar=c(2,2,2,2))
# # Mu
# plot.ts(mu_mean, main=expression(paste("Mean for sampled ",mu)))
# abline(mu_mean[length(mu_mean)],0,col="red")
# text((length(mu_mean)/2),min(mu_mean),(mu_mean[length(mu_mean)]))
# # Sigma
# plot.ts(sigma_mean,main=expression(paste("Mean for sampled ",sigma)))
# abline(sigma_mean[length(sigma_mean)],0,col="red")
# text((length(sigma_mean)/2),min(sigma_mean),(sigma_mean[length(sigma_mean)]))
# # Xi
# plot.ts(xi_mean,main=expression(paste("Mean for sampled ",xi)))
# abline(xi_mean[length(xi_mean)],0,col="red")
# text((length(xi_mean)/2),min(xi_mean),(xi_mean[length(xi_mean)]))

########HPD###############
# Higest Posterior Density. This finds the shortest interval which contains 95% of the MCMC records.
mu_hpd<-hpd(mu.burn,p=0.05)
logsigma_hpd<-hpd(logsigma.burn,p=0.05)
xi_hpd<-hpd(xi.burn,p=0.05)
# Higest Posterior Density. This finds the shortest interval which contains 90% of the MCMC records.
mu_hpd16<-hpd(mu.burn,p=0.1)
logsigma_hpd16<-hpd(logsigma.burn,p=0.1)
xi_hpd16<-hpd(xi.burn,p=0.1)

###############DENSITY PLOTS##########################
#Calculate the posterio Modes. This are has been sampled at the higest probability. 
mudat<-data.frame(mu.x=density(mu.burn)$x,mu.y=density(mu.burn)$y)
mumode<-mudat[mudat$mu.y==max(mudat$mu.y),1]
sigmadat<-data.frame(sigma.x=density(exp(logsigma.burn))$x,sigma.y=density(exp(logsigma.burn))$y)
sigmamode<-sigmadat[sigmadat$sigma.y==max(sigmadat$sigma.y),1]
xidat<-data.frame(xi.x=density(xi.burn)$x,xi.y=density(xi.burn)$y)
ximode<-xidat[xidat$xi.y==max(xidat$xi.y),1]

#dev.off()
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
abline(v = (3.168),col="blue",lwd=2,lty="dashed")
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


# Rejection
# aprobmu.burn<-aprobmu[burn.in:length(aprobmu)]
# aproblogsigma.burn<-aproblogsigma[burn.in:length(aproblogsigma)]
# aprobxi.burn<-aprobxi[burn.in:length(aprobxi)]
# mean(aprobmu.burn)
# mean(aprobxi.burn)
# mean(aproblogsigma.burn)

#######Return Level Calculation##############

parmat<-cbind(c(mu_hpd[1,1],mu_hpd16[1,1],median(mu.burn),mu_hpd16[1,2],mu_hpd[1,2]),
c(exp(logsigma_hpd[1,1]),exp(logsigma_hpd16[1,1]),median(exp(logsigma.burn)),exp(logsigma_hpd16[1,2]),exp(logsigma_hpd[1,2])),
c(xi_hpd[1,1],xi_hpd16[1,1],median(xi.burn),xi_hpd16[1,2],xi_hpd[1,2]))
parmat<-rbind(MLE=c(19.8718901487264,3.16814792683425,0.00515921024408503),parmat)
dimnames(parmat)<-list(c("MLE","5%","16%","Best Guess","84%","95%"),c("mu","sigma","xi"))

parmat

rl_mat<-cbind(year=(1:83),
      rl_MLE=benreturn((1.01:83.01),19.8718901487264,3.16814792683425,0.00515921024408503),
      rl_5=benreturn((1.01:83.01),parmat[2,1],parmat[2,2],parmat[2,3]),
      rl_16=benreturn((1.01:83.01),parmat[3,1],parmat[3,2],parmat[3,3]),
      rl_Best_Guess=benreturn((1.01:83.01),parmat[4,1],parmat[4,2],parmat[4,3]),
      rl_84=benreturn((1.01:83.01),parmat[5,1],parmat[5,2],parmat[5,3]),
      rl_95=benreturn((1.01:83.01),parmat[6,1],parmat[6,2],parmat[6,3]))



# orig_rl_mat<-cbind(return.levels.CI,exTremes=benreturn((1.01:76.01),284.93945299,44.22949852,-0.03288799),
#                    fExtremes=benreturn(1.01:76.01,284.89990369 ,44.21084901 ,-0.03279203),
#                    ismev=benreturn(1.01:76.01,284.88730190 , 44.20871429 , -0.03273583))

#dev.off()
plot.ts(rl_mat[,7],col="red",xlim=c(1,20),main="Return Levels",xlab="Return Period (Years)",ylab="Surge Magnitude")
lines(rl_mat[,6],col="brown")
lines(rl_mat[,5],col="blue")
lines(rl_mat[,4],col="brown")
lines(rl_mat[,3],col="red")
lines(orig_rl_mat[,2],col="green")
lines(orig_rl_mat[,4],col="green")
lines(orig_rl_mat[,3],col="black")
legend("topleft", 
       legend = c("MCMC Estimates",
                  "MLE Estimates",
                  "MCMC 90% Credible Int",
                  "MCMC 95% Credible Int",
                  "MLE 95% Confidence Int"), 
       fill = c("blue","black","brown","red","green"), ncol = 1,
       cex = 0.85,text.width=2.5)

#############SAVE FILES
#save(dataset,startparam,errvect,sdvect,t1,mu,logsigma,xi,burn.in,mu.burn,logsigma.burn,xi.burn,mu_mean,sigma_mean,xi_mean,mu_hpd,logsigma_hpd,xi_hpd,mu_hpd16,
#     logsigma_hpd16,xi_hpd16,aprobmu.burn,aproblogsigma.burn,aprobxi.burn,parmat,rl_mat,orig_rl_mat,mudat,mumode,sigmadat,sigmamode,xidat,ximode,
#     file="SurgeMCMCResults.Rdata")