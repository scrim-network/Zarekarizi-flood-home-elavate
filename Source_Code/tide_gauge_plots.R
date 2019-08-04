###################################
# file: tide_gauge_plots.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Generates plots for tide gauge analysis
# Delfzijl, the Netherlands
#################################### 

# Set working directory 
#setwd("~/vanDantzig/Model_Versions/Uncertainty_SLR_GEV/Storm_Surge_Module")

# Load required libraries
library(zoo)
library(lubridate)
library(fExtremes)

# Load in tide gauge data if not already in environment
#source("tide_gauge.R")

# Load plotting functions
source("Scripts/mycolors.R")
source("Scripts/put_fig_letter.r")
source("plot_sf.r")

#################################### 
# Plots
#################################### 

### Raw data and annual means

# Find indexes of annual means
annual.mean.index = sapply(1:length(annual$index.year.max.), function(x)
{
  max(which(year(index(sl))==annual$index.year.max.[x]))
})

# Find indexes of annual maximas
annual.max.index = mat.or.vec(1, nc = length(annual$index.year.max.))
for(i in 1:length(annual$index.year.max.)){
  annual.max.index[i] = which(coredata(sl)==coredata(year.max)[i] & year(sl)==annual$index.year.max.[i])
}


# Figure S6
png(file = "Figures/figs6.png", width = 6, height = 8, units = "in", res = 300)
par(mfcol = c(2,1), oma = c(3,0,0,0)+0.1, mar = c(1,4,1,0)+0.25)
plot(data$date.time, data$sl, type = 'l', lwd = 1, xaxs = 'i', 
     col = myblue, ylab = "Sea Level [cm]", 
     xlab = NA, ylim = c(-350, 600))
points(data$date.time[annual.mean.index], annual[,2], pch = 21, col = "black", bg= myred, lwd = 1.2, cex = 0.5)
points(data$date.time[annual.max.index], coredata(year.max), pch = 20, col = myblue)
box(lwd = 1.5)

legend("topleft",
       c("Delfzijl tide gauge", "Annual block maximas", "Annual mean sea level"),
       col = c(myblue, myblue,"black"),
       pt.bg = c(NA, myblue,myred),
       lty = c(1, NA, NA),
       pch = c(NA, 20, 21),
       lwd = c(1, 1.5, 1.5),
       bty = 'n',
       inset = c(0.02, 0))
legend("bottomright",
       "(A)",
       bty = 'n',
       cex = 1.2)
#dev.off()

### Plot residuals of annual means
#pdf(file = "Figures/sl_residuals.max.pdf", width = 6, height = 4)
#par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1)+0.1)
plot(index(year.max), coredata(year.max), type = 'o', 
     lwd = 6, pch = 20, xaxs = 'i', col = myblue, 
     ylab = "Sea Level [cm]", xlab = "Year", ylim = c(200, 550))
lines(index(year.res.max), coredata(year.res.max), 
      col = "black", lty = 1, lwd = 2)
#abline(lm(coredata(year.max)~index(year.max)), lty = 2)
abline(lm(coredata(year.res.max)~index(year.res.max)), lty = 2, lwd = 2)
mtext("Year", side = 1, line = 2.5)

legend("topleft",
       c("Annual block maxima", "Residuals of annual means", "Linear regression of annual means"),
       col = c(myblue, "black", "black"),
       lwd = c(5, 2, 2),
       pch = c(20, NA, NA),
       lty = c(1, 1, 2), 
       bty = 'n',
       inset = c(0.02, 0))
legend("bottomright",
       "(B)",
       bty = 'n',
       cex = 1.2)
box(lwd = 1.5)
dev.off()

###################################
### Block maxima of annual residual plots
# Histogram
pdf(file = "Figures/DiagnosticPlots/sea_level_hist.pdf", width = 6, height = 4)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1)+0.1)
hist(year.res.max, breaks = 10, prob = T, col = mybluealpha1, border = "black",
     main = NULL,
     lwd = 1.5,
     axes = T,
     ylab = "Frequency",
     xlab = "Annual Block Maxima of Residuals (cm)")
box(lwd = 1.5)
lines(density(year.res.max), lwd = 2, col = myred)

dev.off()

###################################
### GEV distribution (annual block maxima)
pdf("Figures/DiagnosticPlots/GEV_fit.pdf", width = 6, height = 4)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1)+0.1)
plot(density(coredata(year.res.max)), lwd = 1.5, main = NA)
lines(density(qgev(seq(0,1, length.out = 10^4), year.res.max.fit2@fit$par.ests[1], year.res.max.fit2@fit$par.ests[2], year.res.max.fit2@fit$par.ests[3])), 
      col = myblue, lty = 2, lwd = 1.5)
box(lwd = 1.5)

legend("topright",
       c("Empirical", "Modeled"),
       col = c(myblue, "black"),
       lty = c(2, 1),
       bty = 'n')
dev.off()

###################################
### Block Maxima plots
pdf("Figures/DiagnosticPlots/GEV_block_maxima.pdf", width = 6, height = 4)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1)+0.1)
plot(index(year.res.max), coredata(year.res.max), type = 'h', col = myblue, xaxs = 'i',
     ylab = "Maximum sea level (cm)", xlab = "Year")
box(lwd = 1.5)
dev.off()

###################################
### Return level plots
source("Scripts/return_level_plot.R")

pdf(file = "Figures/DiagnosticPlots/return_level_year.max.pdf", width = 6, height = 4)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1)+0.1)
return_level_plot(coredata(year.res.max), 10^6, legend = T)
dev.off()

###################################
### Survival function plots
source("Scripts/plot_sf.r")
q = seq(0,1,length.out=10^6+1)  # quantile array

# Find closed-form solution of GEV fit
fit_q_year = qgev(q, year.res.max.fit2@fit$par.ests[1], year.res.max.fit2@fit$par.ests[2], year.res.max.fit2@fit$par.ests[3])
fit_q_year = fit_q_year[fit_q_year< max(fit_q_year)]
q = seq(0,1, length.out = 10^6)

# Find linear regression through annual block maxima
year.res.data <- plot.sf(coredata(year.res.max), make.plot = F)
year.res.line <- lm(log10(year.res.data)~coredata(year.res.max))

pdf(file = "Figures/DiagnosticPlots/GEV_survival.pdf", width = 6, height = 4)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1))
plot.sf(coredata(year.res.max), pch = 20,
        ylab = "Probability",
        xlab = "Return Level [meters]",
        yaxt = 'n', xaxt = 'n',
        yaxs = 'i',
        ylim = c(10^-6, 10^0+0.5),
        xlim = c(200, 1200))
axis(1, at = seq(200, 1200, by = 200), labels = seq(2, 12, by = 2), lwd = 1.5, las = 1)
lines(fit_q_year, 1-q, type="l",col=myred,lwd=2)
abline(year.res.line, lwd = 1.5, lty = 2)
abline(h = 1/10^4, col = myblue, lwd = 2)
axis(2, lwd = 1.5, at=10^(seq(-6, -3, by = 1)), label=parse(text=paste("10^", seq(-6,-3, by = 1), sep="")), las = 1)
box(lwd = 1.5)

legend("topright", 
       c("Observations", "GEV Fit", "Linear Fit [van Dantzig (1956)]","1/10,000 year flood"),
       col = c("black", myred, "black", myblue),
       pch = c(20, NA, NA, NA),
       lty = c(NA, 1, 2, 1),
       lwd = c(NA, 3, 2, 3),
       bty = 'n', cex = 0.85)

dev.off()

###################################
### Quantile regression plot
library(Hmisc)

percent = c('1%', '10%', '25%', '50%', '75%', '90%', '95%', '99%')
quantile = c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)

quantplot = sapply(1:8, function(x) 
{
  sl[which(sl>=quantile(sl, quantile[x]))]
})

quantreg1 <- lm(coredata(quantplot[[1]]) ~ index(quantplot[[1]]))
quantreg10 <- lm(coredata(quantplot[[2]]) ~ index(quantplot[[2]]))
quantreg25 <- lm(coredata(quantplot[[3]]) ~ index(quantplot[[3]]))
quantreg50 <- lm(coredata(quantplot[[4]]) ~ index(quantplot[[4]]))
quantreg75 <- lm(coredata(quantplot[[5]]) ~ index(quantplot[[5]]))
quantreg90 <- lm(coredata(quantplot[[6]]) ~ index(quantplot[[6]]))
quantreg95 <- lm(coredata(quantplot[[7]]) ~ index(quantplot[[7]]))
quantreg99 <- lm(coredata(quantplot[[8]]) ~ index(quantplot[[8]]))

quant <- data.frame(percent, quantile)

quant[1,3] <- quantreg1$coefficients[2]
quant[2,3] <- quantreg10$coefficients[2]
quant[3,3] <- quantreg25$coefficients[2]
quant[4,3] <- quantreg50$coefficients[2]
quant[5,3] <- quantreg75$coefficients[2]
quant[6,3] <- quantreg90$coefficients[2]
quant[7,3] <- quantreg95$coefficients[2]
quant[8,3] <- quantreg99$coefficients[2]

quant[1,4] <- coef(summary(quantreg1))[2,2]
quant[2,4] <- coef(summary(quantreg10))[2,2]
quant[3,4] <- coef(summary(quantreg25))[2,2]
quant[4,4] <- coef(summary(quantreg50))[2,2]
quant[5,4] <- coef(summary(quantreg75))[2,2]
quant[6,4] <- coef(summary(quantreg90))[2,2]
quant[7,4] <- coef(summary(quantreg95))[2,2]
quant[8,4] <- coef(summary(quantreg99))[2,2]

colnames(quant) <- c("percent", "quantile", "slope", "error")

pdf("Figures/DiagnosticPlots/quant_reg_raw.pdf", width = 6, height = 4)
par(oma = c(0,1,0,0)+0.1, mar = c(4,4,1,1))

plot(quant[,2], quant$slope, type = 'o', pch = NA, lty = 2, xaxt = 'n', ylab = NA, xlab = "Quantile", las = 1)
errbar(quant[,2], quant$slope, pch = 20, yplus = (quant$slope + quant$error), yminus = (quant$slope - quant$error),
       las = 1, xaxt = 'n', cap = 0.03, add = T)
axis(1, at = quantile, labels = percent, las = 2)
mtext("Slope [mm/a]", side = 2, line = 4)
box(lwd = 1.5)
dev.off()

###################################
### MCMC Survival plots 

# Plot MCMC GEV parameters (unconstrained)
pdf("Figures/DiagnosticPlots/Parameter_full.pdf", width = 8, height = 3.75)
par(oma = c(0,1,0,0)+0.1, mar = c(4,4,1,1), mfcol = c(1,3))

plot(density(mu.burn2), main = NA, lwd = 1.5)
abline(v = mumode2, col=myred, lwd = 1.5)
abline(v = (year.res.max.fit2@fit$par.ests[2]), col="black", lwd=2, lty=2)
abline(v = hpd(mu.burn2, 0.10)[1,1], col=myblue, lwd = 1.5, lty=3)
abline(v = hpd(mu.burn2, 0.10)[1,2], col=myblue, lwd = 1.5, lty=3)
put.fig.letter(expression(mu), font = 2, offset = c(0.24,-0.08), cex = 2)

legend("topright", 
       c("Posterior Mode","MLE Estimate","90% Credible Int"), 
       lty = c(1, 2, 3),
       lwd = 1.5,
       bg = "white",
       col = c(myred, "black", myblue),
       cex = 0.75)
box(lwd = 1.5)

plot(density(exp(logsigma.burn2)), main = NA, lwd = 1.5)
abline(v = sigmamode2, col=myred, lwd = 1.5)
abline(v = (year.res.max.fit2@fit$par.ests[3]), col="black", lwd=2, lty=2)
abline(v = exp(hpd(logsigma.burn2, 0.10))[1,1], col=myblue, lwd = 1.5, lty=3)
abline(v = exp(hpd(logsigma.burn2, 0.10))[1,2], col=myblue, lwd = 1.5, lty=3)
put.fig.letter(expression(beta), font = 2, offset = c(0.24,-0.08), cex = 2)
box(lwd = 1.5)

plot(density(xi.burn2), main = NA, lwd = 1.5)
abline(v = ximode2, col=myred, lwd = 1.5)
abline(v = (year.res.max.fit2@fit$par.ests[1]), col="black", lwd=2, lty=2)
abline(v = hpd(xi.burn2, 0.10)[1,1], col=myblue, lwd = 1.5, lty=3)
abline(v = hpd(xi.burn2, 0.10)[1,2], col=myblue, lwd = 1.5, lty=3)
put.fig.letter(expression(xi), font = 2, offset = c(0.24,-0.08), cex = 2)
box(lwd = 1.5)

dev.off()

# GEV parameters (constrained)
pdf("Figures/DiagnosticPlots/Parameter_constraints.pdf", width = 8, height = 3.75)
par(oma = c(0,1,0,0)+0.1, mar = c(4,4,1,1), mfcol = c(1,3))

plot(density(mu.burn3), main = NA, lwd = 1.5)
abline(v = mumode2, col=myred, lwd = 1.5)
abline(v = (year.res.max.fit2@fit$par.ests[2]), col="black", lwd=2, lty=2)
abline(v = hpd(mu.burn2, 0.10)[1,1], col=myblue, lwd = 1.5, lty=3)
abline(v = hpd(mu.burn2, 0.10)[1,2], col=myblue, lwd = 1.5, lty=3)
put.fig.letter(expression(mu), font = 2, offset = c(0.24,-0.08), cex = 2)

legend("topright", 
       c("Posterior Mode","MLE Estimate","90% Credible Int"), 
       lty = c(1, 2, 3),
       lwd = 1.5,
       bg = "white",
       col = c(myred, "black", myblue),
       cex = 0.75)
box(lwd = 1.5)

plot(density(exp(logsigma.burn3)), main = NA, lwd = 1.5)
abline(v = sigmamode2, col=myred, lwd = 1.5)
abline(v = (year.res.max.fit2@fit$par.ests[3]), col="black", lwd=2, lty=2)
abline(v = exp(hpd(logsigma.burn2, 0.10))[1,1], col=myblue, lwd = 1.5, lty=3)
abline(v = exp(hpd(logsigma.burn2, 0.10))[1,2], col=myblue, lwd = 1.5, lty=3)
put.fig.letter(expression(beta), font = 2, offset = c(0.24,-0.08), cex = 2)
box(lwd = 1.5)

plot(density(xi.burn3), main = NA, lwd = 1.5)
abline(v = ximode2, col=myred, lwd = 1.5)
abline(v = (year.res.max.fit2@fit$par.ests[1]), col="black", lwd=2, lty=2)
abline(v = hpd(xi.burn2, 0.10)[1,1], col=myblue, lwd = 1.5, lty=3)
abline(v = hpd(xi.burn2, 0.10)[1,2], col=myblue, lwd = 1.5, lty=3)
put.fig.letter(expression(xi), font = 2, offset = c(0.24,-0.08), cex = 2)
box(lwd = 1.5)

dev.off()

### Return level with MCMC plot
# Find return levels for each parameter set
MC_rl <- sapply(1:length(xi.burn3), function(x) {
  benreturn((1:10^4), mu.burn3[x], exp(logsigma.burn3[x]), xi.burn3[x])
})

# Mean (expected) return level
MC_rl_mean <- apply(MC_rl, 1, mean)

# Find upper and lower limits for MCMC bounds
lower <- sapply(1:10000, function (x) 
{  min(MC_rl[x,])  })

upper <- sapply(1:10000, function (x) 
{  max(MC_rl[x,]) })

source("Scripts/return_level_basic.R")

pdf("Figures/return_level_MCMC.pdf", width = 6, height = 4.5)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1))
return_level_basic(coredata(year.res.max), 
                   10^4, legend = FALSE, 
                   ylim = c(200, 1200), 
                   xaxs = 'i', yaxs='i',
                   yaxt =  'n')
axis(2, at = seq(200, 1200, by = 200), labels = seq(2, 12, by = 2), lwd = 1.5, las = 1)
polygon(x = c(2:10000, 10000:2), y = c(year.return[,3], rev(year.return[,1])), col = "#0080FF40", border = NA)
polygon(x = c(2:10000, 10000:2), y = c(upper[2:10000], rev(lower[2:10000])), border = NA , col = "#FF666640")
lines(MC_rl_mean, lwd = 2, lty = 1, col = myred)

legend("topleft",
       c("Block maxima observations", "Maximum Likelihood Estimation", "90% Confidence Interval", "MCMC Expected", "90% Credible Interval"),
       col = c("black", myblue, myblue, myred, myred),
       pt.bg = c("white", NA, "#0080FF40", NA, "#FF666640"),
       pch = c(21, NA, 22, NA, 22),
       lty = c(NA, 1, NA, 1, NA),
       lwd = c(1.5, 2, NA, 2, NA),
       bty = 'n',
       pt.cex = c(1, NA, 2, NA, 2),
       inset = c(0.01, -0.01))

dev.off()


### Survival function with MCMC plot (unconstrained)
source("Scripts/plot_sf.r")
#q = seq(0,1,length.out=10^4+1)  # quantile array
q = seq(0,1, length.out = 10^4)

# Find closed-form solution of GEV best-fit (MLE)
fit_q_year = qgev(q, year.res.max.fit2@fit$par.ests[1], year.res.max.fit2@fit$par.ests[2], year.res.max.fit2@fit$par.ests[3])

# Find quantile function of GEV distribution for each parameter set
MC_surv <- sapply(1:length(xi.burn3), function(x)
{qgev(q, xi.burn3[x], mu.burn3[x], exp(logsigma.burn3[x]))})

# Find upper and lower limits for MCMC bounds
upper2 <- sapply(1:10^4, function (x) 
{  max(MC_surv[x,])  })

lower2 <- sapply(1:10^4, function (x) 
{  min(MC_surv[x,])  })

# Mean (expected) return levels
MC_surv_mean <- apply(MC_surv, 1, mean)

# Find linear regression through annual block maxima
year.res.data <- plot.sf(coredata(year.res.max), make.plot = F)
year.res.line <- lm(log10(year.res.data)~coredata(year.res.max))

pdf(file = "Figures/MCMC_survival2.pdf", width = 6.5, height = 4.5)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1))
plot.sf(coredata(year.res.max), pch = 20,
        ylab = "Probability",
        xlab = "Return Level [meters]",
        yaxt = 'n', yaxs = 'i',
        xaxt = 'n',
        ylim = c(10^-4, 10^0+0.5),
        xlim = c(200, 1000))
axis(1, at = seq(200, 1200, by = 200), labels = seq(2, 12, by = 2), lwd = 1.5, las = 1)
polygon(x = c(rev(upper2[2:10000]), (lower2[2:10000])), y = c(rev(1-q[2:10000]), (1-q[2:10000])), border = NA , col = rgb(10/255, 10/255, 10/255, 0.1))
#lines(fit_q_year, 1-q, type="l",col=myred, lwd=2)
lines(MC_surv_mean[1:10000], 1-q, col = myblue, lwd = 2)
#abline(year.res.line, lwd = 1.5, lty = 2)
axis(2, lwd = 1.5, at=10^(seq(-6, -3, by = 1)), label=parse(text=paste("10^", seq(-6,-3, by = 1), sep="")), las = 1)
box(lwd = 1.5)

# legend("topright", 
#        c("Block maxima observations", "GEV Fit", "Linear Fit [van Dantzig (1956)]", "MCMC Expected", "90% Credible Interval"),
#        col = c("black", myred, "black", myblue, "slate gray"),
#        pch = c(20, NA, NA, NA, 22),
#        lty = c(NA, 1, 2, 1, NA),
#        lwd = c(NA, 3, 2, 3, NA),
#        pt.bg = c(NA, NA, NA, NA, "#0A0A0A1A"),
#        pt.cex = c(1, NA, NA, NA, 2),
#        bty = 'n', cex = 0.85)

legend("topright", 
       c("Block maxima observations", "MCMC Expected", "90% Credible Interval"),
       col = c("black", myblue, "slate gray"),
       pch = c(20, NA, 22),
       lty = c(NA, 1, NA),
       lwd = c(NA, 3, NA),
       pt.bg = c(NA, NA, "#0A0A0A1A"),
       pt.cex = c(1, NA, 2),
       bty = 'n', cex = 0.90)
dev.off()


#####
# Plot MCMC return levels for unconstrained parameters

# Find return levels for each parameter set
MC_rl2 <- sapply(1:length(xi.burn2), function(x) {
  benreturn((1:10^4), mu.burn2[x], exp(logsigma.burn2[x]), xi.burn2[x])
})

# Max, Min, and Mean (expected) return level
MC_rl_max2 <- apply(MC_rl2, 1, max)
MC_rl_min2 <- apply(MC_rl2, 1, min)
MC_rl_mean2 <- apply(MC_rl2, 1, mean)

# Find upper and lower limits for 90% CI bounds
lower.90 <- sapply(1:10000, function (x) 
{  quantile(MC_rl2[x,], 0.1)  })

upper.90 <- sapply(1:10000, function (x) 
{  quantile(MC_rl2[x,], 0.9) })

# Find upper and lower limits for 95% CI bounds
lower.95 <- sapply(1:10000, function (x) 
{  quantile(MC_rl2[x,], 0.05)  })

upper.95 <- sapply(1:10000, function (x) 
{  quantile(MC_rl2[x,], 0.95) })

# Upper and lower bounds of total distribution (max and min return levels)
lower3 <- sapply(1:10000, function (x) 
{  min(MC_rl2[x,])  })

upper3 <- sapply(1:10000, function (x) 
{  max(MC_rl2[x,]) })

source("Scripts/return_level_basic.R")

###############################################################################
### Survival function - FULL uncertainty
# Find quantile function of GEV distribution for each parameter set

mu.burn2 <-mu[(length(xi)-10000+1):length(xi)]
xi.burn2 <- xi[(length(xi)-10000+1):length(xi)]
logsigma.burn2 <- logsigma[(length(logsigma)-10000+1):length(logsigma)]

q = seq(0,1, length.out = 10^4)
MC_full <- sapply(1:length(xi.burn2),function(x)
  {qgev(q,xi.burn2[x], mu.burn2[x], exp(logsigma.burn2[x]))
  })

# Find upper and lower (95%) limits for MCMC bounds
upper_full.95 <- sapply(1:10^4, function (x) 
{  quantile(MC_full[x,], 0.95)  })

lower_full.95 <- sapply(1:10^4, function (x) 
{  quantile(MC_full[x,], 0.05)  })

# Find upper and lower (90%) limits for MCMC bounds
upper_full.90 <- sapply(1:10^4, function (x) 
{  quantile(MC_full[x,], 0.9)  })

lower_full.90 <- sapply(1:10^4, function (x) 
{  quantile(MC_full[x,], 0.1)  })

# Mean (expected) return levels
MC_full_mean <- apply(MC_full, 1, mean)

#pdf("Figures/figs7.pdf", width = 6, height = 7.5)
#png("Figures/figs7.png", width = 6, height = 7.5, units = 'in', res = 300)

par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1), mfrow = c(2,1)) 
plot.sf(AnnMaxWL[,1], pch = 21, bg = "white", lwd = 1.5,
        ylab = "Exceedance Probability",
        xlab = "",
        yaxt = 'n', yaxs = 'i',
        xaxt = 'n',
        ylim = c(10^-4, 10^0+0.5),
        xlim = c(200, 1000))
mtext("Return Level [meters]", line = 2.5, side = 1)
axis(1, at = seq(200, 2400, by = 200), labels = seq(2, 24, by = 2), lwd = 1.5, las = 1)
polygon(x = c(rev(upper_full.90[2:10000]), (lower_full.90[2:10000])), y = c(rev(1-q[2:10000]), (1-q[2:10000])), border = NA , col = "#FF666640")
polygon(x = c(rev(year.return[,3]), (year.return[,1])), y = c(rev(1/(2:10000)), 1/(2:10000)), border = NA, col = "#0080FF40")
lines(fit_q_year, 1-q, type="l",col=myblue, lwd=2)
lines(MC_full_mean[1:10000], 1-q, col = myred, lwd = 2)
abline(year.res.line, lwd = 1.5, lty = 2)
axis(2, lwd = 1.5, at=10^(seq(-6, -3, by = 1)), label=parse(text=paste("10^", seq(-6,-3, by = 1), sep="")), las = 1)
box(lwd = 1.5)

legend("topright", 
       c("Block maxima observations", "Approximate Frequentist Method", "90% Confidence Interval", "Bayesian Inversion (MCMC)", "90% Credible Interval", "van Dantzig (1956) Linear Fit"), 
       col = c("black", myblue, myblue, myred, myred, "black"),
       pch = c(21, NA, 22, NA, 22, NA),
       lty = c(NA, 1, NA, 1, NA, 2),
       lwd = c(1.5, 2, NA, 2, NA, 2),
       pt.bg = c("white", NA, "#0080FF40", NA, "#FF666640", NA),
       pt.cex = c(1, NA, 2, NA, 2, NA),
       bty = 'n')

legend('bottomright',
       legend="(A)",
       bty = 'n',
       cex =1.2,
       inset = c(0, -0.01))

### Return level plot
return_level_basic(coredata(year.res.max), 
                   10^4, legend = FALSE, 
                   ylim = c(200, 1000), 
                   xlab = "",
                   xaxs = 'i', yaxs='i',
                   yaxt =  'n')
axis(2, at = seq(200, 2600, by = 200), labels = seq(2, 26, by = 2), lwd = 1.5, las = 1)
mtext("Return Period [years]", line = 2.5, side = 1)
#polygon(x = c(2:10000, 10000:2), y = c(MC_rl_max2[2:10000], rev(MC_rl_min2[2:10000])), border = NA , col = rgb(200/255, 200/255, 200/255, 0.35))
#polygon(x = c(2:10000, 10000:2), y = c(upper.95[2:10000], rev(lower.95[2:10000])), border = NA , col = rgb(200/255, 200/255, 200/255, 0.55))
#polygon(x = c(2:10000, 10000:2), y = c(upper.95[2:10000], rev(lower.95[2:10000])), border = NA , col = rgb(200/255, 200/255, 200/255, 0.25))
polygon(x = c(2:10000, 10000:2), y = c(year.return[,3], rev(year.return[,1])), col = "#0080FF40", border = NA)
polygon(x = c(2:10000, 10000:2), y = c(upper.90[2:10000], rev(lower.90[2:10000])), border = NA , col = "#FF666640")
lines(MC_rl_mean2, lwd = 2, lty = 1, col = myred)
#lines(x = c(seq(1, 10, 1,), seq(100, 1000, 10), seq(1000, 10000, 1000)), upper.95[c(seq(1, 10, 1,), seq(100, 1000, 10), seq(1000, 10000, 1000))], lty =2, col = "black")
#lines(x = c(seq(1, 10, 1,), seq(100, 1000, 10), seq(1000, 10000, 1000)), lower.95[c(seq(1, 10, 1,), seq(100, 1000, 10), seq(1000, 10000, 1000))], lty =2, col = "black")

legend("topleft",
       c("Block maxima observations", "Approximate Frequentist Method", "90% Confidence Interval", "Bayesian Inversion (MCMC)", "90% Credible Interval"),
       col = c("black", myblue, myblue, myred, myred),
       pt.bg = c("white", NA, "#0080FF40", NA, "#FF666640"),
       pch = c(21, NA, 22, NA, 22),
       lty = c(NA, 1, NA, 1, NA),
       lwd = c(1.5, 2, NA, 2, NA),
       bty = 'n',
       pt.cex = c(1, NA, 2, NA, 2),
       inset = c(0.01, -0.01))
box(lwd = 1.5)

legend('bottomright',
       legend="(B)",
       bty = 'n',
       cex=1.2,
       inset = c(0, -0.01))



dev.off()
#####################################################################



####
# This is for figure S2, just showing MCMC uncertainty bounds and expected line.
# Previously used incorrect parameter subsets.
pdf(file = "Figures/Fig2.pdf", width = 6.5, height = 4.5)
par(oma = c(0,0,0,0)+0.1, mar = c(4,4,1,1))
plot.sf(coredata(year.res.max), pch = 21, bg = "white", lwd = 1.5,
        ylab = "Probability",
        xlab = "Return Level [meters]",
        yaxt = 'n', yaxs = 'i',
        xaxt = 'n',
        ylim = c(10^-4, 10^0+0.5),
        xlim = c(200, 1000))
axis(1, at = seq(200, 1200, by = 200), labels = seq(2, 12, by = 2), lwd = 1.5, las = 1)
polygon(x = c(rev(upper_full.95[2:11000]), (lower_full.95[2:11000])), y = c(rev(1-q[2:11000]), (1-q[2:11000])), border = NA , col = rgb(10/255, 10/255, 10/255, 0.2))
polygon(x = c(rev(upper_full.90[2:10000]), (lower_full.90[2:10000])), y = c(rev(1-q[2:10000]), (1-q[2:10000])), border = NA , col = rgb(10/255, 10/255, 10/255, 0.2))
#lines(fit_q_year, 1-q, type="l",col=myred, lwd=2)
lines(MC_full_mean[1:10000], 1-q, col = myblue, lwd = 2)
abline(year.res.line, lwd = 1.5, lty = 1)
axis(2, lwd = 1.5, at=10^(seq(-6, -3, by = 1)), label=parse(text=paste("10^", seq(-6,-3, by = 1), sep="")), las = 1)
abline(h = 1/10^2, lty = 2)
#abline(h = 1/10^4, lty = 2, lwd = 2)
text(x = 200, y = (1/10^2)*1.2, labels = "100-year flood", adj = c(0, 0), cex = 0.85)
text(x = 200, y = (1/10^4)*1.2, labels = "10,000-year flood", adj = c(0, 0), cex = 0.85)
box(lwd = 1.5)

# legend("topright", 
#        c("Block maxima observations", "GEV Fit", "Linear Fit [van Dantzig (1956)]", "MCMC Expected", "90% Credible Interval"),
#        col = c("black", myred, "black", myblue, "slate gray"),
#        pch = c(20, NA, NA, NA, 22),
#        lty = c(NA, 1, 2, 1, NA),
#        lwd = c(NA, 3, 2, 3, NA),
#        pt.bg = c(NA, NA, NA, NA, "#0A0A0A1A"),
#        pt.cex = c(1, NA, NA, NA, 2),
#        bty = 'n', cex = 0.85)

legend("topright", 
       c("Block maxima observations", "Expected Value", "95% Credible Interval", "90% Credible Interval", "van Dantzig (1956) Linear"),
       col = c("black", myblue, "gray 30", "gray 30", "black"),
       pch = c(21, NA, 22, 22, NA),
       lty = c(NA, 1, NA, NA, 1),
       lwd = c(1.5, 3, NA, NA, 2),
       pt.bg = c("white", NA, "light gray", "dark gray", NA),#,#0A0A0A1A"),
       pt.cex = c(1, NA, 2, 2, NA),
       bty = 'n', cex = 1)
dev.off()
