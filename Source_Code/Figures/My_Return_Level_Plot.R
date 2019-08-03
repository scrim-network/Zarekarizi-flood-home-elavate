###############################################################################
### Survival function - FULL uncertainty
# Find quantile function of GEV distribution for each parameter set
load("~/Documents/Research/House_Elevation_Project/data/GEV_Bayes_Parameters.RData")

library(evir)
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




# Find return levels for each parameter set
MC_rl2 <- sapply(1:length(xi.burn2), function(x) {
  benreturn((1:10^4), mu.burn2[x], (logsigma.burn2[x]), xi.burn2[x])
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






source("return_level_basic.R")
source("mycolors.R")
#pdf("Figures/Return_Level_plot_frq_bayes.pdf", width =3.94, height =2.43)
#par(cex=0.5)
### Return level plot
return_level_basic(AnnMaxWL[,2], 
                   10^4, legend = FALSE, 
                   ylim = c(15, 80), 
                   xlab = "Return period [years]",
                   xaxs = 'i', yaxs='i',
                   yaxt =  'n',ylab="Return level [feet]")
axis(2, at = seq(15, 80, by=10), labels = seq(15,80, by = 10), lwd = 1.5, las = 1)
#polygon(x = c(2:10000, 10000:2), y = c(MC_rl_max2[2:10000], rev(MC_rl_min2[2:10000])), border = NA , col = rgb(200/255, 200/255, 200/255, 0.35))
#polygon(x = c(2:10000, 10000:2), y = c(upper.95[2:10000], rev(lower.95[2:10000])), border = NA , col = rgb(200/255, 200/255, 200/255, 0.55))
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

#dev.off()
