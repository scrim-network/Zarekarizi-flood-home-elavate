###################################
# file: tide_gauge.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# GEV Analysis of tide gauge data from
# Delfzijl, the Netherlands
#################################### 

# Clear environment and graphics
rm(list = ls())
graphics.off()

# Load required libraries
library(zoo)
library(lubridate)

# Set working directory 
#setwd("~/vanDantzig/Model_Versions/Uncertainty_SLR_GEV/Storm_Surge_Module")

# Read in tide gauge data
tide1 = read.table("Data/3hr.txt", header = TRUE, sep = '\t')
tide2 = read.table("Data/1hr.txt", header = TRUE, sep = '\t')
tide3 = read.table("Data/10min.87.99.txt", header = TRUE, sep = '\t')
tide4 = read.table("Data/10min.00.15.txt", header = TRUE, sep = '\t')

# Combine data sets into single data frame
data = rbind(tide1, tide2, tide3, tide4)
rm(tide1, tide2, tide3, tide4)

# Define function to aggregate data by years
as.year <- function(x) {floor(as.numeric(as.yearmon(x)))}

# Format data - create Datetime column and reference columns for each year in dataset
# date.time column used to create unique index for each entry 
data$date2      <- as.Date(as.character(data$date), format="%Y%m%d")
data$date.time  <- as.POSIXct(paste(data$date, data$time), format = "%Y%m%d %H:%M", "GMT")
data$year.id    <- as.numeric(as.factor(with(data, as.year(paste(date2)))))

# Create zoo object for tide gauge data for entire time series, ordered by date.time
sl <- zoo(data$sl, order.by=data$date.time)
save(sl, file = "Output/Delfzijl_sea_level")

# Aggregate annual block maximas and mean values
year.max <- aggregate(sl, as.year, max)
year.mean <- aggregate(sl, as.year, mean)

# Create data frame for block maxima, means, and index years
annual <- data.frame(index(year.max), coredata(year.mean), coredata(year.max))
annual$year.id <- index(annual[,1])

# Find residuals of annual means
# Create data frame to index and match annual means based on year.id
annual.res            <- merge(data[, c("year.id", "sl")], annual[, c("year.id", "coredata.year.mean.")])
annual.res$date       <- as.year(data$date2)
annual.res$residual   <- annual.res$sl - annual.res$coredata.year.mean.   # Residuals of Annual Means
annual.res$date.time  <- data$date.time   # Unique index for residuals

# Create zoo obhect for residuals and aggregate block maximas
# year.res.zoo are 'detrended' sea levels
year.res.zoo <- zoo(annual.res$residual, order.by = annual.res$date.time)
year.res.max <- aggregate(year.res.zoo, as.year, max)

# Save object for MCMC analysis
MCMC_coredata <- coredata(year.res.max)
save(MCMC_coredata, file = "MCMC_coredata.RData")

### Fit GEV of residuals ###
library(extRemes)
library(fExtremes)
library(ismev)

year.res.max.fit <- fevd(coredata(year.res.max))   # extRemes package
year.res.max.fit2 <- gevFit(coredata(year.res.max))   # fExtremes package
year.res.max.fit3 <- gev.fit(coredata(year.res.max), show = FALSE)   # ismev package

# Print GEV estimates
print(year.res.max.fit2@fit$par.ests)
#xi           mu         beta 
#-0.03279203 284.89990369  44.21084901 

# Determine return levels using maximum likelihood estimate (90% confidence interval):
year.return <- return.level(year.res.max.fit, return.period = c(2:10000), alpha = 0.1, do.ci = TRUE)

### MCMC Estimates ###

# Load pre-computed data and source functions for MCMC analysis
load("MCMC_Code/SurgeMCMCResults.Rdata")
source("MCMC_Code/GEVMCMCSource.R")

# Calculate MLE of best-fit parameters
bestfit <- benreturn((1:10^4), year.res.max.fit2@fit$par.ests[2], 
                     year.res.max.fit2@fit$par.ests[3], 
                     year.res.max.fit2@fit$par.ests[1])

# Extract 10,000 MCMC samples from GEV parameter chains
mu.burn2 <-mu[(length(xi)-10000+1):length(xi)]
xi.burn2 <- xi[(length(xi)-10000+1):length(xi)]
logsigma.burn2 <- logsigma[(length(logsigma)-10000+1):length(logsigma)]

param_full <- data.frame(mu.burn2, xi.burn2, logsigma.burn2)

# Write table for full parameter PDF
write.table(param_full, file = "Output/param_full.txt", sep = '\t', 
            row.names = FALSE, col.names = c("mu", "xi", "logsigma"))

# Constrain parameter samples by subsetting 90% credible interval
mu.burn3 <- subset(mu.burn2, subset = (mu.burn2 >= hpd(mu.burn2, 0.10)[1,1]) & (mu.burn2 <= hpd(mu.burn2, 0.10)[1,2]))
xi.burn3 <- subset(xi.burn2, subset = (xi.burn2 >= hpd(xi.burn2, 0.10)[1,1]) & (xi.burn2 <= hpd(xi.burn2, 0.10)[1,2]))
logsigma.burn3 <- subset(logsigma.burn2, subset = (logsigma.burn2 >= hpd(logsigma.burn2, 0.10)[1,1]) & (logsigma.burn2 <= hpd(logsigma.burn2, 0.10)[1,2]))

param_constrain <- data.frame(mu.burn3[1:9000], xi.burn3[1:9000], logsigma.burn3[1:9000])

# Write table of constrained parameters 
write.table(param_constrain, file = "Output/param_constrain.txt", sep = '\t', 
            row.names = FALSE, col.names = c("mu", "xi", "logsigma"))

# Find modes of each parameter PDF
mudat2<-data.frame(mu.x=density(mu.burn2)$x,mu.y=density(mu.burn2)$y)
mumode2<-mudat2[mudat2$mu.y==max(mudat2$mu.y),1]

sigmadat2<-data.frame(sigma.x=density(exp(logsigma.burn2))$x,sigma.y=density(exp(logsigma.burn2))$y)
sigmamode2<-sigmadat2[sigmadat2$sigma.y==max(sigmadat2$sigma.y),1]

xidat2<-data.frame(xi.x=density(xi.burn2)$x,xi.y=density(xi.burn2)$y)
ximode2<-xidat2[xidat2$xi.y==max(xidat2$xi.y),1]

# Write table for posterior modes
posterior_mode <- data.frame(mumode2, ximode2, sigmamode2)
write.table(posterior_mode, file = "Output/posterior_mode.txt", sep = '\t',
            row.names = FALSE, col.names = c("mu.mode", "xi.mode", "sigma.mode"))

#save.image(file = "tide_gauge_precomputed.RData")

