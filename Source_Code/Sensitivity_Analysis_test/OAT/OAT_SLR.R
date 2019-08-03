###################################
# file: OAT_SLR.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# One-at-a-time sensitivity analysis
# for van Dantzig (1956) model with updated sea-level rise model
#
# Produces tornado diagrams and objective plots
#################################### 

# Set working directory
#setwd("~/vanDantzig/Model_Versions/Uncertainty_SLR/Sensitivity_Analysis/OAT")

# Compile
rm(list = ls())
graphics.off()

library(compiler)
enableJIT(3)

# Number of observations
n_obs = 10^4

# Load prior parameter values - Section 6 "The Doubtful Constants," van Dantzig (1956)
source("../../Scripts/priors.R")

# Source parameters for sea level rise and storm surge modules
source("Scripts/sample.R")
source("Scripts/slr_OAT.R")
Parameter_PDF <- Parameters 

# Create vector to change parameters from 1% to 99% quantile of PDF
quantile_prior <- seq(0.01, 0.99, by = 0.01)

#################################### 
# van Dantzig problem setup:
#################################### 
# Time horizon until next evaluation of dikes (years)
T = 75

# Initial dike height
H_0 = 4.25

# Current year
year = 2015

# Fix dike height at "optimal" level from baseline model
X = 2.35

# Considered time horizon, in annual increments
time = seq(0,T,by=1)

# Define empty arrays for each variable
p_exceed                = array(NA, dim = c(length(X), length(quantile_prior)))
costs                   = array(NA, dim = c(length(X), length(quantile_prior)))
NPV_expected_losses     = array(NA, dim = c(length(X), length(quantile_prior)))
EV_p_exceed_transient   = array(NA, dim = c(length(X), length(quantile_prior)))
Total_flood_frequency   = array(NA, dim = c(length(X), length(quantile_prior)))
total_costs             = array(NA, dim = c(length(X), length(quantile_prior)))

discount_factor         = array(NA, dim = c(length(time), length(quantile_prior)))

effective_height        = array(NA, dim = c(length(X), length(time), length(quantile_prior)))
p_exceed_transient      = array(NA, dim = c(length(X), length(time), length(quantile_prior)))
NPV_costs_flooding      = array(NA, dim = c(length(X), length(time), length(quantile_prior)))

subsidence              = array(NA, dim = c(length(time), length(quantile_prior)))
sea_level_rise          = array(NA, dim = c(length(time), length(quantile_prior))) 

# Create uniform arrays for all parameters based on expected value of priors
# Uniform arrays are held constant while single parameters are varied
p_zero_p        = array(priors$p_zero_p, length(quantile_prior))
alpha_p         = array(priors$alpha_p, length(quantile_prior))
V_p             = array(priors$V_p, length(quantile_prior))
delta_prime_p   = array(priors$delta_prime_p, length(quantile_prior))
k_p             = array(priors$k_p, length(quantile_prior))
subs_rate       = array(priors$subs_rate, length(quantile_prior))
a               = array(mean(Parameters$a), length(quantile_prior))
b               = array(mean(Parameters$b), length(quantile_prior))
c               = array(mean(Parameters$c), length(quantile_prior))
t.star          = array(mean(Parameters$t.star), length(quantile_prior))
c.star          = array(mean(Parameters$c.star), length(quantile_prior))

Parameters <- data.frame(p_zero_p, alpha_p, V_p, delta_prime_p, k_p, subs_rate, a, b, c, t.star, c.star)

# Create empty vector for 4 objectives 
total_costs_OAT             = array(NA, dim = c(length(quantile_prior), length(Parameters)))
costs_OAT                   = array(NA, dim = c(length(quantile_prior), length(Parameters)))
NPV_expected_losses_OAT     = array(NA, dim = c(length(quantile_prior), length(Parameters)))
EV_p_exceed_transient_OAT   = array(NA, dim = c(length(quantile_prior), length(Parameters)))

# Run model for by holding all parameters constant and changing one at a time
for(n in 1:length(Parameters)){
  
  p_zero_p        = array(priors$p_zero_p, length(quantile_prior))
  alpha_p         = array(priors$alpha_p, length(quantile_prior))
  V_p             = array(priors$V_p, length(quantile_prior))
  delta_prime_p   = array(priors$delta_prime_p, length(quantile_prior))
  k_p             = array(priors$k_p, length(quantile_prior))
  subs_rate       = array(priors$subs_rate, length(quantile_prior))
  a               = array(mean(Parameters$a), length(quantile_prior))
  b               = array(mean(Parameters$b), length(quantile_prior))
  c               = array(mean(Parameters$c), length(quantile_prior))
  t.star          = array(mean(Parameters$t.star), length(quantile_prior))
  c.star          = array(mean(Parameters$c.star), length(quantile_prior))
  
  Parameters <- data.frame(p_zero_p, alpha_p, V_p, delta_prime_p, k_p, subs_rate, a, b, c, t.star, c.star)
  
  # Create vector to change single parameter from 1% to 99% quantile
  Parameters[n] <- sapply(1:length(quantile_prior), function(x)
  {
    quantile(Parameter_PDF[,n], probs = quantile_prior[x])
  })
  
  # Analyze for each considered Deike heightening (eq. 1 in paper)
  # Exceedance probability of current dike height
  p_exceed = t(sapply(1:length(X), function(i) {    
    Parameters$p_zero_p * exp(-(Parameters$alpha_p) * X[i])   }))
  
  # Land subsidence over length of time horizon
  subsidence = t(sapply(1:length(time), function(j) {
    Parameters$subs_rate * time[j]   }))
  
  # Sea level rise over length of time horizon
  sea_level_rise = t(sapply(1:length(time), function(j) {
    sea_level_global(Parameters$a, Parameters$b, Parameters$c, Parameters$c.star, (Parameters$t.star - year), j) /1000  }))#+ res.boot_proj[i,]
  
  # Annual discounting factor as a function of time
  discount_factor = t(sapply(1:length(time), function(j) {
    1/(1+Parameters$delta_prime_p)^time[j]   }))
  
  # The effective dike height is the current height minus the 
  # combined effects of subsidence and sea-level rise
  effective_height[,,] = t(sapply(1:length(X), function(i) {
    t(sapply(1:length(time), function(j) {
      X[i] - subsidence[j,] - sea_level_rise[j,]    }))   }))
  
  # Annual flood frequency using old observations and new effective height 
  # (assumes stationary flooding frequency)
  p_exceed_transient[,,] = t(sapply(1:length(X), function(i) {
    t(sapply(1:length(time), function(j) {
      Parameters$p_zero_p * exp(-(Parameters$alpha_p) * effective_height[i,j,])   }))   }))
  
  # Net Present Value of the discounted expected annual losses (damages due to flooding in a given year)
  NPV_costs_flooding[,,] = t(sapply(1:length(X), function(i) {
    t(sapply(1:length(time), function(j) {
      p_exceed_transient[i,j,]*Parameters$V_p*discount_factor[j,]   }))   }))
  
  # The costs of flood protection (dike building) - increase linearly with respect to height
  costs = t(sapply(1:length(X), function(i) {
    Parameters$k_p * X[i]   }))
  
  # The total discounted expected losses are the sum of the discounted expected annual losses
  NPV_expected_losses = apply(NPV_costs_flooding, c(1,3), sum) 
  
  # Expected value of average annual flood frequency (mean of annual flood frequencies)
  EV_p_exceed_transient = apply(p_exceed_transient, c(1,3), mean)
  
  # The total flood frequency over the life-time of the project is the sum of the flood frequencies,
  # assuming independence, as in the original paper
  Total_flood_frequency = apply(p_exceed_transient, c(1,3), sum)
  
  # The total costs that depend on the dike height. This analysis neglects initial mobilization 
  # costs (I_0) in paper, as well as any costs extending beyond considered time horizon
  total_costs = t(sapply(1:length(X), function(i) {
    costs[i,] + NPV_expected_losses[i,]   }))
  
  # Find the index of the EUM point (minimum along total costs)
  total_costs_OAT[,n]           = total_costs
  costs_OAT[,n]                 = costs
  NPV_expected_losses_OAT[,n]   = NPV_expected_losses
  EV_p_exceed_transient_OAT[,n] = EV_p_exceed_transient
  
}

colnames(total_costs_OAT)             = names(Parameters)
colnames(costs_OAT)                   = names(Parameters)
colnames(NPV_expected_losses_OAT)     = names(Parameters)
colnames(EV_p_exceed_transient_OAT)   = names(Parameters)



#################################### 
# Plot results
#################################### 
library(RColorBrewer)
OAT_col <- brewer.pal(11, "Paired")

# Create data frame for parameter names, descriptions, symbols, and colors
param_name <- names(Parameters)
param_desc <- c("Initial flood frequency", "Exponential flood frequency rate", "Value of goods", "Effective discount rate",
                "Cost rate of heightening", "Subsidence rate", "Sea level rise in 2015", "Sea level rise rate", 
                "Sea level rise acceleration", "Year of abrupt sea level rise", "Rate of abrupt sea level rise")
param_symbol <- c(expression(p[0]), expression(alpha), "V", expression(delta~"'"), "k", expression(eta), "a", "b", "c", "t*", "c*")
param_color <- OAT_col

param_table <- data.frame(param_name, param_desc, param_color)

# Create color vectors for each objective set
# Parameters with no sensitvity have no bar and their descriptions are grayed out
cost_col.1          <- array(NA, dim = length(Parameters))
cost_col.2          <- array(NA, dim = length(Parameters))

damages_col.1       <- array(NA, dim = length(Parameters)) 
damages_col.2       <- array(NA, dim = length(Parameters)) 

reliability_col.1   <- array(NA, dim = length(Parameters))
reliability_col.2   <- array(NA, dim = length(Parameters))

total_cost_col.1    <- array(NA, dim = length(Parameters))
total_cost_col.2    <- array(NA, dim = length(Parameters))

for(i in 1:length(Parameters))
  {
    # Costs
    if(costs_OAT[99,i]-costs_OAT[1,i]==0)
      {
      cost_col.1[i] <- "light gray"
      cost_col.2[i] <- "dark gray"
      } else 
        {
          cost_col.1[i] <- OAT_col[i]
          cost_col.2[i] <- "black"
        }
  
    # Damages
    if(NPV_expected_losses_OAT[99,i]-NPV_expected_losses_OAT[1,i]==0)
      {
      damages_col.1[i] <- "light gray"
      damages_col.2[i] <- "dark gray"
      } else 
        {
          damages_col.1[i] <- OAT_col[i]
          damages_col.2[i] <- "black"
        }
  
    # Reliability
    if(EV_p_exceed_transient_OAT[99,i]-EV_p_exceed_transient_OAT[1,i]==0)
      {
      reliability_col.1[i] <- "light gray"
      reliability_col.2[i] <- "dark gray"
      } else 
        {
          reliability_col.1[i] <- OAT_col[i]
          reliability_col.2[i] <- "black"
        }
  
    # Total Costs
    if(total_costs_OAT[99,i]-total_costs_OAT[1,i]==0)
      {
      total_cost_col.1[i] <- "light gray"
      total_cost_col.2[i] <- "dark gray"
      } else 
        {
        total_cost_col.1[i] <- OAT_col[i]
        total_cost_col.2[i] <- "black"
        }
  }


#################################### 
# Tornado diagram (left panel)
#################################### 

pdf(file = "Tornado_SLR.pdf", width = 6, height = 8.5)
par(oma = c(3.5,0.75,0.5,4.75), mar = c(1,0.25,1,0.5)+0.1, mfcol = c(4,2))
layout(matrix(c(1:8), 4, 2, byrow = F), widths = c(3.4, 2.1), heights = c(rep(2.1, 4)))

# Determine total parameter ranges for each Objective (max - min value)
costs_range <- sapply(1:length(Parameters), function(x){
  max(costs_OAT[,x]) - min(costs_OAT[,x])
})

NPV_expected_losses_range <- sapply(1:length(Parameters), function(x){
  max(NPV_expected_losses_OAT[,x]) - min(NPV_expected_losses_OAT[,x])
})

EV_p_exceed_transient_range <- sapply(1:length(Parameters), function(x){
  max(EV_p_exceed_transient_OAT[,x]) - min(EV_p_exceed_transient_OAT[,x])
})

total_costs_range <- sapply(1:length(Parameters), function(x){
  max(total_costs_OAT[,x]) - min(total_costs_OAT[,x])
})

names(costs_range) <- names(Parameters)
names(NPV_expected_losses_range) <- names(Parameters)
names(EV_p_exceed_transient_range) <- names(Parameters)
names(total_costs_range) <- names(Parameters)

# Determine percentage of total range above and below 50% quantile (center point)
# Below (quantile 50 - quantile 1)
costs_range_minus <- sapply(1:length(Parameters), function(x) {
  abs((costs_OAT[50,x] - costs_OAT[1,x]) / costs_range[x])
})

NPV_expected_losses_range_minus <- sapply(1:length(Parameters), function(x) {
  abs((NPV_expected_losses_OAT[50,x] - NPV_expected_losses_OAT[1,x]) / NPV_expected_losses_range[x])
})

EV_p_exceed_transient_range_minus <- sapply(1:length(Parameters), function(x) {
  abs((EV_p_exceed_transient_OAT[50,x] - EV_p_exceed_transient_OAT[1,x]) / EV_p_exceed_transient_range[x])
})

total_costs_range_minus <- sapply(1:length(Parameters), function(x) {
  abs((total_costs_OAT[50,x] - total_costs_OAT[1,x]) / total_costs_range[x])
})

# Above (quantile 99 - quantile 50)
costs_range_plus <- sapply(1:length(Parameters), function(x) {
  abs((costs_OAT[99,x] - costs_OAT[50,x]) / costs_range[x])
})

NPV_expected_losses_range_plus <- sapply(1:length(Parameters), function(x) {
  abs((NPV_expected_losses_OAT[99,x] - NPV_expected_losses_OAT[50,x]) / NPV_expected_losses_range[x])
})

EV_p_exceed_transient_range_plus <- sapply(1:length(Parameters), function(x) {
  abs((EV_p_exceed_transient_OAT[99,x] - EV_p_exceed_transient_OAT[50,x]) / EV_p_exceed_transient_range[x])
})

total_costs_range_plus <- sapply(1:length(Parameters), function(x) {
  abs((total_costs_OAT[99,x] - total_costs_OAT[50,x]) / total_costs_range[x])
})

# Normalize total range to 1 by dividing by largest range in each series
costs_range                   = costs_range/max(costs_range)
NPV_expected_losses_range     = NPV_expected_losses_range/max(NPV_expected_losses_range)
EV_p_exceed_transient_range   = EV_p_exceed_transient_range/max(EV_p_exceed_transient_range)
total_costs_range             = total_costs_range/max(total_costs_range)

# Normalize 50% +/- vectors to 1
# This makes each variance a fraction of the largest range, as well as showing what percentage
# of each range lies above and below the center line

for(i in 1:length(Parameters)){
  costs_range_plus[i] = costs_range_plus[i] * costs_range[i]
  costs_range_minus[i] = costs_range_minus[i] * costs_range[i]
  
  NPV_expected_losses_range_plus[i] = NPV_expected_losses_range_plus[i] * NPV_expected_losses_range[i]
  NPV_expected_losses_range_minus[i] = NPV_expected_losses_range_minus[i] * NPV_expected_losses_range[i]
  
  EV_p_exceed_transient_range_plus[i] = EV_p_exceed_transient_range_plus[i] * EV_p_exceed_transient_range[i]
  EV_p_exceed_transient_range_minus[i] = EV_p_exceed_transient_range_minus[i] * EV_p_exceed_transient_range[i]
  
  total_costs_range_plus[i] = total_costs_range_plus[i] * total_costs_range[i]
  total_costs_range_minus[i] = total_costs_range_minus[i] * total_costs_range[i]
}

# Sort the full ranges in decreasing order
#costs_range                   = sort(costs_range, decreasing = TRUE)
#NPV_expected_losses_range     = sort(NPV_expected_losses_range, decreasing = TRUE)
#EV_p_exceed_transient_range   = sort(EV_p_exceed_transient_range, decreasing = TRUE)
total_costs_range             = sort(total_costs_range, decreasing = TRUE)

# Sort the plus/minus vectors so each panel shows the same order as the total costs objective
# (i.e. sort everything by total_costs_range)
costs_range_plus = costs_range_plus[order(match(names(costs_range_plus), names(total_costs_range)))]
costs_range_minus = costs_range_minus[order(match(names(costs_range_minus), names(total_costs_range)))]

NPV_expected_losses_range_plus = NPV_expected_losses_range_plus[order(match(names(NPV_expected_losses_range_plus), names(total_costs_range)))]
NPV_expected_losses_range_minus = NPV_expected_losses_range_minus[order(match(names(NPV_expected_losses_range_minus), names(total_costs_range)))]

EV_p_exceed_transient_range_plus = EV_p_exceed_transient_range_plus[order(match(names(EV_p_exceed_transient_range_plus), names(total_costs_range)))]
EV_p_exceed_transient_range_minus = EV_p_exceed_transient_range_minus[order(match(names(EV_p_exceed_transient_range_minus), names(total_costs_range)))]

total_costs_range_plus = total_costs_range_plus[order(match(names(total_costs_range_plus), names(total_costs_range)))]
total_costs_range_minus = total_costs_range_minus[order(match(names(total_costs_range_minus), names(total_costs_range)))]


### Plot results
# Set up matrix for positioning polygons
start = matrix(0, nrow = length(Parameters), ncol = 2)
start[1,] = c(0,1)

space = 0.25  # Space between bars
width = 0.15  # Width of bars

for(i in 2:length(Parameters)){
  start[i,2] = start[i-1,2] - space
}

# Set up matrix for plotting text in figure margins
lab_text = start
lab_text[1:length(Parameters),1] = 1.15
for(i in 1:length(Parameters)){
  lab_text[i,2] = start[i,2]-(width/2)
}

# Set up color vectors and labels for Total Costs objective (all 4 objectives will be plotted in this order)
total_cost_lab = as.character(param_table$param_desc[order(match(names(Parameters), names(total_costs_range)))])
total_cost_col = as.character(param_table$param_color[order(match(names(Parameters), names(total_costs_range)))])
symbol_lab = param_symbol[order(match(names(Parameters), names(total_costs_range)))]

total_cost_col.3 = as.character(total_cost_col.2[order(match(names(Parameters), names(total_costs_range)))])
cost_col.3 = as.character(cost_col.2[order(match(names(Parameters), names(total_costs_range)))])
damages_col.3 = as.character(damages_col.2[order(match(names(Parameters), names(total_costs_range)))])
reliability_col.3 = as.character(reliability_col.2[order(match(names(Parameters), names(total_costs_range)))])


# Plot tornado bars using polygons

# Total Costs
plot(0, type = 'n',
     xlim = c(-1, 2), ylim = c(-2, 1.15),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "", ylab = "")

for(i in 1:length(Parameters)) {
  if(total_costs_range[i]>0){
    polygon(x = c((0-total_costs_range_minus[i]), (0+total_costs_range_plus[i]), (0+total_costs_range_plus[i]), (0-total_costs_range_minus[i])),
            y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
            border = "black",
            col = total_cost_col[i])
  }
}
text(x = 1.7, y = lab_text[,2], cex = 1, labels = total_cost_lab, adj = c(1,0.5), col = total_cost_col.3)
text(x = 1.8, y = lab_text[,2], cex = 1, labels = symbol_lab, adj = c(0,0.5), col = total_cost_col.3)
mtext("A. Discounted total costs", font = 1, side = 3, line = 0.25, at = -1, adj = c(0,0), cex = 0.90)
axis(side = 1, at = seq(-1,1, by = 0.5), labels = FALSE, tck = -0.02)
segments(0, -2, 0, 1.1, lty = 2)
box(lwd = 1.3)

# Reliability
plot(0, type = 'n',
     xlim = c(-1, 2), ylim = c(-2, 1.15),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "", ylab = "")

for(i in 1:length(Parameters)) {
  if(total_costs_range[i]>0){
    polygon(x = c((0-EV_p_exceed_transient_range_minus[i]), (0+EV_p_exceed_transient_range_plus[i]), (0+EV_p_exceed_transient_range_plus[i]), (0-EV_p_exceed_transient_range_minus[i])),
            y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
            border = "black",
            col = total_cost_col[i])
  }
}
text(x = 1.7, y = lab_text[,2], cex = 1, labels = total_cost_lab, adj = c(1,0.5), col = reliability_col.3)
text(x = 1.8, y = lab_text[,2], cex = 1, labels = symbol_lab, adj = c(0,0.5), col = reliability_col.3)
mtext("B. Reliability", font = 1, side = 3, line = 0.25, at = -1, adj = c(0,0), cex = 0.90)
axis(side = 1, at = seq(-1,1, by = 0.5), labels = FALSE, tck = -0.02)
segments(0, -2, 0, 1.1, lty = 2)
box(lwd = 1.3)

# Costs
plot(0, type = 'n',
     xlim = c(-1, 2), ylim = c(-2, 1.15),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "", ylab = "")

for(i in 1:length(Parameters)) {
  if(total_costs_range[i]>0){
    polygon(x = c((0-costs_range_minus[i]), (0+costs_range_plus[i]), (0+costs_range_plus[i]), (0-costs_range_minus[i])),
            y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
            border = "black",
            col = total_cost_col[i])
  }
}

text(x = 1.7, y = lab_text[,2], cex = 1, labels = total_cost_lab, adj = c(1,0.5), col = cost_col.3)
text(x = 1.8, y = lab_text[,2], cex = 1, labels = symbol_lab, adj = c(0,0.5), col = cost_col.3)
mtext("C. Investment costs", font = 1, side = 3, line = 0.25, at = -1, adj = c(0,0), cex = 0.90)
axis(side = 1, at = seq(-1,1, by = 0.5), labels = FALSE, tck = -0.02)
segments(0, -2, 0, 1.1, lty = 2)
box(lwd = 1.3)

# Damages
plot(0, type = 'n',
     xlim = c(-1, 2), ylim = c(-2, 1.15),
     xaxt = 'n', yaxt = 'n',
     xaxs = 'i',
     xlab = "", ylab = "")

for(i in 1:length(Parameters)) {
  if(total_costs_range[i]>0){
    polygon(x = c((0-NPV_expected_losses_range_minus[i]), (0+NPV_expected_losses_range_plus[i]), (0+NPV_expected_losses_range_plus[i]), (0-NPV_expected_losses_range_minus[i])),
            y = c(start[i,2], start[i,2], (start[i,2] - width), (start[i,2]-width)), 
            border = "black",
            col = total_cost_col[i])
  }
}
axis(side = 1, at = seq(-1,1, by = 0.5), labels = seq(-100, 100, by = 50), cex.axis = 1.15)
text(x = 1.7, y = lab_text[,2], cex = 1, labels = total_cost_lab, adj = c(1,0.5), col = damages_col.3)
text(x = 1.8, y = lab_text[,2], cex = 1, labels = symbol_lab, adj = c(0,0.5), col = damages_col.3)
mtext("D. Discounted damages", font = 1, side = 3, line = 0.25, at = -1, adj = c(0,0), cex = 0.90)
mtext("Percent of total variance", side = 1, at = 0, adj = c(0.5, 0.5), line = 3, cex = 0.85)
segments(0, -2, 0, 1.1, lty = 2)
box(lwd = 1.3)


#################################### 
# Objective plots (right panel)
#################################### 
matplot(quantile_prior, (total_costs_OAT/1e+06), type = 'l', lty = 1, lwd = 2, 
        col = total_cost_col.1, 
        xlab = NA,
        ylab = NA,
        xaxt='n',
        axes = FALSE,
        las = 0)
abline(v = 0.5, lty = 2)
box(lwd = 1.3)
mtext("Costs (million Guilders)", font = 1, side = 4, line = 4, cex = 0.90)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = FALSE, tck = -0.02)
axis(4, cex.axis = 1.15, las = 1)

# Reliability
matplot(quantile_prior, (EV_p_exceed_transient_OAT), type = 'l', lty = 1, lwd = 2, 
        col = reliability_col.1, 
        xlab = NA,
        ylab = NA,
        xaxt='n',
        axes = FALSE,
        las = 0)
abline(v = 0.5, lty = 2)
box(lwd = 1.3)
mtext("Flood frequency (1/yr)", font = 1, side = 4, line = 4, cex = 0.9)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = FALSE, tck = -0.02)
axis(4, cex.axis = 1.15, las = 1)

# Investment Costs
matplot(quantile_prior, (costs_OAT/1e+06), type = 'l', lty = 1, lwd = 2, 
        col = cost_col.1,
        xlab = NA,
        ylab = NA,
        xaxt='n',
        axes = FALSE,
        las = 0)
abline(v = 0.5, lty = 2)
box(lwd = 1.3)
mtext("Costs (million Guilders)", font = 1, side = 4, line = 4, cex = 0.9)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = FALSE, tck = -0.02)
axis(4, cex.axis = 1.15, las = 1)

# Damages
matplot(quantile_prior, (NPV_expected_losses_OAT/1e+06), type = 'l', lty = 1, lwd = 2, 
        col = damages_col.1, 
        xlab = NA,
        ylab = NA,
        xaxt='n',
        axes = FALSE,
        las = 0)
axis(side = 1, at = c(0.01, 0.25, 0.5, 0.75, 0.99), labels = c(1, 25, 50, 75, 99), cex.axis = 1.15)
mtext("Quantile of prior (%)", side = 1, line = 3, at = 0.5, cex = 0.85, adj = c(0.5, 0.5))
abline(v = 0.5, lty = 2)
box(lwd = 1.3)
mtext("Costs (million Guilders)", font = 1, side = 4, line = 4, cex = 0.9)
axis(4, cex.axis = 1.15, las = 1)

dev.off()
