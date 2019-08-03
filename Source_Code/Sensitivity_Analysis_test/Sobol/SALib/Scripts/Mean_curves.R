###################################
# file: Mean_curves.R
###################################
# Author and copyright: Perry Oddo
# Pennsylvania State University
# poddo@psu.edu
###################################
# Calculates mean value of model objectives for each dike height, X
# Used to plot "best guess" curves against uncertainty
#################################### 

# considered range of Deike heigtening in meters
X = seq(0,10,by=0.05)

# Define arrays for variables
mean_p_exceed = array(NA,dim=c(length(X)))
mean_costs = array(NA,dim=c(length(X)))
mean_NPV_expected_losses = array(NA,dim=c(length(X)))
mean_EV_p_exceed_transient = array(NA,dim=c(length(X)))
mean_Total_flood_frequency = array(NA,dim=c(length(X)))
mean_total_costs = array(NA,dim=c(length(X)))

# Calcuate mean value for each dike height, X
mean_p_exceed = apply(p_exceed, 1, mean)
mean_costs = apply(costs, 1, mean)
mean_NPV_expected_losses = apply(NPV_expected_losses, 1, mean)
mean_EV_p_exceed_transient = apply(EV_p_exceed_transient, 1, mean)
mean_Total_flood_frequency = apply(Total_flood_frequency, 1, mean)
mean_total_costs = apply(total_costs, 1, mean)

min_ind_mean=seq(along=mean_total_costs)[mean_total_costs == min(mean_total_costs)]
min_cost_X_mean=X[min_ind_mean]