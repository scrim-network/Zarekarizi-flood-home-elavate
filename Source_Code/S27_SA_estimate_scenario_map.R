##==============================================================================
##
## Goal: Calculating and saving the sensitivity indices for all the scenarios   
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

set.seed(0)

main_path=getwd()
source(paste(main_path,"/Source_Code/Functions/random_discount.R",sep=""))
obs_discount <- readRDS(paste(main_path,"/Input_Data/discount.rds",sep=""))

Depth_1 <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
Damage_Factors_1 <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)*100
Depth_2<-c(-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
Damage_Factors_2<-c(0,0,4,8,12,15,20,23,28,33,37,43,48,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81)
depths <- seq(min(min(Depth_1),min(Depth_2)),max(max(Depth_1),max(Depth_2)),length.out = 100)
Damage_Factors_1 <- approx(Depth_1,Damage_Factors_1,xout = depths,rule = 2,yleft=0)$y
Damage_Factors_2 <- approx(Depth_2,Damage_Factors_2,xout = depths,rule = 2,yleft=0)$y



mymodel <- function(X,bds){
  out=mapply(mymodel_onesample,X[,1],X[,2],X[,3],X[,4],X[,5],X[,6],
             bds$mu[1],bds$mu[2],
             bds$sigma[1],bds$sigma[2],
             bds$xi[1],bds$xi[2],
             bds$life[1],bds$life[2],
             SIMPLIFY=TRUE)
  out-mean(out)
  return(out)
}



mymodel_onesample <- function(mu01,sigma01,xi01,dd_err,dr_ind,life,
                              bds_mu1,bds_mu2,
                              bds_sigma1,bds_sigma2,
                              bds_xi1,bds_xi2,
                              bds_life1,bds_life2
){
  
  # dd:   could be 1 or 2. If 1, HAZUS will be used. If 2, EU will be used.
  # dr:   could be 1,2, or 3. 1=random walk model 2=mean reverting model 3=drift model 
  # life: could be 1 or 2. if 1, house lifetime will be sampled from truncated normal 
  #       with mean 30 and if 2, house lifetime will be sampled from truncated normal with mean 50
  m=       map_range(life,   c(0,1),c(bds_life1,bds_life2))
  my_mu=   map_range(mu01,   c(0,1),c(bds_mu1,bds_mu2))
  my_sigma=map_range(sigma01,c(0,1),c(bds_sigma1,bds_sigma2))
  my_xi=   map_range(xi01,   c(0,1),c(bds_xi1,bds_xi2))
  dd =     map_range(dd_err, c(0,1),c(-30,30))
  dr =     map_range(dr_ind ,c(0,1),c(1,1e4))
  # 2. Calculate discount rate 
  Dfs=Dfs_model[dr,]
  
  # 3. Calculate the depth-damage function 
  damage_fac=Damage_Factors+Damage_Factors*dd/100
  damage_fac[damage_fac>100]=100
  damage_fac[damage_fac<0]=0
  
  House_Current_Stage=House_Initial_Stage+0 # The stage of the house after being elevaed. reminder: Stage the elevation of the house with respect to the gage datum in feet 
  # how much is lost (in USD) at each depth. This depends on the value of the house 
  damage_fac=damage_fac/100
  damage_vals=damage_fac*Struc_Value
  
  # critical depths are depths where the damage factor changes. 
  Critical_Depths=depths+House_Current_Stage # Calculates the stage of critical depths
  # What is the probability that water level exceeds each critical depth?
  Critical_Probs=1-pgev(q=Critical_Depths, shape=my_xi, loc=my_mu, scale=my_sigma)
  # The following block is just to avoid NaNs and NAs
  if(sum(is.nan(Critical_Probs))>0){
    test_x=rgev(10^6, shape=my_xi, loc=my_mu, scale=my_sigma)
    Critical_Probs[Critical_Depths<min(test_x)]=0
    Critical_Probs[Critical_Depths>max(test_x)]=1
  }
  if(sum(is.nan(Critical_Probs))>0){
    stop("ERROR in EAD Calculation because of NaNs....")
  }
  
  # Calculate expected annual damages (EAD)
  EADfrac=rep(NA,length(Critical_Depths))
  for(i in 1:length(Critical_Depths)){
    if(i==length(Critical_Depths)){
      EADfrac[i]=Critical_Probs[i]*damage_vals[i]
    }else{
      EADfrac[i]=(Critical_Probs[i]-Critical_Probs[i+1])*damage_vals[i]
    }
  }
  EAD=sum(EADfrac)
  disc_sum=sum(Dfs[1:m])
  expected_damages=EAD*disc_sum
  
  return(expected_damages)
}

# Packages/functions to load 
library(sensitivity)
source(paste(main_path,"/Source_Code/Functions/House_chars.R",sep=""))
source(paste(main_path,'/Source_Code/Functions/Cost_Damage_Calculator.R',sep=""))
source(paste(main_path,'/Source_Code/Functions/sobol_functions.R',sep=""))

# Global variables 
n_samp <- 1e5 # define size of ensemble
n_boot <- 1e4 # number of bootstrap samples

sqft=house_charactersitics()[1,'sqft']
Struc_Value=house_charactersitics()[1,'Struc_Value']
del=house_charactersitics()[1,'del']

# Load required data 
load(paste(main_path,"/",load_path,"/GEV_Parameters_MCMC.RData",sep=""))

# Calculate GEV parameters (choosing the mode; the most probable prediction)
mu=getmode(mu_chain) # Location parameter used for ignoring-uncertainty scenario
xi=getmode(xi_chain) # Shape parameter used for ignoring-uncertainty scenario
sigma=getmode(sigma_chain) # Scale parameter used for ignoring-uncertainty scenario
# Given the above parameters, calculate the base flood elevation. Please note that we calculate the BFE using GEV and we do not use USGS data (in order to be consistent)
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3
# House initial stage (elevation difference with the benchmark of the river bed)
House_Initial_Stage=BFE+del

########################## From Vivek's code ######################
## load and combine MCMC output
post=cbind(mu=mu_chain,sigma=sigma_chain,xi=xi_chain)
parnames <- c('mu','sigma','xi')
names(parnames) <- parnames
## set up Sobol sample ensembles
# get max and min values for each posterior distribution's samples
bds <- lapply(parnames, function(p) {c(min(post[, p]), max(post[, p]))})
# fit KDEs to each marginal posterior
dens <- lapply(parnames, function(p) density(post[, p], from=bds[[p]][1], to=bds[[p]][2], kernel='gaussian'))
# sample MCMC indices
sob_samp1_idx <- sample(1:nrow(post), n_samp, replace=TRUE)
sob_samp2_idx <- sample(1:nrow(post), n_samp, replace=TRUE)
# sample values for each parameter
sob_samp1 <- lapply(parnames, function(p) sample_value(n=n_samp, parvals=post[sob_samp1_idx, p], bw=dens[[p]]$bw, bds=bds[[p]]))
sob_samp2 <- lapply(parnames, function(p) sample_value(n=n_samp, parvals=post[sob_samp2_idx, p], bw=dens[[p]]$bw, bds=bds[[p]]))
# convert sample lists to data frame for sobolSalt function
sob_samp1 <- as.data.frame(do.call(cbind, sob_samp1))
sob_samp2 <- as.data.frame(do.call(cbind, sob_samp2))

###############################################

# sob_samp1<-as.data.frame(matrix(NA,n_samp,6))
# colnames(sob_samp1)<-c('mu','sigma','xi','dd','dr','life')
# sob_samp1[, 'mu'] <- runif(n_samp,1,nrow(sob_samp1))
# sob_samp1[, 'sigma'] <- runif(n_samp,min(sigma_chain),max(sigma_chain))
# sob_samp1[, 'xi'] <- runif(n_samp,min(xi_chain),max(xi_chain))
life_vector <- rweibull(1e7,2.8,73.5)
bd_life_min <- min(life_vector)
bd_life_max <- max(life_vector)
bd_life=c(bd_life_min,bd_life_max)
bds$life=bd_life

sob_samp1[, 'dd'] <- map_range(runif(n_samp,-30,30),c(-30,30),c(0,1))
sob_samp1[, 'dr'] <- map_range(round(runif(n_samp,1,1e4)),c(1,1e4),c(0,1))
sob_samp1[, 'life'] <- map_range(sample(life_vector,n_samp,replace = TRUE),bd_life,c(0,1))

sob_samp2[, 'dd'] <- map_range(runif(n_samp,-30,30),c(-30,30),c(0,1))
sob_samp2[, 'dr'] <- map_range(round(runif(n_samp,1,1e4)),c(1,1e4),c(0,1))
sob_samp2[, 'life'] <- map_range(sample(life_vector,n_samp,replace = TRUE),bd_life,c(0,1))



dr_options=c('rw_discount','mrv_discount','drift_discount')
dd_options=c('hazus','eu')

for(dr_ind in 1:3){
  for(dd_ind in 1:2){
    print(paste('Discount rate model=',dr_options[dr_ind],'and Depth-Damage is',dd_options[dd_ind]))
    sobolout1 <- paste0(main_path,"/Output_Data/sobol_outputs/Sobol-1_",strsplit(dr_options[dr_ind],'_')[[1]][1],"_",dd_options[dd_ind],".txt")
    sobolout2 <- paste0(main_path,"/Output_Data/sobol_outputs/Sobol-2_",strsplit(dr_options[dr_ind],'_')[[1]][1],"_",dd_options[dd_ind],".txt")
    Dfs_model=(compute_dfactors(do.call(dr_options[dr_ind],list(log(obs_discount[,2]),200,1e4))))

    if(dd_options[dd_ind]=='hazus'){
      Damage_Factors=Damage_Factors_2
    }else if(dd_options[dd_ind]=='eu'){
      Damage_Factors=Damage_Factors_1
    }else{
        stop('ERROR')
    }

    sens_out <- sobolSalt(model = mymodel, 
                      sob_samp1, 
                      sob_samp2, 
                      scheme="B", 
                      nboot =n_boot,
                      conf=0.95,
                      bds=bds)
    print(sens_out)

    # From Vivek's code 
    # write output file as in Tony and Perry's analysis codes
    headers.1st.tot <- matrix(c('Parameter', 'S1', 'S1_conf_low', 'S1_conf_high',
                            'ST', 'ST_conf_low', 'ST_conf_high'), nrow=1)
    parnames=c("mu","sigma","xi","dd","dr","life")
    output.1st.tot  <- data.frame(cbind( parnames,
                                     sens_out$S[,1],
                                     sens_out$S[,4],
                                     sens_out$S[,5],
                                     sens_out$T[,1],
                                     sens_out$T[,4],
                                     sens_out$T[,5]))
    write.table(headers.1st.tot, file=sobolout1, append=FALSE, sep = " ",
            quote=FALSE    , row.names = FALSE , col.names=FALSE)
    write.table(output.1st.tot , file=sobolout1, append=TRUE , sep = " ",
            quote=FALSE    , row.names = FALSE , col.names=FALSE)
    headers.2nd     <- matrix(c('Parameter_1', 'Parameter_2', 'S2', 'S2_conf_low','S2_conf_high'), nrow=1)
    output2.indices <- sens_out$S2[,1]
    output2.conf1   <- sens_out$S2[,4]
    output2.conf2   <- sens_out$S2[,5]
    names2  <- rownames(sens_out$S2)
    names2a <- rep(NA, length(names2))
    names2b <- rep(NA, length(names2))
    cnt <- 1
    for (i in seq(from=1, to=(length(parnames)-1), by=1)) {           # i = index of first name
      for (j in seq(from=(i+1), to=(length(parnames)), by=1)) {   # j = index of second name
        names2a[cnt] <- parnames[i]
        names2b[cnt] <- parnames[j]
        cnt <- cnt+1
      }
    }

    output.2nd <- data.frame(cbind( names2a,
                                names2b,
                                output2.indices,
                                output2.conf1,
                                output2.conf2 ))
    write.table(headers.2nd    , file=sobolout2, append=FALSE , sep = " ",
            quote=FALSE    , row.names = FALSE , col.names=FALSE)
    write.table(output.2nd     , file=sobolout2, append=TRUE , sep = " ",
            quote=FALSE    , row.names = FALSE , col.names=FALSE)
  }
}

