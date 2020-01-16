##==============================================================================
## sobol_functions.R
##
## Original codes by Calvin Whealton, Cornell University
## https://github.com/calvinwhealton/SensitivityAnalysisPlots
## and
## Perry Oddo, Penn State
##
## Modified (condensed - codes are almost entirely unchanged) for brevity by
## Tony Wong (twong@psu.edu). The only code that was changed is the plotting
## routine 'plotRadCon'; Tony added
## (1) inputs for the first-, total- and second-order % values used for legend
## (2) generate labels for first-, total- and second-order in legend
## (3) write legend in black ink instead of gray
## (4) include '%' sign and cut legend labels off at the decimal (whole numbers
##     only)
##
## Tony also modified the 'sig' test for significance to test for confidence
## interval bounds of the same sign (otherwise, 0 is in CI for sensitivity
## index) and greater than 1%.
##
## Some functions were added by Vivek Srikrishnan (vivek@psu.edu),
## such as sobol_func_eval, sobol_func_wrap, and co2_yr. map_range and
## sample_value are based on Tony Wong's workflow.
##=============================================================================

library(parallel)

# map between [0,1] and a bounded parameter range
map_range <- function(x, bdin, bdout) {
  bdout[1] + (bdout[2] - bdout[1]) * ((x - bdin[1]) / (bdin[2] - bdin[1]))
}

# sample from a Gaussian distribution near parameter values and map to [0, 1]
sample_value <- function(n, parvals, bw, bds) {
  samps <- rnorm(n, mean=parvals, sd=bw) # sample from distribution
  # if any samples are outside of the parameter bounds, resample
  resample <- (samps < bds[1]) | (samps > bds[2])
  while (any(resample)) {
    samps[resample] <- rnorm(sum(resample), mean=parvals[resample], sd=bw)
    resample <- (samps < bds[1]) | (samps > bds[2])
  }
  map_range(samps, bdin=bds, bdout=c(0, 1)) # return after mapping to [0, 1]
}

# evaluate function in parallel
temp_eval_par <- function(vals, parnames, par_bds, baseline) {
  export_names <- c('parnames', 'temp_2100')
  # map samples from [0, 1] to parameter values
  samps <- vapply(parnames, function(p) map_range(vals[, match(p, parnames)], bdin=c(0, 1), bdout=par_bds[[p]]), numeric(nrow(vals)))
  
  cl <- makeCluster(detectCores()) # start cluster
  clusterEvalQ(cl, library(IAMUQ))
  clusterExport(cl, export_names) # export function to evaluate
  
  ## apply function to samples
  out <- parApply(cl, samps, 1, temp_wrap, parnames=parnames, baseline=baseline)
  
  stopCluster(cl) # stop cluster
  
  out - mean(out) # return function output centered at zero, which is required
}

# wrapper for function evaluation
temp_wrap <- function(pars, parnames, baseline) {
  # simulate model
  model_out <- run_model(pars, parnames, start=1700, end=2100)
  # call summary function and return
  do.call(temp_2100, list(emis=model_out$C, yrs=model_out$year, tcre=pars[match('TCRE', parnames)], baseline=baseline))
}

# compute temperature based on cumulative emissions
temp_2100 <- function(emis, yrs, tcre, start=2014, end=2100, baseline=0.92) {
  cum_emis <- cum_co2(emis, yrs, start=start, end=end)
  cum_emis * tcre / 1000 + baseline
}


#####################################################
# (Tony-modified) -- function for testing significance of S1 and ST
# functions assume the confidence are for already defined type I error
stat_sig_s1st <- function(df
                          ,greater = 0.01
                          ,method='sig'
                          ,sigCri = 'either'){
  
  # initializing columns for the statistical significance of indices
  df$s1_sig <- 0
  df$st_sig <- 0
  df$sig <- 0
  
  # testing for statistical significance
  if(method == 'sig'){
    # testing for statistical significance using the confidence intervals
    df$s1_sig[which((s1st$S1) - s1st$S1_conf > 0)] <- 1
    df$st_sig[which((s1st$ST) - s1st$ST_conf > 0)] <- 1
  }
  else if(method == 'gtr'){
    # finding indicies that are greater than the specified values
    df$s1_sig[which((s1st$S1) > greater)] <- 1
    df$st_sig[which((s1st$ST) > greater)] <- 1
  } else if(method == 'con') {
    df$s1_sig[which(s1st$S1_conf_low * s1st$S1_conf_high > 0)] <- 1
    df$st_sig[which(s1st$ST_conf_low * s1st$ST_conf_high > 0)] <- 1
  } else if(method == 'congtr'){
    df$s1_sig[which(s1st$S1_conf_low * s1st$S1_conf_high > 0 &
                      (s1st$S1) > greater)] <- 1
    df$st_sig[which(s1st$ST_conf_low * s1st$ST_conf_high > 0 &
                      (s1st$ST) > greater)] <- 1
  } else {
    print('Not a valid parameter for method')
  }
  
  # determining whether the parameter is significant
  if(sigCri == 'either'){
    for(i in 1:nrow(df)){
      df$sig[i] <- max(df$s1_sig[i],df$st_sig[i])
    }
  }
  else if(sigCri == 'S1'){
    df$sig <- df$s1_sig
  }
  else if(sigCri == 'ST'){
    df$sig <- df$st_sig
  }
  else{
    print('Not a valid parameter for SigCri')
  }
  
  # returned dataframe will have columns for the test of statistical significance
  return(df)
}

#####################################################
# function to test statistical significane of S2 indices
stat_sig_s2 <- function(dfs2
                        ,dfs2Conf_low
                        ,dfs2Conf_high
                        ,method='sig'
                        ,greater = 0.01
                        ){
  
  # initializing matrix to return values
  s2_sig <- matrix(0,nrow(s2),ncol(s2))
  
  # testing for statistical significance
  if(method == 'sig'){
    # testing for statistical significance using the confidence intervals
    s2_sig[which((s2) - s2_conf > 0)] <- 1
  }
  else if(method == 'gtr'){
    # finding indicies that are greater than the specified values
    s2_sig[which((s2) > greater)] <- 1
  }
  else if(method == 'con'){
    s2_sig[which(dfs2Conf_low * dfs2Conf_high > 0)] <- 1
  }
  else if(method == 'congtr'){
    s2_sig[which(dfs2Conf_low * dfs2Conf_high > 0 &
                   (s2) > greater)] <- 1
  }
  else{
    print('Not a valid parameter for method')
  }
  
  # returned dataframe will have columns for the test of statistical significance
  return(s2_sig)
}


upper.diag <- function(x){
  m<-(-1+sqrt(1+8*length(x)))/2
  X<-lower.tri(matrix(NA,m,m),diag=TRUE)
  X[X==TRUE]<-x
  X[upper.tri(X, diag = FALSE)] <- NaN
  t(X)
}

