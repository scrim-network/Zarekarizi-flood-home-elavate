find_MAP <- function(mu, sigma, xi) {
  library(extRemes)
  library(mvtnorm)
  load(paste(main_path,"/",load_path,"/AnnMaxWL.RData",sep=""))
  
  mat <- diag(c(1000,100,1))
  lpost <- sapply(1:length(mu), function(i) {
    lp <- dmvnorm(c(mu[i], sigma[i], xi[i]), sigma=mat, log=T)
    ll <- sum(devd(AnnMaxWL[,2], mu[i], sigma[i], xi[i], log=T, type='GEV'))
    lp + ll
  })
  idx <- which.max(lpost)
  c(mu=mu[idx], sigma=sigma[idx], xi=xi[idx])
}

