
##############Metropolis Hastings Algorithm

ALLVARnsGEVmh<-function(dataset,n,startparam,errvect,sdvect){
  
  canmat<-matrix(data=NA,nrow=1,ncol=3);holdermat<-matrix(data=NA,nrow=1,ncol=3);aprobmat<-matrix(data=NA,nrow=1,ncol=3)
  errsdmat<-matrix(NA,nrow=2,ncol=3,byrow=TRUE)
  rownames(errsdmat)<-c("err_rw","sd_n")
  colnames(errsdmat)<-colnames(aprobmat)<-colnames(holdermat)<-colnames(canmat)<-c("mu","sigma","xi")
  canmat[1,]<-t(startparam);holdermat[1,]<-t(startparam);aprobmat[1,]<-NA;
  errsdmat[1,]<-t(errvect);errsdmat[2,]<-t(sdvect)
  
  # Define the log likelihood function  
  loglik<-function(dataset,MU,ETA,XI)
  {
    m<-min((1+(XI*(dataset-MU)/exp(ETA))))
    if(m<0.00001)return(as.double(-1000000))
    if(exp(ETA)<0.00001)return(as.double(-1000000))
    loglik<--length(dataset)*ETA-(1/XI+1)*sum(log(1+(XI*(dataset-MU)/exp(ETA))))-sum((1+(XI*(dataset-MU)/exp(ETA)))**(-1/XI))
    loglik
  }
  
  # Metropolis Hastings Algorithm 
  # Markov Chain works through a Gaussian random walk centered on the current value of the parameter
  # 
  
  for(i in (nrow(canmat)+1):n){
    print(i)
    canmat<-rbind(canmat,NA);holdermat<-rbind(holdermat,NA);aprobmat<-rbind(aprobmat,NA)
    set.seed(i)
    canmat[i,1]<-holdermat[i-1,1]+rnorm(1,0,errsdmat[1,1]) # Gaussian random Walk centered on the current value of the parameter
    likely<-exp((loglik(dataset,canmat[i,1],holdermat[i-1,2],holdermat[i-1,3]))-
                  (loglik(dataset,holdermat[i-1,1],holdermat[i-1,2],holdermat[i-1,3])))
    aprobmat[i,1]<-min(1,(likely*((dnorm(canmat[i,1],0,errsdmat[2,1]))*(dnorm(holdermat[i-1,2],0,errsdmat[2,2]))*(dnorm(holdermat[i-1,3],0,errsdmat[2,3])))/((dnorm(holdermat[i-1,1],0,errsdmat[2,1]))*(dnorm(holdermat[i-1,2],0,errsdmat[2,2]))*(dnorm(holdermat[i-1,3],0,errsdmat[2,3])))))
    u<-runif(1)
    if(u<aprobmat[i,1]){holdermat[i,1]<-canmat[i,1]}
    if(u>=aprobmat[i,1]){holdermat[i,1]<-holdermat[i-1,1]}
    
    
    canmat[i,2]<-holdermat[i-1,2]+rnorm(1,0,errsdmat[1,2]) # Gaussian random Walk centered on the current value of the parameter
    likely<-exp((loglik(dataset,holdermat[i,1],canmat[i,2],holdermat[i-1,3]))-
                  (loglik(dataset,holdermat[i,1],holdermat[i-1,2],holdermat[i-1,3])))
    aprobmat[i,2]<-min(1,(likely*((dnorm(holdermat[i,1],0,errsdmat[2,1]))*(dnorm(canmat[i,2],0,errsdmat[2,2]))*(dnorm(holdermat[i-1,3],0,errsdmat[2,3])))/((dnorm(holdermat[i,1],0,errsdmat[2,1]))*(dnorm(holdermat[i-1,2],0,errsdmat[2,2]))*(dnorm(holdermat[i-1,3],0,errsdmat[2,3])))))
    u<-runif(1)
    if(u<aprobmat[i,2]){holdermat[i,2]<-canmat[i,2]}
    if(u>=aprobmat[i,2]){holdermat[i,2]<-holdermat[i-1,2]}
    
    
    canmat[i,3]<-holdermat[i-1,3]+rnorm(1,0,errsdmat[1,3]) # Gaussian random Walk centered on the current value of the parameter
    likely<-exp((loglik(dataset,holdermat[i,1],holdermat[i,2],canmat[i,3]))-
                  (loglik(dataset,holdermat[i,1],holdermat[i,2],holdermat[i-1,3])))
    aprobmat[i,3]<-min(1,(likely*((dnorm(holdermat[i,1],0,errsdmat[2,1]))*(dnorm(holdermat[i,2],0,errsdmat[2,2]))*(dnorm(canmat[i,3],0,errsdmat[2,3])))/((dnorm(holdermat[i,1],0,errsdmat[2,1]))*(dnorm(holdermat[i,2],0,errsdmat[2,2]))*(dnorm(holdermat[i-1,3],0,errsdmat[2,3])))))
    u<-runif(1)
    if(u<aprobmat[i,3]){holdermat[i,3]<-canmat[i,3]}
    if(u>=aprobmat[i,3]){holdermat[i,3]<-holdermat[i-1,3]}
  }

  # Final values
  finvals<-list(finparameters=holdermat,candidates=canmat,aprob=aprobmat,errsd=errsdmat)
  return(finvals)
}
  

#################SAMPLE MEAN FOR MARKOV CHAIN
####################SAMPLE MEANS ANALYSIS###########################
MC_mean<-function(x){
  up_x<-vector("numeric")
  for ( i in 1:length(x)){
    up_x[i]<-mean(x[1:i]) 
  }
  return(up_x)
}

#############HPD Calculation
hpd <- function(samp,p=0.05)
{
  ## to find an approximate (1-p)*100% HPD interval from a
  ## given posterior sample vector samp
  
  r <- length(samp)
  samp <- sort(samp)
  rang <- matrix(0,nrow=trunc(p*r),ncol=3)
  dimnames(rang) <- list(NULL,c("low","high","range"))
  for (i in 1:trunc(p*r)) {
    rang[i,1] <- samp[i]
    rang[i,2] <- samp[i+(1-p)*r]
    rang[i,3] <- rang[i,2]-rang[i,1]
  }
  hpd <- rang[order(rang[,3])[1:5],]
  return(hpd)
}

###############Return Level
# Return Level Function for GEV
benreturn<-function(t,mu,sigma,xi){
  x<-mu - (sigma/xi)*(1-((-log(1-(1/t)))^(-xi)))
  return(x)
}


#######POSTERIOR MODE
mode <- function(data) {
  # Function for mode estimation of a continuous variable
  # Kernel density estimation by Ted Harding & Douglas Bates  

x<-data
lim.inf=min(x)-1; lim.sup=max(x)+1

s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
n<-length(s$y)
v1<-s$y[1:(n-2)];
v2<-s$y[2:(n-1)];
v3<-s$y[3:n]
ix<-1+which((v1<v2)&(v2>v3))

md <- s$x[which(s$y==max(s$y))] 

md
}
#mode(mu)
