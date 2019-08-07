##==============================================================================
##
## Script plots the figure in Zarekarizi et al. to showcase the value of uncertainty 
## quantifiction in EAD estimations
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
##          and Klaus Keller (klaus@psu.edu)
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
# Global variables
rm(list=ls())
main_path="~/Documents/Research/House_Elevation_Project/GitHub/Zarekarizi-flood-home-elavate/"
set.seed(1)
load(paste(main_path,"Results_RData/GEV/GEV_Parameter_Chains.RData",sep=""))

mygreen <- rgb(44/255, 185/255, 95/255, 1) 
myblue <- rgb(0/255, 128/255, 1, 1)
myred <- rgb(1, 102/255, 102/255, 0.4)

# Change the directory
setwd(paste(main_path,"Source_Code",sep=""))

# load necessary libraries and s
library(evd) # We would use pgev, qgev from this package

# FUNCTIONS 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

expected_damages <- function(Struc_Value,House_Initial_Stage,
                             delta_h,life_span,disc_rate,
                             mu,sigma,xi,plot,lines,colarg,polygon=0,polycol='gray'){
  # This function calculates the expected damages for the lifetime of the house 
  # under certainty. It needs three expected values for GEV parameters.
  # Expected shape, location, and scale parameters could come from MCMC chains or just MLE estimates
  
  # Arguments:
  # Struc_Value: House value; the price of the house
  # House_Initial_Stage: Elevation of the house with respect to gage datum (stage) before being raised.
  # delta_h: How much is the house elevated from the initial stage above? This is the amount of heightening; could be zero
  # life_span: The expected lifespan of the house; usually 30 years 
  # disc_rate: The expected discount rate; usually 0.04 (4%)
  # mu: The expected location parameter of the GEV function.
  # sigma: The expected scale parameter of the GEV function.
  # xi: The expected shape parameter of the GEV function.
  
  
  House_Current_Stage=House_Initial_Stage+delta_h # The stage of the house after being elevaed. reminder: Stage the elevation of the house with respect to the gage datum in feet 
  
  # Establish the damage-depth relationship 
  EU_Depth <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  RES_Damage_Factors <- c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
  
  # how much is lost (in USD) at each depth. This depends on the value of the house 
  damage_vals=RES_Damage_Factors*Struc_Value
  
  # critical depths are depths where the damage factor changes. 
  Critical_Depths=EU_Depth+House_Current_Stage # Calculates the stage of critical depths
  #print("Critical_Depths")
  #print(Critical_Depths)
  # What is the probability that water level exceeds each critical depth?
  Critical_Probs=1-pgev(q=Critical_Depths, shape=xi, loc=mu, scale=sigma)
  
  # The following block is just to avoild NaNs and NAs
  if(sum(is.nan(Critical_Probs))>0){
    test_x=rgev(10^6, shape=xi, loc=mu, scale=sigma)
    Critical_Probs[Critical_Depths<min(test_x)]=0
    Critical_Probs[Critical_Depths>max(test_x)]=1
  }
  if(sum(is.nan(Critical_Probs))>0){
    stop("I dont know what to do....")
  }
  #print(Critical_Probs)
  
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
  
  # Expected damages is the summation of EAD over lifespan times discount factor for that year.
  disc_fac <- rep(NA,floor(life_span))
  for (i in 1:floor(life_span)){
    disc_fac[i]=1/(1+disc_rate)^(i-1)
  }
  disc_sum=sum(disc_fac)
  expected_damages=EAD*disc_sum
  if(plot==1 & polygon==0){
    plot(Critical_Probs,damage_vals,type="l",col=colarg,xlab="",ylab="",xlim=c(0,0.2))
  }else if(lines==1){
    lines(Critical_Probs,damage_vals,col=colarg)
  }
  if(polygon==1){
    plot(Critical_Probs,damage_vals,type="l",col='black',xaxt="n",yaxt="n",xlab="",ylab="")
         #xlab="Exceedence probability",ylab="Damage [US$]")
    polygon(x=c(Critical_Probs,rev(Critical_Probs)),y=c(rep(0,length(damage_vals)),rev(damage_vals)),col=polycol)
    mtext('Exceedence probability',side=1,line=0.5,cex=0.5)
    mtext('Damage [US$]',side=2,line=0.5,cex=0.5)
    
  }
  return(c(expected_damages,EAD,Critical_Probs,damage_vals))
}

# Calculate GEV parameters (choosing the mode; the most probable prediction)
mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

# Given the above parameters, calculate the base flood elevation
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

# House characteristics
sqft=1500
Struc_Value=350000 #USD
del=-5
life_span=30
disc_rate=0.04
House_Initial_Stage=BFE+del

# To account for uncertainty
nsow=10000
z<- randomLHS(nsow, 1)
SOWs=matrix(NA,nsow,1)
SOWs[,1] <- floor(qunif(z[,1],1,length(mu_chain))) 
dxt<-matrix(NA,nsow,nsow)


# PLOTING 
pdf(paste(main_path,"Figures/value_uncertainty_v4_2.pdf",sep=""),width =1*3.94, height =1*3.94)
par(cex=0.5,fig=c(0,1,0,1))

temp<-temp2<-rep(NA,nsow)
exc_damage<-exc_probs<-matrix(NA,nsow,8)

for(j in 1:nsow){
  if(j==1){
    temp_func=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]],0,0,mygreenalpha05)
  }else{
    temp_func=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]],0,0,mygreenalpha05)
  }
  temp[j]= temp_func[1] #Expected damages
  temp2[j]=temp_func[2] #EAD
  exc_probs[j,]=temp_func[3:10]
  exc_damage[j,]=temp_func[11:18]
}

temp_func_cert=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu,sigma,xi,0,0,'blue')

plot(temp_func_cert[3:10],temp_func_cert[11:18],xlim=c(0,0.2),ylim=c(min(colMeans(exc_damage)),max(colMeans(exc_damage))),type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
axis(1,pos=min(colMeans(exc_damage)),at=seq(0,0.2,by=0.05),labels = paste(100*seq(0,0.2,by=0.05),'%',sep=""))
axis(2,pos=0,at=seq(min(colMeans(exc_damage)),max(colMeans(exc_damage)),by=50000),labels = seq(min(colMeans(exc_damage)),max(colMeans(exc_damage)),by=50000)/10000)
lines(c(0,0.2),c(max(colMeans(exc_damage)),max(colMeans(exc_damage))))
lines(c(0.2,0.2),c(min(colMeans(exc_damage)),max(colMeans(exc_damage))))


lb_exc_prob <- ub_exc_prob <- rep(NA,8)
for (i in 1:8){
  lb_exc_prob[i]=quantile(exc_probs[,i],probs = c(0.05))
  ub_exc_prob[i]=quantile(exc_probs[,i],probs = c(0.95))
}
polygon(x=c(rev(lb_exc_prob),ub_exc_prob),y=c(rev(colMeans(exc_damage)),colMeans(exc_damage)),col=myred,border = NA)

lines(temp_func_cert[3:10],temp_func_cert[11:18],xlim=c(0,0.2),type="l",col="blue")
lines(colMeans(exc_probs),colMeans(exc_damage),col='red')

mtext('Exceedence probability',side=1,line=2,cex=0.7)
mtext('Damage [1,000 U.S.$]',side=2,line=2,cex=0.7)

par(fig=c(0.5,0.95,0.5,0.95),new=T)
temp_func_cert=expected_damages(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu,sigma,xi,1,0,'blue',polygon=1,polycol='darkgray')

par(fig=c(0.5,0.95,0.15,0.65),cex=0.5,new=T)
barplot(height=c(temp_func_cert[2],mean(temp2)),width = c(1,1),space = 0.5,xaxt="n",yaxt="n",ylim=c(0,(max(temp2)+0.1*max(temp2))),col=c('blue',"red"))
axis(1,at=c(1,2.5),labels = c('',''),pos=0)
mtext(c('Ignoring \n uncertainty','Considering \n uncertainty'),at=c(1,2.5),side=1,line=1.5,cex=0.5)
mtext('EAD',side=2,line=1.5,cex=0.5)
axis(2,pos=0.5)
lines(x=c(2.5,2.5),y=c(min(temp2),max(temp2)),col="black")
lines(x=c(2.4,2.6),y=c(min(temp2),min(temp2)),col="black")
lines(x=c(2.4,2.6),y=c(max(temp2),max(temp2)),col="black")

par(fig=c(0,1,0,1),cex=0.5,new=T)
plot(NA,NA,type="n",xlim=c(0,1),ylim=c(0,1),bty="n",xaxt="n",yaxt="n",xlab="",ylab="")
text(0.75,0.72,'EAD',bty="n",xpd=T,cex=2)
legend(x=0.15,y=.99,c('Ignoring uncertainty','Expected value \nunder uncertainty','90% confidence intervals'),lty=c(1,1,NA),
       col=c('blue',"red",myred),pch=c(NA,NA,22),pt.cex=c(NA,NA,3),bty="n",pt.bg=c(NA,NA,myred))
text(0,1.05,'a)',bty="n",xpd=T,cex=1.5)
text(0.95,0.94,'b)',bty="n",xpd=T,cex=1.5)
text(0.65,.5,'c)',bty="n",xpd=T,cex=1.5)

dev.off()


