

rm(list=ls())
set.seed(6)
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_Parameter_Chains.RData")
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




# Establish the damage-depth relationship 
EU_Depth <-           c(0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
RES_Damage_Factors <- 100*c(0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)


mu=getmode(mu_chain)
xi=getmode(xi_chain)
sigma=getmode(sigma_chain) 

# Given the above parameters, calculate the base flood elevation
BFE=qgev(p=0.99,shape=xi,scale=sigma,loc=mu) # FEMA BFE=35.3

sqft=1500
Struc_Value=350000 #USD
del=-5
life_span=30
disc_rate=0.04
# House charachteristics
House_Initial_Stage=BFE+del

nsow=1000
require(lhs)
z<- randomLHS(nsow, 1)
SOWs=matrix(NA,nsow,1)
SOWs[,1] <- floor(qunif(z[,1],1,length(mu_chain))) 

temp <- tmp2 <- tmp3 <- rep(NA,nsow)

for(j in 1:nsow){
  temp[j]=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu_chain[SOWs[j,1]],sigma_chain[SOWs[j,1]],xi_chain[SOWs[j,1]])
  tmp2[j]=qgev(p=0.99,shape=xi_chain[SOWs[j,1]],scale=sigma_chain[SOWs[j,1]],loc=mu_chain[SOWs[j,1]]) 
  
}
expected_damages_cert=lifetime_expected_damages_GEV_MCMC(Struc_Value,House_Initial_Stage,0,life_span,disc_rate,mu,sigma,xi)

pdf("Figure/depth_damage_uncertainty_plot.pdf", width =3.94, height =3.94)
#par(mfrow=c(2,2))
#par(cex=0.5)

depth_min=floor(min(0,min(tmp2-House_Initial_Stage)))
depth_max=floor(max(15,max(tmp2-House_Initial_Stage)))+1
perdamage_min=floor(min(0,min(100*temp/Struc_Value)))
perdamage_max=floor(max(100,max(100*temp/Struc_Value)))+1

mean_depth=mean(tmp2-House_Initial_Stage)
mode_depth=BFE-House_Initial_Stage

mode_damage=approx(x=c(EU_Depth),y=c(RES_Damage_Factors),xout = mode_depth)$y
mean_damage=approx(x=c(EU_Depth),y=c(RES_Damage_Factors),xout = mean_depth)$y


# Start plottng 
## First panel

par(cex=0.5,fig=c(0,0.5,0.28,.78))
hstt<-hist(tmp2-House_Initial_Stage,breaks=seq(depth_min,depth_max,length.out = 10),plot=F)#,xlab="",ylab="",xaxt="n",yaxt="n",main='')
barplot(hstt$density,axes=F,space=0,ylab="")

v_mean=approx(x=c(depth_min,depth_max),y=c(0,length(hstt$density)),xout=mean_depth)$y
v_mode=approx(x=c(depth_min,depth_max),y=c(0,length(hstt$density)),xout=mode_depth)$y

lines(x=c(v_mean,v_mean),y=c(-100,0.13),col="darkgreen",lwd=1,xpd=T)
lines(x=c(v_mode,v_mode),y=c(-100,0.15),col="blue",lwd=1,xpd=T)
text(v_mean+1.4*v_mean,0.13,'Considering Uncertainty',srt=0,xpd=T,col="darkgreen")
text(v_mode-1*v_mode,0.16,'Neglecting Uncertainty',srt=0,xpd=T,col="blue")


par(cex=0.5,fig=c(0,0.5,0.28,1),new=T)
hstt<-hist(tmp2-House_Initial_Stage,breaks=seq(depth_min,depth_max,length.out = 10),plot=F)#,xlab="",ylab="",xaxt="n",yaxt="n",main='')
plot(NA,NA,ylab="",xlab="",type="n",xlim=c(0,length(hstt$breaks)),ylim=c(0,1),bty="n",xaxt="n",yaxt="n")

## Second panel: none
par(fig=c(0.5,1,0.5,1),new=T)
plot(NA,NA,xlim=c(0,1),ylim=c(0,1),type="n",xaxt="n",yaxt="n",bty="n",
     xlab='',ylab='')

## Third panel
par(fig=c(0,0.5,0,0.5),new=T)
plot(EU_Depth,RES_Damage_Factors,type="l",xlim=c(depth_min,depth_max),ylim=c(perdamage_min,perdamage_max),xlab="Depth of water in house [ft]",ylab='Damages as a percent of house value [%]')

grid()
### Horizontal lines 
lines(x=c(mean_depth,40),y=c(mean_damage,mean_damage),col="darkgreen",xpd=T)
lines(x=c(mode_depth,40),y=c(mode_damage,mode_damage),xpd=T,col="blue")

## Vertical lines
lines(x=c(mean(tmp2-House_Initial_Stage),mean(tmp2-House_Initial_Stage)),y=c(mean_damage,500),xpd=T,col="darkgreen")
lines(x=c(BFE-House_Initial_Stage,BFE-House_Initial_Stage),y=c(mode_damage,500),xpd=T,col="blue")

# Arrows
arrows(x0=mean_depth,y0=150,x1=mean_depth,y1=100,col="darkgreen",length = 0.05)
arrows(x0=mode_depth,y0=150,x1=mode_depth,y1=100,col="blue",length = 0.05)

arrows(x0=15,y0=mean_damage,x1=20,y1=mean_damage,col="darkgreen",length = 0.05)
arrows(x0=15,y0=mode_damage,x1=20,y1=mode_damage,col="blue",length = 0.05)

#axis(3,at=,labels = "")#
#axis(4)

## Fourth panel
par(fig=c(0.35,0.8,0,0.5),new=T)
hst<-hist(100*temp/Struc_Value,plot=F,breaks=seq(perdamage_min,perdamage_max,length.out = 10))
barplot(hst$density, axes=F,space=0, horiz=TRUE)
h1=approx(x=c(perdamage_min,perdamage_max),y=c(0,length(hst$density)),xout=mean_damage)$y
h2=approx(x=c(perdamage_min,perdamage_max),y=c(0,length(hst$density)),xout=mode_damage)$y

lines(x=c(-100,0.020),y=c(h1,h1),col="darkgreen",lwd=1,xpd=T)
lines(x=c(-100,0.020),y=c(h2,h2),col="blue",lwd=1,xpd=T)

dev.off()
