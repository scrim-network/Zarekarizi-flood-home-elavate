rm(list=ls())

# Global variables
run_community=0 #if set to 1, houses will be generated and cost-benefit will be analyzed for each; if set to 0, only plotting would be active
n_houses=10000 # number of houses to generate/analyze 
load("~/Documents/Research/House_Elevation_Project/Source_Code/GEV/GEV_MCMC_Mean_Params.RData") # loads the MCMC chains for this gage 
mu=Mean_mu # average of last 10,000 iterations in the chain
xi=Mean_xi
sigma=Mean_sigma 
life_span=30 # assumes the lifespan of all houses are the same 
disc_rate=0.04 # assumes discounting rate does not change over time
FEMA_Return_periods=c(2,5,10,25,50,100,500) # These numbers are from FEMA; but they do not have a role in selecting the optimal policy
FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)

# load the cost-benefit calculator function
source('~/Documents/Research/House_Elevation_Project/Source_Code/Damages/Cost_Damage_Calculator.R')


if(run_community==1){
require(lhs) # package for latin hypercube sampling
z<- randomLHS(n_houses, 3)
SOWs=matrix(NA,n_houses,3)
SOWs[,1] <- qunif(z[,1],100,5000) # square footage of the house
SOWs[,2] <- qunif(z[,2],10000,1000000) # value of each house 
SOWs[,3] <- qunif(z[,3],-10,0) # location of each house compared to BFE 

# pre-allocation of variables
heightening_no <- heightening_fema <- heightening_opt <- rep(NA,n_houses)
total_cost_no  <-   total_cost_fema  <-  total_cost_opt<- rep(NA,n_houses)
totcost_to_value_no   <-   totcost_to_value_fema  <-  totcost_to_value_opt<- rep(NA,n_houses)
bc_no  <-   bc_fema  <- bc_opt<- rep(NA,n_houses)
safety_no   <-  safety_fema  <-  safety_opt<- rep(NA,n_houses)

# go over each house, run the cost-benefit analysis and save the results
for(i in 1:n_houses){
 print(i)
 tmp=findopt(SOWs[i,1],SOWs[i,2],SOWs[i,3],(SOWs[i,3]+35.3),life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi)
 
 heightening_no[i]=tmp[1,1]
 heightening_fema[i]=tmp[2,1]
 heightening_opt[i]=tmp[3,1]
 
 total_cost_no[i]=tmp[1,2]
 total_cost_fema[i]=tmp[2,2]
 total_cost_opt[i]=tmp[3,2]
 
 totcost_to_value_no[i]=tmp[1,5]
 totcost_to_value_fema[i]=tmp[2,5]
 totcost_to_value_opt[i]=tmp[3,5]
 
 bc_no[i]=tmp[1,4]
 bc_fema[i]=tmp[2,4]
 bc_opt[i]=tmp[3,4]
 
 safety_no[i]=tmp[1,3]
 safety_fema[i]=tmp[2,3]
 safety_opt[i]=tmp[3,3]
}
# save the results for all houses in one .RData file 
save(SOWs,heightening_no,heightening_fema,heightening_opt,total_cost_no,total_cost_fema,total_cost_opt,totcost_to_value_no,totcost_to_value_fema,totcost_to_value_opt,
     bc_no,bc_fema,bc_opt, safety_no, safety_fema,safety_opt,file="RData/Hypothetical_community_CBA.RData")
}
load("RData/Hypothetical_community_CBA.RData")

ConTable_Saf_No=rep(0,n_houses);ConTable_Saf_No[safety_no>=0.5]=1
ConTable_Saf_FE=rep(0,n_houses);ConTable_Saf_FE[safety_fema>=0.5]=1
ConTable_Saf_OP=rep(0,n_houses);ConTable_Saf_OP[safety_opt>=0.5]=1

ConTable_bc_NO=rep(0,n_houses);ConTable_bc_NO[bc_no>=1]=1
ConTable_bc_FE=rep(0,n_houses);ConTable_bc_FE[bc_fema>=1]=1
ConTable_bc_OP=rep(0,n_houses);ConTable_bc_OP[bc_opt>=1]=1

ConTable_tot2val_NO=rep(0,n_houses);ConTable_tot2val_NO[totcost_to_value_no<=0.5]=1
ConTable_tot2val_FE=rep(0,n_houses);ConTable_tot2val_FE[totcost_to_value_fema<=0.5]=1
ConTable_tot2val_OP=rep(0,n_houses);ConTable_tot2val_OP[totcost_to_value_opt<=0.5]=1

ConTable_Inertia_NO=rep(0,n_houses);ConTable_Inertia_NO[heightening_no==0]=1
ConTable_Inertia_FE=rep(0,n_houses);ConTable_Inertia_FE[heightening_fema==0]=1
ConTable_Inertia_OP=rep(0,n_houses);ConTable_Inertia_OP[heightening_opt==0]=1

score_no=rep(NA,n_houses);  score_no=ConTable_Inertia_NO+  ConTable_tot2val_NO+ ConTable_bc_NO+ ConTable_Saf_No
score_fema=rep(NA,n_houses);score_fema=ConTable_Inertia_FE+ConTable_tot2val_FE+ ConTable_bc_FE+ ConTable_Saf_FE
score_opt=rep(NA,n_houses); score_opt=ConTable_Inertia_OP+ ConTable_tot2val_OP+ ConTable_bc_OP+ ConTable_Saf_OP

sum(score_no==1 & score_fema==2 & score_opt==2)

100*sum(score_fema==3 & score_no<3 & score_opt<3)/n_houses
100*sum(score_no==3 & score_fema<3 & score_opt<3)/n_houses
100*sum(score_opt==3 & score_no<3 & score_fema<3)/n_houses
100*sum(score_opt==3 & score_no==3 & score_fema==3)/n_houses
100*sum(heightening_opt==0)/n_houses
Cont_Table=matrix(NA,5,3)

Cont_Table[1,1]<-sum(ConTable_Saf_No==1)/n_houses
Cont_Table[1,2]<-sum(ConTable_Saf_FE==1)/n_houses
Cont_Table[1,3]<-sum(ConTable_Saf_OP==1)/n_houses

Cont_Table[2,1]<-sum(ConTable_bc_NO==1)/n_houses
Cont_Table[2,2]<-sum(ConTable_bc_FE==1)/n_houses
Cont_Table[2,3]<-sum(ConTable_bc_OP==1)/n_houses

Cont_Table[3,1]<-sum(ConTable_tot2val_NO==1)/n_houses
Cont_Table[3,2]<-sum(ConTable_tot2val_FE==1)/n_houses
Cont_Table[3,3]<-sum(ConTable_tot2val_OP==1)/n_houses

Cont_Table[4,1]<-sum(ConTable_Inertia_NO==1)/n_houses
Cont_Table[4,2]<-sum(ConTable_Inertia_FE==1)/n_houses
Cont_Table[4,3]<-sum(ConTable_Inertia_OP==1)/n_houses

Cont_Table[5,1]<-sum(Cont_Table[1:4,1])
Cont_Table[5,2]<-sum(Cont_Table[1:4,2])
Cont_Table[5,3]<-sum(Cont_Table[1:4,3])

library(gridExtra)
par()
colnames(Cont_Table)<-c("Not Elevating","Elevating to FEMA","Elevating to OPT")
rownames(Cont_Table)<-c("Safety","Benefit/Cost","Total Cost/House value","Inertia","Total score")
grid.table(Cont_Table)

par(mfrow=c(3,1))
boxplot(cbind(safety_no,safety_fema,safety_opt))
boxplot(cbind(bc_no,bc_fema,bc_opt))
boxplot(cbind(totcost_to_value_no,totcost_to_value_fema,totcost_to_value_opt))

pdf(paste("Figure/Community_results_boxplots.pdf",sep=""), width =3.94, height =6.375)

par(cex=0.7,mai=c(0.1,0.1,0.1,0.1))
par(cex=0.7,fig=c(0.07,0.93,0.9,1))
boxplot(safety_no,horizontal = TRUE,col="orange")
mtext("Safety of no-action policy",side=3,line=-0.95,cex=0.7)
par(fig=c(0.07,0.93,0.77,0.87), new=TRUE)
boxplot(safety_opt,horizontal = TRUE,col="orange")
mtext("Safety of cost-benefit optimal policy",side=3,line=-0.95,cex=0.7)
par(fig=c(0.07,0.93,0.64,0.74), new=TRUE)
boxplot(bc_fema,horizontal = TRUE,col="orange")
mtext("B/C ratio of no-action policy",side=3,line=-0.95,cex=0.7)
par(fig=c(0.07,0.93,0.51,0.61), new=TRUE)
boxplot(bc_opt,horizontal = TRUE,col="orange")
mtext("B/C ratio of cost-benefit optimal policy",side=3,line=-0.95,cex=0.7)
par(fig=c(0.07,0.93,0.38,0.48), new=TRUE)
boxplot(totcost_to_value_no,horizontal = TRUE,col="orange")
mtext("Total cost-to-house value ratio of no-action policy",side=3,line=-0.95,cex=0.7)
par(fig=c(0.07,0.93,0.25,0.35), new=TRUE)
boxplot(totcost_to_value_fema,horizontal = TRUE,col="orange")
mtext("Total cost-to-house value ratio of FEMA policy",side=3,line=-0.95,cex=0.7)
par(fig=c(0.07,0.93,0.12,0.22), new=TRUE)
boxplot(totcost_to_value_opt,horizontal = TRUE,col="orange")
mtext("Total cost-to-house value ratio of cost-benefit optimal policy",side=3,line=-0.95,cex=0.7)
dev.off()

pdf(paste("Figure/barplot_results.pdf",sep=""), width =7, height =7)
par(cex=0.7,mai=c(0.1,0.1,0.1,0.1),bty="l")
library("colorspace") 
data=c(1767,1598,1353,1217,1189,978,873,637,150,67,63,48,33,21,4,2)
par(cex=0.7,fig=c(0.01,0.5,0.1,1))
barplot(rev(100*data/10000),horiz=TRUE,col=diverge_hcl(16),space=0)
dev.off()

# # load the results for all the houses. 
# load("RData/Hypothetical_community_CBA.RData")
# pdf("Figure/chain_of_hypothetical_houses_total_cost_comparison_with_FEMA.pdf", width =3.94, height =2.43)
# par(cex=0.5)
# plot(cba_opt_Totcost/1000,fema_recom_Totcost/1000,pch=20,col="blue",ylab="FEMA recommendation total cost [1,000 US$]",xlab="Optimal cost [1,000 US$]",cex=0.8)
# abline(1,1,col="gray",lwd=3)
# legend("right","1:1 line",bty="n",col="gray",lty=1,lwd=3)
# dev.off()
# pdf("Figure/chain_of_hypothetical_houses_boxplot_differences.pdf", width =3.94, height =2.43)
# par(cex=0.5)
# boxplot((fema_recom_Totcost-cba_opt_Totcost)/1000,horizontal = TRUE,xlab="Extra expenses [1,000 US$]",col="orange")
# dev.off()
# 100*sum(abs(fema_recom_Totcost-cba_opt_Totcost)<100)/length(cba_opt_Totcost)
# print()
# #plot(cba_opt_height,fema_recom_height)
# pdf("Figure/hypothetical_community_safety_histogram_differences.pdf", width =3.94, height =2.43)
# par(cex=0.5)
# hist(cba_opt_height-fema_recom_height,col="orange",freq=FALSE,xlab="Optimal Height - FEMA recommendation",ylab="Density",main="")
# abline(v=0,lwd=3,col="blue")
# dev.off()
# findopt(sqft,Struc_Value,del,House_Initial_Stage,life_span,disc_rate,FEMA_Return_periods,FEMA_Return_levels,mu,sigma,xi)
#Create data
set.seed(112)
data=matrix(sample(1:30,15) , nrow=3)
colnames(data)=c("A","B","C","D","E")
rownames(data)=c("var1","var2","var3")

# Get the stacked barplot
barplot(data, col=colors()[c(23,89,12)] , border="white", space=0.04, font.axis=2, xlab="group")

# Grouped barplot
barplot(data, col=colors()[c(23,89,12)] , border="white", font.axis=2, beside=T, legend=rownames(data), xlab="group", font.lab=2)


