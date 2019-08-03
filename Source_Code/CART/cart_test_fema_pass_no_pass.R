library(rpart)
library(rpart.plot)
load("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/Hypothetical_community_CBA_Unc_2019-05-31.RData")
mymat=cbind(SOWs,(fema_bc>1))

inds=which(    (!is.nan(fema_bc) & !is.na(fema_bc)) & 
               (!is.nan(fema_const_frac) & !is.na(fema_const_frac)) & 
                 (!is.nan(fema_safety) & !is.na(fema_safety)) 
               )

fema_pass<-fema_pass_char<-rep(NA,length(inds))
for(j in 1:length(inds)){
  i=inds[j]
  if(fema_bc[i]<1 & fema_const_frac[i]< 1){fema_pass[j]=1}
  else if(fema_bc[i]>1 & fema_const_frac[i]>=1){fema_pass[j]=2}
  else if(fema_bc[i]<1 & fema_const_frac[i]>=1){fema_pass[j]=3}
  else{fema_pass[j]=4}
}

for(i in 1:length(inds)){
  if(fema_pass[i]==1){fema_pass_char[i]="NO PASS"}
  else if(fema_pass[i]==2){fema_pass_char[i]="NO PASS"}
  else if(fema_pass[i]==3){fema_pass_char[i]="NO PASS"}
  else if(fema_pass[i]==4){fema_pass_char[i]="PASS"}
}
  
# fema_pass[i]=toString(sum(fema_bc[inds[i]]>1))
SOWss=SOWs
SOWss[,2]=SOWs[,2]/1000
mydf=cbind.data.frame(SOWss[inds,],fema_pass_char)
colnames(mydf)<-c('size[sqft]','value[1,000 US$]','elevation wrt BFE[ft]','lifespan[yrs]','pass')

z.auto <- rpart(pass ~ .,mydf)
z.auto
post(z.auto,file="",title. = "",horizontal = F,bg="red",fg="blue",colormodel="gray")

pdf("Figures/houses_fema_no_pass.pdf",width = 3.94,height=2.83)#   5.47)
par(cex=0.7)
rpart.plot(z.auto)
dev.off()
