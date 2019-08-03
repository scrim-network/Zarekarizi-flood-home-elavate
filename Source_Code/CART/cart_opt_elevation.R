library(rpart)
library(rpart.plot)
load("~/Documents/Research/House_Elevation_Project/Source_Code/Damages/RData/Hypothetical_community_CBA_Unc_2019-05-31.RData")
SOWss=SOWs
SOWss[,2]=SOWs[,2]/1000
elevate<-elevate_char<-rep(NA,length(SOWs[,1]))
for(i in 1:length(SOWss[,1])){
  if(optunc_height[i]>0){elevate[i]=1}
  else if(optunc_height[i]==0){elevate[i]=0}
}
for(i in 1:length(SOWss[,2])){
  if(elevate[i]==1){elevate_char[i]="RIASE"}
  else if(elevate[i]==0){elevate_char[i]="NO RAISE"}
}

mydf=cbind.data.frame(SOWss,elevate_char)
colnames(mydf)<-c('size[sqft]','value[1,000 US$]','elevation wrt BFE[ft]','lifespan[yrs]','Elevate')

z.auto <- rpart(Elevate ~ .,mydf)
z.auto
post(z.auto,file="",title. = "",horizontal = F,bg="red",fg="blue",colormodel="gray")

pdf("Figures/all_houses_if_elevate.pdf",width = 3.94,height=2.83)#   5.47)
par(cex=0.7)
rpart.plot(z.auto)
dev.off()
