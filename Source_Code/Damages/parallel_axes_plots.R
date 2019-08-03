# parallel_axes_plot.R 
# Author: Mahkameh Zarekarizi (mahkameh.zare@gmail.com)
# Goal: Making and saving parallel axes plots 
# Description: Parallel plots for optimal elevation for a sample set of houses 
# with respect to their characteristics such as 
# i. Their location with respect to the base flood elevation
# ii. Their value (inclusing content)
# iii. Their size
# House size indicates the cost of elevation 


load("RData/Hypothetical_community_CBA_Unc_2019-05-31.RData")
save_figures=0

########################################################################
indexs=which(optunc_height!=opt_height )
if(save_figures==1){
pdf("Figure/parallel_plots/unc_matters.pdf", width =3.94, height =2.43)
par(cex=0.45)
}
plot(NA,NA,type="n",xlim=c(1,6),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title('Parallel plot in houses where considering uncertainty changes the decision')
for (i in 1:length(indexs)){
  
  lines(1:6,c(punif(SOWs[indexs[i],1],min=min(SOWs[,1]),max=max(SOWs[,1])),
              punif(SOWs[indexs[i],2],min=min(SOWs[,2]),max=max(SOWs[,2])),
              punif(SOWs[indexs[i],3],min=min(SOWs[,3]),max=max(SOWs[,3])),
              punif(fema_height[indexs[i]],min=min(fema_height),max=max(fema_height)),
              punif(optunc_height[indexs[i]],min=min(optunc_height),max=max(optunc_height)),
              punif(opt_height[indexs[i]],min=min(opt_height),max=max(opt_height))
  ),
  col="orange",
    #rgb(0,0,1,alpha=0.5),
  lwd=0.1)
  
  #punif(Pr_0_floods_certain[i],min=min(Pr_0_floods),max=max(Pr_0_floods)),
  #punif(construction_cost_certain[i],min=min(Construction_SOW),max=max(Construction_SOW)),
  #punif(major_cost_certain[i],min=0,max=1),
  #punif(major_damage_certain[i],min=0,max=1)),lwd=0.5)
}
abline(v=c(1:6),col="black",lwd=2)
#axis(1,at=1:6,labels = c(paste(expression("sqft")),
#                         paste(expression("Value")),
#                         paste(expression("Location")),
#                         paste(expression("FEMA recomm-\nended elevation")),
#                         paste(expression("Optimal elevation \nunder uncertainty")),
#                         paste(expression("Optimal elevation \nignoring uncertainty"))
#                         ),
#     line=2,padj=0.5
#)

lines(x=c(1,6),y=c(0,0),lwd=2)
lines(x=c(1,6),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,1]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,1]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House \nsize[sqft]")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(SOWs[,2])/1000,2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(SOWs[,2])/1000,2),xpd=TRUE)
text(2,adj_y,paste(expression("House value\n[1,000 USD]")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(3,adj_y,paste(expression("House elevation \nw.r.t. BFE")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(fema_height),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(fema_height),2),xpd=TRUE)
text(4,adj_y,paste(expression("FEMA recomm-\nended elevation")),xpd=TRUE)

text(5-adj_x,-adj_y_num,signif(min(optunc_height),2),xpd=TRUE)
text(5-adj_x,1+adj_y_num,signif(max(optunc_height),2),xpd=TRUE)
text(5,adj_y,paste(expression("Optimal elevation \nunder uncertainty")),xpd=TRUE)

text(6-adj_x,-adj_y_num,signif(min(opt_height),2),xpd=TRUE)
text(6-adj_x,1+adj_y_num,signif(max(opt_height),2),xpd=TRUE)
text(6,adj_y,paste(expression("Optimal elevation \nignoring uncertainty")),xpd=TRUE)
if(save_figures==1){
  
dev.off()
}
########################################################################
indexs=which(optunc_height==opt_height & opt_height>0)
if(save_figures==1){
  
pdf("Figure/parallel_plots/unc_doesnt_matter_height_greater_than0.pdf", width =3.94, height =2.43)
par(cex=0.45)
}

plot(NA,NA,type="n",xlim=c(1,6),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title('Parallel plot in houses where elevating is cost optimal \nand considering uncertainty does not change the decision')
for (i in 1:length(indexs)){
  
  lines(1:6,c(punif(SOWs[indexs[i],1],min=min(SOWs[,1]),max=max(SOWs[,1])),
              punif(SOWs[indexs[i],2],min=min(SOWs[,2]),max=max(SOWs[,2])),
              punif(SOWs[indexs[i],3],min=min(SOWs[,3]),max=max(SOWs[,3])),
              punif(fema_height[indexs[i]],min=min(fema_height),max=max(fema_height)),
              punif(optunc_height[indexs[i]],min=min(optunc_height),max=max(optunc_height)),
              punif(opt_height[indexs[i]],min=min(opt_height),max=max(opt_height))
  ),
  col=rgb(0,0,1,alpha=0.5),
  lwd=1)
  
  #punif(Pr_0_floods_certain[i],min=min(Pr_0_floods),max=max(Pr_0_floods)),
  #punif(construction_cost_certain[i],min=min(Construction_SOW),max=max(Construction_SOW)),
  #punif(major_cost_certain[i],min=0,max=1),
  #punif(major_damage_certain[i],min=0,max=1)),lwd=0.5)
}
abline(v=c(1:6),col="black",lwd=2)
#axis(1,at=1:6,labels = c(paste(expression("sqft")),
#                         paste(expression("Value")),
#                         paste(expression("Location")),
#                         paste(expression("FEMA recomm-\nended elevation")),
#                         paste(expression("Optimal elevation \nunder uncertainty")),
#                         paste(expression("Optimal elevation \nignoring uncertainty"))
#                         ),
#     line=2,padj=0.5
#)

lines(x=c(1,6),y=c(0,0),lwd=2)
lines(x=c(1,6),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,1]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,1]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House \nsize[sqft]")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(SOWs[,2])/1000,2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(SOWs[,2])/1000,2),xpd=TRUE)
text(2,adj_y,paste(expression("House value\n[1,000 USD]")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(3,adj_y,paste(expression("House elevation \nw.r.t. BFE")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(fema_height),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(fema_height),2),xpd=TRUE)
text(4,adj_y,paste(expression("FEMA recomm-\nended elevation")),xpd=TRUE)

text(5-adj_x,-adj_y_num,signif(min(optunc_height),2),xpd=TRUE)
text(5-adj_x,1+adj_y_num,signif(max(optunc_height),2),xpd=TRUE)
text(5,adj_y,paste(expression("Optimal elevation \nunder uncertainty")),xpd=TRUE)

text(6-adj_x,-adj_y_num,signif(min(opt_height),2),xpd=TRUE)
text(6-adj_x,1+adj_y_num,signif(max(opt_height),2),xpd=TRUE)
text(6,adj_y,paste(expression("Optimal elevation \nignoring uncertainty")),xpd=TRUE)
if(save_figures==1){
  
dev.off()
}
########################################################################
indexs=which(optunc_height==opt_height & opt_height==0)
if(save_figures==1){
  
pdf("Figure/parallel_plots/unc_doesnt_matter_height_opt0.pdf", width =3.94, height =2.43)
par(cex=0.45)
}
plot(NA,NA,type="n",xlim=c(1,6),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title('Parallel plot in houses where elevating is not cost optimal \nand considering uncertainty does not change the decision')
for (i in 1:length(indexs)){
  
  lines(1:6,c(punif(SOWs[indexs[i],1],min=min(SOWs[,1]),max=max(SOWs[,1])),
              punif(SOWs[indexs[i],2],min=min(SOWs[,2]),max=max(SOWs[,2])),
              punif(SOWs[indexs[i],3],min=min(SOWs[,3]),max=max(SOWs[,3])),
              punif(fema_height[indexs[i]],min=min(fema_height),max=max(fema_height)),
              punif(optunc_height[indexs[i]],min=min(optunc_height),max=max(optunc_height)),
              punif(opt_height[indexs[i]],min=min(opt_height),max=max(opt_height))
  ),
  col="orange",
    #rgb(0,0,1,alpha=0.5),
  lwd=0.1)
  
  #punif(Pr_0_floods_certain[i],min=min(Pr_0_floods),max=max(Pr_0_floods)),
  #punif(construction_cost_certain[i],min=min(Construction_SOW),max=max(Construction_SOW)),
  #punif(major_cost_certain[i],min=0,max=1),
  #punif(major_damage_certain[i],min=0,max=1)),lwd=0.5)
}
abline(v=c(1:6),col="black",lwd=2)
#axis(1,at=1:6,labels = c(paste(expression("sqft")),
#                         paste(expression("Value")),
#                         paste(expression("Location")),
#                         paste(expression("FEMA recomm-\nended elevation")),
#                         paste(expression("Optimal elevation \nunder uncertainty")),
#                         paste(expression("Optimal elevation \nignoring uncertainty"))
#                         ),
#     line=2,padj=0.5
#)

lines(x=c(1,6),y=c(0,0),lwd=2)
lines(x=c(1,6),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,1]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,1]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House \nsize[sqft]")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(SOWs[,2])/1000,2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(SOWs[,2])/1000,2),xpd=TRUE)
text(2,adj_y,paste(expression("House value\n[1,000 USD]")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(3,adj_y,paste(expression("House elevation \nw.r.t. BFE")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(fema_height),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(fema_height),2),xpd=TRUE)
text(4,adj_y,paste(expression("FEMA recomm-\nended elevation")),xpd=TRUE)

text(5-adj_x,-adj_y_num,signif(min(optunc_height),2),xpd=TRUE)
text(5-adj_x,1+adj_y_num,signif(max(optunc_height),2),xpd=TRUE)
text(5,adj_y,paste(expression("Optimal elevation \nunder uncertainty")),xpd=TRUE)

text(6-adj_x,-adj_y_num,signif(min(opt_height),2),xpd=TRUE)
text(6-adj_x,1+adj_y_num,signif(max(opt_height),2),xpd=TRUE)
text(6,adj_y,paste(expression("Optimal elevation \nignoring uncertainty")),xpd=TRUE)
if(save_figures==1){
  
dev.off()

}
########################################################################################################################
########################################################################################################################
########################################################################################################################

# A parallel plot color coded for the houses where raising makes sense and houses that does not make sense.
# This is for hosues where uncertainty quantification results agree with ignoring uncertainty results

indexs=which(optunc_height>0 & opt_height>0)
naxis=5

if(save_figures==1){
  pdf("Figure/parallel_plots/parallel_brush_raise_noraise.pdf", width =3.94, height =2.43)
  par(cex=0.45)
}

plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
for (i in 1:length(indexs)){
  
  lines(1:naxis,c(punif(SOWs[indexs[i],1],min=min(SOWs[,1]),max=max(SOWs[,1])),
                  punif(SOWs[indexs[i],2],min=min(SOWs[,2]),max=max(SOWs[,2])),
                  punif(SOWs[indexs[i],3],min=min(SOWs[,3]),max=max(SOWs[,3])),
                  punif(opt_height[indexs[i]],min=min(opt_height),max=max(opt_height)),
                  punif(optunc_height[indexs[i]],min=min(optunc_height),max=max(optunc_height))
  ),
  col=rgb(0,0,1,alpha=0.5),
  lwd=0.1)
}

indexs=which(optunc_height==0 & opt_height==0)
for (i in 1:length(indexs)){
  lines(1:naxis,c(punif(SOWs[indexs[i],1],min=min(SOWs[,1]),max=max(SOWs[,1])),
                  punif(SOWs[indexs[i],2],min=min(SOWs[,2]),max=max(SOWs[,2])),
                  punif(SOWs[indexs[i],3],min=min(SOWs[,3]),max=max(SOWs[,3])),
                  punif(opt_height[indexs[i]],min=min(opt_height),max=max(opt_height)),
                  punif(optunc_height[indexs[i]],min=min(optunc_height),max=max(optunc_height))
  ),
  col="orange",
  lwd=0.1)
}


indexs=which(optunc_bc<1 | opt_bc<1 | optunc_const_frac>1 | opt_const_frac>1)
for (i in 1:length(indexs)){
  lines(1:naxis,c(punif(SOWs[indexs[i],1],min=min(SOWs[,1]),max=max(SOWs[,1])),
                  punif(SOWs[indexs[i],2],min=min(SOWs[,2]),max=max(SOWs[,2])),
                  punif(SOWs[indexs[i],3],min=min(SOWs[,3]),max=max(SOWs[,3])),
                  punif(opt_height[indexs[i]],min=min(opt_height),max=max(opt_height)),
                  punif(optunc_height[indexs[i]],min=min(optunc_height),max=max(optunc_height))
  ),
  col="red",
  lwd=0.1)
}



abline(v=c(1:naxis),col="black",lwd=2)
lines(x=c(1,naxis),y=c(0,0),lwd=2)
lines(x=c(1,naxis),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,1]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,1]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House \nsize[sqft]")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(SOWs[,2])/1000,2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(SOWs[,2])/1000,2),xpd=TRUE)
text(2,adj_y,paste(expression("House value\n[1,000 USD]")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(3,adj_y,paste(expression("House elevation \nw.r.t. BFE")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(opt_height),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(opt_height),2),xpd=TRUE)
text(4,adj_y,paste(expression("Optimal elevation \nignoring uncertainty")),xpd=TRUE)

text(5-adj_x,-adj_y_num,signif(min(optunc_height),2),xpd=TRUE)
text(5-adj_x,1+adj_y_num,signif(max(optunc_height),2),xpd=TRUE)
text(5,adj_y,paste(expression("Optimal elevation \nunder uncertainty")),xpd=TRUE)

if(save_figures==1){dev.off()}
########################################################################################################################
########################################################################################################################
########################################################################################################################





########################################################################
########################################################################
########################################################################
if(save_figures==1){
  pdf("Figure/parallel_plots/objectives_for_femaandOPT.pdf", width =3.94, height =6.375)
  par(cex=0.5,mai=c(0.05,0.1,0.3,0.1))
  
  par(cex=0.5,fig=c(0.07,0.93,0.6,0.95))
}
indexes=which((fema_height+fema_bc+fema_safety+fema_const_frac+fema_totcost_frac>=0  | 
                 fema_height+fema_bc+fema_safety+fema_const_frac+fema_totcost_frac<0) &
                fema_bc>1)
naxis=4
plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),
                  punif(fema_height[indexes[i]],min=min(fema_height,na.rm = TRUE),max=max(fema_height,na.rm = TRUE)),
                  punif(fema_bc[indexes[i]],min=min(fema_bc,na.rm = TRUE),max=max(fema_bc,na.rm = TRUE)),
                  #punif(fema_safety[indexes[i]],min=min(fema_safety,na.rm = TRUE),max=max(fema_safety,na.rm = TRUE)),
                  #punif(fema_safety[indexes[i]],min=0,max=1),
                  punif(fema_const_frac[indexes[i]],min=min(fema_const_frac,na.rm = TRUE),max=max(fema_const_frac,na.rm = TRUE))
                  #punif(fema_totcost_frac[indexes[i]],min=min(fema_totcost_frac,na.rm = TRUE),max=max(fema_totcost_frac,na.rm = TRUE))
  ),
  col="darkgreen",
  lwd=0.5)
}

indexes=which((fema_height+fema_bc+fema_safety+fema_const_frac+fema_totcost_frac>=0  | 
                 fema_height+fema_bc+fema_safety+fema_const_frac+fema_totcost_frac<0) &
                fema_bc<1 | fema_const_frac>1)
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),
                  punif(fema_height[indexes[i]],min=min(fema_height,na.rm = TRUE),max=max(fema_height,na.rm = TRUE)),
                  punif(fema_bc[indexes[i]],min=min(fema_bc,na.rm = TRUE),max=max(fema_bc,na.rm = TRUE)),
                  #punif(fema_safety[indexes[i]],min=min(fema_safety,na.rm = TRUE),max=max(fema_safety,na.rm = TRUE)),
                  #punif(fema_safety[indexes[i]],min=0,max=1),
                  punif(fema_const_frac[indexes[i]],min=min(fema_const_frac,na.rm = TRUE),max=max(fema_const_frac,na.rm = TRUE))
                  #punif(fema_totcost_frac[indexes[i]],min=min(fema_totcost_frac,na.rm = TRUE),max=max(fema_totcost_frac,na.rm = TRUE))
  ),
  col=rgb(1,0,0,alpha=0.5),
  lwd=0.5)
}




abline(v=c(1:naxis),col="black",lwd=2)
lines(x=c(1,naxis),y=c(0,0),lwd=2)
lines(x=c(1,naxis),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House elevation\nw.r.t. BFE")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(fema_height,na.rm = TRUE),2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(fema_height,na.rm=TRUE),2),xpd=TRUE)
text(2,adj_y,paste(expression("FEMA recommen-\nded Height")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(fema_bc,na.rm=TRUE),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(fema_bc,na.rm=TRUE),2),xpd=TRUE)
text(3,adj_y,paste(expression("B/C")),xpd=TRUE)

#text(4-adj_x,-adj_y_num,signif(min(fema_safety,na.rm=TRUE)*100,2),xpd=TRUE)
#text(4-adj_x,1+adj_y_num,signif(max(fema_safety,na.rm=TRUE)*100,2),xpd=TRUE)
#text(4,adj_y,paste(expression("Safety")),xpd=TRUE)

#text(4-adj_x,-adj_y_num,0,xpd=TRUE)
#text(4-adj_x,1+adj_y_num,100,xpd=TRUE)
#text(4,adj_y,paste(expression("Safety")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(fema_const_frac,na.rm=TRUE),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(fema_const_frac,na.rm=TRUE),2),xpd=TRUE)
text(4,adj_y,paste(expression("Construction cost as a \nfraction of house value")),xpd=TRUE)
#if(save_figures==1){dev.off()}
########################################################################
########################################################################
########################################################################
if(save_figures==1){
  #pdf("Figure/parallel_plots/objectives_for_opt.pdf", width =3.94, height =2.43)
  par(cex=0.5,fig=c(0.07,0.93,0.1,0.45),new=TRUE)
}
indexes=which((opt_height+opt_bc+opt_safety+opt_const_frac+opt_totcost_frac>=0  | 
                 opt_height+opt_bc+opt_safety+opt_const_frac+opt_totcost_frac<0) &
                opt_bc>1)
naxis=4
plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),
                  punif(opt_height[indexes[i]],min=min(opt_height,na.rm = TRUE),max=max(opt_height,na.rm = TRUE)),
                  punif(opt_bc[indexes[i]],min=min(opt_bc,na.rm = TRUE),max=max(opt_bc,na.rm = TRUE)),
                  #punif(opt_safety[indexes[i]],min=min(opt_safety,na.rm = TRUE),max=max(opt_safety,na.rm = TRUE)),
                  #punif(opt_safety[indexes[i]],min=0,max=1),
                  punif(opt_const_frac[indexes[i]],min=min(opt_const_frac,na.rm = TRUE),max=max(opt_const_frac,na.rm = TRUE))
                  #punif(opt_totcost_frac[indexes[i]],min=min(opt_totcost_frac,na.rm = TRUE),max=max(opt_totcost_frac,na.rm = TRUE))
  ),
  col="darkgreen",
  lwd=0.5)
}

indexes=which((opt_height+opt_bc+opt_safety+opt_const_frac+opt_totcost_frac>=0  | 
                 opt_height+opt_bc+opt_safety+opt_const_frac+opt_totcost_frac<0) &
                opt_bc<1 | opt_const_frac>1)
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),
                  punif(opt_height[indexes[i]],min=min(opt_height,na.rm = TRUE),max=max(opt_height,na.rm = TRUE)),
                  punif(opt_bc[indexes[i]],min=min(opt_bc,na.rm = TRUE),max=max(opt_bc,na.rm = TRUE)),
                  #punif(opt_safety[indexes[i]],min=min(opt_safety,na.rm = TRUE),max=max(opt_safety,na.rm = TRUE)),
                  #punif(opt_safety[indexes[i]],min=0,max=1),
                  punif(opt_const_frac[indexes[i]],min=min(opt_const_frac,na.rm = TRUE),max=max(opt_const_frac,na.rm = TRUE))
                  #punif(opt_totcost_frac[indexes[i]],min=min(opt_totcost_frac,na.rm = TRUE),max=max(opt_totcost_frac,na.rm = TRUE))
  ),
  col=rgb(1,0,0,alpha=0.5),
  lwd=0.5)
}




abline(v=c(1:naxis),col="black",lwd=2)
lines(x=c(1,naxis),y=c(0,0),lwd=2)
lines(x=c(1,naxis),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House elevation\nw.r.t. BFE")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(opt_height,na.rm = TRUE),2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(opt_height,na.rm=TRUE),2),xpd=TRUE)
text(2,adj_y,paste(expression("opt recommen-\nded Height")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(opt_bc,na.rm=TRUE),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(opt_bc,na.rm=TRUE),2),xpd=TRUE)
text(3,adj_y,paste(expression("B/C")),xpd=TRUE)

#text(4-adj_x,-adj_y_num,signif(min(opt_safety,na.rm=TRUE)*100,2),xpd=TRUE)
#text(4-adj_x,1+adj_y_num,signif(max(opt_safety,na.rm=TRUE)*100,2),xpd=TRUE)
#text(4,adj_y,paste(expression("Safety")),xpd=TRUE)

#text(4-adj_x,-adj_y_num,0,xpd=TRUE)
#text(4-adj_x,1+adj_y_num,100,xpd=TRUE)
#text(4,adj_y,paste(expression("Safety")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(opt_const_frac,na.rm=TRUE),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(opt_const_frac,na.rm=TRUE),2),xpd=TRUE)
text(4,adj_y,paste(expression("Construction cost as a \nfraction of house value")),xpd=TRUE)
if(save_figures==1){dev.off()}

########################################################################
########################################################################
########################################################################
if(save_figures==1){
  pdf("Figure/parallel_plots/everything_for_optunc.pdf", width =3.94, height =2.43)
  par(cex=0.35)
}
#indexes=which((optunc_height+optunc_bc+optunc_safety+optunc_const_frac+optunc_totcost_frac>=0  | 
#                 optunc_height+optunc_bc+optunc_safety+optunc_const_frac+optunc_totcost_frac<0) &
#                optunc_bc>1)
indexes=1:1000
naxis=8
plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],1],min=min(SOWs[,1],na.rm = TRUE),max=max(SOWs[,1],na.rm = TRUE)),
                  punif(SOWs[indexes[i],2],min=min(SOWs[,2],na.rm = TRUE),max=max(SOWs[,2],na.rm = TRUE)),
                  punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),
                  punif(SOWs[indexes[i],4],min=min(SOWs[,4],na.rm = TRUE),max=max(SOWs[,4],na.rm = TRUE)),
                  punif(optunc_height[indexes[i]],min=min(optunc_height,na.rm = TRUE),max=max(optunc_height,na.rm = TRUE)),
                  #punif(optunc_safety[indexes[i]],min=min(optunc_safety,na.rm = TRUE),max=max(optunc_safety,na.rm = TRUE)),
                  punif(optunc_safety[indexes[i]],min=0,max=1),
                  punif(optunc_const_frac[indexes[i]],min=min(optunc_const_frac,na.rm = TRUE),max=max(optunc_const_frac,na.rm = TRUE)),
                  punif(optunc_bc[indexes[i]],min=min(optunc_bc,na.rm = TRUE),max=max(optunc_bc,na.rm = TRUE))
                  #punif(optunc_totcost_frac[indexes[i]],min=min(optunc_totcost_frac,na.rm = TRUE),max=max(optunc_totcost_frac,na.rm = TRUE))
  ),
  col="darkgreen",
  lwd=0.1)
}

indexes=which(optunc_height==0)
naxis=8
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],1],min=min(SOWs[,1],na.rm = TRUE),max=max(SOWs[,1],na.rm = TRUE)),
                  punif(SOWs[indexes[i],2],min=min(SOWs[,2],na.rm = TRUE),max=max(SOWs[,2],na.rm = TRUE)),
                  punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),
                  punif(SOWs[indexes[i],4],min=min(SOWs[,4],na.rm = TRUE),max=max(SOWs[,4],na.rm = TRUE)),
                  punif(optunc_height[indexes[i]],min=min(optunc_height,na.rm = TRUE),max=max(optunc_height,na.rm = TRUE)),
                  #punif(optunc_safety[indexes[i]],min=min(optunc_safety,na.rm = TRUE),max=max(optunc_safety,na.rm = TRUE)),
                  punif(optunc_safety[indexes[i]],min=0,max=1),
                  punif(optunc_const_frac[indexes[i]],min=min(optunc_const_frac,na.rm = TRUE),max=max(optunc_const_frac,na.rm = TRUE)),
                  punif(optunc_bc[indexes[i]],min=min(optunc_bc,na.rm = TRUE),max=max(optunc_bc,na.rm = TRUE))
                  #punif(optunc_totcost_frac[indexes[i]],min=min(optunc_totcost_frac,na.rm = TRUE),max=max(optunc_totcost_frac,na.rm = TRUE))
  ),
  col="orange",
  lwd=0.1)
}

indexes=which(optunc_bc<1 | optunc_const_frac>1 | optunc_safety<0.5)
naxis=8
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],1],min=min(SOWs[,1],na.rm = TRUE),max=max(SOWs[,1],na.rm = TRUE)),
                  punif(SOWs[indexes[i],2],min=min(SOWs[,2],na.rm = TRUE),max=max(SOWs[,2],na.rm = TRUE)),
                  punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),
                  punif(SOWs[indexes[i],4],min=min(SOWs[,4],na.rm = TRUE),max=max(SOWs[,4],na.rm = TRUE)),
                  punif(optunc_height[indexes[i]],min=min(optunc_height,na.rm = TRUE),max=max(optunc_height,na.rm = TRUE)),
                  #punif(optunc_safety[indexes[i]],min=min(optunc_safety,na.rm = TRUE),max=max(optunc_safety,na.rm = TRUE)),
                  punif(optunc_safety[indexes[i]],min=0,max=1),
                  punif(optunc_const_frac[indexes[i]],min=min(optunc_const_frac,na.rm = TRUE),max=max(optunc_const_frac,na.rm = TRUE)),
                  punif(optunc_bc[indexes[i]],min=min(optunc_bc,na.rm = TRUE),max=max(optunc_bc,na.rm = TRUE))
                  #punif(optunc_totcost_frac[indexes[i]],min=min(optunc_totcost_frac,na.rm = TRUE),max=max(optunc_totcost_frac,na.rm = TRUE))
  ),
  col="red",
  lwd=0.1)
}




abline(v=c(1:naxis),col="black",lwd=2)
lines(x=c(1,naxis),y=c(0,0),lwd=2)
lines(x=c(1,naxis),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,1]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,1]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House Size\n[sqft]")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(SOWs[,2]),2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(SOWs[,2]),2),xpd=TRUE)
text(2,adj_y,paste(expression("House value\n[1,000 US$]")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(3,adj_y,paste(expression("House elevation\nw.r.t. BFE[ft]")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(SOWs[,4]),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(SOWs[,4]),2),xpd=TRUE)
text(4,adj_y,paste(expression("House life-\nspan[years]")),xpd=TRUE)

text(5-adj_x,-adj_y_num,signif(min(optunc_height,na.rm = TRUE),2),xpd=TRUE)
text(5-adj_x,1+adj_y_num,signif(max(optunc_height,na.rm=TRUE),2),xpd=TRUE)
text(5,adj_y,paste(expression("optimal height-\nunder uncertainty")),xpd=TRUE)

text(6-adj_x,-adj_y_num,signif(min(optunc_safety,na.rm=TRUE)*100,2),xpd=TRUE)
text(6-adj_x,1+adj_y_num,signif(max(optunc_safety,na.rm=TRUE)*100,2),xpd=TRUE)
text(6,adj_y,paste(expression("Safety[%]")),xpd=TRUE)

text(7-adj_x,-adj_y_num,signif(min(optunc_const_frac,na.rm=TRUE),2),xpd=TRUE)
text(7-adj_x,1+adj_y_num,signif(max(optunc_const_frac,na.rm=TRUE),2),xpd=TRUE)
text(7,adj_y,paste(expression("Construction cost to\nhouse value ratio")),xpd=TRUE)


text(8-adj_x,-adj_y_num,signif(min(optunc_bc,na.rm=TRUE),2),xpd=TRUE)
text(8-adj_x,1+adj_y_num,signif(max(optunc_bc,na.rm=TRUE),2),xpd=TRUE)
text(8,adj_y,paste(expression("B/C")),xpd=TRUE)

if(save_figures==1){dev.off()}




########################################################################
########################################################################
########################################################################
if(save_figures==1){
  pdf("Figure/parallel_plots/houses_with_fema_bc_less_than1.pdf", width =3.94, height =2.43)
  par(cex=0.35)
}
indexes=which(fema_bc<1 & optunc_height==0)
naxis=5
plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
title("Houses where FEMA's recommendation does not pass the cost-benefit test")
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],1],min=min(SOWs[,1],na.rm = TRUE),max=max(SOWs[,1],na.rm = TRUE)),
                  punif(SOWs[indexes[i],2],min=min(SOWs[,2],na.rm = TRUE),max=max(SOWs[,2],na.rm = TRUE)),
                  punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),                  
                  punif(fema_height[indexes[i]],min=min(fema_height,na.rm = TRUE),max=max(fema_height,na.rm = TRUE)),
                  punif(optunc_height[indexes[i]],min=min(optunc_height,na.rm = TRUE),max=max(optunc_height,na.rm = TRUE))
  ),
  col="blue",
  lwd=0.1)
}

indexes=which(fema_bc<1 & optunc_height>0)
for (i in 1:length(indexes)){
  lines(1:naxis,c(punif(SOWs[indexes[i],1],min=min(SOWs[,1],na.rm = TRUE),max=max(SOWs[,1],na.rm = TRUE)),
                  punif(SOWs[indexes[i],2],min=min(SOWs[,2],na.rm = TRUE),max=max(SOWs[,2],na.rm = TRUE)),
                  punif(SOWs[indexes[i],3],min=min(SOWs[,3],na.rm = TRUE),max=max(SOWs[,3],na.rm = TRUE)),                  
                  punif(fema_height[indexes[i]],min=min(fema_height,na.rm = TRUE),max=max(fema_height,na.rm = TRUE)),
                  punif(optunc_height[indexes[i]],min=min(optunc_height,na.rm = TRUE),max=max(optunc_height,na.rm = TRUE))
  ),
  col="orange",
  lwd=0.1)
}


abline(v=c(1:naxis),col="black",lwd=2)
lines(x=c(1,naxis),y=c(0,0),lwd=2)
lines(x=c(1,naxis),y=c(1,1),lwd=2)

adj_y=-0.15
adj_x=0.15
adj_y_num=0.05

text(1-adj_x,-adj_y_num,signif(min(SOWs[,1]),2),xpd=TRUE)
text(1-adj_x,1+adj_y_num,signif(max(SOWs[,1]),2),xpd=TRUE)
text(1,adj_y,paste(expression("House Size\n[sqft]")),xpd=TRUE)

text(2-adj_x,-adj_y_num,signif(min(SOWs[,2]/1000),2),xpd=TRUE)
text(2-adj_x,1+adj_y_num,signif(max(SOWs[,2]/1000),2),xpd=TRUE)
text(2,adj_y,paste(expression("House value\n[1,000 US$]")),xpd=TRUE)

text(3-adj_x,-adj_y_num,signif(min(SOWs[,3]),2),xpd=TRUE)
text(3-adj_x,1+adj_y_num,signif(max(SOWs[,3]),2),xpd=TRUE)
text(3,adj_y,paste(expression("House elevation\nw.r.t. BFE [ft]")),xpd=TRUE)

text(4-adj_x,-adj_y_num,signif(min(fema_height,na.rm = TRUE),2),xpd=TRUE)
text(4-adj_x,1+adj_y_num,signif(max(fema_height,na.rm=TRUE),2),xpd=TRUE)
text(4,adj_y,paste(expression("FEMA \nrecommendation [ft]")),xpd=TRUE)

text(5-adj_x,-adj_y_num,signif(min(optunc_height,na.rm=TRUE),2),xpd=TRUE)
text(5-adj_x,1+adj_y_num,signif(max(optunc_height,na.rm=TRUE),2),xpd=TRUE)
text(5,adj_y,paste(expression("Optimal height \nunder uncertainty [ft]")),xpd=TRUE)

if(save_figures==1){dev.off()}


