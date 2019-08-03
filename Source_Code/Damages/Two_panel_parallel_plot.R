
########################################################################
########################################################################
########################################################################
if(save_figures==1){
  pdf("Figure/parallel_plots/objectives_for_femaandOPT.pdf", width =3.94, height =6.375)
  par(cex=0.5,mai=c(0.3,0.2,0.2,0.2))
  
  par(cex=0.5,fig=c(0.07,0.93,0.6,0.9))
}
indexes=which((fema_height+fema_safety+fema_const_frac+fema_totcost_frac>=0  | 
                 fema_height+fema_safety+fema_const_frac+fema_totcost_frac<0))
naxis=4
plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
text(2.5,1.15,"FEMA recommended policy",xpd=TRUE)
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

indexes=which((fema_height+fema_safety+fema_const_frac+fema_totcost_frac>=0  | 
                 fema_height+fema_safety+fema_const_frac+fema_totcost_frac<0) &
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
text(4,adj_y,paste(expression("Construction cost \nto house value")),xpd=TRUE)
#if(save_figures==1){dev.off()}
########################################################################
########################################################################
par(cex=0.5,fig=c(0.07,0.93,0.4,0.62),new=TRUE)
plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
legend("top",c("B/C<1 or Upfront cost > House value","Otherwise"),lty=c(1,1),col=c(2,3),bty="o",horiz=FALSE,cex=1.2,bg="lightgray",box.col="lightgray")
########################################################################
########################################################################
if(save_figures==1){
  #pdf("Figure/parallel_plots/objectives_for_opt.pdf", width =3.94, height =2.43)
  par(cex=0.5,fig=c(0.07,0.93,0.2,0.5),new=TRUE)
}
indexes=which((opt_height+opt_safety+opt_const_frac+opt_totcost_frac>=0  | 
                 opt_height+opt_safety+opt_const_frac+opt_totcost_frac<0))
naxis=4
plot(NA,NA,type="n",xlim=c(1,naxis),ylim=c(0,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
text(2.5,1.15,"Cost optimal policy",xpd=TRUE)
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

indexes=which((opt_height+opt_safety+opt_const_frac+opt_totcost_frac>=0  | 
                 opt_height+opt_safety+opt_const_frac+opt_totcost_frac<0) &
                opt_bc<1 | opt_const_frac>1 | is.nan(opt_bc))
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




#abline(v=c(1:naxis),col="black",lwd=2)
#lines(x=c(1,naxis),y=c(0,0),lwd=2)
#lines(x=c(1,naxis),y=c(1,1),lwd=2)

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
text(4,adj_y,paste(expression("Construction cost \nto house value")),xpd=TRUE)
if(save_figures==1){dev.off()}
