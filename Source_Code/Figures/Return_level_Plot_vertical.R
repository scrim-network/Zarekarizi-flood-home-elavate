rm(list=ls())
setwd('~/Documents/Research/House_Elevation_Project/Source_Code/Freq_Analysis')

# Flood chances based on FEMA 
FEMA_Return_periods=c(2,5,10,25,50,100,500)
FEMA_No_Exceed_Chance=1-1/(FEMA_Return_periods)
FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)

pdf("Figures/FEMA_return_levels_vertical.pdf", width =3.94, height =2.43)
par(cex=0.5)
barplot(width=c(21.3,3.6,2.4,3.1, 2.4 , 2.5 , 6.0),
        space=0,height=1-FEMA_No_Exceed_Chance,
        horiz=TRUE,xlab="Flooding Probability",axes=TRUE,
        ylim=c(0,50),axis.lty=0,xlim=c(0,0.6),ylab="Stage (ft)",
        col="darkgray",xaxt="n",border=NA)
lines(c(0,0.5),c(34.3,34.3),lwd=2,col="blue",lty=3)
abline(h=FEMA_Return_levels,col="darkgreen",lwd=2)
text(0.57,FEMA_Return_levels[1]+0.7,"2-yr",cex=0.9)
text(0.57,FEMA_Return_levels[2]+0.7,"5-yr",cex=0.9)
text(0.57,FEMA_Return_levels[3]+0.7,"10-yr",cex=0.9)
text(0.57,FEMA_Return_levels[4]+0.7,"25-yr",cex=0.9)
text(0.57,FEMA_Return_levels[5]+0.7,"50-yr",cex=0.9)
text(0.57,FEMA_Return_levels[6]+0.7,"100-yr",cex=0.9)
text(0.57,FEMA_Return_levels[7]+0.7,"500-yr",cex=0.9)
#points(0.3,36.6,pch=15,cex=5,col="blue")
#points(0.3,40,pch=17,cex=5,col="orange")

axis(2)
axis(1,pos=0)

legend("topleft",c("FEMA Return levels","Initial house elevation"),
       col=c("darkgreen","blue"),lty=c(1,3),lwd=c(2,2),bty="n")
dev.off()