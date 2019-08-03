pdf("Figure/xlrm.pdf", width =3.94, height =2.43)
par(cex=0.35)
plot(NA,NA,type="n",xlim=c(1,10),ylim=c(1,10),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")

# System relationsips
polygon(x=c(4,6,6,4),y=c(2,2,7,7),border=NA,col="darkgreen")
text(5,5,"System \nrelationships",cex=2)


# Objectives 
text(8,11,'Objectives',xpd=T,cex=3,col="gray",srt=0)

polygon(x=c(6.9,9,9,6.9),y=c(8,8,10,10),border=NA,col="gray")
polygon(x=c(6.9,9,9,6.9),y=c(5.5,5.5,7.5,7.5),border=NA,col="gray")
polygon(x=c(6.9,9,9,6.9),y=c(3,3,5,5),border=NA,col="gray")
polygon(x=c(6.9,9,9,6.9),y=c(0.5,0.5,2.5,2.5),border=NA,col="gray")

arrows(x0=6,y0=3,x1=6.9,y1=1.5,col="black",length = 0.05)
arrows(x0=6,y0=4,x1=6.9,y1=4,col="black",length = 0.05)
arrows(x0=6,y0=5,x1=6.9,y1=6.5,col="black",length = 0.05)
arrows(x0=6,y0=6,x1=6.9,y1=9,col="black",length = 0.05)




text(8,9,"Minimizing \ntotal cost",cex=2)
text(8,6.5,"Benefit/cost",cex=2)
text(8,4,"Upfront cost to \nhouse value",cex=2)
text(8,1.5,"Safety",cex=2)

# Levers 
text(5,11,'Levers',xpd=T,cex=3,col="orange")
polygon(x=c(4,6,6,4),y=c(8,8,10,10),border=NA,col="orange")
text(5,9,"Elevating?\nhow high?",cex=2)
arrows(x0=5,y0=8,x1=5,y1=7,col="black",length = 0.05)

# Uncertainties 
text(2,7,'Uncertainties',xpd=T,cex=3,col="gray",srt=0)
#polygon(x=c(1,3,3,1),y=c(7,7,9,9),border=NA,col="gray",xpd=T)
#polygon(x=c(1,3,3,1),y=c(1,1,3,3),border=NA,col="gray",xpd=T)

#text(2,8,"House size",cex=2)
polygon(x=c(1,3,3,1),y=c(4,4,6,6),border=NA,col="gray",xpd=T)
text(2,5,"Flooding \nfrequency",cex=2)
arrows(x0=3,y0=5,x1=4,y1=5,col="black",length = 0.05)
#text(2,2,"Discount rate",cex=2)

#grid()
dev.off()