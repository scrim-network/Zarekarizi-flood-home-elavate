##==============================================================================
##
## Script plots the considered uncertainty in the house lifetime 
##
## Authors: Mahkameh Zarekarizi (mahkameh.zare@gmail.com) 
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
## Instructions to run:
##    change the directory to the folder called "flood_mititgation_uncertainty" and run the script 
##==============================================================================

library(truncnorm)
main_path=getwd()

mygreen <- rgb(44/255, 185/255, 95/255, 0.5) #Colors we will use in the plot
myblue <- rgb(0/255, 128/255, 1, 0.3)
mygreenl <- rgb(44/255, 185/255, 95/255, 1) #Colors we will use in the plot
mybluel <- rgb(0/255, 128/255, 1, 1)


#png(paste(main_path,"/Figures/S13_Lifetime_Unc.png",sep=""),width =3.94, height =2.43,units="in",res=300)
#jpeg(paste(main_path,"/Figures/S13_Lifetime_Unc.jpeg",sep=""),width =3.94, height =2.43,units="in",res=300)
pdf(paste(main_path,"/Figures/S13_Lifetime_Deep_Unc.pdf",sep=""),width =3.94, height =2.43)
par(cex=0.5)
x=sort(rweibull(100000,shape=2.8,scale=73.5))
d=dweibull(x,2.8,73.5)

plot(x,d,col="blue",type="n",
     xlab="Lifetime [yrs]",ylab="Probability density",xlim=c(min(x),max(x)),
     ylim=c(min(d),max(d)),axes=F,yaxs="i",xaxs="i")
axis(1,pos=min(d),at<-seq(min(x),max(x),length.out = 10),labels = round(at))
axis(2,pos=min(x),at<-seq(min(d),max(d),length.out = 5),labels = signif(at,1))
lines(x=c(min(x),max(x)),y=c(max(d),max(d)),lwd=2)
lines(x=c(max(x),max(x)),y=c(min(d),max(d)),lwd=2)

polygon(x=c(x,rev(x)),y=c(rep(0,length(x)),rev(d)),col=myblue,border=NA)
abline(v=c(30,50,80),col=c("black","black","black"))
text(25,max(d)+max(d)*0.05,xpd=T,'Xian et al., 2017',cex=0.8,col="black")
text(50,max(d)+max(d)*0.11,xpd=T,'Veerbeek and Zevenbergen, 2009',cex=0.8,col="black")
arrows(x0=50,y0=max(d),y1=max(d)+0.09*max(d),length = 0.05,xpd=T,col="black")
text(85,max(d)+max(d)*0.05,xpd=T,'de Ruig et al., 2019',cex=0.8,col="black")
text(27,max(d)-0.2*max(d),srt=90,"30 years",col="black")
text(47,max(d)-0.2*max(d),srt=90,"50 years",col="black")
text(77,max(d)-0.2*max(d),srt=90,"80 years",col="black")
legend(110,0.01,c("Residential building lifetime \nunder uncertainty \n(Aktas and Bilec, 2012)"),
       col=c(myblue),pch=c(22),pt.cex=c(2),
       pt.bg = c(myblue),bty="n")
dev.off()


