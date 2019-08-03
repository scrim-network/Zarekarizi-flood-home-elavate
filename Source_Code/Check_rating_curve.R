# This manuscript is written in hope of drawing the rating curve downloaded from USGS and compare it with observed gage height and streamflow data available
# Written by Mahkameh Zarekarizi

# Change the directory
setwd("~/Documents/Research/House_Elevation_Project/Source_Code")

# read the text file (rating curve data) downloaded from USGS 
rating_curve=read.table(file="~/Documents/Research/House_Elevation_Project/data/rating_curves_01554000.txt",header = F,sep="\t",skip=44,
             colClasses=c("numeric","NULL","numeric","NULL"),col.names=c("Height","","Discharge",""))

# Pot the gage and streamflow 
pdf(file="~/Documents/Research/House_Elevation_Project/Results/USGS_Rating_CUrve_Plot.pdf",width=3.93, height=2.43)
par(cex=0.5)
plot(rating_curve$Discharge/1000,rating_curve$Height,log="x",type="l",lwd=2,col=4,xlab='Discharge (1,000 cfs)',ylab='Gage Height (ft)')

# Load the gage height and streamflow data for the period that Gage height is available
load("../data/time_discharge_level.RData")

# Add the observations to the plot as points (This line is commented because there are too many points)
#points(x$Qcfs/1000,x$WL,pch=20,col= rgb(red = 0, green = 1, blue = 0, alpha = 0.2))

# Add legend
legend('topleft',c('USGS Rating Curve'),lwd=c(2),col=c(4),lty=c(1),pch=c(-1),bty="n")
dev.off()
