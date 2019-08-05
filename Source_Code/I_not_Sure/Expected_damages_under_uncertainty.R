# This script is written by Mahkameh Zarekarizi to calculate the expected damages to a hypothetical house 
# This house worth 350,000 USD
# This house is located 1 feet below the base flood elevation 
# We assume it is located right next to our USGS gage
# BFE here is 449.2 ft
# House level is 439.2 ft
# The house is 1500 square feet

# EAD is the expected annual damage
# EAD is the probability of a certain flood times its associated damages

# Global variables
Struc_Value=350000 #USD
House_Initial_Stage=(35.3)-1
life_span=30
disc_rate=0.04
FEMA_Return_periods=c(2,5,10,25,50,100,500)
FEMA_Return_levels=c(21.3,24.9,27.3,30.4,32.8,35.3,41.3)

source("../Freq_Analysis/mycolors.R")
# lifetime_expected_damages(Struc_Value,House_Initial_Stage,
#                                       14,life_span,disc_rate,
#                                       FEMA_Return_levels,FEMA_Return_periods)

lifetime_expected_damages <- function(Struc_Value,House_Initial_Stage,
                                      delta_h,life_span,disc_rate,
                                      FEMA_Return_levels,FEMA_Return_periods){
  House_Current_Stage=House_Initial_Stage+delta_h
  
  # Establish the damage-depth relationship 
  EU_Depth <-           c(-100,-1,0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40)
  RES_Damage_Factors <- c(0,0,0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96)
  damage_vals=RES_Damage_Factors*Struc_Value
  #plot(EU_Depth,damage_vals/1000,xlab="Depth of water in the structure (ft)",ylab="Damages [1,000 US$]")
  
  # Flood chances based on FEMA 
  
  FEMA_No_Exceed_Chance=1-(1/(FEMA_Return_periods))
  FEMA_Exceed_Chance=1/(FEMA_Return_periods)
  
  #plot(FEMA_Return_levels,FEMA_No_Exceed_Chance)
  #plot(FEMA_Exceed_Chance,FEMA_Return_levels,xlab="Exceedence Probability",ylab="Return Level")
  
  # Interpolate FEMA data
  flood_chance_seq=seq(min(FEMA_Exceed_Chance),max(FEMA_Exceed_Chance),by=0.001)
  flood_rp_seq=1/(flood_chance_seq)
  flood_level_seq=approx(FEMA_Exceed_Chance,FEMA_Return_levels,xout=flood_chance_seq)$y
  #plot(flood_rp_seq,flood_level_seq,log="x",xlab="Seq of return periods",ylab="seq of return levels")
  
  InHouse_Water_Depth=flood_level_seq-House_Current_Stage
  #InHouse_Water_Depth[InHouse_Water_Depth<0]=0
  InHouse_Damage=approx(EU_Depth,damage_vals,xout=InHouse_Water_Depth)$y
  #plot(InHouse_Water_Depth,InHouse_Damage,xlab="Seq Depths",ylab="Seq Damages")
  
  wid <- pol_area <- rep(NA,length(flood_chance_seq))
  #plot(flood_chance_seq,InHouse_Damage,xlab="Flooding chance",ylab="Damage")
  for(i in 2:length(flood_chance_seq)){
    wid[i]=flood_chance_seq[i]-flood_chance_seq[i-1]
    pol_area[i]=0.5*wid[i]*(InHouse_Damage[i]+InHouse_Damage[i+1])
  }
  
  EAD=sum(pol_area,na.rm=TRUE)
  #print(EAD)
  disc_fac <- rep(NA,life_span)
  for (i in 0:(life_span-1)){
    disc_fac[i+1]=1/(1+disc_rate)^i
  }
  #plot(disc_fac)
  disc_sum=sum(disc_fac)
  expected_damages=EAD*disc_sum
  #print(c(EAD,expected_damages))
  return(expected_damages)
}




Rising_Cost <- function(sqft,delta_h){
  # Calculate cost of elevating according to CLARA 
  # 82.5/sqft (3 to 7)
  # 86.25/sqft (7 to 10)
  # 103.75/sqft (10 to 14)
  Base_cost= 10000 + 300 + 470 + 4300 + 2175 + 3500
  #Base_cost= 0
  
  if(delta_h>=3 && delta_h<7){
    rate=82.5
  }else if(delta_h>=7 && delta_h<10){
    rate=86.25
  }else if(delta_h>=10 && delta_h<=14){
    rate=103.75
  }else if(delta_h<3){
    rate=80 # i made that up for now
  }else{
    rate=NA
    print('Sorry, your Delta_H is not in the acceptable range')
  }
  rise_cost=Base_cost+rate*sqft
  if(delta_h==0){rise_cost=0}
  
  x=0:14
  y=c(0.0,5717.5,10820.0  ,16845.0 , 23865.0  ,31960.0  ,41292.5  ,51662.5,
      63422.5 , 76557.5 , 91140.0 ,107245.0 ,124952.5, 144330.0, 163707.5)
  rise_cost=approx(x,y,xout=delta_h)$y
  return(rise_cost)
}

construction_cost_seq <-expected_damages_seq <- rep(NA,15)
expected_damages_seq_max <- rep(NA,15)
expected_damages_seq_min <- rep(NA,15)
for (i in 0:14){
  print(i)
  construction_cost_seq[i+1]=Rising_Cost(1500,i)
  expected_damages_seq[i+1]=lifetime_expected_damages(350000,34.3,i,30,0.04,FEMA_Return_levels,FEMA_Return_periods)
  temp <- rep(NA,100)
  
  
  
  # LHS Function
  require(lhs)
  z<- randomLHS(10000, 2)
  SOWs=matrix(NA,10000,2)
  SOWs[,1] <- qunif(z[,1],10,200)
  SOWs[,2] <- qunif(z[,2],0.01, 0.09) 
  
  for(j in 1:10000){
    temp[j]=lifetime_expected_damages(350000,34.3,i,SOWs[j,1],SOWs[j,2],FEMA_Return_levels,FEMA_Return_periods)
  }
  expected_damages_seq_max[i+1]=max(temp)
  expected_damages_seq_min[i+1]=min(temp)
}




# cost benefit analysis
#total_cost=construction_cost_seq+expected_damages_seq
pdf("Figures/FEMA_Expected_damages_under_n_r_uncertatinty.pdf", width =3.94, height =2.43)
par(cex=0.5)

plot(0:14,expected_damages_seq/1000,type="l",lwd=4,col="black",
     ylim=c(0,max(expected_damages_seq_max/1000)),
     xlab="Added elevation [ft]",ylab="Cost [1,000 US$]")
#lines(0:14,expected_damages_seq_min/1000,type="l",lwd=1,col="slateblue4")
#lines(0:14,expected_damages_seq_max/1000,type="l",lwd=1,col="slateblue4")
polygon(x=c(0:14,14:0),y=c(expected_damages_seq_max/1000,rev(expected_damages_seq_min/1000)),
         col = "#FF666640",border=NA)


abline(v=2,col="black",lwd=1,lty=2)
abline(v=1,col="black",lwd=1,lty=3)
abline(v=0,col="black",lwd=1,lty=4)

axis(3,at=0:14,labels=(0:14)+House_Initial_Stage,main="Stage [ft]")
mtext("Stage [ft]",side=3,line=2.5,cex=0.5)
legend("topright",c("Lifetime Expected Damages","90% Credible Intervals"),
       col=c("black","#FF666640"),
       lwd=c(4,NA),pch=c(NA,22),bty="n",pt.cex=c(NA,2),pt.bg=c(NA,"#FF666640"))
text(-0.1,100,"Initial house elevation",adj=c(0,0),srt=90,cex=0.7)
text(0.9,100,"Base flood elevation",adj=c(0,0),srt=90,cex=0.7)
text(1.9,100,"FEMA recommendation",adj=c(0,0),srt=90,cex=0.7)
dev.off()
