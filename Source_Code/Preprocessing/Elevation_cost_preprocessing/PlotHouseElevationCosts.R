# This script is written by Mahkameh Zarekarizi, mxz414@psu.edu
# The goal is to plot the cost of elevating a house with regards to 
# House size and the added elevation

# The function for calculating the cost of levating a house!
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
  
  return(rise_cost)
} # end of function

# Create a vector of possible house size values 
sqft_seq=seq(50,50000,length.out=100)

# create a vector of plausible added elevatuion values
delta_seq=seq(0.00000001,14,length.out=100)

# Calculate the cost of elevating a house 
rise_cost=matrix(NA,length(sqft_seq),length(delta_seq))

for(i in 1:length(sqft_seq)){
  for(j in 1:length(delta_seq)){
    rise_cost[i,j]=Rising_Cost(sqft_seq[i],delta_seq[j])  
}}

# 2D plot of elevating cost!
pdf("Figures/Cost_of_elevating_map.pdf", width =3.94, height =2.43)
par(mar=c(5,5,4,6),cex=0.5) 

library(matlab)
imagesc(delta_seq,sqft_seq/1000,rise_cost,xlab="Added height ([ft]",ylab=expression("House size [1,000 sqft]"))

library(fields)
image.plot(x=2,y=30,legend.only=T, zlim=range(0,max(rise_cost)/1000),legend.mar =3,legend.lab="Cost of heightening [1,000 US$]",legend.line = 3,legend.cex=0.5,legend.shrink = 0.5,legend.width = 0.8)

dev.off()


## simple plot of raising cost!
rise_cost_rate=rep(NA,length(delta_seq))
for(j in 1:length(delta_seq)){
  rise_cost_rate[j]=Rising_Cost(1500,delta_seq[j])  
}
pdf("Figures/Cost_of_elevating.pdf", width =3.94, height =2.43)
par(cex=0.5) 
plot(delta_seq,rise_cost_rate/1000,type="l",xlab="Added height [ft]",ylab=expression("Elevating cost rate[1,000 US$]"))
dev.off()


# The end 