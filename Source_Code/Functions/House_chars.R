house_charactersitics <- function(i=8){

# In the paper, we are focusing on three specific hypothetical houses. 
# Below is the information of these houses:

houses=rbind(
  c(1000,250000,-6,30), # i=1: Typical house
  c(3708.575257,316329.770178,-6.789922,95.430767), # i=2: Considering uncertainty increases the optimal elevation but then it doesnt pass the B-C test
  c(1000,277000,-4.7,30), # i=3: Considering uncertainty increases the optimal elevation and it totally makes sense.
  c(1500,320000,-6,30),
  c(1500,300000,-6,60),
  c(2000,320000,-4,30),
  c(2000,350000,-4,30),
  c(1500,300000,-4,30)
)

# Set i to the number of the house you would like to run the program for.
sqft=houses[i,1]
Struc_Value=houses[i,2]
del=houses[i,3]
life_span=houses[i,4] 
disc_rate=0.04

output=cbind(sqft=sqft,Struc_Value=Struc_Value,del=del,life_span=life_span,disc_rate=disc_rate)
return(output)
}
