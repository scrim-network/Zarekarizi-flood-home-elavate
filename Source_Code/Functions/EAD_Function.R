
EAD_Certainty <- function(Struc_Value,          # Struc_Value: House value; the price of the house
                          House_Initial_Stage,  # House_Initial_Stage: Elevation of the house with respect to gage datum (stage) before being raised.
                          delta_h,              # delta_h: How much is the house elevated from the initial stage above? This is the amount of heightening; could be zero
                          mu,                   # mu: The expected location parameter of the GEV function.
                          sigma,                # sigma: The expected scale parameter of the GEV function.
                          xi                    # xi: The expected shape parameter of the GEV function.
                          ){
  House_Current_Stage=House_Initial_Stage+delta_h # The stage of the house after being elevaed. reminder: Stage the elevation of the house with respect to the gage datum in feet 
  
  # Establish the damage-depth relationship 
  DD_Depth<-c(-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
  DD_Damage<-c(0,0,4,8,12,15,20,23,28,33,37,43,48,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81)/100
  
  # how much is lost (in USD) at each depth. This depends on the value of the house 
  damage_vals=DD_Damage*Struc_Value
  
  # critical depths are depths where the damage factor changes. 
  Critical_Depths=DD_Depth+House_Current_Stage # Calculates the stage of critical depths
  
  # What is the probability that water level exceeds each critical depth?
  Critical_Probs=1-pgev(q=Critical_Depths, shape=xi, loc=mu, scale=sigma)
  
  # The following block is just to avoid NaNs and NAs
  if(sum(is.nan(Critical_Probs))>0){
    test_x=rgev(10^6, shape=xi, loc=mu, scale=sigma)
    Critical_Probs[Critical_Depths<min(test_x)]=0
    Critical_Probs[Critical_Depths>max(test_x)]=1
  }
  if(sum(is.nan(Critical_Probs))>0){
    stop("ERROR....")
  }
  
  # Calculate expected annual damages (EAD)
  EADfrac=rep(NA,length(Critical_Depths))
  for(i in 1:length(Critical_Depths)){
    if(i==length(Critical_Depths)){
      EADfrac[i]=Critical_Probs[i]*damage_vals[i]
    }else{
      EADfrac[i]=(Critical_Probs[i]-Critical_Probs[i+1])*damage_vals[i]
    }
  }
  EAD=sum(EADfrac)
  return(EAD)
}
