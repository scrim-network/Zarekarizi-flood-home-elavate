#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr  5 18:47:45 2019

@author: mxz414
"""

def expected_damages(delta_h,Struc_Value=350000,House_Initial_Stage=34.3,life_span=30,disc_rate=0.04,mu=19.85,sigma=3.24,xi=0.02):
    from scipy.stats import genextreme
    import numpy as np
    House_Current_Stage=House_Initial_Stage+delta_h
    # Establish the damage-depth relationship 
    EU_Depth =           np.array([-100,-1,0, 1.64, 3.28, 4.92, 6.56, 9.84, 13.12, 16.40])
    RES_Damage_Factors = np.array([0,0,0.20, 0.44, 0.58, 0.68, 0.78, 0.85, 0.92, 0.96])
    damage_vals=RES_Damage_Factors*Struc_Value
    Critical_Depths=EU_Depth+House_Current_Stage
    Critical_Probs=1-genextreme.cdf(Critical_Depths, c=xi, loc=mu, scale=sigma)
    EADfrac=np.zeros((len(Critical_Depths),),float)
    for i in range(0,len(Critical_Depths)):
        if i==len(Critical_Depths)-1:
            EADfrac[i]=Critical_Probs[i]*damage_vals[i]
        else:
            EADfrac[i]=(Critical_Probs[i]-Critical_Probs[i+1])*damage_vals[i]
    EAD=np.sum(EADfrac)  
    #print(EAD)
    disc_fac = np.zeros(int(life_span,),float)
    
    for i in range(0,int(life_span)):
        disc_fac[i]=1/((1+disc_rate)**i)
    
    
    disc_sum=np.sum(disc_fac)
    lifetime_expected_damages=EAD*disc_sum
    #print(lifetime_expected_damages)
    return(lifetime_expected_damages)



#mu=19.85
#sigma=3.24
#xi=0.02
#disc_rate=0.04
#life_span=30
#delta_h=1
#House_Initial_Stage=34.3
#Struc_Value=350000

#print(expected_damages(Struc_Value,House_Initial_Stage,delta_h,life_span,disc_rate,mu,sigma,xi))