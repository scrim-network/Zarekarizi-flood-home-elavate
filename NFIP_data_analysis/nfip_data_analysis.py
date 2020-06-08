'''
Analysis of NFIP data by Mahkameh Zarekarizi 
'''

# libraries
import numpy as np
import pandas as pd

# Read the data 
filename='openfema_claims20190331.csv'
data = pd.read_csv(filename)
pa_data=data[data['state']=='PA']
##print(pa_data.head(5))
#print(pa_data.state)
# count the number of cliams with elevation certificate 
x1=pa_data[pa_data['elevationcertificateindicator']==3].elevationcertificateindicator.count()
print('Number of claims with elevation certificate type 3 in PA= ',x1)

# average damage to houses with elevationc ertificate 
y1=pa_data[pa_data['elevationcertificateindicator']==3].amountpaidonbuildingclaim.sum()
y2=pa_data[pa_data['elevationcertificateindicator']==3].amountpaidoncontentsclaim.sum()
y3=pa_data[pa_data['elevationcertificateindicator']==4].amountpaidonbuildingclaim.sum()
y4=pa_data[pa_data['elevationcertificateindicator']==4].amountpaidoncontentsclaim.sum()
print('average damage to houses with elevation certificate according to FEMA standards (in million dollars)=',(y1+y2+y3+y4)/1000000)
#print(y1,y2,y3,y4)

# sum of the amount of money paid to claims in Pennsylvania 
PA_total_insurance_paid=data[data['state']=='PA'].amountpaidonbuildingclaim.sum()
print('Total money paid on claims in Pennsylvania (in million dollars)=',PA_total_insurance_paid/1000000)

# total amount paid in the U.S.
x2=data.amountpaidonbuildingclaim.sum()
x3=data.amountpaidoncontentsclaim.sum()
print('Total amount paid over the U.S.(in billion dollars) =',(x2+x3)/1000000000)

# average damage to houses
x4=data.amountpaidonbuildingclaim.mean()
x5=data.amountpaidoncontentsclaim.mean()
print('Average damage to houses over the U.S. is ( dollars) =',(x5+x4))



