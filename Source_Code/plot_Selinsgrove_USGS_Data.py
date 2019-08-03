#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar  4 20:07:54 2019

@author: mxz414
"""

import matplotlib
import matplotlib.pyplot as plt
from climata.usgs import DailyValueIO
import pandas as pd
import numpy as np
import matplotlib as mpl

mpl.rcParams.update(mpl.rcParamsDefault)

param_id = "00065"
station_id = "01554000"
nyears=100
#plt.style.use('ggplot')
#%matplotlib inline
#matplotlib.rcParams['figure.figsize'] = (14.0, 8.0)

# set parameters
ndays = 365 * nyears
datelist = pd.date_range(end=pd.datetime.today(), periods=ndays).tolist()
dataa = DailyValueIO(start_date=datelist[0],end_date=datelist[-1],station=station_id,parameter=param_id)

# create lists of date-flow values
for series in dataa:
    dates = [r[0] for r in series.data]
    flow = [r[1] for r in series.data]

fig=plt.figure(figsize=(3.94,2.43))

plt.plot(dates, flow,'-',color="navy")
plt.xlabel('Date',fontsize=5)
plt.ylabel(series.variable_name,fontsize=5)
#plt.title(series.site_name)
plt.xticks(rotation=0)
#plt.margins(0.9)



#plt.xlabel("Cost of House Elevating [1,000 US$]",fontsize=8)
#plt.ylabel("Flood Damages [1,000 US$]",fontsize=8)

#plt.xlim(2001,2019)
plt.ylim(5,40)

fig = plt.gcf()

ax=fig.gca()
ax.tick_params(axis = 'both',labelsize = 4)

l0=ax.axhline(y=-10,color="navy")
l1=ax.axhline(y=np.mean(flow),color="crimson")
l2=ax.axhline(y=14.23,color="orange")


plt.legend([l0,l1,l2],[series.site_name+"(USGS 01554000)","Average","Hypothetical house elevation"], ncol=1, frameon=False, fontsize=5)                



fig.set_size_inches(3.94,2.43)  


plt.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/Selinsgrove_WL_Timeseries.png',dpi=300)

plt.show()
