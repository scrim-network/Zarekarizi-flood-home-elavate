import matplotlib
import matplotlib.pyplot as plt
from climata.usgs import DailyValueIO
import pandas as pd
import numpy as np

plt.style.use('ggplot')
matplotlib.rcParams['figure.figsize'] = (14.0, 8.0)


nyears = 1
ndays = 365 * nyears
station_id = "04213075"
param_id = "00065"

datelist = pd.date_range(end=pd.datetime.today(), periods=ndays).tolist()
data = DailyValueIO(
    start_date=datelist[0],
    end_date=datelist[-1],
    station=station_id,
    parameter=param_id,
)



for series in data:
    flow = [r[0] for r in series.data]
    dates = [r[1] for r in series.data]

plt.plot(dates, flow)
plt.xlabel('Date')
plt.ylabel(series.variable_name)
plt.title(series.site_name)
plt.xticks(rotation='vertical')
plt.margins(0.2)
plt.show()



