#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 14 15:02:12 2019

@author: mxz414
"""
import matplotlib
import matplotlib.pyplot as plt
import numpy as np

x=[166189.45,148075.11,130490.59,114371.88,100317.22,88618.23,79289.44,72589.63,70231.72,71704.68,81081.23,95424.77,120611.83,161500.25,210331.54]

fig, ax = plt.subplots()
ax.plot(x)

ax.set(xlabel='Elevation (ft)', ylabel='Expected cost (USD)',
       title='Figure Optimal Economic Elevation Outcome for a Fill Foundation Residential Building')
ax.grid()
