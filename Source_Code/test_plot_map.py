#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Feb 15 11:56:52 2019

@author: mxz414
"""

from shapely.geometry import Point, mapping, shape
from fiona import collection

with collection("/Users/mxz414/Documents/Research/House_Elevation_Project/data/SelinsgroveStructures2013/SelinsgroveStructures2013.shp", "r") as input:
    for point in input:
        print(shape(point['geometry']))

with collection("/Users/mxz414/Documents/Research/House_Elevation_Project/data/SelinsgroveStructures2013/SelinsgroveStructures2013.shp", "r") as input:
    #schema = input.schema.copy()
    schema = { 'geometry': 'Polygon', 'properties': { 'name': 'str' } }
    with collection("/Users/mxz414/Documents/Research/House_Elevation_Project/data/SelinsgroveStructures2013/SelinsgroveStructures2013_buffer.shp", "w", "ESRI Shapefile", schema) as output:
        for point in input:
            output.write({
                'properties': {
                    'name': point['properties']['STREETNAME']
                },
                'geometry': mapping(shape(point['geometry']).buffer(5.0))
            })
    
    
import shapefile
import matplotlib.pyplot as plt

sf = shapefile.Reader("/Users/mxz414/Documents/Research/House_Elevation_Project/data/SelinsgroveStructures2013/SelinsgroveStructures2013.shp")

print("Initializing Display")
fig = plt.figure()
ax = fig.add_subplot(111)
plt.xlim([76, 85])
plt.ylim([12, 21])
print("Display Initialized")

for shape in sf.shapes():
    print("Finding Points")
    points = shape.points
    print("Found Points")    

    print("Creating Polygon")
    ap = plt.Polygon(points, fill=False, edgecolor="k")
    ax.add_patch(ap)
    print("Polygon Created")

print("Displaying Polygons")
plt.show()

import geopandas as gpd
shape=gpd.read_file("/Users/mxz414/Documents/Research/House_Elevation_Project/data/SelinsgroveStructures2013/SelinsgroveStructures2013.shp")
shape.plot()
