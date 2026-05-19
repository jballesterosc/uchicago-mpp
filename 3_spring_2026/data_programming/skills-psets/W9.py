import matplotlib.pyplot as plt
from matplotlib_scalebar.scalebar import ScaleBar
import pandas as pd
import os
import shapely
from shapely import Point, LineString, Polygon
import geopandas as gpd

PATH = "/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/skills-psets/_data/w9/"

"""
1. GeoData Loading
"""

gdf_health = gpd.read_file(os.path.join(PATH, 'comarea.zip'))

gdf_groc = gpd.read_file(os.path.join(PATH, 'grocery.zip!grocery/chicago_sup.shp'))

gdf_comgroc = gpd.read_file(os.path.join(PATH, 'comgrocery.gpkg'))

# 1.2 Exploring the Data: Explore the data both in its raw/numeric form and its spatial form.
# • What is the unit of observation in each of the datasets?

### gdf_groc: Addresses! Running nunique for that column we get 148, which is the same number of rows in .shape
### gdf_health: Communities. And same as above. We have the same number of nunique() for "community" as the total row numbers we get with .shape
s### gdf_comgroc: Communities too. 

# • What is the geometry in each of the datasets?

### They're all polygons. 

# • Display three simple plots, one of each of the three datasets.

gdf_health.plot().set_axis_off()
gdf_groc.plot().set_axis_off()
gdf_comgroc.plot().set_axis_off()

# 1.3 Shapley Distance Calculation: We can use Shapley to do simple distance calculations. We’ll
# practice doing so by calculating the distance between two local grocery stores.
# • Create two Shapley points by extracting the geometries from the GeoDataFrame for the Trea-
# sure Island Foods (at 1526 E 55TH ST, CHICAGO, IL. 60615-5527) and the (one and only)
# Hyde Park Produce. (Note: the Treasure Island was previously located where our local Trader
# Joe’s is currently.) To do so, you’ll need to subset your GeoDataFrame to each location, then
# extract the Shapley object from the value in the GeoDataFrame geometry column.
# • Check the Well-Known Text (WKT) on each of your points to confirm that the previous step
# worked as expected. What do you notice about the type of Shapley object you’ve extracted?
# • Create a Shapley LineString from your two MultiPoint locations by first converting those
# locations to Shapley Points using the .geoms attribute, which gives you access to the Points
# contained in a MultiPoint. Hint: multipoint.geoms[0] is a point.
# • Check the WKT of your LineString to confirm that your line connects the correct points.
# • Calculate the length of your line to get the as-the-crow-flies distance between the points. The
# units will be in feet. Does this seem reasonable based on what you know of Hyde Park?