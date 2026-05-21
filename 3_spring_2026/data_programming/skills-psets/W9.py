import matplotlib.pyplot as plt
#from matplotlib_scalebar.scalebar import ScaleBar
import seaborn as sns
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
### gdf_comgroc: Communities too. 

# • What is the geometry in each of the datasets?

### They're all polygons. 

# • Display three simple plots, one of each of the three datasets.

gdf_health.plot().set_axis_off()
gdf_groc.plot().set_axis_off()
gdf_comgroc.plot().set_axis_off()

# 1.3 Shapley Distance Calculation: We can use Shapley to do simple distance calculations. 
# We’ll practice doing so by calculating the distance between two local grocery stores.
# • Create two Shapley points by extracting the geometries from the GeoDataFrame for the Trea-
# sure Island Foods (at 1526 E 55TH ST, CHICAGO, IL. 60615-5527) and the (one and only)
# Hyde Park Produce. (Note: the Treasure Island was previously located where our local Trader
# Joe’s is currently.) To do so, you’ll need to subset your GeoDataFrame to each location, then
# extract the Shapley object from the value in the GeoDataFrame geometry column.

treasure_island = gdf_groc[gdf_groc["Address"].str.contains("1526 E 55TH", case=False)]
hyde_park_produce = gdf_groc[gdf_groc["Chain"].str.contains("HYDE PARK PRODUCE", case=False, na=False)]

ti_geom = treasure_island.geometry.values[0]
hpp_geom = hyde_park_produce.geometry.values[0]

# • Check the Well-Known Text (WKT) on each of your points to confirm that the previous step
# worked as expected. What do you notice about the type of Shapley object you’ve extracted?

print(ti_geom.wkt)
print(hpp_geom.wkt)

### they're multipoint objects


# • Create a Shapley LineString from your two MultiPoint locations by first converting those
# locations to Shapley Points using the .geoms attribute, which gives you access to the Points
# contained in a MultiPoint. Hint: multipoint.geoms[0] is a point.

ti_point = ti_geom.geoms[0]
hpp_point = hpp_geom.geoms[0]

line = LineString([ti_point, hpp_point])
print(line.wkt)

# • Check the WKT of your LineString to confirm that your line connects the correct points.
# • Calculate the length of your line to get the as-the-crow-flies distance between the points. The
# units will be in feet. Does this seem reasonable based on what you know of Hyde Park?

distance = line.length
print(f"Distance: {distance:.0f} feet")

### 2673 feet. Seems reasonable reasonable. According to gooble maps that's about a 10 minute walk.

# 1.4 Merge the Community-Level Data: GeoPandas GeoDataFrames can be merged just like pan-
# das DataFrames based on a common attribute, or they can be merged based on their spatial
# relationship to one another. The former is called an attribute join, and the later is a spatial join.
# You will learn about spatial joins (and a host of other geometric operations!) in the DAV class.
# • What common attribute (or attributes) that identify observations in the health/socio-demographic
# indicators and community grocery store counts GeoDataFrames can you use as a key?

### "ComAreaID" and "community" identify observations in both datasets.

# • Merge the health/socio-demographic indicators and community grocery store counts Geo-
# DataFrames using an attribute merge. Hint: the syntax is the same as a pandas merge!

# • Use the indicator=True kwarg to audit your merge. Why do you think the merge is not
# one-to-one (that is, every observation from the left GeoDataFrame matches to an observation
# in the right GeoDataFrame)? Come up with some potential explanations. You’ll investigate
# this issue in the next question.

gdf_merged = gdf_health.merge(gdf_comgroc, on=["ComAreaID", "community"],
                               how="outer", indicator=True)

print(gdf_merged["_merge"].value_counts())

### 23 od the health data have no match in the grocery store counts. This likely means 
### those 23 communities have zero grocery stores, so they were never included in gdf_comgroc.

# 1.5 Plot Your Merged and Grocery Locations Datasets Together: You can use MatPlotLib to
# overlay multiple GeoDataFrames on the same plot. Just assign each plot to the same axis object.
# • First, try to plot your newly merged GeoDataFrame. What error do you encounter?
# • Examine the geometry column(s) in your merged GeoDataFrame. What do you notice?

### Trying gdf_merged.plot() raises an error because there are two geometry
### columns ("geometry_x" and "geometry_y") and GeoPandas doesn't know which to use.

#• To fix the issue, you will need to set the “active geometry” in GeoPandas. Set the geometry
# from the health/socio-demographic indicators data as your active geometry.

gdf_merged = gdf_merged.set_geometry("geometry_x")

#• Confirm that you can plot your merged GeoDataFrame.

gdf_merged.plot().set_axis_off()

# • Use MatPlotLib to plot both the polygons from your merged GeoDataFrame and the grocery
# store locations together. Hint: assign each plot to the same axis object. Hint: if your plot is
# unintelligible, make sure that all of your geometries are using the same Coordinate Reference
# System (CRS) before plotting.

gdf_groc_reproj = gdf_groc.to_crs(gdf_merged.crs)

fig, ax = plt.subplots(figsize=(10, 10))
gdf_merged.plot(ax=ax, color="lightgrey", edgecolor="black")
gdf_groc_reproj.plot(ax=ax, color="red", markersize=5)
ax.set_axis_off()
plt.show()


# • Are there grocery stores in every community in Chicago? Does this explain why your merge was
# not one-to-one? Use what you’ve learned to address the NaNs in the ’groc stores’ column.

### Not every community has a grocery store. The 23 "left_only" communities
### from the merge confirm this. 

# • What do you notice about the availability of grocery stores across communities in Chicago?

### Grocery stores are concentrated on the north side and in potentially wealthier areas.

"""
2. Mapping
"""

# 2.1. summary statistics

print(gdf_merged[["Colorect", "DiabetM"]].describe())

fig, axes = plt.subplots(1, 2, figsize=(12, 5))
sns.histplot(gdf_merged["Colorect"], ax=axes[0], kde=False)
axes[0].set_title('Colorectal cancer deaths per 100,000')
sns.histplot(gdf_merged["DiabetM"], ax=axes[1], kde=False)
axes[1].set_title('Diabetes-related deaths per 100,000')
plt.tight_layout()
plt.show()


# 2.2. Health measure choropleth maps

fig, axes = plt.subplots(1, 2, figsize=(16, 8))

gdf_merged.plot(ax=axes[0], column="Colorect", legend=True, cmap="OrRd",
                edgecolor="black", linewidth=0.3)
gdf_groc_reproj.plot(ax=axes[0], color="blue", markersize=5)
axes[0].set_axis_off()
axes[0].set_title("Colorectal cancer deaths & grocery stores")

gdf_merged.plot(ax=axes[1], column="DiabetM", legend=True, cmap="OrRd",
                edgecolor="black", linewidth=0.3)
gdf_groc_reproj.plot(ax=axes[1], color="blue", markersize=5)
axes[1].set_axis_off()
axes[1].set_title("Diabetes deaths & grocery stores")

plt.tight_layout()
plt.savefig(os.path.join(PATH, "health_choropleths.png"), dpi=150, bbox_inches="tight")
plt.show()

gdf_sorted = gdf_merged.sort_values("groc_stores", ascending=True).reset_index(drop=True)
gdf_sorted["cumsum_colorect"] = gdf_sorted["Colorect"].cumsum()
gdf_sorted["cumsum_diabetm"] = gdf_sorted["DiabetM"].cumsum()


fig, axes = plt.subplots(1, 2, figsize=(14, 6))

axes[0].plot(gdf_sorted["groc_stores"], gdf_sorted["cumsum_colorect"], marker="o", markersize=3)
axes[0].set_xlabel("Number of grocery Stores")
axes[0].set_ylabel("Cumulative colorectal cancer deaths per 100k")
axes[0].set_title("Cumulative colorectal cancer deaths vs. grocery stores")

axes[1].plot(gdf_sorted["groc_stores"], gdf_sorted["cumsum_diabetm"], marker="o", markersize=3)
axes[1].set_xlabel("Number of grocery Stores")
axes[1].set_ylabel("Cumulative diabetes deaths per 100k")
axes[1].set_title("Cumulative diabetes deaths vs. grocery stores")

plt.tight_layout()
plt.savefig(os.path.join(PATH, "cumulative_sums.png"), dpi=150, bbox_inches="tight")
plt.show()

### Communities with 0-2 grocery stores account for a disproportionately large share of both colorectal cancer and diabetes deaths. 
### The curve flattens out as grocery store counts increase.

# 2.3.

uchicago_3 = gdf_merged[gdf_merged["community"].isin(["HYDE PARK", "KENWOOD", "WOODLAWN"])]

fig, ax = plt.subplots(figsize=(8, 8))
uchicago_3.plot(ax=ax, column="DiabetM", legend=True, cmap="OrRd",
                edgecolor="black", linewidth=0.5)
ax.set_axis_off()
ax.set_title("Diabetes Deaths: Hyde Park, Kenwood, Woodlawn")
plt.show()

uchicago_5 = gdf_merged[gdf_merged["community"].isin(
    ["HYDE PARK", "KENWOOD", "WOODLAWN", "GRAND BOULEVARD", "WASHINGTON PARK"])]

fig, ax = plt.subplots(figsize=(8, 8))
uchicago_5.plot(ax=ax, column="DiabetM", legend=True, cmap="OrRd",
                edgecolor="black", linewidth=0.5)
ax.set_axis_off()
ax.set_title("Diabetes Deaths: Greater UChicago Area")
plt.show()


### Hyde Park looks nearly the same shade in both maps, but Kenwood appears more darker in the 3-community map compared to the 5-community map. 
### Adding Grand Boulevard and Washington Park (with higher diabetes rates) changes the color scale, making Kenwood's value look relatively lighter. 




