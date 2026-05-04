import os
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
plt.style.use('fivethirtyeight') #https://matplotlib.org/stable/gallery/style_sheets/fivethirtyeight.html

PATH = ("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_2b/")
print(PATH)


df = pd.read_csv(os.path.join(PATH, "rus_imports.csv"))

# 7.1. 

"""
## 7.1 Revisiting Imports: Read in the file “rus imports.csv” - make sure you get the updated one, 
# not the one from part 2A! It has more rows but is otherwise the same data.
"""

agg_imports = df[["ref_year", "cmd_desc", "net_wgt", "primary_value"]].copy()
agg_imports = agg_imports[agg_imports["ref_year"] >= 2021]
agg_imports = agg_imports.groupby(["ref_year", "cmd_desc"], as_index=False).sum().round(1)


    # **Sort the data so that it shows by energy import type, then within that, by year (ascending). Answer in a brief comment: which resources types have decreased by weight in every year? Which resource types have decreased in value in every year?**
    # All resources have decreased by weight in every year. In value we have a different story though, since only oil value has decreased consistently in every year, while coal and natural gas had increases from 2021 to 2022. However, coal value decreased significantly in 2023 compared to the two previous years. In the case of Gas, its value decreased in 2023 compared to 2022, nonetheless 2023's value is higher than 2021.

agg_imports = agg_imports.sort_values(by=["cmd_desc", "ref_year"], ascending=True)

# 7.2. 
"""
## 7.2 Plotting Imports: Now we will visualize imports over time using this data:
"""
mapping = {"Liquefied Natural Gas Divestment":"Gas", "Coal; bituminous, whether or not pulverised, but not agglomerated":"Coal", "Oils; petroleum oils and oils obtained from bituminous minerals, crude":"Oil"} # https://pythonguides.com/pandas-replace-values-in-column/
agg_imports["cmd_desc"] = agg_imports["cmd_desc"].replace(mapping)

agg_imports = agg_imports.pivot(index="ref_year", columns="cmd_desc", values=["net_wgt", "primary_value"])

fig, (ax1, ax2) = plt.subplots(2, 1)

agg_imports["net_wgt"].plot(kind="bar", ax=ax1)
agg_imports["primary_value"].plot(kind="bar", ax=ax2)


    # **Use Matplotlib to create a figure that holds two axis objects, one on top of the other. Do not plot any data yet!**
    # **Now use the Pandas plot method to create two grouped bar charts, one on the top axis for net wgt, and one on the bottom axis for primary value. The x-axis should be the three values for year, each with three bars next to each other for the three resource types.**
    # - **But wait, we have not used the Pandas plotting method before! Fortunately, the Pandas plot method just builds Matplotlib in the background using an interface that is sometimes more convenient. Look at this StackOverflow answer for how to create a nice grouped bar using Pandas plot.**
    # - **But we’re missing one more piece! To put Pandas plot output on an existing figure (like the one we just created with two axis objects in it), rather than having it create its own, you simply pass the axis object you want it to use into the plot method using the ax=ax kwarg.**

ax1.set_title("Imports by weigth")
ax1.set_ylabel("Net weigth")
ax1.set_xlabel("")

ax2.set_title("Imports by value")
ax2.set_ylabel("Primary value")
ax2.set_xlabel("Year")

plt.tight_layout()

# 7.3.

"""
7.3. Wide Data: Next we will revisit Question 2.2 from Applied Problem Set 2A. 
There we loaded in wide data directly from file. Take a look at that file, named “rus imports wide”, 
load it is a DataFrame named rus imports wide 2a, and then recreate a matching DataFrame from the data you loaded in Question 7.1 as follows:
"""

rus_imports_wide_2a = pd.read_csv("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_2a/rus_imports_wide.csv")

    # First, use the pivot method to put the data in wide format, where each row is uniquely identified by “ref year’ and “reporter iso”, the columns are from “cmd desc” (with shortened names), and both “primary value” and “net wgt” are used as the values.**
df_pivot = df.copy()

mapping = {"Liquefied Natural Gas Divestment":"Gas", "Coal; bituminous, whether or not pulverised, but not agglomerated":"Coal", "Oils; petroleum oils and oils obtained from bituminous minerals, crude":"Oil"} # https://pythonguides.com/pandas-replace-values-in-column/
df_pivot["cmd_desc"] = df_pivot["cmd_desc"].replace(mapping)

df_pivot = df_pivot.pivot(index=["ref_year", "reporter_iso"], columns="cmd_desc", values=["net_wgt", "primary_value"])


    # This will leave us with a MultiIndex as the column names. While we can work with that,it does not match the data we loaded from “rus imports wide” in Pset 2A! There are many programmatic ways we might solve this, but here we will simply assign a list of column names that matches those used in the loaded data from Pset 2A to the .columns attribute. Before you do this, make sure the columns are in the expected order.**
df_pivot.columns = ["Coal (weight)", "Gas (weight)", "Oil (weight)", "Coal (value)", "Gas (value)", "Oil (value)"]

df_pivot = df_pivot.reset_index()

df_pivot = df_pivot[rus_imports_wide_2a.columns] #ordering columns because the equality testing from below was not working

    # Restrict both datasets to the years 2021 and 2022, and make sure the indexes are reset if they do not align. They should look the same to a visual inspection at this point!**

df_pivot = df_pivot[df_pivot["ref_year"].isin([2021, 2022])]
rus_imports_wide_2a = rus_imports_wide_2a[rus_imports_wide_2a["ref_year"].isin([2021, 2022])]

    # - **Try testing that the two DataFrames are equal to eachother using the== operator. Why are there both Trues and Falses in the result? Hint: Inspect the data to see exactly which cells are evaluating as not equal.**
    # The Falses are for missing values in the data. Somehow, even if both have missing values in the same cell, we get false as a result.
    
df_pivot == rus_imports_wide_2a

    # - **Finally, test them with the .equals method. Read the documentation, or check the docstring, for this method to see why it works where== fails. If it does not return True, you still have some steps to align the data.**
    # Seems like `.equals()`treats differently null values compared to `==`. Therefore is evaluating that all data (types, values and columns) are the same. 

df_pivot.equals(rus_imports_wide_2a)

# 8.1. 

"""
## 8.1 Groupby: Load in the “nato ukraine aid.csv” file from question 3.1 and limit the observations to 2022. Reproduce the “nato ukraine aid total.csv” file by using groupby to sum the types of aid. Hint: this will be much easier to read if you drop the “source” and “dataset name” columns. Go ahead and do that, but then write 1-2 sentences in a comment explaining why you might want such columns in your DataFrames during a large project.
"""

ukraine = pd.read_csv("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_2a/nato_ukraine_aid.csv")

ukraine = ukraine.drop(["dataset_name", "source"], axis=1)

ukraine = ukraine.groupby(["new_year", "country_iso3"], as_index=False).sum()

gdp = pd.read_csv("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_2a/gdp.csv")


# 8.2.
"""
## 8.2 Merging: Next merge the GDP data from the World Bank into the “nato ukraine aid total” data, keeping all observations that are in the aid DataFrame.
"""

df_merged = ukraine.merge(gdp, left_on="country_iso3", right_on="iso3c", how="left")


# 8.3. 

"""
## 8.3 Total Aid by Place: Create a new column named “NATO Status” that is equal to “USA” for the United States, “CAN” for Canada, “NATO” for other NATO members, and “Non-NATO” for all others. Use groupby to show the sum of aid from these four categories of places.
"""

nato_members = ['BEL', 'BGR', 'CAN', 'CZE', 'DEU', 'DNK', 'ESP', 'EST', 
                'FRA', 'GBR', 'GRC', 'HUN', 'ISL', 'ITA', 'LTU', 'LUX', 
                'LVA', 'NLD', 'NOR', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 
                'TUR', 'USA', 'HRV', 'ALB', 'MNE', 'MKD', 'F IN', 'SWE'] # i got this list with chatgpt help

df_merged["NATO Status"] = "Non-NATO"
df_merged.loc[df_merged['country_iso3'].isin(nato_members), "NATO Status"] = "NATO"
df_merged.loc[df_merged['country_iso3'] == "CAN", "NATO Status"] = "CAN"
df_merged.loc[df_merged['country_iso3'] == "USA", "NATO Status"] = "USA"

# 9.1. 

"""
## 9.1 Merging: To improve the bar chart we created in Question 5 of Applied Problem set 2A, it will help to merge the datasets together.
"""

    #- **First, create the “total imports” column, as in Question 2.2, in your wide imports data from Question 7.3**

df_pivot["total_imports"] = df_pivot[["Coal (value)", "Gas (value)", "Oil (value)"]].fillna(0).sum(axis=1)

    # - **Then merge the wide import data with the “nato ukraine aid totals” data from Question 8.1, keeping only observations where both the place and year are in both DataFrames. Once you drop the USA data, your DataFrame should have 24 rows.**

df_headline = df_pivot.merge(ukraine, left_on=["reporter_iso", "ref_year"], right_on=["country_iso3", "new_year"], how="inner")
df_headline = df_headline[df_headline["reporter_iso"] != "USA"]

# 9.2.
"""
9.2 Now continue to reproduce the figure using only the merged data. Sort the x-axis for both figures
by total Ukranian aid. Improve the readability of the figure by cleaning up axis labels, fix the
scales of the tick marks on the axis (no scientific notation!), add a legend and colors, and so on.
You’re welcome to look up some Matplotlib commands that we haven’t used yet, though you will be
evaluated only on the figure accurately showing the relation between aid to Ukraine and spending
on Russian energy.
"""

df_headline = df_headline.sort_values("constant_2022_USD_billions", ascending=False)

fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 8))

ax1.bar(df_headline["reporter_iso"], df_headline["constant_2022_USD_billions"])
ax2.bar(df_headline["reporter_iso"], df_headline["total_imports"])

ax1.set_title("Ukraine aid by country")
ax1.set_ylabel("Aid")

ax2.set_title("Russian energy imports by country")
ax2.set_ylabel("Total imports")
ax2.set_xlabel("Country")

ax1.ticklabel_format(style="plain", axis="y")
ax2.ticklabel_format(style="plain", axis="y")

ax1.tick_params(axis="x", rotation=45)
ax2.tick_params(axis="x", rotation=45)

plt.tight_layout()
plt.show()