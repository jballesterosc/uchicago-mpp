import pandas as pd
import seaborn as sns
sns.set_style("darkgrid")


penguins = sns.load_dataset("penguins")


# 1. Palmer Penguins

## 1.2 Exploration: Do some basic exploration of the dataset. Then answer these questions:

### What percentage of the data has missing values?
#### 5.52

count_na = penguins.isnull().sum().sum()
total_rows = len(penguins)

print(((count_na / total_rows) * 100).round(2))

### What are the average body mass and flipper length for each penguin species?
#### Answered below with the groupby

print(penguins.groupby("species")[["body_mass_g", "flipper_length_mm"]].mean().round(2))

### Which island has the heaviest penguins on average?
#### The Gentoo species, with an average body mass of 5076.02

print(penguins.groupby("species")["body_mass_g"].mean().round(2).sort_values(ascending=False))


## 1.3 Exploratory Plots: Principles of good data visualization will be taught in the Data Analytics and Visualization class, but we have all the tools we need to create basic exploratory plots!

### Look up how to create a box plot in Seaborn. Show the distribution of weight, split by sex, of each of the three species.

sns.boxplot(data=penguins, x="body_mass_g", y="species", hue="sex")

### Is there a correlation between flipper length and body mass? Explore this with a scatter plot, using a different color for each species.

sns.scatterplot(data=penguins, x="flipper_length_mm", y="body_mass_g", hue="species")

### With the iris dataset, we explore the basics of classification, which you will get much deeper into in Machine Learning! Does it look like any of the continuous variables in this data would be good for classifying species of penguins? Use the Seaborn pairplot function to explore.

sns.pairplot(penguins, hue="species", markers=["o", "s", "D"])


# 2. Time Series

## 2.1 Combining Data: The penguins data we load from Seaborn is but a small fraction of the full dataset collected by researchers. One missing element is the year of the observation. Fortunately we found the record! Load in the “penguin years.csv” file, which is, fortunately for us, sorted in the same order as the penguins data is by default. Add it as a column in the penguins data you’ve been working with.

PATH = ("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/skills-psets/_data")

penguin_years = pd.read_csv(os.path.join(PATH, "penguin_years.csv"))

penguins = penguins.join(penguin_years) # by doing it this way, join is matching rows by index alone https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.join.html

## 2.2 Analysis: Based on the steps you took in Part 1, show some new analysis across time period. What are you few things you can learn about what has changed over time for our penguins?
#### By using a lineplot for time series, we can see  that the body mass of the Gentoo species modestly increased from 2007 to 2009. Whereas Adelie descrased. 
#### Something interesting is that, even if Gentoo body mass increased, flipper length decreased. A similar treand, but in the inverse, happend with Adelie: even if body mass went down, flipper length has an upside trend.
#### Note: we don't have data 2009 data for the Chinstrap species, but it has a similar trend than Adelie -- at least until 2008. 

print(penguins.groupby(["species", "year"])[["body_mass_g", "flipper_length_mm"]].mean().round(2))

sns.lineplot(data=penguins, x="year", y="body_mass_g", hue="species")

sns.lineplot(data=penguins, x="year", y="flipper_length_mm", hue="species")

# 3. Weather Data
## 3.1 Data Loading: While not part of the classic Palmer Penguins data, we also have weather recordings from Palmer Station.
### Download monthly weather observations data here.
### Load the data into a Pandas DataFrame

df = pd.read_csv("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/skills-psets/_data/PalmerStation_Monthly_Weather.csv", index_col='Date', parse_dates=True)

### Take a look at the processing script in Python that they make available at the above URL. Ignore the part about merging in data from 1974-1989, and the parts about argparse, but otherwise try to implement their code that applies directly to the DataFrame you loaded in. If their commands don’t work, investigate to see if they can be fixed, or if they aren’t useful.
#### I think that the script calculates the monthly averages but in the Full Data Package data. Therefore, the monthly data downloaded is already processed by the same script.

### Aggregate the observations to annual

df["year"] = df.index.year

df_year = df.groupby("year")["Mean Temperature (C)"].mean()

## 3.2 Merging: Merge the temperatures into the penguins data by year

penguins =  penguins.merge(df_year, left_on="year", right_on="year", how="left")

## 3.3 Analysis: Using what you’ve applied in earlier steps, look for some insights based on weather patterns. Note that since this data is limited to only three years of observations, you may be limited in what patterns you can find, but that’s ok!
#### Across all three years, Gentoo penguins are consistently the heaviest species. 
#### Interestingly, 2009 has the highest average body mass and flipper length across all three species despite having the coldest temperature, while 2008 was the warmest year but had lower averages. 
#### This suggests that annual mean temperature alone does not explain changes in penguin body size, but other factors likely play a wider role.

penguins.rename(columns={"Mean Temperature (C)":"mean_temperature"}, inplace=True)

penguins['mean_temperature'] = penguins['mean_temperature'].round(2)

sns.barplot(data=penguins, x="mean_temperature", y='bill_length_mm', hue="species",)

print(penguins.groupby("year")[['body_mass_g', 'flipper_length_mm', 'bill_length_mm', 'bill_depth_mm', 'mean_temperature']].mean().round(2))

print(penguins.groupby(['year', "species"])[['body_mass_g', 'flipper_length_mm', 'bill_length_mm', 'bill_depth_mm', 'mean_temperature']].mean().round(2))

sns.scatterplot(data=penguins, x='mean_temperature', y="body_mass_g", hue='species', style="sex")

sns.boxplot(data=penguins, x="year", y='body_mass_g', hue='species')

