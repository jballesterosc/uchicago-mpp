import pandas as pd
import seaborn as sns

df = sns.load_dataset("penguins")


# 1. Palmer Penguins

## 1.2 Exploration: Do some basic exploration of the dataset. Then answer these questions:

### What percentage of the data has missing values?
#### 5.52

count_na = df.isnull().sum().sum()
total_rows = len(df)

print(((count_na / total_rows) * 100).round(2))

### What are the average body mass and flipper length for each penguin species?
#### Answered below with the groupby

print(df.groupby("species")[["body_mass_g", "flipper_length_mm"]].mean().round(2))

### Which island has the heaviest penguins on average?
#### The Gentoo species, with an average body mass of 5076.02

print(df.groupby("species")["body_mass_g"].mean().round(2).sort_values(ascending=False))


## 1.3 Exploratory Plots: Principles of good data visualization will be taught in the Data Analytics and Visualization class, but we have all the tools we need to create basic exploratory plots!

### Look up how to create a box plot in Seaborn. Show the distribution of weight, split by sex, of each of the three species.

sns.boxplot(data=df, x="body_mass_g", y="species", hue="sex")

### Is there a correlation between flipper length and body mass? Explore this with a scatter plot, using a different color for each species.

sns.scatterplot(data=df, x="flipper_length_mm", y="body_mass_g", hue="species")

### With the iris dataset, we explore the basics of classification, which you will get much deeper into in Machine Learning! Does it look like any of the continuous variables in this data would be good for classifying species of penguins? Use the Seaborn pairplot function to explore.
