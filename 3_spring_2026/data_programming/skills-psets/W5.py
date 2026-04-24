import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
plt.style.use('fivethirtyeight') https://matplotlib.org/stable/gallery/style_sheets/fivethirtyeight.html



# Pandas
# 1.1.

"""
Loading Data: Load the Iris data from the iris.csv file (available on Canvas) into a Pandas
DataFrame with a descriptive name (e.g., df iris). Do so in a manner that can easily be modified
and run by a colleague. Once you have imported the data, swap code with a classmate and confirm
that you both can load the data using each other’s code.
"""
PATH = ("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/skills-psets/_data")
print(PATH)

df_iris = pd.read_csv(os.path.join(PATH, "iris.csv"))

# 1.2.

"""
Explore: Use the following methods and attributes of your DataFrame instance to explore the
data:
    • .index
    • .columns
    • .head()
    • .tail()
    • .dtypes
    • .shape
You can look up any you aren’t familiar with in the official documentation.
"""


df_iris.index
df_iris.columns
df_iris.head()
df_iris.tail()
df_iris.dtypes
df_iris.shape

# 1.3. 

"""
Write code that displays how many unique types of flowers are in the data, what they are, and
how many observations there are of each.
"""

df_iris["species"].unique()
df_iris["species"].value_counts()


# 1.4. 

"""
What are the mean, median, and the standard deviation of the four numeric columns (sepal and
petal length and width)? First try this on the entire DataFrame, and look at the error you get.
There are multiple ways to deal with this error! Try looking for a method that selects columns
based on their datatypes (Google will tell you!), and then also see what happens if you move the
species column into the index (the .set index method).
"""

    # df_iris.mean() this prints error, since it cannot calculate species (object dtype) 

np.mean(df_iris.select_dtypes(include=["float64"]))

df_iris.select_dtypes(include=["float64"]).mean()
df_iris.select_dtypes(include=["float64"]).median()
df_iris.select_dtypes(include=["float64"]).std()


df_iris.set_index("species")

# 1.5.

"""
Display a table where each variable is a row and the columns are the mean, standard deviation,
and median. Hint: There is one handy method to do this, plus a few other summary statistics!
One might say it is a helpful method for describing your data...
• But that helpful method has the rows and columns swapped from what is asked for! Fortunately
the transpose method will swap those for us. Try it out - we will get into more complex
reshaping operations next week.
"""

df_iris.describe()


df_iris.describe().T # I found this option in stackoverflow: https://stackoverflow.com/questions/34085081/pandas-df-describe-is-it-possible-to-do-it-by-row-without-transposing

# 1.6.

"""
Create a new DataFrame by subsetting only the petal columns using a list comprehension. Do this
three ways - one that creates a view, and two that create new copies.
"""


    # this is the view option
petals = [p for p in df_iris.columns if p.startswith("petal")]
df_petals = df_iris[petals]
df_petals


    # this is the copy
df_petals_copy1 = df_iris.loc[:, ["petal_length", "petal_width"]]
df_petals_copy1

    #  another copy
df_petals_copy2 = df_iris[petals].copy()
df_petals_copy2

# 1.7. 

"""
Create a new column named “petal area” which is equal to the length times 
the width. Note that this isn’t really the area of the petal, since petals 
presumably aren’t rectangles. I don’t know,I’m not a botanist! Regardless, 
try this on both a view and a copy of the data from the previous
question, so you see the warning message.
"""

    # view type
df_petals = df_iris[petals]
df_petals["petal_area"] = df_petals["petal_length"] * df_petals["petal_width"]


    # copy type
df_petals_copy1 = df_iris.loc[:, ["petal_length", "petal_width"]]
df_petals_copy1["petal_area"] = df_petals_copy1["petal_length"] * df_petals_copy1["petal_width"]
df_petals_copy1

# 1.8.

"""
Return to the full data with species as the index. How would you find the mean values for one
type of flower (your choice of the three)? Right now you can implement this with subsetting; next
week we will cover how to do this using groupby. Look up how to set a column as the index if you
have not already done that.
"""

df_iris_index = df_iris.set_index("species") # https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.set_index.html
df_iris_index.loc['versicolor'].mean()

# 1.9. 

"""
Subset your data to create a DataFrame with every species except virginica flowers, using three
methods:
    • First use != to not select virginica - if the “species” column is in your index, you can use
        df.index instead of df["species"]
    • Next, use the to negate selecting virginica with == (Hint: wrap your logic in parentheses with
        the tilde outside for it to work on the result of your test of equality)
    • Finally, use the .isin method to select the other two flower types.
"""

    # != for virginica
df_iris_index[df_iris_index.index != "virginica"]

    # negate selecting virginica with == 
df_iris_index[~(df_iris_index.index == "virginica")]
    
    # .isin method to select the other two flower types
df_iris_index[df_iris_index.index.isin(["setosa", "versicolor"])]


# 1.10.

"""
1.10. Compare the means you calculated to the means in the iris means.csv, specifically for petal width.
To do this without groupby and merge (coming next week!), you will need to do the following:
    • Write a for-loop over the three types of flowers
        – Subset the df iris means data to just one type of flower (the loop iterator)
            ∗ Subset that single row to just “petal width”
            ∗ You now have a DataFrame with a single observation. To get that value out, use
            .iloc[0]
        – Use .loc to select the rows for that flower from your df iris data and call the .mean()
        method on that to get your calculated mean
            ∗ Then select just “petal width” from the resulting row
                – Finally, print out both values using the round function to 2 decimal places, then assert
                that they are equal (documentation).
"""

df_iris_means = pd.read_csv(os.path.join(PATH, "iris_means.csv"))


for flower in ["setosa", "versicolor", "virginica"]:
    filter_species_mean = df_iris_means[df_iris_means["species"] == flower]
    means = filter_species_mean["petal_width"]
    means = means.iloc[0]

    filter_species = df_iris.loc[df_iris["species"] == flower]
    calculated_means = filter_species["petal_width"].mean()

    print(f"{flower}. Results from df_iris_means = {round(means, 2)}. Calculated mean from df_iris = {round(calculated_means, 2)}")
    assert round(means, 2) == round(calculated_means, 2)

# MATPLOTLIB

# 2.1.

"""
One of the big uses of this dataset is to try and classify flowers based on the four numeric columns.
You wil work on this in more detail in Machine Learning, but for now we can do a simple version
using the tools we learned this week. Create two scatter plots of the main iris dataset, where each
species is represented by a different color:
    • Create one figure with two axis objects (try both side-by-side and stacked on top, pick
    whichever you prefer)
    • The first figure should plot sepal length against sepal width
    • The second figure should plot petal length against petal width
To make each of the three types of flowers a different color, try calling the plot method three times
on each axis - once for each flower - and setting the color differently on each. Try following the
steps in the Matplotlib Examples lecture, ending with a tidy for-loop! Clean up the figure using
some of the basic operations we’ve covered, or looking up others.
"""

fig, ax = plt.subplots(1, 2)

flowers = ["setosa", "versicolor", "virginica"]
colors = ["blue", "orange", "green"]

for flower, color in zip(flowers, colors):
    filtered = df_iris[df_iris["species"] == flower]
    ax[0].scatter(filtered["sepal_width"], filtered["sepal_length"], color=color)
    ax[1].scatter(filtered["petal_width"], filtered["petal_length"], color=color, label=flower)
fig.legend(loc="upper right")
#fig.supxlabel("Width")
#fig.supylabel("Length")
plt.show()

# 2.2.

"""
Looking at your figures, you should see that both plots show a pretty good separation of the flower
types, which is crucial to a type of problem called “classification”, but one of them is quite a bit
better than the other at cleanly separating one flower type from the other two.

Using that visual, identify a straight (vertical or horizontal) line that you could use to subset
the data so that one flower type is separated from the other two. Subset the data using just the
numeric columns at the point you identified. Do NOT separate based on the species column - we
want to know how well the point of separation works on some new flower where we only observe
the four numeric values, but not the species label! Once you have subsetted your data, use the
value counts method to see how many of each species you got into each dataframe.
"""
df_petal_1 = df_iris[df_iris["petal_length"] < 2]
df_petal_2 = df_iris[df_iris["petal_length"] >= 2]

df_petal_1["species"].value_counts()
df_petal_2["species"].value_counts()

# 2.3.

"""
2.3 Some stretch goals for yourself:
• Focus on the subset with the two species of flower that aren’t separated, and see if you can
identify another reasonable split point. Try that out and see what your new results are!
• If you aren’t happy with how your figure looks, try finding some more methods to improve the
visual. Everything in Matplotlib can be controlled, though the code tends to become verbose
quickly.
• Look up how to add vertical and horizontal lines to your Matplotlib output, and then add a
line at the point you split your data.
"""

# here an additional viz based on the filter I used in 2.2.
fig, ax = plt.subplots(1, 2)

flowers = ["setosa", "versicolor", "virginica"]
colors = ["blue", "orange", "green"]

for flower, color in zip(flowers, colors):
    filtered = df_petal_2[df_petal_2["species"] == flower]
    ax[0].scatter(filtered["sepal_width"], filtered["sepal_length"], color=color)
    ax[1].scatter(filtered["petal_width"], filtered["petal_length"], color=color, label=flower)
fig.legend(loc="upper right")
#fig.supxlabel("Width")
#fig.supylabel("Length")
plt.show()


# here the horizontal line in the original df.

fig, ax = plt.subplots(1, 2)

flowers = ["setosa", "versicolor", "virginica"]
colors = ["blue", "orange", "green"]

for flower, color in zip(flowers, colors):
    filtered = df_iris[df_iris["species"] == flower]
    ax[0].scatter(filtered["sepal_width"], filtered["sepal_length"], color=color)
    ax[1].scatter(filtered["petal_width"], filtered["petal_length"], color=color, label=flower)
    ax[1].axhline(y=2, color="gray", linestyle="--", linewidth=2)
fig.legend(loc="upper right")
#fig.supxlabel("Width")
#fig.supylabel("Length")
plt.show()