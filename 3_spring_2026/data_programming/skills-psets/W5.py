import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# 1.1.

PATH = ("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/skills-psets/_data")
print(PATH)

df_iris = pd.read_csv(os.path.join(PATH, 'iris.csv'))

# 1.2.

df_iris.index
df_iris.columns
df_iris.head()
df_iris.tail()
df_iris.dtypes
df_iris.shape

# 1.3. 

df_iris['species'].unique()
df_iris['species'].value_counts()


# 1.4. 

    # df_iris.mean() this prints error, since it cannot calculate species (object dtype) 

np.mean(df_iris.select_dtypes(include=["float64"]))

df_iris.select_dtypes(include=["float64"]).mean()
df_iris.select_dtypes(include=["float64"]).median()
df_iris.select_dtypes(include=["float64"]).std()


df_iris.set_index('species')

# 1.5.
df_iris.describe()


df_iris.describe().T # I found this option in stackoverflow: https://stackoverflow.com/questions/34085081/pandas-df-describe-is-it-possible-to-do-it-by-row-without-transposing

# 1.6.
    # this is the view option
petals = [p for p in df_iris.columns if p.startswith('petal')]
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

df_iris_index = df_iris.set_index('species') # https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.set_index.html
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







