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


# 1.6. Create a new DataFrame by subsetting only the petal columns using a list comprehension. Do this 
# three ways - one that creates a view, and two that create new copies.

petals = [p for p in df_iris.columns if p.startswith('petal')]
df_petals = df_iris[petals]
df_petals

