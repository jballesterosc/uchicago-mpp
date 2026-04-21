import os
import pandas as pd
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