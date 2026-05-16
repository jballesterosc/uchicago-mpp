import pandas as pd
import seaborn as sns
import numpy as np 
import os

PATH = "/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_3b"

provider_hhrg = pd.read_csv(os.path.join(PATH, "provider_hhrg.csv"))
case_mix_weight = pd.read_csv(os.path.join(PATH, "case_mix_weight.csv"))

"""
3. Merge Provider Costs with Case Mix, part 2
"""

# 3.5. Which two columns in case mix weight will you use to extract the information necessary to merge 
# with the five new columns in provider hhrg? How many unique groups are created with these columns as 
# they currently exist?

### To extract the information necessary fopr the merging, I have to use both the 'Description' and 'Clinical, Functional, and Service Levels' colummns. 

print(case_mix_weight.groupby(["Description", "Clinical, Functional, and Service Levels"]).ngroups) # https://stackoverflow.com/questions/27787930/how-to-get-number-of-groups-in-a-groupby-object-in-pandas

### And for the groups, we currently have 153 unique combinations. 

# 3.6. Take the columns you selected and split them into five columns containing the same information 
# (and having the same names) as the five columns you created in provider hhrg. Hint: The 
# values won’t be exactly the same yet, so look for the same information, not exact matches! The
# .str.slice method might be helpful here.



# 3.3. Take the column you chose and split it into separate columns, each containing one part of the information. 
# Hint: Look up the .str.split() method.

split_cols = provider_hhrg["Grpng_Desc"].str.split(",", expand=True)
print(split_cols.head())
print(split_cols.shape)

# Splitting on commas produces 6 columns, but only 5 contain meaningful info tho

# 3.4. The information should fit in five columns, but a sixth column will be generated 
# based on a handful of rows with an inconsistency. What is causing the issue, and what 
# should you do with the sixth column? Should you drop the rows with the inconsistency? 
# Correct the issue of the sixth column and describe what you did and why in a brief comment.

split_cols = split_cols.drop(columns=[5])

provider_hhrg[["episode", "therapy", "clinical", "functional", "service"]] = split_cols

print(provider_hhrg[["episode", "therapy", "clinical", "functional", "service"]].head())