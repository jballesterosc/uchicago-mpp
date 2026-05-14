import pandas as pd
import numpy as np

PATH = "/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/skills-psets/_data/w8/"



"""
1. Data Loading
"""


# 1.2. Loading, Joining, Reshaping: Load in all eight files and combine them into one “tidy” dataset, where each row is uniquely identified by year, MSA Fips, and MSA Name, and there is a column for each of military employment, civilian government employment, and total employment.
# • Audit your merging to be certain you are doing what you expect!
# • Before you join them into one final file and before you do any reshaping, your civilian government employment data should have 927 rows and 31 columns (29 years, GeoName, and GeoFips), while your military and total data should have 1,852 rows and 32 columns (29 years, GeoName, GeoFips, and Description). Why are there a different number of rows in the two DataFrames?
# • Your final merge after reshaping to long will have 29 place-year observations in the civilian government data that don’t match anything in the military and total data. What place is it?
# • When you remove the unmatched observation and before you calculate any other columns, your data should have 26,854 rows and 6 columns. 
# • Note that the Pandas .str.replace method has a key-word argument, regex=False. If you set it to True then it will take a regular expression pattern as its replace string. You


### Civilians

df_1 = pd.read_csv(os.path.join(PATH, "civilian government employment 1990-2000 Metro.csv"), skiprows = 3, skipfooter = 8)
df_2 = pd.read_csv(os.path.join(PATH, "civilian government employment 1990-2000 Micro.csv"), skiprows = 3, skipfooter = 8)
df_3 = pd.read_csv(os.path.join(PATH, "civilian government employment 2001-current Metro.csv"), skiprows = 3, skipfooter = 7)
df_4 = pd.read_csv(os.path.join(PATH, "civilian government employment 2001-current Micro.csv"), skiprows = 3, skipfooter = 7)

df_1["Cat1"] = "Metro"
df_2["Cat1"] = "Micro"
df_3["Cat1"] = "Metro"  
df_4["Cat1"] = "Micro"

# for civilian governmment, we need to pivot because years are attributes 
# instead of variables now (long format). Then GeoFlips and next GeoName

df_1 = df_1.melt(
    id_vars=["GeoFips", "GeoName", "Cat1"],
    var_name="Year",
    value_name="Value"
)

df_2 = df_2.melt(
    id_vars=["GeoFips", "GeoName", "Cat1"],
    var_name="Year",
    value_name="Value"
)

df_3 = df_3.melt(
    id_vars=["GeoFips", "GeoName", "Cat1"],
    var_name="Year",
    value_name="Value"
)

df_4 = df_4.melt(
    id_vars=["GeoFips", "GeoName", "Cat1"],
    var_name="Year",
    value_name="Value"
)

merged_civilian = pd.concat([df_1, df_2, df_3, df_4])


### military

df1 = pd.read_csv(os.path.join(PATH, "military employment 1990-2000 Metro.csv"), skiprows=4, skipfooter=8)
df2 = pd.read_csv(os.path.join(PATH, "military employment 1990-2000 Micro.csv"), skiprows=4, skipfooter=7)
df3 = pd.read_csv(os.path.join(PATH, "military employment 2001-current Metro.csv"), skiprows=4, skipfooter=7)
df4 = pd.read_csv(os.path.join(PATH, "military employment 2001-current Micro.csv"), skiprows=4, skipfooter=6)

df1["Cat2"] = "Metro"
df2["Cat2"] = "Micro"
df3["Cat2"] = "Metro"
df4["Cat2"] = "Micro"



# year, MSA Fips, and MSA Name