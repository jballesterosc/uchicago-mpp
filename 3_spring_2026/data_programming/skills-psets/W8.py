import pandas as pd
import numpy as np

PATH = "/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/skills-psets/_data/w8/"

"""
1. Data Loading
"""


# 1.2. Loading, Joining, Reshaping: Load in all eight files and combine them into one “tidy” dataset, where each row is uniquely identified by year, MSA Fips, and MSA Name, and there is a column for each of military employment, civilian government employment, and total employment.
# • Audit your merging to be certain you are doing what you expect!
# • Before you join them into one final file and before you do any reshaping, your civilian government employment data should have 927 rows and 31 columns (29 years, GeoName, and GeoFips), 
#   while your military and total data should have 1,852 rows and 32 columns (29 years, GeoName, GeoFips, and Description). Why are there a different number of rows in the two DataFrames?

### civilians

df_1 = pd.read_csv(os.path.join(PATH, "civilian government employment 1990-2000 Metro.csv"),
                    skiprows=3, skipfooter=8, engine="python")
df_2 = pd.read_csv(os.path.join(PATH, "civilian government employment 1990-2000 Micro.csv"),
                    skiprows=3, skipfooter=8, engine="python")
df_3 = pd.read_csv(os.path.join(PATH, "civilian government employment 2001-current Metro.csv"),
                    skiprows=3, skipfooter=7, engine="python")
df_4 = pd.read_csv(os.path.join(PATH, "civilian government employment 2001-current Micro.csv"),
                    skiprows=3, skipfooter=7, engine="python")

civ_1 = df_1.merge(df_3, on=["GeoFips"], how="outer")
civ_2 = df_2.merge(df_4, on=["GeoFips"], how="outer")

civ_1 = civ_1.drop(columns=["GeoName_x"]).rename(columns={"GeoName_y": "GeoName"})
civ_2 = civ_2.drop(columns=["GeoName_x"]).rename(columns={"GeoName_y": "GeoName"})

civ_wide = pd.concat([civ_1, civ_2])
print(civ_wide.shape)

civ_long = civ_wide.melt(
    id_vars=["GeoFips", "GeoName"],
    var_name="Year",
    value_name="civ_gov_emp"
)

### military

df1 = pd.read_csv(os.path.join(PATH, "military employment 1990-2000 Metro.csv"),
                  skiprows=4, skipfooter=8, engine="python")
df2 = pd.read_csv(os.path.join(PATH, "military employment 1990-2000 Micro.csv"),
                  skiprows=4, skipfooter=7, engine="python")
df3 = pd.read_csv(os.path.join(PATH, "military employment 2001-current Metro.csv"),
                  skiprows=4, skipfooter=7, engine="python")
df4 = pd.read_csv(os.path.join(PATH, "military employment 2001-current Micro.csv"),
                  skiprows=4, skipfooter=6, engine="python")

for df in [df1, df2, df3, df4]:
    df.drop(columns=["LineCode"], inplace=True)
    df["Description"] = df["Description"].str.strip()

df1 = df1[df1["Description"] != "Employment by place of work"]
df2 = df2[df2["Description"] != "Employment by place of work"]
df3 = df3[df3["Description"] != "Employment by place of work"]
df4 = df4[df4["Description"] != "Employment by place of work"]

mil_1 = df1.merge(df3, on=["GeoFips", "Description"], how="outer")
mil_2 = df2.merge(df4, on=["GeoFips", "Description"], how="outer")

mil_1 = mil_1.drop(columns=["GeoName_x"]).rename(columns={"GeoName_y": "GeoName"})
mil_2 = mil_2.drop(columns=["GeoName_x"]).rename(columns={"GeoName_y": "GeoName"})

mil_wide = pd.concat([mil_1, mil_2])
print(mil_wide.shape)


mil_long = mil_wide.melt(
    id_vars=["GeoFips", "GeoName", "Description"],
    var_name="Year",
    value_name="Value"
)

## It seems that the civilian government file has one data series per MSA (in this case civilian government employment), so each MSA appears in exactly one row.
## In the case of the the military, the files contain two data series per MSA, differentiated by the description "Military"  and "Total employment ". 
## That means each MSA appears in two rows (one for each series). So military/total has roughly two times the rows of civilian even though both cover the same set of MSAs.

# • Your final merge after reshaping to long will have 29 place-year observations in the civilian government data that don’t match anything in the military and total data. What place is it?
# • When you remove the unmatched observation and before you calculate any other columns, your data should have 26,854 rows and 6 columns. 
# • Note that the Pandas .str.replace method has a key-word argument, regex=False. If you set it to True then it will take a regular expression pattern as its replace string. You

mil_pivot = mil_long.pivot_table(
    index=["GeoFips", "GeoName", "Year"],
    columns="Description",
    values="Value"
).reset_index()

mil_pivot.columns.name = None
mil_pivot = mil_pivot.rename(columns={
    "Military": "mil_emp",
    "Total employment (number of jobs)": "total_emp"
})

final = mil_pivot.merge(civ_long, on=["GeoFips", "Year"],
                        how="outer", indicator=True)

final = final[final["_merge"] == "both"].drop(columns=["_merge"])

final = final.drop(columns=["GeoName_x"]).rename(columns={"GeoName_y": "GeoName"})

print(final.shape) 
print(final.columns.tolist())

## The unmatched observations belong to Bluffton, IN (Micropolitan Statistical Area). This observation exists in the civilian government data but not in military/total data.

"""
2. Summaries
"""

# 2.1 Now that your data is loaded and parsed, show the top 5 MSAs by share of military employment, 
# by share of civilian government employment, and by ratio of military to civilian government em- 
# ployment. Look at these rankings for the average values between 1990 and 1999, and for 2009 through 2018.

final["mil_share"] = final["mil_emp"] / final["total_emp"]
final["civ_gov_share"] = final["civ_gov_emp"] / final["total_emp"]
final["mil_to_civ_ratio"] = final["mil_emp"] / final["civ_gov_emp"]

final["Year"] = final["Year"].astype(int)

final["mil_share"] = final["mil_emp"] / final["total_emp"]
final["civ_gov_share"] = final["civ_gov_emp"] / final["total_emp"]
final["mil_to_civ_ratio"] = final["mil_emp"] / final["civ_gov_emp"]

period_90s = final[(final["Year"] >= 1990) & (final["Year"] <= 1999)]
period_00s = final[(final["Year"] >= 2009) & (final["Year"] <= 2018)]

avg_90s = period_90s.groupby(["GeoFips", "GeoName"])[["mil_share", "civ_gov_share", "mil_to_civ_ratio"]].mean()
avg_00s = period_00s.groupby(["GeoFips", "GeoName"])[["mil_share", "civ_gov_share", "mil_to_civ_ratio"]].mean()

print("Top 5 by military share:")
print("\n1990-1999:")
print(avg_90s.nlargest(5, "mil_share")[["mil_share"]])
print("\n2009-2018:")
print(avg_00s.nlargest(5, "mil_share")[["mil_share"]])

print("Top 5 by civilian share:")
print("\n1990-1999:")
print(avg_90s.nlargest(5, "civ_gov_share")[["civ_gov_share"]])
print("\n2009-2018:")
print(avg_00s.nlargest(5, "civ_gov_share")[["civ_gov_share"]])

print("Top 5 by military to civilian government ratio:")
print("\n1990-1999:")
print(avg_90s.nlargest(5, "mil_to_civ_ratio")[["mil_to_civ_ratio"]])
print("\n2009-2018:")
print(avg_00s.nlargest(5, "mil_to_civ_ratio")[["mil_to_civ_ratio"]])


"""
4. Voting Data
"""

# 4.1 We will now look at how military and civilian government share of employment relate to voting patterns. To begin, we will load in data on the 2018 US House votes by county from the MIT 
# Election Lab. You can download that file directly, or from Canvas (HOUSE precinct general.csv).Explore the data, and look at the code book on the website.
# • The only columns we need are the county FIPS codes, the simplified party, and the vote totals.
# • What should you do with NaNs? Look up what “OVER VOTES” and “UNDER VOTES” mean.
# • What uniquely identifies each observation in this data (looking at the whole dataset, before any coumns are dropped)? We want the data to have party 
# (REPUBLICAN, DEMOCRAT, etc) as the columns with values, while each row is uniquely identified by county. Use groupby and pivot to do this. You should have 3,150 rows and 6 columns.
# • What should you do with the NaNs in this reshaped DataFrame?


house = pd.read_csv(os.path.join(PATH, "HOUSE_precinct_general.csv"), engine="python")

house = house[["county_fips", "party_simplified", "votes"]]

house = house.dropna(subset=["party_simplified"])

house = house[house["county_fips"].notna()]
house = house[house["county_fips"] != "NAN"]

house["county_fips"] = house["county_fips"].astype(str).str.strip()

house = house.groupby(["county_fips", "party_simplified"])["votes"].sum().reset_index()

house = house.pivot(index="county_fips", columns="party_simplified", values="votes").reset_index()
house.columns.name = None

house = house.fillna(0)

print(house.shape)  


# 4.2 To connect this with our MSA-level employment data, we need a “crosswalk” - a dataset that has counties on one side matched up with the MSA each county is a part of.
# • Navigate to the Mable Geocorr (Geographic Correspondence Engine) website that will build
# custom crosswalks for US geographies.
# • Select all states (use shift+click)
# • Select “County” from the left geography box (2010)
# • Select “Core Based Statistical Area (CBSA)” from the right geography box (2010)
# • Leave all other settings at default and click “Run request” at the bottom of the “Output
# Options” box
# • Load in the crosswalk as a DataFrame
# – Assert that the allocation factor is 1 for all places (that is, the entire county is part of
# each MSA it is matched with). This must be true, because MSAs are delineated along the
# boundaries of counties, but that is not true for all types of geography!
# – Limit the crosswalk to the county, cbsa10, and cbsaname10 columns, and rename them to
# match the relevant columns in the voting and employment data

crosswalk = pd.read_csv(os.path.join(PATH, "geocorr2018_2613501676.csv"),
                         encoding="latin-1", skiprows=[1])

crosswalk["afact"] = pd.to_numeric(crosswalk["afact"].astype(str).str.strip())
assert (crosswalk["afact"] == 1).all(), "Not all allocation factors are 1!"

crosswalk = crosswalk[["county", "cbsa10", "cbsaname10"]].rename(columns={
    "county": "county_fips",
    "cbsa10": "GeoFips",
    "cbsaname10": "GeoName"
})

crosswalk["county_fips"] = crosswalk["county_fips"].astype(str).str.strip().str.zfill(5)

print(crosswalk.shape)

# 4.3 Now we will merge the crosswalk into our voting data.
# • Restrict the employment data to 2018 only
# • What is the merge key between these two DataFrames? Make sure the merge is one-to-one.
# • How many observations merged both, left, and right only? Normally we would have to inves-
# tiate each one of these and see if we can resolve the failed match! For now, just keep the ones
# that are in both DataFrames (2,961 rows)
# • After we have merged MSA information into our county-level data, we want to drop the county
# identifier column and use groupby to aggregate our vote totals to the MSA level.


### 4.3 - Merge crosswalk into voting data

voting = house.merge(crosswalk, on="county_fips", how="outer", indicator=True)
print(voting["_merge"].value_counts())

voting = voting[voting["_merge"] == "both"].drop(columns=["_merge"])

voting = voting.drop(columns=["county_fips"])
voting = voting.groupby(["GeoFips", "GeoName"], as_index=False).sum()

print(voting.shape)

# 4.4 Finally, we merge our vote data with our employment data. Do the same investigations - what is
# the merge key, and how many don’t align? We will once again skip investigating those, and keep
# only the 812 rows that match with both.

emp_2018 = final[final["Year"] == 2018].copy()

voting["GeoFips"] = voting["GeoFips"].astype(int)

combined = emp_2018.merge(voting, on="GeoFips", how="outer",
                          indicator=True, suffixes=("_emp", "_vote"))

print(combined["_merge"].value_counts())

combined = combined[combined["_merge"] == "both"].drop(columns=["_merge"])

combined = combined.drop(columns=["GeoName_vote"]).rename(columns={"GeoName_emp": "GeoName"})

print(combined.shape)
print(combined.columns.tolist())

"""
5. Analysis
"""
# 5.1 Create a new column named rep dem, equal to the republican vote divided by the democratic vote.
# There are some unexpected values in here! Explore it to see what they are.

combined["rep_dem"] = combined["REPUBLICAN"] / combined["DEMOCRAT"]
print(combined["rep_dem"].describe())
print("\ninf values:", np.isinf(combined["rep_dem"]).sum())
print("zero values:", (combined["rep_dem"] == 0).sum())
print("NaN values:", combined["rep_dem"].isna().sum())

# 5.2 There are multiple ways to deal with the problem of the republican or democrat vote total being
# zero. In this case we will use a simple form of a statistical technique called winsorization, where
# extreme values are replaced with less extreme values from another part of the distribution:
# • Find the max value of rep dem for all MSAs where there are non-zero votes for both. Then
# find the min value.
# • Now replace inf (dem = 0) with the max, and 0 (repub = 0) with the min.

valid = combined[(combined["DEMOCRAT"] > 0) & (combined["REPUBLICAN"] > 0)]
max_ratio = valid["rep_dem"].max()
min_ratio = valid["rep_dem"].min()

print(f"\nmax_ratio: {max_ratio}")
print(f"min_ratio: {min_ratio}")

combined.loc[np.isinf(combined["rep_dem"]), "rep_dem"] = max_ratio

combined.loc[combined["rep_dem"] == 0, "rep_dem"] = min_ratio

print(combined["rep_dem"].describe())

# 5.3 Fit the model: rep dem∼ mil share. What is the coefficient and p-value on mil share?

import statsmodels.formula.api as smf

reg_data = combined.dropna(subset=["rep_dem", "mil_share", "civ_gov_share"])

model_mil = smf.ols("rep_dem ~ mil_share", data=reg_data).fit()
print(f"Coefficient on mil_share: {model_mil.params['mil_share']:.4f}")
print(f"P-value on mil_share: {model_mil.pvalues['mil_share']:.4f}")

# 5.4 Fit the model: rep dem∼ civ gov share. What is the coefficient and p-value on civ gov share?


model_civ = smf.ols("rep_dem ~ civ_gov_share", data=reg_data).fit()

print(f"Coefficient on civ_gov_share: {model_civ.params['civ_gov_share']:.4f}")
print(f"P-value on civ_gov_share: {model_civ.pvalues['civ_gov_share']:.4f}")

# 5.5 There are obviously many more confounding elements that go into analyzing voting patterns, but
# does this simple analysis confirm what you expect about voter behavior?

# Results:
# - For mil_share the coef is 27.56, and p < 0.0001. This means that it is positive and significant,
# - In the case of civ_gov_share, the coef = 36.06, p = 0.004. Results are positive and significant,
#
# The military result validates that MSAs with higher military employment share are more Republican. 
# Also, communities built around military bases are conservative as well.
#
# In the case of the civilian result, employees are mostly democrats. 
#
# Nonetheless this descriptive analysis, without proper controls like education and income, 
# we cannot interpret this coefs as casual, but just as simpler correlations.
