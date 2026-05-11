import pandas as pd
import seaborn as sns
import numpy as np 
import os

PATH = "/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_3a"
print(PATH)


"""
1. Data loading
"""

# 1.1. Provider data

provider = pd.read_csv(os.path.join(PATH, 
"unformatted_medicare_post_acute_care_hospice_by_provider_and_service_2014_12_31.csv"), na_values=["*"])
assert provider.shape == (31665, 122)
print(provider.dtypes.value_counts())

# 1.2. HHRG Data

provider_hhrg = pd.read_excel(os.path.join(PATH, "Provider_by_HHRG_PUF_2014.xlsx"), sheet_name="Data", thousands=",")
dollar_cols = ['Bene_Dstnct_Cnt', 'Tot_Epsd_Stay_Cnt', 'Tot_Srvc_Days', 
                'Avg_Chrg_Per_Bene', 'Avg_Pymt_Amt_Per_Bene', 'Avg_Stdzd_Pymt_Amt_Per_Bene', 
                'Avg_Chrg_Per_Epsd', 'Avg_Pymt_Amt_Per_Epsd', 'Avg_Stdzd_Pymt_Amt_Per_Epsd', 
                'Avg_Chrg_Per_Day','Avg_Pymt_Amt_Per_Day', 'Avg_Stdzd_Pymt_Amt_Per_Day']

for col in dollar_cols:
    if provider_hhrg[col].dtype == "object":
        provider_hhrg[col] = (provider_hhrg[col]
                              .str.replace("$", "", regex=False)
                              .str.replace(",", "", regex=False)
                              .astype(float))


assert provider_hhrg.shape == (111904, 20)

# 1.3. Case Weight Mix data


case_mix_weight = pd.read_excel(os.path.join(PATH, "CY 2014 Final HH PPS Case-Mix Weights.xlsx"))

case_mix_weight = case_mix_weight.drop(columns=["2013 HH PPS Case-Mix Weights"])
case_mix_weight = case_mix_weight.rename(columns={"2014 HH PPS Case-Mix Weights": "casemix_2014"})

assert case_mix_weight.shape == (153, 4)

"""
2. Data orientation and validation
"""

# 2.1. What are the five types of Service Categories in provider? 
# Unless you are quite familiar with Medicare home health care data, 
# you likely will need to look these up! Once you know, show them in your DataFrame, 
# and briefly describe them in comments.

print(provider["Srvc_Ctgry"].value_counts())

# The five service categories are, by descending order:
# SNF (Skilled Nursing Facility),
# HH (Home Health),
# HOS (Hospice),
# IRF (Inpatient Rehabilitation),
# LTC (Long-term care hospital).


# 2.2. The data in provider and provider hhrg contain observations at three different 
# levels of aggregation. What are they? Hint: You don’t need to study all 122 columns 
# to figure it out, because it’s pretty close to the start!

print(provider["Smry_Ctgry"].value_counts())

# The levels of aggregation are in "Smry_Ctgry", and are:
# PROVIDER, STATE and NATION  

# 2.3. Search the internet to discover how many people received home health care benefits 
# (Srvc Ctgry == "HH") from Medicare in calendar year 2014. Include a link to a primary 
# source. Compare this to the total number of beneficiaries in provider. 
# Hint: Familiarize yourself with the way names are abbreviated in the columns of this 
# data, then find one that looks like “Beneficiaries Distinct Counts.” 
# Do these numbers roughly align?

# According to cms.gov, approx 3.5 million https://www.cms.gov/newsroom/press-releases/medicare-finalizes-home-health-payments-2014

hh_medicare = provider[(provider["Srvc_Ctgry"] == "HH") & (provider["Smry_Ctgry"] == "NATION")]
print(hh_medicare["Bene_Dstnct_Cnt"])

# In the provider dataframe, we have 3,416,037 beneficiares. Roughly aligned. 
# Maybe CMS rounded the data? Still there´s a huge difference.

# 2.4. Compare the total number of episodes for home health care in provider and provider hhrg. An
# “episode” is measured as a “stay count”, which includes all services rendered over a 60-day period
# of care. Do these numbers align? Why do you think they do or do not? Briefly explain in a comment.

print(hh_medicare["Tot_Epsd_Stay_Cnt"].sum())

hhrg_medicare = provider_hhrg[provider_hhrg["Smry_Ctgry"] == "NATION"]
print(hhrg_medicare["Tot_Epsd_Stay_Cnt"].sum())

# provider shows 6558889 vs 5988839 in provider_hhrg. Maybe the difference is due to a 
# subreporting by provider_hhrg that only included episodes that are competence for HHRG


# 2.5 Focus on just the provider-level rows in provider hhrg. Within this subset of rows, what column(s)
# uniquely identify each row? To test this, note that all groupby objects have a property that reports
# the number of unique groups - use this to assert that the size of the data doesn’t change when you
# group. There is an equivalent way to test this - write a second assert that checks that each group
# contains exactly one row. 

hhrg_providers = provider_hhrg[provider_hhrg["Smry_Ctgry"] == "PROVIDER"]

print(hhrg_providers.columns.tolist())

grouped = hhrg_providers.groupby(["Prvdr_ID", "Grpng"])

assert grouped.ngroups == len(hhrg_providers)

assert (grouped.size() == 1).all()

"""
3. Merge Provider Costs with Case Mix, part 1
"""

# 3.1. To assess whether a provider is overbilling, we need to merge the weights from case mix weight
# with the data in provider hhrg. However, while there is no single column that links the two
# datasets as they currently are, we can use existing columns to create the merge keys we need.
# Review both datasets. What five types of information (potential merge keys) exist in the existing
# columns that are common to both? Hint: Start with case mix weight since it has fewer columns!
# In your own words, briefly explain what these five pieces of information are in a comment.

print(case_mix_weight.columns.tolist())

print(provider_hhrg.columns.tolist())


print(case_mix_weight.head())
print(provider_hhrg[["Grpng", "Grpng_Desc"]].head(10))

print(provider_hhrg["Grpng_Desc"].iloc[0])

# The common information on both are:
# 1. Whether it's an early (1st or 2nd) or late (3rd+) episode
# 2. Range of therapy visits (e.g., 0-13, 14-19, 20+)
# 3. Clinical severity level (C1, C2, C3)
# 4. Functional severity level (F1, F2, F3)
# 5. Service severity level (S1-S5)

# In case_mix_weight, these are encoded in the Payment group and the Clinical, 
# Functional, and Service Levels column. In provider_hhrg, all five are packed 
# into the Grpng and described in Grpng_Desc.


# 3.2. Which column has the information necessary for merging from provider hhrg? 
# How many unique values are in this column? What appears to be separating the information 
# in this column, and how many pieces of information does it hold?

print(provider_hhrg["Grpng_Desc"].nunique())
print(provider_hhrg["Grpng_Desc"].unique())

# 153 unique values. It contains the same pieces of information that the ones provided in the last question

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

# The 6th column is caused by rows where Grpng_Desc has a  comma (all are "Late Episode, 14-19 therapies"). 
# This is creating an extra  split. To only drop the 6th column should be enough -- not the rows.

# EXTRA CREDIT: 15 hours