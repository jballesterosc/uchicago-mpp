import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np 
import os

PATH = "/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_3b"

provider_hhrg = pd.read_csv(os.path.join(PATH, "provider_hhrg.csv"))
case_mix_weight = pd.read_csv(os.path.join(PATH, "case_mix_weight.csv"))
case_mix_weight = case_mix_weight.rename(columns={"2014 Final HH PPS Case-Mix Weights": "casemix_2014"})



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

cols_descr = case_mix_weight["Description"].str.split(",", expand=True)
case_mix_weight[["episode", "therapy"]] = cols_descr
cols_cfs = case_mix_weight["Clinical, Functional, and Service Levels"]

case_mix_weight["clinical"]   = cols_cfs.str.slice(0, 2)
case_mix_weight["functional"] = cols_cfs.str.slice(2, 4)
case_mix_weight["service"]    = cols_cfs.str.slice(4, 6)

# 3.7. The values in each of the five merge key columns must be exactly the same, or they will not match. 
# Look at the unique values in the five columns in both datasets, then use Pandas string methods
# to adjust the values in case mix weight so that they match those in provider hhrg. This will
# require some careful organization!
# • Hint 1: There are many ways to visualize this. One suggestion is to write a for-loop over the
# five columns you created in each DataFrame, then print out the unique values of that column
# from provider hhrg and case mix weight to see them side by side.
# • Hint 2: For episodes, 1st and 2nd are “early”, 3rd+ is “late”
# • Hint 3: For the severity levels, look at the first letter of each in provider hhrg and compare
# that to the values in case mix weight.
# • Hint 4: The information on number of visits in case mix weight is more fine-grained than
# its counterpart in provider hhrg. Since you cannot make the provider hhrg data more fine-
# grained, you should instead map the provider hhrg values onto the case mix weight values.

for columns in ["episode", "therapy", "clinical", "functional", "service"]:
    print("provider_hhrg:", provider_hhrg[columns].unique())
    print("case_mix_weight:", case_mix_weight[columns].unique())
    print()
    print()

provider_hhrg["therapy"] = provider_hhrg["therapy"].str.strip() # Weird spaces before and after values. 
provider_hhrg["functional"] = provider_hhrg["functional"].str.strip() # I noticed inconsistency in spaces between the categories in this column 
case_mix_weight["therapy"] = case_mix_weight["therapy"].str.strip()

case_mix_weight["episode"] = case_mix_weight["episode"].replace({
    '1st and 2nd Episodes': 'Early Episode',
    "3rd+ Episodes": 'Late Episode',
    "All Episodes": 'Early or Late Episode'
})


case_mix_weight["therapy"] = case_mix_weight["therapy"].replace({
    "0 to 5 Therapy Visits": "0-13 therapies",
    "6 Therapy Visits": "0-13 therapies",
    "7 to 9 Therapy Visits": "0-13 therapies",
    "10 Therapy Visits": "0-13 therapies",
    "11 to 13 Therapy Visits": "0-13 therapies",
    "14 to 15 Therapy Visits": "14-19 therapies",
    "16 to 17 Therapy Visits": "14-19 therapies",
    "18 to 19 Therapy Visits": "14-19 therapies",
    "20+ Therapy Visits": "20+ therapies"
})

case_mix_weight["clinical"] = case_mix_weight["clinical"].replace({
    "C1": "Clinical Severity Level 1",
    "C2": "Clinical Severity Level 2",
    "C3": "Clinical Severity Level 3"
})

case_mix_weight["functional"] = case_mix_weight["functional"].replace({
    "F1": "Functional Severity Level 1",
    "F2": "Functional Severity Level 2",
    "F3": "Functional Severity Level 3"
})

case_mix_weight["service"] = case_mix_weight["service"].replace({
    "S1": "Service Severity Level 1",
    "S2": "Service Severity Level 2",
    "S3": "Service Severity Level 3",
    "S4": "Service Severity Level 4",
    "S5": "Service Severity Level 5"
})


# 3.8 Create a new DataFrame named provider hhrg wt by merging case mix weight with provider hhrg
# using the five columns you created. If you have set up these columns correctly, the data should
# perfectly merge multiple rows from provider hhrg to each unique row in case mix weight. We
# will use several methods to make sure this merge has worked properly:
# • Use the indicator=True argument to see where all rows in the new dataframe come from.
# Clean up your dataframe after this step (do not leave merge in your data.)
# • Use the validate argument to force an error to be raised if the merge doesn’t behave as
# expected.
# • Use an assert function to test that provider hhrg wt has the same number of rows as prodiver hhrg,
# both of which are equal to 111,904.
# • Use an assert function to test that the column casemix 2014 in the new DataFrame is not
# NaN for any row

provider_hhrg_wt = provider_hhrg.merge(
    case_mix_weight,
    on=["episode", "therapy", "clinical", "functional", "service"],
    how="left",
    indicator=True,
    validate="m:1"
)

print(provider_hhrg_wt["_merge"].value_counts())

provider_hhrg_wt = provider_hhrg_wt.drop(columns="_merge")

assert len(provider_hhrg_wt) == len(provider_hhrg) == (111904)

assert provider_hhrg_wt["casemix_2014"].notna().all()


"""
4. Billing Outlier Analysis
"""

# 4.1 To answer the questions in this section, create a new DataFrame named provider sum by:
# • Filtering the provider hhrg wt data so that only PROVIDER level observations remain,
# • Grouping by provider ID, name, and state,
# • Then calculating three new columns:
# – avg cost, equal to the mean payment amount per episode weighted by total episodes,
# – avg case mix, equal to the mean case mix in 2014 weighted by total episodes (recall that
# a higher case mix weight represents a higher average severity or complexity of the patients
# treated),
# – total episodes, equal to the sum of total episodes
# • Hint 1: It is possible to do all three of these in one step, but it is a challenging solution to
# arrive it, and results in a complex line of code for no particular gain! Instead, calculate each
# of these one at a time, storing each resulting Series as its own variable. Then use pd.concat
# to assemble the three into one DataFrame named provider sum.
# • Hint 2: The Pandas mean method does not take weights, so you should look up the Numpy
# average function.
# • Hint 3: The final provider sum data should have 8,652 rows and 6 columns (the three grouping
# columns and the three calculated columns)

provider_level = provider_hhrg_wt[provider_hhrg_wt["Smry_Ctgry"] == "PROVIDER"]

group_cols = ["Prvdr_ID", "Prvdr_Name", "State"]
grouped = provider_level.groupby(group_cols)

avg_cost = grouped.apply(lambda g: np.average(g["Avg_Pymt_Amt_Per_Epsd"], weights=g["Tot_Epsd_Stay_Cnt"]))
avg_cost.name = "avg_cost"

avg_case_mix = grouped.apply(lambda g: np.average(g["casemix_2014"], weights=g["Tot_Epsd_Stay_Cnt"]))
avg_case_mix.name = "avg_case_mix"

total_episodes = grouped["Tot_Epsd_Stay_Cnt"].sum()
total_episodes.name = "total_episodes"

provider_sum = pd.concat([avg_cost, avg_case_mix, total_episodes], axis=1).reset_index()




# 4.2 How much variation is there in average cost per episode by provider? Show your code to create
# a figure that answers this question. Hint: Look at the Seaborn histplot function, and make
# sure your figure is clearly labeled. This distribution has a “fat” right tail - what does this tell us
# about providers? Can we make the claim that relatively high-cost providers are defrauding the
# government from the density of average costs? Briefly explain why or why not. Hint: Re-read the
# introduction of this assignment!

fig, ax = plt.subplots(figsize=(10, 6))
sns.histplot(data=provider_sum, x="avg_cost", ax=ax)
ax.set_xlabel("Average Cost per Episode ($)")
ax.set_ylabel("Number of Providers")
ax.set_title("Distribution of Average Cost per Episode by Provider")
plt.tight_layout()
plt.show()

### There's significant variation in the average cost per episode, where most providers are between 2,000 and 2,500.
### We also get a fat right tail extending past above 6,000. Despite all of this, I don't think we can assert fraud 
### from high cost providers with only this data. We might need to include or control for variables like the severity 
### of the patients before making claims.

# 4.3 What is the relationship between average cost and average case-mix? Show the relationship using
# a figure. Hint: Look at the Seaborn regplot function, and make sure your figure is clearly labeled.
# Keeping in mind the definition of case-mix weight, what does this imply, and how is it relevant to
# our attempts to detect fraud?

fig, ax = plt.subplots(figsize=(10, 6))
sns.regplot(data=provider_sum, x="avg_case_mix", y="avg_cost", ax=ax, scatter_kws={"alpha": 0.3})
ax.set_xlabel("Average Case-Mix Weight")
ax.set_ylabel("Average Cost per Episode ($)")
ax.set_title("Average Cost vs. Average Case-Mix Weight by Provider")
plt.tight_layout()
plt.show()


### There is a positive relationship: providers with higher case-mix weights
# (sicker patients) tend to have higher average costs. This means some high-cost
# providers are expensive simply because they treat more severe cases. To detect
# fraud, we need to normalize costs by case-mix to identify providers charging
# more than expected given their patient severity.



# 4.4 Create a new column, cost normalized, which is the ratio of average cost to the average case-mix
# weight for each provider.

provider_sum["cost_normalized"] = provider_sum["avg_cost"] / provider_sum["avg_case_mix"]

# 4.5 What do the distributions of avg cost and cost normalized tell us? Recall from the introduction
# that we are (rather simplistically) looking to distinguish between two reasons for high average costs
# by a provider - accepting sicker patients, or committing fraud. Consider how this relates to the
# definitions of the numerator and denominator in cost normalized. Create a figure that shows the
# distributions of both of these variables overlaid on each other. Hint: Use the Seaborn histplot
# function twice on the same axis.

fig, ax = plt.subplots(figsize=(10, 6))
sns.histplot(data=provider_sum, x="avg_cost", ax=ax, label="Average Cost", alpha=0.5)
sns.histplot(data=provider_sum, x="cost_normalized", ax=ax, label="Cost Normalized", alpha=0.5)
ax.set_xlabel("Cost ($)")
ax.set_ylabel("Number of Providers")
ax.set_title("Distribution of Average Cost vs. Normalized Cost by Provider")
ax.legend()
plt.tight_layout()
plt.show()


### The normalized cost distribution is tighter (less spread) than the raw average cost,
### meaning that much of the variation in avg_cost is explained by differences in patient
### severity. Providers that still appear as outliers AFTER normalizing are the suspicious
### ones — their high costs can't be explained by having sicker patients, suggesting
### potential overbilling or fraud.

# 4.6 What are the top ten home health care providers with the highest average billing per episode
# in Illinois? What are the top ten providers with the highest average costs adjusted for case-mix
# weight in Illinois? Which group of ten would you recommend investigators focus on when looking
# for fraud? What would likely happen if investigators instead cracked down on the other group that
# you don’t recommend?


il_providers = provider_sum[provider_sum["State"] == "IL"]

print("Top 10 by Average Cost:")
print(il_providers.nlargest(10, "avg_cost")[["Prvdr_ID", "Prvdr_Name", "avg_cost"]])

print("\nTop 10 by Normalized Cost:")
print(il_providers.nlargest(10, "cost_normalized")[["Prvdr_ID", "Prvdr_Name", "cost_normalized"]])

#### The top 10 by avg_cost and top 10 by cost_normalized are completely different lists.
# The avg_cost list (e.g., Forum Health Care at $5,325) shows providers with the highest
# raw billing, but their costs may be justified by treating sicker patients. The 
# cost_normalized list (e.g., Care Solutions at $4,377 per unit of case-mix) shows 
# providers billing far more than expected given their patient severity.
#
# Investigators should focus on the cost_normalized top 10, since these providers' high
# costs cannot be explained by patient severity — making them the strongest fraud suspects.
# If investigators cracked down on the avg_cost group instead, they risk punishing agencies
# that legitimately care for the sickest patients, which could discourage providers from
# accepting high-severity cases and ultimately harm the patients who need care the most.


"""
EXTRA CREDIT
"""

# Approximately 20 hours this week