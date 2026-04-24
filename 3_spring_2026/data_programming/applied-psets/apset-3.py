import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
plt.style.use('fivethirtyeight') 


# 1. NATO Member Defense Spending
"""
1.1. Data Loading: Read in the file “nato spending.csv”
• Explore the structure of the DataFrame. You always want to get to know a new dataset before
continuing with analysis!
• Write an assert function to test that each row is uniquely identified by the “country iso3”
column. Hint: There are multiple ways to do this, but try using the value counts method as
a starting point. What should the value counts be if this one column uniquely identifies each
row, and how would you reduce that to a single True or False?
"""

PATH = ("/Users/jayballesteros/_github/uchicago-mpp/3_spring_2026/data_programming/applied-psets/_data/apset_2a/")
print(PATH)

df_nato = pd.read_csv(os.path.join(PATH, "nato_spending.csv"))


df_nato.columns
df_nato.head()
df_nato.tail()
df_nato.dtypes
df_nato.shape

assert (df_nato["country_iso3"].value_counts() == 1).all()



"""1.2 Descriptive Statistics: Use a single method of a DataFrame that reports the count, mean, std,
min, max, and quantiles. The numbers display in scientific notation because they are so large, so
transform the column to be in millions of dollars (it is currently in dollar amounts), and round the
reported values to two decimal points."""

df_nato["constant_2022_USD_billions"] = df_nato["constant_2022_USD_billions"] / 1000000

print(df_nato["constant_2022_USD_billions"].describe().round(2))


"""1.3 Kurtosis: This common statistic is not part of describe. Kurtosis is a measure of normality, with
a value of 3 meaning normally-distributed data. Report the kurtosis of defense spending. In a single
sentence comment, what does the kurtosis value tell us? Feel free to look up the interpretation if
necessary."""



"""1.4 USA in Standard Deviations: Massive US defense spending seems to have skewed the data.
Use Pandas methods to extract the standard deviation and the mean into their own variables, then
use these to calculate how many standard deviations US spending is above the mean. All values
used must be extracted from the DataFrame, not typed in directly."""



"""1.5 Ex-USA: The presence of extreme outliers (like US defense spending in this case) often obscures
the distribution of the rest of the data. Run the same descriptive statistics on the rest of the NATO
members with the US removed. What did this do to kurtosis, and was it what you expected?"""


"""1.6 Sorting: Sort the data excluding the US in descending order of spending.
"""

"""1.7 Spending Share: On the full data, calculate a new column named “spending share percent”
that shows each country’s defense spending as a percentage of total defense spending by NATO
members. Make sure the values of the new column sum to 100 with an assert statement."""

# 2. Russian Energy Imports
"""2.1 Long Data: We’ve learned that the US spends a lot on defense, but let’s explore more of the story.
Load the data on imports of Russian energy resources by NATO members, “rus imports.csv”.
• Show your code exploring the dataset.
• Which time periods are covered? Answer with code.
• Look the data over carefully, and figure out which combination of columns uniquely identifies
each row. Show code to test that those columns really do uniquely identify each row, then list
the columns in a comment."""


"""2.2 Wide Data: Load in the wide version of the Russian imports data (next week we will do this step
with code).
• Calculate how many missing values are in each row, and in each column.
• Filter the data so that only the years 2021 and 2022 remain.
• Assuming that a missing value in these years means the given country imports none of that
good from Russia, calculate a new column named “total imports” that is equal to the sum of
the three resource types.
• Transform that column to be in millions of dollars with two decimal points."""


"""2.3 Import Rankings: In 2021, which country imported the least from Russia? Show your answer
with code by sorting so that the country with the lowest value is at the top.
• Did the total value of imports by NATO members go up or down between 2021 and 2022?
Show your code with an answer in comments, and include a percentage change.
• Is this result surprising? Write 1-2 sentences in a comment for the rest of the research team
explaining why."""


"""2.4 Resources by Weight: While the results in total value may seem surprising, you do some
investigating and realize that the invasion of Ukraine also set off major energy price shocks world-
wide. Repeat your exploration, but looking at the weight (in kgs) of individual resources instead.
Using a for-loop over the three types of energy resources in this data, print out the percentage
change in each."""


"""2.5 Estimating 2022 Change in Value: If each country had maintained the imports of 2021 by
weight, while the prices had remained at their observed 2022 levels, how much additional money
would have been spent importing Russian energy in 2022 by NATO members (in aggregate averages,
not per country)? You can use a for-loop to solve this again.
• When printing your total, look up how to format a numeric value with comma separators for
the thousand’s place and two decimal points in an f-string, so that your output looks nice."""


# 3. Ukraine Aid Data
"""3.1 Total Aid by Category: Load in the direct aid to Ukraine data, “nato ukraine aid.csv”. Note
that, despite the filename, some of the countries in this data are not in NATO! In 2022, how much
was spent on each of humanitarian, military, and financial aid by NATO members? Show the
answers with code that splits the data by category of aid before summing, again using a for-loop."""

"""3.2 Total Aid by Place: Next, load the NATO Ukraine aid totals data, “nato ukraine aid totals.csv”
and, using subsetting, split it up by: 1. USA, 2. Canada, 3. Rest of NATO. Print out the total
aid from each of these three groups, alongside the percent of the total aid that it represents."""

"""4. GDP Weighting: While the level of contribution is a meaningful value (especially if you’re a
commander in Ukraine trying to buy equipment!), it does not accurately represent the burden each
NATO member is taking on. To find that, load in the GDP data from the World Bank, “gdp.csv”.
• Using a for-loop, loop over the country names that are common to both datasets, then in
the loop, divide their total aid to Ukraine by their GDP, to then print out their aid as a
percentage of each member country’s GDP.
• In a comment, write out the three countries that took on the largest burden relative to the
size of their economy, and explain in 1-2 sentences why you think these three countries might
be contributing so much."""


# 5. Creating a “Headline” Figure
"""5.1 It is often important to anchor your empirical results with a headline figure. We will set up a rough
figure here, that we will use as a starting point to improve when we introduce new tools next week.
• Create a figure using Matplotlib that has two axis in it, one at the top and one below it.
• On the top axis, create a bar chart with country names along the x-axis, and the total aid
sent to Ukraine along the y-axis. Sort the data so the countries on the x-axis are listed in
descending order by total contributions.
• On the bottom figure, create a bar chart that has country names along the x-axis, and the
total energy imports from Russia in 2022 up the y-axis, again sorting country order by total
energy imports.
• Add a title and axis labels to make the figure more clear, but do not worry about fixing other
issues with the display."""

"""5.2 This figure roughly shows money sent to Ukraine at the top, and money sent to Russia at the
bottom. Imagine the story you are trying to tell with this figure, and what ways would quickly
and clearly communicate that story to a viewer. In comments (no code necessary) identify three
ways to improve this figure. As with the other parts of this analysis, we will revisit this later!"""


# 6. Extra Credit
"""6.1 Time Reporting: For one free point, write in a comment an estimate of how many hours you
spent on Data and Programming this week (Monday morning to Sunday at midnight). Include
time spent watching lectures, attending lab, and doing work and study. Your answer will in no way
be used in assessing you; it is purely for our internal information to be used in balancing workloads
in this class in future quarters!"""