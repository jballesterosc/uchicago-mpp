############################
### HW 2 Template        ###
### PPHA 31002: Stats I  ###
### Due 27 October 2025  ###
############################


library(ggplot2)
library(dplyr)
library(tidyr)


### set the working directory
setwd("/Users/jayballesteros/_github/uchicago-mpp/stats1/hw2")

### DATA EXERCISE ###
### read in the cbo.csv data set
cbo <- read.csv("_data/cbo-4.csv")

### Q20
# How many observations are there in the sample?
nrow(cbo) ### ANSWER: 127181, and they are households. 


### Q21: Plot a histogram of household income before taxes and transfers 
# ("htotval"). Use the `freq=F` option to display a density on the vertical 
# axis. As always, be sure to label all axes (the default label of "density" 
# is fine on the vertical axis) and include a title for the plot. 
# *Hint*: Before you can plot this histogram, you must do some data preparation. 
# Specifically, the htotval variable is a string variable that is =None for 
# households with zero income. Let’s first re-code values =None as =0 and then 
# use the as.numeric() function to store htotval as a numeric variable. You’ll 
# then be able to plot the histogram.

cbo$htotval[cbo$htotval == "None"] <- 0
cbo$htotval <- as.numeric(cbo$htotval)

hist(cbo$htotval, freq=F, 
     main="Household income before taxes and transfers", 
     xlab="Income before taxes and transfers",
     ylab="")

### Q22
# What is the mean household income before taxes and transfers in this period? 
mean(cbo$htotval) ## 135477.2
# What is the median household income before taxes and transfers in this period?
median(cbo$htotval) ## 101000

## Q23


### Q24
###  What fraction of the sample has federal taxes<0 at baseline?



### Q25
# Create three variables that represent the difference between the BBB policy 
# scenario and the baseline policy for the three sets of variables that start 
# with inkind, states_response, and other_spending (see the hint in the prompt).


## What is the value for the (see the hint in the prompt about missing values):
# average of "inkind_diff"

# average of "state_response_diff"

# average of "other_spending_diff"


### Q26: Create variable change in "fedtaxes_transfers_diff"

# What is the average value for "fedtaxes_transfers_diff"

### Q27
# Create a variable that is the sum of the four changes in questions 25-26.
# You will need this variable to create the diamonds in Figure 1. 

# What is the average value of this new variable?


### Q28 Create variable "cbo_income". Create this variable at baseline in the 
# data by adding transfers to htotval and subtracting state and federal taxes.

# what is the mean of "cbo_income"?

# What is the median of "cbo_income"?

### Q29 
# For the horizontal axis of the figures, CBO constructs deciles in which the sample
# is divided into 10 equalsized groups according to income. Use the quantile() 
# function to create a variable coded 1-10, where 1 is the lowest income group 
# and 10 the highest. You can use the default settings for this function in R.
# Hint: see the code snippet in the prompt.

# How many households do you have in each group?

### Q30 The CBO excludes households with negative income from their analysis. 
# How many households with negative CBO income are there? 

# How many households with zero CBO income? 

## Recode the decile variable you constructed in question 29 to be missing 
# (i.e., =NA) for these two types of households


### Q31 
# Create a bar plot that displays the mean change in household resources for each 
# income group from question 27. This plot should (approximately) correspond to 
# the height of the diamonds in Figure 1 from the CBO memo (you can ignore the 
# sub-components of the total change displayed in the memo’s figure). 

## First you need to calculate the mean of your variable by decile. 
# Hint: See the prompt for sample code.

## Create the graph. Label the axes as in Figure 1 from the CBO memo.


### Q32 
# Let’s now reproduce part of Figure 2 from the CBO memo. In particular, we will 
# create a bar plot in which the height of each bar corresponds to the height of 
# the diamonds in Figure 2. To construct this figure, you will need to take the
# mean annual change in resources for a given decile that you calculated in 
# question 31 and divide that value by the mean cbo_income for households in 
# that decile. Label the axes as in Figure 2 from the CBO memo.




### Q33 
# We will now explore changes in resources for households near the median of the
# income distribution and near the mean value in the income distribution. 

# Find and report the mean of "cbo_income" for all households within plus/minus 1000
# of the MEDIAN of "cbo_income". Flag these households as another group.

# Find and report the mean of "cbo_income" for all households within plus/minus 1000
# of the AVERAGE of "cbo_income". Flag these households as one group.


# Find the average of the changes for the four components and the total change for the first group (Median income)

# Find the average of the changes for the four components and the total change for the second group (Mean income)

