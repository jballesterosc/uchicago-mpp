###########################
### HW 1 Template       ###
### PPHA 31002: Stats I ###
### Due 13 October 2025 ###
###########################

library(tidyverse)
library(ggthemes)
library(lubridate)
library(skimr)

### set the working directory
getwd()
setwd("/Users/jayballesteros/_github/uchicago-mpp/stats1/hw1")

### read in the chi_victims.csv data set
victims <- read.csv("chi_victims.csv")

### create year variable
victims$year <- substr(victims$date,7,10)
victims$year <- as.numeric(victims$year)

### create homicide indicator

victims <- victims %>%
  mutate(homicide = victimization_primary == "HOMICIDE") %>%  # TRUE/FALSE
  mutate(homicide = as.integer(homicide))                     # 1/0

### Q8: How many total homicide victims in 2024? 
# how many total homicide victims in 2024? 
### ANSWER: 583 homicides in 2024

victims %>%
  filter(year == 2024) %>%
  summarise(total_homicides = sum(homicide, na.rm = TRUE))


### Let's create a new dataframe by "collapsing" the data to the year level. 
### Specifically, we want a dataframe in which each row is a year and a column 
### indicates the count of homicide victims in each year.
# use tidyverse syntax to collapse data to year-level
# drop 2025, as we don't have data for the full year

victims_year <- victims %>%
  group_by(year) %>%
  summarise(total_homicides = sum(homicide, na.rm = TRUE))

victims_year <- victims_year %>%
  filter(year != 2025)

### To examine over-time trends, it's appropriate to adjust for changes in
### population, so we need to merge population data into our `yr.homicide` 
### dataframe. Then, we will create a new variable equal to the number of 
### homicide victims per 100,000 in the population.
## read chi_population.csv into R
population <- read.csv("chi_population.csv")

### merge pop into our dataframe
victims_year <- merge(victims_year, population, by="year")

## create new variable: homicide victims per 100,000 in the population
victims_year <- victims_year %>%
  mutate(
    homicides = as.numeric(total_homicides),
    population = as.numeric(population),
    rate_100k = (total_homicides / population) * 100000
  )

### Q9: In what year was the homicide rate the highest? Write a single line of
### code to determine this answer.
### ANSWER: 1992 with a rate of 33.98 homicides per 100,00 of the population

victims_year %>% filter(rate_100k == max(rate_100k))

### Q10: In what year was the homicide rate the lowest? Write a single line of
### code to determine this answer.
### ANSWER: 2014 with a rate of 15.43 homicides per 100,000 of the population

victims_year %>% filter(rate_100k == min(rate_100k))

### Q11: Plot the trend over time in homicides per 100,000 in the population 
### from 1991 to 2024.

ggplot(victims_year, aes(year, rate_100k)) +
  geom_col() +
  scale_x_continuous(breaks = seq(min(victims_year$year), max(victims_year$year), by = 3)) +
  labs(title = "Rate of homicide victims per 100,000 in the population per year",
       x = "Year", y = "Rate per 100,000") + theme_minimal() +
  ggsave("Q11_homiciderate_1991_2025.png")

### Let's incorporate 2025 data into our trend, so we can examine how violence 
### in Chicago in 2025 (thus far) compares to recent years. Because there is 
### likely seasonal variation in these data and because we only have data for 
### 2025 through the end of August, we are going to examine trends in homicide
### rates for January through August for 1991-2025. We will return to our 
### original `victims` dataframe, and subset the dataframe to only include the
### months January through August.
# only keep months January through August (i.e., month<=8 )

victims_8 <- victims %>%
  filter(month <= 8)

# use tidyverse syntax to collapse data to year

victims_8 <- victims_8 %>%
  group_by(year) %>%
  summarise(homicides = sum(homicide, na.rm = TRUE))

# merge pop into our dataframe

victims_8 <- merge(victims_8, population, by="year")

# create new variable: homicide victims per 100,000 in the population
# yr.homicide.aug$homicide.per100k <- yr.homicide.aug$tot.homicide / (yr.homicide.aug$population / 100000)

victims_8 <- victims_8 %>%
  mutate(
    homicides = as.numeric(homicides),
    population = as.numeric(population),
    rate_100k = (homicides / population) * 100000
  )

### Q12: Plot the trend over time in homicides per 100,000 from January to August for the period 1991 to 2025.
### Briefly compare the homicide rate in 2025 thus far to the rate in recent decades in Chicago in 1-2 sentences..
### ANSWER: The homicide rate (per 100,000) in 2025 from January to August is lowest since 2014. And the second lowest in the entire dataframe, signaling a descalation of this type of crime since the highest point in 2021.
ggplot(victims_8, aes(year, rate_100k)) +
  geom_col() +
  scale_x_continuous(breaks = seq(min(victims_year$year), max(victims_year$year), by = 3)) +
  labs(title = "Rate of homicide victims per 100,000 in the population per year (Only Jan to Aug)",
       x = "Year", y = "Rate per 100,000") + 
  theme_minimal() +
  ggsave("Q12_homicides_jan_aug_1991_2025.png")

### Let's now focus our attention on gun violence in particular. To do so, let's
### subset our original `victims` dataframe to only include shooting victims.
## drop non-shooting victims

victims_shootings <- victims %>%
  filter(gunshot_injury_i == "YES")

victims_shootings <- victims_shootings %>%
  mutate(gunshot_injury_i = as.integer(gunshot_injury_i == "YES"))

### Q13: How many total victims of shootings (both fatal and non-fatal) were 
### there in 2024? How many victims of fatal shootings were there in 2024?

### ANSWER: 2854 shooting victims in 2024 and 515 fatal shootings the same year.

victims_shootings %>%
  filter(year == 2024) %>%
  summarise(gunshot_injuries = sum(gunshot_injury_i, na.rm = TRUE))

victims_shootings %>%
  filter(year == 2024) %>%
  summarise(total_homicides = sum(homicide, na.rm = TRUE))

### Q14: Plot a histogram that displays the distribution of fatal shootings by 
### time of day (i.e., hour and minute in the day). The time of day variable 
### should range from $0$ (i.e., 12:00am) to $23\frac{59}{60}=23.983$ 
### (i.e., 11:59pm). Use the `freq=F` option to display a density on the 
### vertical axis. As always, be sure to label all axes (the default label of 
### "density" is fine on the vertical axis) and include a title for the plot. 
### Around what time of day are fatal shootings *least* frequent?

# ANSWER: Fatal shootings are least frequent during the early morning hours, mostly between 6 to 9 am. Nonetheless, the lowest peak of the day it's specifically between 7:00 to 7:30 am.

fatal_shootings <- victims_shootings %>%
  filter(homicide == 1)

fatal_shootings <- fatal_shootings %>%
  mutate(
    # parse your timestamp once
    date = mdy_hms(date),
    # numeric hour-of-day: 0–24 (e.g., 13.5 = 1:30 pm)
    time_of_day = hour(date) + minute(date)/60 + second(date)/3600
  )

ggplot(fatal_shootings, aes(time_of_day)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.5,  # 30-min bins; try 0.25 for 15-min
                 boundary = 0, closed = "left") +
  scale_x_continuous(limits = c(0, 24), breaks = 0:24) +
  labs(title = "Fatal shootings by time of day",
       x = "Hour of day (0–24)") +
  theme_minimal() +
  ggsave("Q14_fatal_shootings_timeofday.png")

### Now, let's examine the trend over time in gun shot victims (both fatal and 
### non-fatal) per 100,000 in the population through 2025. Recall that we only 
### have data on non-fatal shooting victims starting in 2010, so we will subset 
### our data to start in 2010. Again, because we would like to include 2025 in 
### our analysis, we will need to subset the data for each year to only include 
### shootings from January through August.Thus, to prepare our data for analysis, 
### we must do the following:
###

victims_shootings <- victims %>%
  filter(gunshot_injury_i == "YES")

###    a. Subset the data to only include shootings from January through August
victims_shootings <- victims %>%
  filter(month <= 8)
###    b. Subset the data from 2010 onward
victims_shootings <- victims_shootings %>%
  filter(year >= 2010)

#   filter(year >= 2010 & year <= 2025)



###    c. "Collapse" the data on shootings to the year level

yr_victims_shootings <- victims_shootings %>%
  group_by(year) %>%
  summarise(total_victims_shootings = n())

###    d. Merge in the yearly population data
yr_victims_shootings <- merge(yr_victims_shootings, population, by="year")
###    e. Create a new variable indicating the number of shooting victims per 
###       100,000 in the population for each year.

yr_victims_shootings <- yr_victims_shootings %>%
  mutate(
    total_victims_shootings = as.numeric(total_victims_shootings),
    population = as.numeric(population),
    rate_100k = (total_victims_shootings / population) * 100000
  )

### Q15: Create a plot in which you graph the over-time trend in shooting 
### victims per 100,000 in the population for the period from 2010-2025 (for 
### January through August). In other words, year should be plotted on the 
### horizontal axis, and shooting victims per 100,000 should be plotted on the 
### vertical axis. As always, make sure that the axes are labeled and that your
### plot has an appropriate title. How does 2025 compare to the pre-pandemic 
### period for fatal shootings based on your plot?


ggplot(yr_victims_shootings, aes(year, rate_100k)) +
  geom_col() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(breaks = seq(min(yr_victims_shootings$year), max(yr_victims_shootings$year), by = 1)) +
  labs(title = "Rate of shooting victims (both fatal and non-fatal) per 100,000 (Only Jan to Aug)",
       x = "Year", y = "Rate per 100,000") + 
  theme_minimal() #+ 
  #ggsave("Q15_shootingvictims_jan_aug_2010_2025.png")


