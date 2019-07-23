#######################################################
#######################################################
############    COPYRIGHT - DATA SOCIETY   ############
#######################################################
#######################################################

## INTRO TO R PROGRAMMING DAY4 EXERCISE ANSWERS ##

## NOTE: To run individual pieces of code, select the line of code and
##       press ctrl + enter for PCs or command + enter for Macs


#### Exercise 1 ####
# =================================================-

# Load the datasets we will be using for this exercise
# (this dataset is native to R).
# install.packages("babynames")
library(babynames)

#----------------------------------------------+
#### Question 1 ####

# Arrange baby names by year in descending order.
desc_years_babies = arrange(babynames, desc(year))
desc_years_babies

#----------------------------------------------+
#### Question 2 ####

# Arrange now by year, then by sex, both in ascending order. 
# What is the name and year in row 1?

asc_babies = arrange(babynames, year, sex)
asc_babies #Mary


#----------------------------------------------+
#### Question 3 ####

# Arrange now by sex, then by year in descending order, then by name in descending order. 
# What name is the name in first row?

desc_babies = arrange(babynames, sex, desc(year), desc(name))
desc_babies #zyva


#----------------------------------------------+
#### Question 4 ####

View(babynames)

# Select only year, name and count columns from `babynames`.
col_keep = c("year", "name", "n")

baby_data_keep = babynames[col_keep]
baby_data_keep


#----------------------------------------------+
#### Question 5 ####

# Now select the same columns by specifying which NOT to include in the subset.

leftover_baby_data = select(babynames, -(sex), -(prop))
leftover_baby_data

#----------------------------------------------+
#### Question 6 #### 

# Use the helper functions of `select` to find all columns that contain the letter `e`.

cols_with_e = select(babynames, contains("e"))
cols_with_e

#### Exercise 2 ####
# =================================================-

##### Question 1 ####
# Subset `babynames` to be all names from 2015, keep all columns, save the subset as babynames_small.

babynames_small = filter(babynames, year == 2015)
babynames_small

#----------------------------------------------+
#### Question 2 ####
# Using the newly created subset and `mutate`, create a new column "rank" that ranks (without gaps between ranks) by the name count. 
# Save the new subset as `babynames_mutate`.

babynames_mutate = mutate(babynames_small, ranks = dense_rank(n))
babynames_mutate



#----------------------------------------------+
##### Question 3 ####
# Arrange the mutated dataset by rank in descending order and then by name. 
# What is the rank, name and sex in the first row?
arrange(babynames_mutate, desc(ranks), name)
#2015 F Emma 20435 1386


#----------------------------------------------+
#### Question 4 ####
# Now instead of the `babynames_mutate` subset, use a function that will simply create one column that gives you
# the rank of the counts where there are no gaps between ranks. Name the column RANK. 
# Use the dataset `babynames_small`.
RANK = transmute(babynames_small, ranks = dense_rank(n))
RANK


#### Exercise 3 ####
# =================================================-

##### Question 1 ####
# Only using `summarise`, find the sum of `n` and name it "total_counts". 
summarise(babynames, total_counts = sum(n))

#----------------------------------------------+
#### Question 2 ####
# Use `group_by` and `summarise` to find the sum of female and male names by year. 
# Name the final dataframe 'summary' and the variable 'by_year'.

summary = summarise(group_by(babynames, year), by_year=sum(n))
summary

#----------------------------------------------+
#### Question 3 ####
# Perform the same summary function, this time subsetting the data to only 2010 and later. 
# Also, arrange the output by year, starting with 2010.
#	Use pipes this time. Name the new dataframe 'pipes'.
summary_sub = filter(summary, year >= 2010)
summary_sub

pipes =  babynames %>% group_by(year) %>% summarise (by_year = sum(n)) %>% filter(year>=2010)
pipes

#----------------------------------------------+
#### Question 4 ####
# Now, take the original `babynames` dataset and do the following (Hint: read through all the requirements before writing code!):
#	- Name this new dataset `new_summary`
#	- Subset the dataset to only contain names after 2010
#	- Keep all columns except for `prop`
# - Group dataset by sex and year
#	- Summarize the dataset by creating 2 columns:
#   - `counts`, which contains group summary for counts
#   - `distinct_names`, which contains unique names in groups
# - Ungroup the data
#	- Create a new column `new_prop` that calculates a new proportion, only including the total counts from the new subset
#   - New proportion column is calculated as the proportion of the counts towards the total sum of counts
#	- Sort by year in descending order

View(babynames)
new_summary = filter(babynames, year>=2010)
new_summary
new_summary = select(new_summary, -(prop))
new_summary
new_summary = group_by(new_summary, year, sex)
new_summary
new_summary = summarise(counts=sum(n), distinct_names=n_distinct(name))
new_summary
#mutate(new_summary, counts = sum(n), distinct_names = n_distinct(name))

#View(new_summary)

new_summary = ungroup(new_summary)
new_summary
new_summary = mutate(new_summary, new_prop = counts/sum(counts))
new_summary
new_summary = arrange(new_summary, desc(year))
new_summary

View(new_summary)



#### Exercise 4 ####
# =================================================-

# Load the datasets we will be using for this exercise:
# (this dataset is native to R)
data(USPersonalExpenditure)
USPersonalExpenditure


#----------------------------------------------+
##### Question 1 ####
# What is the class of the data we just loaded?
class(USPersonalExpenditure)

#matrix

#----------------------------------------------+
#### Question 2 ####
# Change this to a tibble, so we can begin to turn it into tidy data. 
USPE_tib = tibble(USPersonalExpenditure)
USPE_tib

# Make sure to include the current `row.names` as a column. 

USPE_tib = mutate(USPE_tib, areas_of_expense=c("Food and Tobacco","Household Operation","Medical and Health","Personal Care","Private Education"))
USPE_tib

# Name that column `areas_of_expense`.


# Name the remaining columns as they are named in the current matrix.


#----------------------------------------------+
#### Question 3 ####
# Use `gather` to convert this data to `tidy` format. 
# There should be two new columns, you can decide on names based on what these columns will contain. 
# Save the gathered tibble as `gathered`.

gathered = USPE_df %>% gather('1940','1945','1950','1955','1960', key="year", value="value")
gathered

#----------------------------------------------+
#### Question 4 ####
# Use `spread` to convert the tibble back to its original state.

gathered2 = spread(gathered, key = "year", value="value")
gathered2

#----------------------------------------------+
#### Question 5 ####
# For the next two questions, we are going to create a dataframe for the purpose of using `separate` and `unite` functions.
set.seed(2)
date = as.Date('2016-01-01') + 0:14
hour = sample(1:24, 15)
min = sample(1:60, 15)
second = sample(1:60, 15)
event = sample(letters, 15)
data = tibble(date, hour, min, second, event)
data


# Unite the date, hour, minute, and second columns making the new column look like a date-time-stamp format. 
# Name this new dataset - `data_unite`.
data_unite = unite(data, timestamp, date, hour, min, second, sep = "-")
data_unite

#----------------------------------------------+
#### Question 6 ####
# Now separate the `data_unite` into these columns: year, month, day, time, event.
data_sep = separate(data_unite, timestamp, into=c("year","month","day","hour","min","sec"), sep="-")
data_sep
data_sep = unite(data_sep, time, hour, min, sec, sep="-")
data_sep
