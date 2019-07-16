#######################################################
#######################################################
############    COPYRIGHT - DATA SOCIETY   ############
#######################################################
#######################################################

## INTRO TO R PROGRAMMING DAY2 EXERCISE ##

## NOTE: To run individual pieces of code, select the line of code and
##       press ctrl + enter for PCs or command + enter for Macs


#### Exercise 1 ####
# =================================================-

#### Question 1 ####

# Check your working directory to confirm it is the same for exercises as you have used during class.
# If your working directory is not the data directory used in class, please set it now.

getwd()

#----------------------------------------------+-
#### Question 2 ####

# Use `read.csv` to read `fast_food_data.csv` into R and name it `fastfood`. 
# Set headers to TRUE and do not read in strings as factors.
# What is the class of `fastfood`?

fast_food_data = read.csv("fast_food_data.csv", header = T, stringsAsFactors = F)
str(fast_food_data)
class(fast_food_data) # data fram dawg

#----------------------------------------------+-
#### Question 3 ####

# Inspect the structure, attributes, and dimensions of `fastfood`.
# How many rows and columns does fastfood have?

str(fast_food_data)
attributes(fast_food_data)
dim(fast_food_data)

#----------------------------------------------+-
#### Question 4 #### 

# What's the difference in protein content between the 12th and 56th observation?
# What's the type of item 12 and item 56? 
# What is the specific item of each?

pro1 = fast_food_data[12,12]
pro2 = fast_food_data[56,12]

diffpro = pro2 - pro1
diffpro  #56

typeof(pro1)
typeof(pro2) # double doubles yo

item1 = fast_food_data[12,]
item1 # BK Hamburger Burger 
item2 = fast_food_data[56,]
item2 # Sonic SuperSonic Bacon Dbl Chsbrgr w mayo

 #----------------------------------------------+-
#### Question 5 ####

# Create a subset of `fastfood` named `fastfood_sub` that contains rows 12:56 and the columns 
# "Fast.Food.Restaurant", "Item", "Type", "Protein..g.", and "Total.Fat..g.".
# Hint: Create a vector of column names you wish to keep.
# Use `head` and `tail` to return the top and last 5 observation of `fastfood_sub`.

colneeded = c("Fast.Food.Restaurant", "Item", "Type", "Protein..g.","Total.Fat..g.")
fastfood_sub = fast_food_data[12:56,colneeded]
write.csv(fastfood_sub, "fastfood_sub.csv")

View(fastfood_sub)

head(fastfood_sub)
tail(fastfood_sub)

#----------------------------------------------+-
#### Question 6 ####

# Save `fastfood_sub` to your data directory as "fast_food_subset.csv".

##reference the write.csv command in Q5


#### Exercise 2 ####
# =================================================-

#### Question 1 ####

# Create a vector of column indices named "column_keep" with columns 24:26 and 52:55.
# Subset CMP using `column_keep` to retain the specified columns in a smaller dataset named `sub_cmp`.
# Confirm the structure of `sub_cmp`. 
# How many columns are in `sub_cmp`? How many columns are integers?

column_keep = c(24:26, 52:55)
sub_cmp = CMP[,column_keep]
str(sub_cmp)
dim(sub_cmp)

#columns = 7 , int =  1 (MP12)
#----------------------------------------------+
#### Question 2 ####

# Summarize the variables in `sub_cmp`.
summary(sub_cmp)

# Which variable has the most NAs?
#11 broski

#----------------------------------------------+
#### Question 3 ####

# Create a list of logical values where NAs in ManufacturingProcess11 are TRUE.
is_na11 = is.na(sub_cmp$ManufacturingProcess11) 
is_na11

# Save this list of logical values to a variable names `also_na`.
also_na = which(is_na11)
also_na 

# Use `which` to find the row indexes of the NA values and save that list of row indexes to `idofnas`.


#----------------------------------------------+
#### Question 4 ####

# Compute the mean of the `ManufacturingProcess11` with the NAs removed.

mean_process11 = mean(sub_cmp$ManufacturingProcess11, na.rm = T) 
mean_process11


#----------------------------------------------+
#### Question 5 ####

# Assign the mean to the entry(s) with the `NA` in ManufacturingProcess11.
# Check the result.

is_na11 = is.na(sub_cmp$ManufacturingProcess11) 

sub_cmp$ManufacturingProcess11[is_na11] = mean_process11

sub_cmp$ManufacturingProcess11

#### Exercise 3 ####
# =================================================-

#### Question 1 ####



# Write an `ifelse` statement that states:

# If entries in `temp_var` are greater than 15,
#   then subtract 15 from `temp_var`.
# Otherwise, return temp_var unchanged.


temp_var = c(24, 26, 4, 12, 52, 3, 16)
length(temp_var)

#for(i in 1:length(temp_var)){
  
#  if(temp_var[i] >= 15){  ##will ring true if equals true, triggered by previous command which results in logical value (true or false)
#     temp_var = temp_var - 15
#  }
#  temp_var
#}

ifelse(temp_var > 15,
  temp_var = temp_var - 15, 
  temp_var)
  




#----------------------------------------------+
#### Question 2 ####

vec1 = c(24, 26, 4, 12, 52, 3, 16)

# Let's get the length of `vec1` and store it a variable `vec1_len`.
vec1_len = length(vec1)
vec_sum = 0

# Write the first part of the for loop for `i` from 1 to `vec1_len`, 
for(i in 1:vec1_len){
  vec1[i] = vec1[i] + 5
#  vec_sum = vec_sum + vec1[i]
}
mean(vec1)

#vec_mean = vec_sum/vec1_len
#vec_mean
#
# with closed curly braces at the end. 
# Create this for loop to go through each individual element in vec1 and add 5.
# What is the mean of vec1?


#----------------------------------------------+
#### Question 3 ####

# Create another for loop that goes through each element in vec1. If it is above the 
# mean, subtract 5. If it is not above 5, then add 10.
# What is the new mean of vec1?

vec_sum2 = 0
vec_mean = 24.57143

for(i in 1:vec1_len){
  if(vec1[i] > vec_mean){
    vec1[i] = vec1[i] - 5
    vec_sum2 = vec_sum2 + vec1[i]
  
    }else{
    vec1[i] = vec1[i] + 10
    vec_sum2 = vec_sum2 + vec1[i]}
}

vec_mean2 = vec_sum2/length(vec1)
vec_mean2

## SECOND METHOD-----

old_mean = mean(vec1)
old_mean

for(i in 1:vec1_len){
  vec1[i] = ifelse(vec1[i] > old_mean, vec1[i] - 5, vec1[i] +10)
  
}
vec1
mean(vec1)

#----------------------------------------------+
#### Question 4 ####

namelist = c("Susie", "Nick", "Kate", "Simon", "Jamal", "Eduardo")

# Create a function named `Greetings` that takes a `listofnames` as an argument.
# The function should go through a vector of names one by one, and print:


Greetings = function(listofnames){
  for(i in 1:length(listofnames)){
    GreetingExp = paste0("Welcome to coding in R, ", listofnames[i], "!")
    print(GreetingExp)
  }
  
}

Greetings(namelist)

# "Welcome to coding in R, NAME!" 

# Where NAME is the entry in `listofnames` vector.
# Hint: You will need to use `length`, `paste0`, `print`, and a `for` loop.


#----------------------------------------------+
#### Question 5 ####

# Execute `Greetings` using `namelist`.
# DONE BROSKI


