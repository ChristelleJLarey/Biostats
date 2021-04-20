# Day 2
# Quiz
# Christelle Larey
# 20/04/2021

# Question 1
# various data classes with examples for each

#Numeric class - numeric numbers, 1,2,3
#ordinal class - 1st, 2nd, 3rd, etc. 

#list some of the functions used in R
#summarise
#select
#mutate 
#filter

#Discuss skewness and Kurtosis
# Skewness is a measurement of central decency. We can see how skew the graph is
# when we plot it. If a plot had a perfect bulb shape than there was no skewness 
# in the graph. But if the plot had more data to the left than the graph will be
# skewed to the left meaning the mean was more to the left than to the middle. 

# Question 2
Orange <- datasets::Orange

View(Orange)
?Orange

#type of data class - This data belongs in the Normical class.
# The data has a ordered factor (tree) and numeric vector (age and circumference)

# Apply the correct functions to show the 
# first 6 and last 6 rows, column names and summary statistics of the data
head(Orange)
tail(Orange)
colnames(Orange)
summary(Orange)

# Determine the Mean, Median and Standard deviation of 
# the age and circumference of the oranges for each of the trees
mean(Orange$age)
mean(Orange$circumference)

median(Orange$age)
median(Orange$circumference)

sd(Orange$age)
sd(Orange$circumference)

#The skewness and Kurtosis
library(e1071)
skewness(Orange$age, na.rm = FALSE, type = 3)

kurtosis(Orange$age, na.rm = FALSE, type = 3)

skewness(Orange$circumference, na.rm = FALSE, type = 3)

kurtosis(Orange$circumference, na.rm = FALSE, type = 3)


#Using the summarise function, 
#determine the minimum, maximum, 
#first and third quantiles for the circumference of the oranges
library(tidyverse)

    sum_orange <- Orange %>% 
        group_by(age, circumference) %>%
        summarise(min-o = min(sum_orange), q1 = quantile(sum_orange, p = 0.25), q3 = quantile(sum_orange, p = 0.75), maxorange = max(sum_orange))
    sum_orange

#Create two plots (with labels) of your choice and explain visible trends
ggplot(sum_orange, aes(x =, y=, fill=))
    
# Question 3
# mutate() 
# add new columns to the dataset. 

#select()
#you can select different or just one variable out of the dataset to work with.

#group_by()
# group together different columns

#filter()
#if you want for example only the diet and time of a dataset. You can filter
#diet and time out of the data. 

#separate()
#separate data from each other. 
#For example, if you have year and month in the same column, you can put
#them in different columns.  


