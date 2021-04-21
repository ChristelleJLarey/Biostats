# Day 1
# Assignment 1
# Christelle Larey
# Due Date: 21 March 2021 at 5am

library(dplyr)
library(tidyr)

# Section 1 ---------------------------------------------------------------
View(BOD)

#The answer is C. 

# Section 2 ---------------------------------------------------------------

library(dplyr)
library(dslabs)
library(tidyverse)

data(murders)

glimpse(murders)

head(murders)

tail(murders)

summary(murders)

colnames(murders)

unique(murders)

#Paragraph describing the murder dataset
# The dataset has 51 rows with 5 columns.
# The data is organized according to states in different regions with their
# population and gun murder totals for 2010.
# The mininum number of the data is 2 and the maxinum is 1257.
# the column stats is in the class, character. 

#use the select function to get the states and population size
states_pop <- murders %>% 
  select(state, population)

#Removing Florida from the dataset
states_pop2 <- subset(states_pop, state!= "Florida")
  
#Create a new data frame called no_south that removes states 
#from the South region. 
#How many states are in this category? 
no_south <- murders %>% 
  filter(region %in% c("West", "Northeast", "North Central"))
no_south

#Calculate the population size of the South and West regionally
#population size together
Popsize_SW <- murders %>% 
  select(region, population) %>% 
  filter(region %in% c("South", "West")) %>% 
  summarise(length = n())
Popsize_SW

#pop size of South
Popsize_S <- murders %>% 
  select(region, population) %>% 
  filter(region %in% c("South")) %>% 
  summarise(length = n())
Popsize_S

#pop size of West
Popsize_W <- murders %>% 
  select(region, population) %>% 
  filter(region %in% c("West")) %>% 
  summarise(length = n())
Popsize_W

#Create a new data frame with only the population size of 
#the Northeast region
Northeast_popsize <- murders %>% 
    select(region, population) %>% 
    filter(region %in% c("Northeast"))
    
#Create two plots of your choice and explain visible trends
#line plot
ggplot(murders, aes(x = total, colour = region)) +
      geom_bar(position = "fill", size = 2) 

#violin plot
ggplot(murders, aes(x = total, y = region, colour = region)) +
  geom_violin(size = 4) +
  coord_flip() +
  ggtitle("Total density probability of gun murders by US regions") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Trends in the two plots
# In the violin plot, it shows the kernel probability density of the different regions of gun murders.
# You can see in both plots, that the South and West region shows more
# frequently in the line graph and the density probability is bigger in the violin graph.
# This can be due to more people owning guns in those states. I could have compare the populations
# in the regions compared to the total amounts of gun murders. As I can see from the data and graphs
# there are more murders in regions with bigger populations. 


#Compare with population size of the South with 
#the population size of the West
#South has a population size of 17 while West had a population of 13.
#Meaning that the South region has more people than the West region.

#Create a new data frame where the total>20 but <100 
#and to exclude the value 120

#Create an object, containing from 10th to 24th row and 26th row. 
#Hint: consider using the slice() function.

#Use as_tibble to convert the murders data table into a tibble 
#and save it in an object called murders_tibble.
murders_tibble <- murders %>% 
       as_tibble()

#Use the group_by function to convert murders into a tibble that is grouped by region.
 m_tibble <- murders_tibble %>% 
           (group_by(region, totals))

# Section 3 ---------------------------------------------------------------
#The summarize function in dplyr provides a way to compute summary statistics with intuitive and readable code. 
# We start with a simple example based on heights. 
#The heights dataset includes heights and sex reported by students in an in class survey.

library(dplyr)
library(dslabs)

data(heights)

#Write a paragraph describing the heights dataset
# This dataset contains two variables, sex and height.Sex falls under the data class, 
# Qualitative data, specifically, categorical data.Height falls under the data class, 
# discrete data, basically whole numbers.The data set describes the height difference 
# between genders.

#Explore the datasets using the various exploring functions. 
#Such as glimpse(), head(), tail () and many others
glimpse(heights) #gives more details about the heights dataset
head(heights) #gives the first six lines of data
tail(heights) #gives the last six lines of data
names(heights) #gives the names of the columns

#Determine the average and standard deviation for males and females. 
average_std_h <- heights %>% 
     summarise(mean_h = mean(height),
               std_h = sd(height))
average_std_h
  
#Then calculate the median, minimum and maximum values.
  summary_h <- heights %>% 
    summarise(min_h = min(height),
              med_h = median(height),
              max_h = max(height))
   summary_h

# Section 4 ---------------------------------------------------------------

x <- c( -1, 6, 21, 19 , NA, 73, NA)
y <- c(NA, NA, 3, NA, 13, 24, NA)

#a) Count the number of elements are missing in both x and y
sum(is.na(x))

sum(is.na(y))

#b) Transform the code, used above (a), into a function
count_x <- sum(is.na(x))

count_y <- sum(is.na(y))

#c) Create three new vectors and test the function created in (b)
new_x <- count_x %>% 
        mean(xx = mean(new_x))

# Section 5 ---------------------------------------------------------------
library(tidyverse)
library(plotly)
library(ggpubr)
library(ggplot2)

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))

#gather the seasons into one column
s_data <- Seasonal_data %>% 
    select(-year) %>% 
    gather(key = "seasons",
          value = "temperature")
s_data

# Design hypothesis
#H0: Rainfall in Summer is not greater than in Winter.
#H1: Rainfall in Summer is greater than in Winter.

#Plots
#box_plot
ggplot(data = s_data, aes(x = seasons, y = temperature, fill = seasons)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "temperature", x = "") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#histogram 
ggplot(data = s_data, aes(x = temperature, fill = seasons)) +
  geom_bar(position = "dodge", alpha = 0.8) 
 
# write paragraph discussing the findings
# It is clear that from the graphs that the highest amount of 
# rainfall is present during the Summer months.
# Therefore, my hypothesis will be rejected as the data shows
# it is wrong. The alternative hypothesis will be accepted. 

cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data

catsplit <- cats_data %>% 
  separate(position, 
           into = c("first_place", "second_place", "third_place")) %>% 
  unite(total_time, minutes, seconds)

# Section 6 ---------------------------------------------------------------
library(dplyr)
library(tidyr)
data(PlantGrowth)

PG_data <- gather(PlantGrowth, 
                  key = "type",
                  value = "weight value")
PG_data
# I placed the groups column first then the weight column with their values. 

Spread_plant <- PlantGrowth %>% 
                spread(key = "group", value = "weight")
Spread_plant
#Move the data from less columns to more columns.

split_p <- PlantGrowth %>% 
      separate(col = weight, into = c("ctrl"), sep = "/")
      subset(select = -c(group))
split_p
#separated the weight into only ctrl weight

join_data <- left_join(arrange_p, split_p, by = "group")
# I joined two dataframes together by the column group. 

arrange_p <- PlantGrowth %>% 
    arrange(weight)
arrange_p
#arranged the values of the weight column from smallest to biggest value.

select_plant <- PlantGrowth %>% 
               select(weight)
select_plant
# I only selected the weight column.

group_p <- PlantGrowth %>% 
           group_by(group %in% c("ctrl"))
group_p
# I grouped the variable ctrl in the group column.

mean_plant <- PlantGrowth %>% 
         mutate(mean_p = weight / mean(weight, na.rm = TRUE))
mean_plant
# I made a new column with the mean plant weight.
