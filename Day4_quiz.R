# Day 4
# Quiz
# Christelle Larey
# 21/04/2021


#  Question 1 -------------------------------------------------------------
#Load in the necessary packages and 
#explore three built in dataset (see below) by writing hypotheses and 
#selecting the correct statistical tests to prove or disprove the hypotheses.

library(tidyverse)
library(plotly)
library(ggpubr)

Orange <- datasets::Orange

g_orange <- Orange %>% 
  gather(age, circumference, key = "AC", value = "count")

#check if the data is normally distributed
plot1 <- ggplot(data = g_orange, aes(x = count, fill = AC)) +
  geom_histogram(position = "dodge", alpha = 0.8) +
  labs(x = "Numeric vector")
plot1

#According to the graph, the data is not normally distributed. 
#Have to do the shapiro test to find the p value.

#do the shapiro test
shapiro.test(g_orange$count)
#p-value = 1.413e-08
#p value is below 0.05 the data is then not normally distributed. 

#do the normality test
 norm <- g_orange %>%
   group_by(AC) %>% 
   summarise(norm_dat = as.numeric(shapiro.test(count)[2]))

# the dataset is not normal
 
#Homoscedasticity
 varorgange <- g_orange %>% 
   group_by(AC) %>% 
   summarise(sample_var = var(count))
 varorgange
 
#The variance samples shows it is more than 3 or 4 times bigger than the 
#smallest var sample. 

ToothGrowth <- datasets::ToothGrowth 

#check if the data is normally distributed
plot2 <- ggplot(data = ToothGrowth, aes(x = len, fill = supp)) +
  geom_histogram(position = "dodge",binwidth = 2, alpha = 0.8) +
  labs(x = "Type of supplement")
plot2

#the plot2 looks more normal than plot1. 

#do the shapiro test
shapiro.test(ToothGrowth$len)
#p-value = 0.1091
#p value is more than 0.05 the data is normally distributed according to this test.

# Test for normality
normtooth <- ToothGrowth %>%
  group_by(supp) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(len)[2]))
normtooth

#according to the data, the normality value of OJ is less than 0.05 while
#VC has a normality value more than 0.05

#Homoscedasticity
vartooth <- ToothGrowth %>% 
  group_by(supp) %>% 
  summarise(sample_var = var(len))
vartooth

#According to the results of the test above,
#the data is homoscedasticity.

# traditional output
t.test(len ~ supp, data = ToothGrowth, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(len ~ supp, data = ToothGrowth, method = "t.test", var.equal = TRUE, alternative = "greater")

#The p value in this t-test was 0.0302.
#There is a greater significance of the mean in group OJ than in group VC
#of the ToothGrowth data. 

warpbreaks <- datasets::warpbreaks
#check if the data is normally distributed
plot3 <- ggplot(data = warpbreaks, aes(x = breaks, fill = wool)) +
  geom_histogram(position = "dodge",binwidth = 2, alpha = 0.8) +
  labs(x = "Types of wool")
plot3

#According to plot3, the data is slightly skewed but not as much as in plot1.
#But I think the data will be normally distributed.

#Check Normality and Homoscedasticity
# Test for normality
normwarpb <- warpbreaks %>%
  group_by(wool) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(breaks)[2]))
normwarpb

#Both the normality value for wool A and B are below the p = 0.05.
#The data is not normal and disproves my previous statement that the 
#data will be normal.

#Homoscedasticity
varwarpb <- warpbreaks %>% 
  group_by(wool) %>% 
  summarise(sample_var = var(breaks))
varwarpb

#According to the results of the test above,
#the data is homoscedasticity because the variance of one sample is 
#three times greater than the other. 
#Still cant do t-test for this dataset as the assumptions were not met. 

# Question 2 --------------------------------------------------------------

library(tidyverse)
library(plyr)
library(ggplot2)

load("data/SACTN_daily_v4.2.RData")

SACTN_1 <- SACTN_daily_v4.2 %>% 
  separate(col = index, into = c("site", "src"), sep = "/") %>% 
  mutate(day = lubridate::day(date),
         month = lubridate::month(date),
         year = lubridate::year(date))

SACTN_2 <- SACTN_1 %>% 
  select(site, src, temp, month)

SACTNplot <- ggplot(data = SACTN_2, aes(x = month, y = temp)) +
  geom_line(aes(colour = site, group = paste0(site, src))) +
  labs(x = "", y = "Temperature (°C)", colour = "Site") +
  theme_bw()
SACTNplot



