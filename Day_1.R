# Day 1
# Basic Stats_Chapter 2 types of data 
# CJ Larey
# 19 April 2021

#try different graphs and function

#load data packages
pines <- Loblolly
str(pines)

#calculate sample size
library(tidyverse)
chicks <- as_tibble(ChickWeight)

# note the distinction between 'nrow()' and the 
# true sample size
nrow(chicks)
unique(chicks$Chick)

# how many weights are available across all Diets and Times?
chicks %>% 
  summarise(length = n())

#calculate the mean final mass at 20days
library(tidyverse)
library(e1071)

chicks %>% 
  group_by(Diet) %>% 
  filter(Time == 20) %>% 
  summarise(mean_wt = mean(weight) / n(), 
            sd_wt = sd(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())

#another example for summarise
# summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
 #           med_wt = median(weight, na.rm = TRUE),
  #          sd_wt = round(sd(weight, na.rm = TRUE), 2),
   #         sum_wt = sum(weight),
    #        min_wt = min(weight),
     #       qrt1_wt = quantile(weight, p = 0.25),
      #      med_wt = median(weight),
       #     qrt3_wt = median(weight, p = 0.75),
        #    max_wt = max(weight),
          #  n_wt = n())

#range
range(chicks$weight)[2] - range(chicks$weight)[1]


#standand deviation
chicks %>% 
summarise(sd_wt = sd(weight))

#median
chicks %>% 
  summarise(med_wt = median(weight))

#kurtosis
kurtosis(chicks$weight)

chicks %>% 
  summarise(mean_wt = sum(weight) / n())

# var and std
chicks %>% 
  summarise(sd_wt = sd(weight))

#Quantiles 
quantile(chicks$weight)

#missing values
dat1 <- c(NA, 12, 76, 34, 23)

# Without telling R to omit missing data
mean(dat1)

# Omitting the missing data
mean(dat1, na.rm = TRUE)


