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

#plot different graphs 
library(ggpubr)

grp_stat <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat

plt1 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight, shape=Diet, color=Diet)) +
  geom_point(data = grp_stat, aes(x = Diet, y = mean_wt), 
             col = "black", fill = "red", shape = 23, size = 3) +
  geom_jitter(width = 0.05) + # geom_point() if jitter not required
  labs(y = "Chicken mass (g)") +
  scale_colour_manual(values = c("firebrick1", "deeppink", "darkviolet", "cyan")) +
  theme_pubr()
plt1

plt2 <- ggplot(data = grp_stat, aes(x = Diet, y = mean_wt, fill = Diet)) +
  geom_bar(position = position_dodge(), stat = "identity", 
           col = NA) +
  geom_errorbar(aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt),
                width = .2) +
  labs(y = "Chicken mass (g)") +
  scale_fill_manual(values = c("firebrick1", "deeppink", "coral1", "springgreen")) +
  theme_pubr()
plt2

# position_dodge() places bars side-by-side
# stat = "identity" prevents the default count from being plotted

# a description of the components of a boxplot is provided in the help file
# geom_boxplot()
plt3 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Chicken mass (g)") + 
  scale_fill_manual(values = c("cyan", "deeppink", "coral1", "springgreen"))
  theme_pubr()
plt3

plt4 <- chicks %>%
  filter(Time %in% c(10, 21)) %>% 
  ggplot(aes(x = Diet, y = weight, fill = as.factor(Time))) +
  geom_boxplot() +
  geom_jitter(shape = 21, width = 0.1) +
  labs(y = "Chicken mass (g)", fill = "Time") +
  theme_pubr()
plt4

library("RColorBrewer")
display.brewer.all()

plt5 <- plt4 + scale_fill_brewer(palette="PiYG")
plt5

ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2, labels = "AUTO")

ggarrange(plt4, plt5, ncol = 2, nrow = 1, labels = "AUTO")