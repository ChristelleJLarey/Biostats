# Day 2
# Chapter 6 & 7
# CJ Larey
# 20/04/2021

library(tidyverse)
library(plotly)
library(ggpubr)
library(ggplot2)

#Normality
# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  scale_fill_manual(values = c("lightseagreen", "plum")) +
  labs(x = "value")
h

#need to test the data
# p-value is above 0.05 we may assume the data to be normally distributed
# this test took the whole data set

shapiro.test(r_dat$dat)

#run the test by taking the samples separately 
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

#Homoscedasticity
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))
#Above we see that the variance of our two samples are 
#homoscedastic because the variance of one is not more than 
#two to four times greater than the other.

#do t-test after our assumptions are done
#if the data is not normal then transform data

#do normality and shapiro tests in one 
#helpful to have a function that checks for both simultaneously
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])

#One-sample t-tests
# t-test less than 0.05 not significance 
# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat)

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 20)

# compare random data against a population mean of 30
t.test(r_one$dat, mu = 30)

#plot in a graph
ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()

# Two-sample t-tests (most common)
# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#ecklonia example (A t-test workflow)
ecklonia <- read_csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

#scatterplot
ggplot(data = ecklonia, aes(x = variable, y = value, colour = site)) +
  geom_jitter() +
  coord_flip()


# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
box_e <- ecklonia_sub %>% 
  ggplot(aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
box_e 

#scatterplot
plot_sc <- ecklonia_sub %>% 
  ggplot(aes(x = variable, y = value, colour = site)) +
  geom_point(size = 10, alpha = 0.4) +
  coord_flip() +
  labs(main = "", y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
plot_sc

plot_sc + scale_colour_manual(values = c("firebrick1", "deeppink"))

plot_sc + scale_color_brewer(palette="Dark2")

#putting the plots together
ggarrange(box_e, plot_sc,  ncol = 1, nrow = 2, labels = "AUTO")

#Choosing tests

#Checking assumptions
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])

#Running analysis (t-test)
# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
# p=0.03657

# dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

#Chapter 7 ANOVA (look at journals)
# the t-test
# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
# filter out diet 1 & 2 at time 21
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")
# p=0.218 > 0.05 

#ANOVA
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

#Tukey HSD test
TukeyHSD(chicks.aov1)

#put everything together
chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 2, 10, 21)))
summary(chicks.aov2)

#Alternatives to ANOVA
# First activate libraries
library(tidyverse)
library(ggpubr)

# Then check for failing assumptions
chicks %>% 
  filter(Time == 0) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]),
            var_wt = var(weight))

# Wilcox rank sum test
compare_means(weight ~ Diet, data = filter(chicks, Time == 0, Diet %in% c(1, 2)), method = "wilcox.test")

#Kruskall-Wallis rank sum test
compare_means(weight ~ Diet, data = filter(chicks, Time == 0), method = "kruskal.test")

 

