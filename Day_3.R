# Day 3
# Chapter 8 & 9
# Christelle Larey
# 21 April 2021


# Chapter 8 - Simple linear regressions -----------------------------------

library(tidyverse)
library(ggplot2)

data("faithful")
head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

#how to interpret linear model (predicts trends)
#use journals


slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

#graph
#point graph with a regression line
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")


# Chapter 9 - Correlations ------------------------------------------------

#assumptions:
#pair-wise data
#absence of outliers
#linearity
#normality of distribution
#homoscedasticity
#level (type) of measurement
#Continuous data (Pearson correlation)
#Ordinal data (Spearman correlation)

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("data/ecklonia.csv")

#got rid of categorical variables
#dataframe where each column represents pair-wise 
#continuous/ordinal measurements
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

# Pearson correlation 
# continuous data
# look at cor value
# closer to 1, stronger the correlation

# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the default settings.
# They are only shown here to illustrate that they exist.
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")

# look at the correlation for the whole dataset
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

#Spearman rank correlation
#ordinal data

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "spearman")

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

# Kendall rank correlation
# both continuous and ordinal data

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

#change correlation by method
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# one panel visual
# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# Multiple panel visual 
corrplot(ecklonia_pearson, method = "circle")

#basic heat map without explanation of colour correlation 
heatmap(ecklonia_pearson, Colv = NA, Rowv = NA, scale="column")

# Add classic arguments like main title and axis title
heatmap(ecklonia_pearson, Colv = NA, Rowv = NA, scale="column", 
        xlab="", ylab="", main="Ecklonia_Pearson")

#pretty heat map with correlation bar
library("pheatmap")
pheatmap(ecklonia_pearson, cutree_rows = 9,
         xlab = "", ylab = "", main = "Ecklonia_pearson")

#figure title at the bottoms
#tables = at top


