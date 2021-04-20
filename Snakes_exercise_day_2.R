#Snakes exercise
#Example 7.2.3.1 in the workbook

library(tidyverse)

#Load the data
snakes <- read_csv("data/snakes.csv")
#Convert the day column to a factor. ANOVA work with factor independent
#variables
snakes$day = as.factor(snakes$day)

#Create some summaries of the snakes data
#Group the columns 
#get the mean of the openings columns 
#get the standard deviation of the openings columns
snakes.summary1 <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup() # closing ungroup()
snakes.summary

#Task: Something seems… off. What’s going on here? Please explain this outcome.
#R could not calculate the standard deviation for openings columns

#only group for the column, day.
snakes.summary2 <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary2

#got the summary for sd, se and ci.
library(Rmisc)
snakes.summary3 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

box_snakes <- ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, 
              y = openings - ci, yend = openings + ci, colour = day),
        size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F)

#scatter plot
ggplot(data = snakes, aes(x = day, y = openings, colour = snake)) +
  geom_point(aes(fill = day), alpha = 0.6, show.legend = F, size = 4) + 
  geom_jitter(width = 0.05)

#
ggplot(data = snakes, aes(x = day, y = openings, colour = snake)) +
  geom_point(aes(alpha = 0.6, show.legend = F, size = 10)) +
  geom_line(linetype = 2) 
  
#What are our null hypotheses?
#H0: There is no difference between snakes with respect to the number of openings at which they habituate.
#H0: There is no difference between days in terms of the number of openings at which the snakes habituate.

#Test the hypotheses by fitting the ANOVA model
snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)

par(mfrow = c(2, 2))
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

#made a boxplot
boxplot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")


