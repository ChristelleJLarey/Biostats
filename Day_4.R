# Day 4
# Confidence intervals
# Christelle Larey
# 22/04/2021

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

str(data)

library(rcompanion)
library(tidyverse)
library(ggplot2)
# ungrouped data is indicated with a 1 on the right side of the formula, 
# or the group = NULL argument.

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3, traditional = FALSE) 

Steps1 <- groupwiseMean(Steps ~ Sex,data = data, conf = 0.95, digits = 3) 

#plot how the steps differ between males and females
library(tidyverse)
plot1 <- ggplot(Steps1) +
         geom_col(aes(x = Sex, y = Mean), fill = "white", col = "black") +
         geom_errorbar(aes(ymin = Trad.lower,
                           ymax = Trad.upper,
                           x = Sex),
                       col = "black",
                       width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")
plot1

plot1.2 <- ggplot(Steps1) +
  geom_bar(aes(x = Sex, y = Mean, colour = Sex), fill = "white", stat = "identity") +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")
plot1.2

plot1.2 + scale_color_brewer(palette="Dark2")

# two-way data
Steps2 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

plot2 <- ggplot(data = Steps2) +
  geom_col(aes(x = Sex, y = Mean, fill = Sex)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
  facet_wrap(~Teacher, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps") +
  scale_fill_manual(values = c("firebrick1", "deeppink")) +
  ggtitle("Mean steps from student per teacher")
plot2

# Reds
plot2 + scale_fill_brewer(palette="Reds")

#another method through bootstrapping
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)

anova <- aov(Steps~Sex*Teacher, data = data)
summary(anova)

anova_Tukey <- TukeyHSD(anova)
plot(anova_Tukey, col = "darkorchid")

#CI of compared means
# First calculate ANOVA of sepal length of different iris species
iris_aov <- aov(Sepal.Length ~ Species, data = iris)

# Then run a Tukey test
iris_Tukey <- TukeyHSD(iris_aov)

# Lastly use base R to quickly plot the results
plot(iris_Tukey)