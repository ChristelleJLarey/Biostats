library(ggplot2)
library(tidyverse)

snakes <- read_csv("data/snakes.csv")
snakes$day = as.factor(snakes$day)

snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary


library(Rmisc)
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

#box_plot
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#scatter_plot
ggplot(data = snakes, aes(x = day, y = openings, colour = snake)) +
  geom_point(aes(fill = day), alpha = 0.6, show.legend = F, size = 5) + 
  geom_jitter(width = 0.05) +
  coord_flip()

#hex (the same as the scatter_plot, the values are in a hex shape)
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_hex(aes(fill = day), alpha = 0.6, show.legend = F, size = 5) 

#histogram
histo_snakes <- ggplot(data = snakes, aes(x = openings, colour = snake)) +
              geom_histogram(binwidth = 10, position = "dodge", size = 2, alpha = 0.02)

# Add mean line
histo_snakes + geom_vline(aes(xintercept = mean(openings)),
                          color = "blue", linetype = "dashed", size = 1)

#Bar plot
bar_snakes <- ggplot(data = snakes, aes(x = day, y = openings, fill = snake)) +
  geom_bar(stat = "identity", width = 1, position = position_dodge()) 

#bar_plot in a specific palette
bar_snakes + scale_fill_brewer(palette="Dark2")

# Greens
bar_snakes + scale_fill_brewer(palette="Greens") + theme_minimal()

  

