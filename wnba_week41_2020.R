# I. Retrieve Data
# 1. Install tidytuesdayR from CRAN [This loads the datasets for the week of interest]
install.packages("tidytuesdayR")
# 2. Load data 
tuesdata <- tidytuesdayR::tt_load('2020-10-06')
# 3. Extract Tournament data
tournament <- tuesdata$tournament

# II. Load packages 
library(tidyverse)
library(dplyr)
library(ggplot2)

# III. My Question: How has Stanford's percentage of games won at the women's NCAA changed over time?
# Data viz tool: Use a scatterplot!

# 1. Extract Stanford's data. Excluding year 1982 to keep intervals consistent
stanford_data <- filter(tournament, school == "Stanford" & year > 1982)

# 2. Scatterplot details 
# a. Store Stanford colors to use for geom_line
color_conference <- c("#990000", "#339900")
# b. Store x, y labels for later
labels <- c(stanford_data$full_percent)
# c. Create column in dataframe that comprises the image for the data points
install.packages("ggimage")
library(ggimage)
stanford_data$image <- "/Users/eliane/Desktop/R Folder/basketball_ball.png"
    # install ggimage
# d. Introduce new fonts
install.packages('extrafont')
library('extrafont')
font_import()
y
fonttable()
loadfonts()
library(crayon)
    
# 3. Create scatterplot
stanford_graph <- ggplot(stanford_data, aes(x = year, y = full_percent)) + 
                  geom_line(aes(color = conference)) +
                  geom_image(aes(image = image), size = 0.04) +
                  scale_x_continuous(breaks = seq(1990, 2020, 4)) +
                  scale_y_continuous(breaks = seq(60, 100, 5)) +
                  scale_colour_manual(values = color_conference) +
                  theme_classic() +
                  labs(title = "Stanford Women's Basketball Team Performance",
                       subtitle = "Their Winning Percentage at the NCAA Over Time",
                       col = "Conference",
                       caption = "Created by @eliane_mitchll | #TidyTuesday Week 41") +
                  theme(axis.title.x = element_blank(), 
                        axis.title.y = element_blank(),
                        text = element_text(family = "Verdana", size = 12),
                        plot.title = element_text(size = 12,face ="bold"),
                        # add grey vertical lines to aid readability
                        panel.grid.major.x = element_line(colour = "grey96"),
                        panel.grid.minor.x = element_line(colour = "grey96"), 
                        plot.caption = element_text(size = 7, hjust = 0.5))
# 4. Add labels for each point and labels for when the women's basketball team
# won the championship that year
# a. Find which years the Stanford women's basketball team won the championship
Champ_percents <- filter(stanford_data, tourney_finish == "Champ")
# b. Draw graph with labels 
graph <- stanford_graph + geom_text(data = stanford_data, aes(x = year, y = full_percent, label=labels),
                          size = 2, family = "Verdana", nudge_x = -0.20, nudge_y = -1.5) 

# c. add geom_curves for labelling years in which the Cardinal won the championships 
graph + geom_curve(x = 1993, y = 98.5, xend = 1991.2, yend = 97.3,
                   colour = "black", curvature = 0.05, 
                   arrow = arrow(length = unit(0.05, "inches"))) +
      geom_curve(x = 1993.5, y = 95.5, xend = 1992.5, yend = 92.2,
                   colour = "black", curvature = -0.05,
                   arrow = arrow(length = unit(0.05, "inches"))) +
      annotate("text", x = 1996.65, y = 97.7, 
               label = "The Cardinal won the NCAA \nchampionships in 1990 and 1992. \n They came close in 2008 & 2010", 
               size = 2, family = "Verdana", color = "#333333") 


