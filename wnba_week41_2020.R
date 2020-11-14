# I. Retrieve Data

# 1. Install tidytuesdayR from CRAN [This loads the datasets for the week of interest]
install.packages("tidytuesdayR")

# 2. Load data (either ISO-8601 date or year/week works)!
tuesdata <- tidytuesdayR::tt_load('2020-10-06')
tuesdata <- tidytuesdayR::tt_load(2020, week = 41)

# 3. Extract Tournament data
tournament <- tuesdata$tournament

# Alternative route: read in the data manually
tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

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
library(ggimage) # install ggimage
stanford_data$image <- "/Users/eliane/Desktop/basketball_ball.png"
    

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
                       col = "Conference") +
                  theme(axis.title.x = element_blank(), 
                        axis.title.y = element_blank(),
                        text = element_text(family = "Verdana", size = 12),
                        plot.title = element_text(size = 12,face ="bold"))

# 4. Add labels for each point and labels for when the women's basketball team
# won the championship that year

# a. Find which years the Stanford women's basketball team won the championship
Champ_percents <- filter(stanford_data, tourney_finish == "Champ")

# b. Draw graph with labels 
stanford_graph + geom_text(data = stanford_data, aes(x = year, y = full_percent, label=labels),
                          size = 2, nudge_x = 0.78, nudge_y = 0.78) +
                          annotate("text", x = 1990, y = 98.5, label = "CHAMPIONS", size = 1.5) +
                          annotate("text", x = 1992, y = 92.4, label = "CHAMPIONS", size = 1.5) 


