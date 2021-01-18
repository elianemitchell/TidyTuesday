library(tidyverse)
library(showtext)
font_add_google("Helvetica", "linotype")
showtext_auto()

# load data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
ny <- transit_cost %>% filter(city == "New York")

# convert year to numeric because it's originally class character
ny$year <- as.numeric(ny$year)
ny$year <- as.numeric(ny$year)

# make plot
ggplot(data = data, aes(y = index, x=start_year, xend=end_year)) +

# make dumbells
  geom_dumbbell(size = 5,
                color = "white") + 

# add segments - it took me a while figuring out how to change the color of each segment by a third variable, so credits to @4nsgarW!! 
# see his work here: https://twitter.com/4nsgarW/status/1348376794144137217
  geom_segment(aes(x=start_year, xend = end_year, yend = index, col = line), 
               size = 3) +

# do the same with the geom points 
  geom_point(aes(x=start_year, col = line, size = 4)) +
  geom_point(aes(x=end_year, col = line, size = 4)) +

# change x scale
  scale_x_continuous(breaks=seq(2006, 2030)) +

# add line to represent 2020
  geom_vline(xintercept = 2020, color = "white", linetype = "dashed", size = 0.5) +
  theme_light() +

# add labels
  labs(title = "New York City Subway\nTransit Projects &\nTheir Timelines",
       caption = "Created by @elianemitchll | Source: Transit Costs Project | #TidyTuesday Week 2") +

# other theme elements, in no particular order
  theme(text=element_text(family="Helvetica", face = "bold", color = "white", size=12),
        
# axis ticks and labels
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "white", size = 5),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        
# change plot background to black 
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        
# title and grid line manipulations
        plot.title = element_text(),
        plot.caption = element_text(color = "white", size = 5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
# making geom_points be colored by a third variable produces a weird legend, so just got rid of that
        legend.title = element_blank(),
        legend.position = "none") +

# setting lines to "NYC subway colors", see: https://en.wikipedia.org/wiki/New_York_City_Subway_nomenclature
        scale_color_manual(values = c("Second Avenue Phase 2" = "#fccc0a", 
                                      "Second Avenue Phase 1" = "#fccc0a",
                                      "Gateway" = "#0039a6",
                                      "East Side Access" = "#4d5357",
                                      "7 extension" = "#b933ad"
                                      )) +

# adding the name of each line to each segment
        geom_text(aes(x = (end_year - start_year)/2 + start_year, y = index + 0.27, label = line),
                  family = "Helvetica", fontface = "bold", color = "white", 
                  size = 3)


ggsave("nyc_subway_plot.png")
