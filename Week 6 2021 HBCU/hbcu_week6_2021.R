# Library
library(tidyverse)
library(showtext)
library(ggtext)
font_add_google("Bebas Neue")
showtext_auto()

# Read in data
tuesdata <- tidytuesdayR::tt_load('2021-02-02')
hbcu_black <- tuesdata$hbcu_black


# Change column names 
colnames(hbcu_black) <- c("year", "total_enroll", "males", "females",
                          "four_year", "two_year", "total_public", 
                          "four_year_public", "two_year_public", "total_private",
                          "four_year_private", "two_year_private")
  
# Save data frame
# See original data for this kind of plot here! https://github.com/gkaramanis/tidytuesday/blob/master/2020-week08/food-consumption.R

d <- hbcu_black %>% 
  mutate(females_in_thousands = females/1000,
         males_in_thousands = males/1000,
         diff = round(females_in_thousands - males_in_thousands, 0)) %>%
  select(year, females_in_thousands, males_in_thousands, diff) %>%
    filter(year >= 2000) %>%
    mutate(pos = -8:7) %>%
    rowwise() %>%
    mutate(
      x = list(c(-10, -5, -5, -10)),
      y = list(c(pos*4 - 1.4, pos*2 - 0.7, pos*2 + 0.7, pos*4 + 1.4))) %>% 
    unnest(cols = c(x, y))

d %>%
    ggplot() +
  # first rectangle
    geom_rect(aes(xmin = -12, ymin = pos*4 - 1.4,
                  xmax = -10, ymax = pos*4 + 1.4), fill = "#de425b", color = NA) +
  # second rectangle
    geom_polygon(aes(x, y, group = year), fill = "#ec838a", color = NA) +
  # third rectange
    geom_rect(aes(xmin = -5.0, ymin = pos*2 - 0.75,
                  xmax = females_in_thousands/20, ymax = pos*2 + 0.75), 
              fill = "#aecdc2", color = NA) +
  # male rectangle
  geom_rect(aes(xmin = -5.0, ymin = pos*2 - 0.50,
                xmax = males_in_thousands/20, ymax = pos*2 + 0.50), 
            fill = "#ffeb94", color = NA) +
  geom_text(aes(-11, pos*4, label = year), family = "Bebas Neue", 
            color = "#FFFFFF", hjust = 0.5, size = 4, check_overlap = TRUE) +
  #geom_text(aes(5.5, pos*2, label = diff), 
            #family = "Bebas Neue", color = "white", size = 3, 
            #check_overlap = TRUE) +
  labs(title = "Black Student Enrollment at Historically Black Universities & Colleges",
       caption = "Created by @elianemitchll | Source: Data World | #TidyTuesday Week 6",
       subtitle = "<span style='color:#ffeb94;'>_Men_</span> and <span style='color:#c1e1dc;'>_Women_</span> in the thousands") +
  theme(plot.title = element_text(hjust = .9, family = "Bebas Neue", size = 16, 
                                  color = "white"),
        plot.caption = element_text(hjust = .9, family = "Bebas Neue", color="white"),
        plot.subtitle = element_markdown(hjust = .9, family = "Bebas Neue", color="white"), 
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  geom_rect(xmin=4, xmax=6, ymin=19, ymax=21, color = "black") +
  geom_rect(xmin=6.5, xmax=8.5, ymin=19, ymax=21, color = "black") +
        geom_text(x = 7.5, y =20, label = "150,000", color="white", 
                  family = "Bebas Neue", size = 3) +
        geom_text(x=5, y=20, label = "100,000", color = "white",
                  family = "Bebas Neue", size = 3) 

ggsave("HBCU Enrollment TidyTuesday Last.png")

