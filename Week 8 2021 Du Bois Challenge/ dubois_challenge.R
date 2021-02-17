library(tidyverse)
library(showtext)
showtext_auto()
library(extrafont)
font_import()

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)
freed_slaves <-tuesdata$freed_slaves


data <- freed_slaves %>%
  group_by(Year) %>%
  mutate(n = sum(Slave, Free)) %>%
  pivot_longer(cols = Slave:Free,
               names_to = "freed_slave") %>%
  mutate(percent = value / n) 

data %>%
  ggplot(aes(x = Year, y = percent,
             fill = freed_slave)) +
  geom_area() +
  scale_fill_manual(values=c("#298355","#121312")) +
  labs(title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES\n\nPROPORTION DES NÈGRES LIBRES ET DES ESCALVES EN AMÉRIQUE.",
       subtitle = "\nDONE BY ATLANTA UNIVERSITY .",
       caption = "Submission by @elianemitchll | Source: #DuBoisChallenge | #TidyTuesday Week 8") +

  theme(panel.background = element_rect("#d7cdc0"),
        plot.background = element_rect("#d7cdc0"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, family = "sports", size = 9),
        plot.subtitle = element_text(hjust=0.5, family = "sports", size = 7),
        plot.caption = element_text(family = "sports", size = 6)) +
  xlim(1790, 1870) +

  # Text label for year
  geom_text(aes(x = Year, y = 1.02, label = Year),
            family = "sports", fontface = "bold") +
  # Text label for percent
  geom_text(aes(x = Year, y = ifelse(Year == 1870, .92, 1.0-percent + 0.02),
                label = ifelse(freed_slave == "Free", paste0(data$value,"%"), "")),
            family = "sports", size = 3) +
  geom_text(x = 1830, y = .6, label = "SLAVES\nESCLAVES", color = "#d7cdc0",
            family = "sports", size = 5) +
  geom_text(x = 1830, y = .97, label = "FREE - LIBRE", family = "sports") +
  geom_segment(aes(x = Year, xend = Year, y = 1.0, yend = ifelse(
    freed_slave == "Free", 1.0 - percent + 0.05, 1.0)), size = 0.1) 
  
  
font_add("sports", regular = "/Users/eliane/Downloads/octin_sports_free/octin sports rg.ttf")

ggsave("Tidy Tuesday Week 8 #DuBois Challenge.png")
