library(dplyr)
library(reshape)
library(ggplot2)
library(tidyverse)

#Load Data
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-10-20')
beer_awards <- tuesdata$beer_awards

#Tidy Data
beer_awards$state[which(beer_awards$state == 'Ak')] <- "AK"
beer_awards$state[which(beer_awards$state == 'wa')] <- "WA"

#After looking around, decided to look at New York's Data
newyork_data <- beer_awards %>% filter(state == "NY") %>% 
    group_by(city) %>% count(medal)

#Arrange total medals won by NY in descending order
#Note: I later  realized another way to do this would be to look at which cities in NY had the highest proportion of gold medals but found coding-wise this was easier
medals_summed <- newyork_data %>% group_by(city) %>% 
  mutate(total_medals = sum(n)) %>%
  arrange(desc(total_medals)) 

#Created data frame with just the top 5 cities (says 15 because each city has three levels for medals
experiment <- medals_summed %>% head(15) #Definitely could have named this something more helpful

#Rearranged factor to emulate the Silver, Gold, Bronze podium arrangement at the U.S. Olympics 
experiment$medal <- factor(experiment$medal, levels = c("Silver", "Gold", "Bronze"))

#Draw facetted plot
facet_plot <- ggplot(experiment, aes(x = medal, y=n, fill=medal)) + geom_bar(stat="identity") + 
  scale_fill_manual(values = c("#DCDCDC","#FFD700", "#CB8C08")) +
  facet_wrap(.~city) +
  theme(panel.background = element_blank(),
  strip.background = element_rect(fill="#096B8C"),
  strip.text.x = element_text(size = 9, color = "white")) +
  ggtitle("Medal Distribution Among Most Awarded Cities for Beer in New York") +
  labs(title = "Best New York Beers",
       subtitle = "5 Most Awarded NY Cities for GABF Medals (1987-2020)",
       caption = "Created by @eliane_mitchell | #TidyTuesday Week 43 | Data: Great American Beer Festival") +
  theme(axis.title.x = element_blank(),
      axis.title.y = element_blank())
