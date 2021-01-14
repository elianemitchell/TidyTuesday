library(tidyverse)
install.packages("ggbump")
library(ggbump)
install.packages("remotes")
library(remotes)
library(ggplot2)
install.packages("gghighlight")
library(gghighlight)
library(countrycode)
library(RColorBrewer)
install.packages("flexdashboard")
library(flexdashboard)
install.packages("ggalt")
library(ggalt)
library(showtext)
install.packages("showtext")
font_add_google()
font_add_google("Helvetica", "linotype")
showtext_auto()

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
View(transit_cost)
new_york <- transit_cost %>% filter(city == "New York")
sum(duplicated(transit_cost))

us_transit <- transit_cost %>% filter(country == "US")
#Take out the rows at bottom showing summary stats
without_nas <- transit_cost %>% filter(!is.na(ppp_rate))
add_country_code <- without_nas %>% mutate(continent = countrycode(country, 'iso2c', 'continent'))
which(is.na(add_country_code$continent))  
add_country_code[46,]
add_country_code[100,]
add_country_code[141,]
add_country_code[141, 21] = "Europe"
add_country_code[46, 21] = "Europe"
add_country_code[100, 21] = "Europe"

add_country_code %>% ggplot(aes(x = stations, y = real_cost)) + geom_point(alpha = 0.2)
cor(d$real_cost, d$stations)

colors <- c("#996633", "#0039a6", "#ee352e", "#ff6319", "#fccc0a", "#00933c")

add_country_code %>% ggplot(aes(x = real_cost, y = length)) +
  geom_point(aes(size = stations, color = continent)) +
  scale_color_manual(values = colors)

d <- add_country_code %>% group_by(country) %>%
  mutate(total_real_cost = sum(real_cost)) %>%
  ungroup()


us_transit %>% ggplot(aes(x = length, y = real_cost, color = city)) + 
  geom_point(aes(size = stations, color = city)) + theme_classic() +
  scale_color_manual(values = colors) 


ggplot() +
  geom_segment(data = us_transit,
               aes(y=start_year, yend=end_year, 
                   x=0, xend=.5), color="#b2b2b2", size=0.15)

p <- ggplot(data = us_transit, aes(y = line, x=start_year, xend=end_year)) +
  geom_dumbbell(size = 2,
    color = "purple") + 
  geom_vline(xintercept = "2020", linetype = "dashed", size = 0.5)

ny <- us_transit %>% filter(city == "New York")

ny$year <- as.numeric(ny$year)

ny$year <- as.numeric(ny$year)

#Second Avenue Phase 1 and 2 are the Q train

ggplot(data = data, aes(y = index, x=start_year, xend=end_year)) +
  geom_dumbbell(size = 5,
                color = "white") + 
  geom_segment(aes(x=start_year, xend = end_year, yend = index, col = line), 
               size = 3) +
  scale_x_continuous(breaks=seq(2006, 2030)) +
  geom_vline(xintercept = 2020, color = "white", linetype = "dashed", size = 0.5) +
  geom_point(aes(x=start_year, col = line, size = 4)) +
  geom_point(aes(x=end_year, col = line, size = 4)) +
  theme_light() +
  labs(title = "New York City Subway\nTransit Projects &\nTheir Timelines",
       caption = "Created by @elianemitchll | Source: Transit Costs Project | #TidyTuesday Week 2") +
  theme(text=element_text(family="Helvetica", face = "bold", color = "white", size=12),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "white", size = 5),
        #axis.text.y = element_text(color = "white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.caption = element_text(color = "white", size = 5),
        legend.position = "none") +
        scale_color_manual(values = c("Second Avenue Phase 2" = "#fccc0a", 
                                      "Second Avenue Phase 1" = "#fccc0a",
                                      "Gateway" = "#0039a6",
                                      "East Side Access" = "#4d5357",
                                      "7 extension" = "#b933ad" 
                                      )) +
        geom_text(aes(x = (end_year - start_year)/2 + start_year, y = index + 0.27, label = line),
                  family = "Helvetica", fontface = "bold", color = "white", 
                  size = 3)
ggsave("nyc_subway_plot.png")

class(ny$e)

data <- ny %>% mutate(index = row_number())

start_year <- ny$start_year
end_year <- ny$end_year

add_points <- for(i in seq(from = start_year, to = end_year, by = 1)) {
  print(geom_point(color = "black"))
}

  
  font("xy.text", size = 12, color = "white", face = "bold")

  scale_x_continuous(breaks = seq(2007, 2027, 1))

class(ny$start_year)
class(ny$end_year)

ny$start_year <- as.numeric(as.character(ny$start_year))
class(ny$start_year)

ny$end_year <- as.numeric(as.character(ny$end_year))

ny <- as.numeric(ny$start_year)
ny <- as.numeric(ny$end_year)
class(ny$end_year)

p + geom_vline(xintercept = 2020)  

#Helvetica

us_transit$real_cost <- as.numeric(us_transit$real_cost)

# Convert real_cost from a character to numeric data type in dataframe d
add_country_code$real_cost <- as.numeric(add_country_code$real_cost)
# Find correlation between real_cost and number of stations.
# For all countries, the correlation was 0.7325878
cor(d$real_cost, d$stations)
# Graph relationship between real_cost and stations, facetting by country
d %>% ggplot(aes(x = log(real_cost), y = stations, color = country)) + geom_point(alpha = 0.5) +
  #geom_smooth(method = "lm") +
  gghighlight() +
  facet_wrap(~country)

try %>% ggplot(aes(x = length, y = real_cost, color = continent)) + 
  geom_point(alpha = 0.5) +
  gghighlight() +
  #geom_smooth() +
  facet_wrap(~continent) +
  theme_bw() 

try %>% group_by(continent) %>% cor(try$real_cost, try$length)

correlate <- try %>% 
  select(continent, length, real_cost) %>%
  filter(continent == "Asia") %>%
  select(length, real_cost)

cor(correlate)

correlate <- try %>% group_by(continent) %>%
  summarize(r = cor(length, real_cost))

select <- try %>% filter(continent %in% continents_wanted) 

continents_wanted <- c("Europe", "Americas", "Asia")

select %>% ggplot(aes(x = real_cost, y = stations, color = continent)) +
  geom_point(alpha = 0.5) +
  gghighlight() +
  #geom_smooth() +
  facet_wrap(~continent) +
  theme_classic() 




select %>% filter(city == "New York") %>% 
  ggplot(aes(x = real_cost, y = stations)) +
  geom_point(alpha = 0.5) +
  gghighlight() +
  #geom_smooth() +
  facet_wrap(~continent) +
  theme_classic()

try %>% count(continent)

  
# Again, change data type from character to numeric for the US
us_transit$real_cost <- as.numeric(us_transit$real_cost)
# Find the correlation between real_cost & # of stations
cor(us_transit$real_cost, us_transit$stations)
# The correlation in the U.S. is weaker, and negative 
# I'd be curious to know if NYC is pulling the data down a lot 

# Graph relationship between real_cost and stations in the U.S.
us_transit %>% ggplot(aes(x = log(real_cost), y = stations)) + geom_point(alpha = 0.5)

# POTENTIAL DIRECTIONS TO GO IN
# Make a plotly graph? Something that allows you to highlight one country at
# a time - perhaps see Cedric's workshop that he did with Heureka Lab
# This could be a facetted bar chart, a singular bar chart, several options
# Basically, my hypothesis would be that - the more stations, the higher
# the cost. This seems true of China and India, but not true of the U.S.
# Then again, the n is much lower for U.S. than it is for China and India

lm(real_cost ~ stations, d)







