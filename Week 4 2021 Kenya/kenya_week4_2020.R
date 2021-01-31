# PACKAGES 
library(tidyverse)
library(sf)
library(rKenyaCensus)
library(cowplot)

library(showtext)
font_add_google("Alfa Slab One", "Alice")
font_add_google("Roboto", "Roboto Condensed")
showtext_auto()

# PREP: PULL IN DATA
# pull data from rKenyaCensus and TidyTuesday Github
disability <- V4_T2.27
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv') #why not just use TidyTuesday's clean data :3

# read in map and lower case county names. Map downloaded from: https://www.igismap.com/kenya-shapefile-download-boundary-line-administrative-state-and-polygon/
map <- KenyaCounties_SHP %>%
  sf::st_as_sf() %>%
  mutate(County = str_to_title(County))

# WRANGLE DATA
# Change households data to contain only columns needed and modify county names
population <- households %>% select(County, Population) %>%
  filter(County != "Kenya") %>%
  mutate(County = case_when(County == "TanaRiver" ~ "Tana River",
                            County == "UasinGishu" ~ "Uasin Gishu",
                            County == "WestPokot" ~ "West Pokot",
                            County == "TransNzoia" ~ "Trans Nzoia",
                            County == "NairobiCity" ~ "Nairobi City",
                            County == "HomaBay" ~ "Homa Bay", 
                            TRUE ~ County))

# add up all reported disabilities and lowercase county names
sum_disabilities <- disability %>% filter(AdminArea == "County") %>%
  mutate(sumDisability = sum(Vision_Total, Hearing_Total, Mobility_Total, 
                             Cognition_Total, SelfCare_Total, Communication_Total),
         County = str_to_title(County)) %>%
  select(County, sumDisability)

# merge sum disability with population and calculate per 1000
first_merge <- merge(sum_disabilities, population, by="County", all.y = TRUE) %>%
  mutate(per_thousand = (sumDisability/Population)*1000,
         above_below = per_thousand - median(per_thousand)) %>%
  select(County, sumDisability, Population, per_thousand, above_below)

# merge merged data (between sum disabilities and population) with map data
plot_df <- st_as_sf(merge(first_merge, map, by="County"))

# MAP TIME
# make the map
plot <- plot_df %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = above_below), lwd = 0) +
  theme_void() +

# set colors, breaks
  scale_fill_gradient2(
    low = "#009e73", 
    mid = "floralwhite",
    high = "#cc79a7",
    limits = c(-20, 40),
    breaks = seq(-20, 40, by = 20),
    labels = c("-20", "0 (median)", "20+", "40+")) +

# write labels
  labs(title = "Kenya: Reported Disabilities by County",
       subtitle = "Reported disabilities per 1,000 people. Reported disabilities include impairments to \ncognition, vision, hearing, mobility, ability to communicate, and ability to self-care.\n \nCounties are shaded based on distance from the median reported disabilities per\n1,000 people (the median lies in Bungoma).\n \nSome people have more than one disability so the actual number of people with\ndisabilities would be lower than this number.",
       caption = "Created by @elianemitchll | Source rKenyaCensus | #TidyTuesday Week 4") +

# final theme things(font, positioning
  theme(plot.title = element_text(family = "Alice", face = "bold"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 7, lineheight = 1),
        legend.direction = "vertical",
        legend.position = c(1.1, 0.56),
        legend.title = element_blank(),
        plot.caption = element_text(size = 6, family = "Roboto Condensed", vjust=5, hjust=1.25),
        legend.text = element_text(family = "Roboto Condensed", size =6))

ggsave("Kenya plot.png")


