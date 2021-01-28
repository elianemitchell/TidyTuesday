# Starting all over again
# Mission: make a BASIC map of the population in
# Kenya and maybe I distinguish myself just by
# using good fonts (unlike what I have seen)

#Packages 
library(ggtext)
library(png)
library(tidyverse)
install.packages("sf")
library(sf)
install.packages("raster",
                 "tmap", "leaflet", "sf")
updateR()
tm_shape(Kenya)
library(tmap)
install.packages("maptools")
library(maptools)
install.packages("rgdal")
library(rgdal)
install.packages("rKenyaCensus")
library(rKenyaCensus)

library(devtools)
devtools::install_github("Shelmith-Kariuki/rKenyaCensus")

disability <- V4_T2.27

households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')
population <- households %>% select(County, Population)

## SUCCESSFUL attempt to read in shape file and view shape file
#country2 <- read_sf("/Users/eliane/Downloads/kenyan-counties/County.shp")
 
# NEW KENYA COUNTY TO USE
kenya <- read_sf("/Users/eliane/Downloads/County/County.shp")

#try <- country2 %>% ggplot() + geom_sf() 

kenya %>%
  ggplot() +
  geom_sf()

# Rename COUNTY column in country2 to County, in order to match with population
#country3 <- rename(country2, County = COUNTY)
kenya2 <- rename(kenya, County = Name)

# Create dataframe that now adds the population numbers to the geosp dataframe
# This doesn't work because it deletes the geo data from the country3 df 
#plot <- merge(population, country3, by = "County", all = TRUE)
#plot_tryagain <- merge(population, kenya2, by = "County", all = TRUE)

# Leaves the geoms which is more important... this uses TRY, so don't use
new_df <- st_as_sf(merge(try, kenya2, by = "County", all.y = TRUE))

new_pop <- population %>% mutate(County = case_when(County == "TanaRiver" ~ "Tana River",
                                            County == "UasinGishu" ~ "Uasin Gishu",
                                            County == "WestPokot" ~ "West Pokot",
                                            County == "TransNzoia" ~ "Trans Nzoia",
                                            County == "NairobiCity" ~ "Nairobi",
                                            County == "Murang'a" ~ "Murang?a",
                                            County == "HomaBay" ~ "Homa Bay", 
                                            TRUE ~ County))
                                            
# THIS WORKS:
plot <- st_as_sf(merge(new_pop, kenya2, by = "County", all.y = TRUE))
          

# Add variable to make population more wieldy
# not sure if I want this anymore
new_df <- new_df %>% mutate(pop_new = Population / 100000)

# Why won't this workkk
plot %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = Population), color = "white", size = 0.5) +
  theme_void() +
  #paletteer::scale_fill_paletteer_c("scico::tokyo") +
  scale_fill_gradient(low = "#fbd0d2", high = "#ee1f25") +
  theme_void()




disability_data_noMobility <- disability %>% filter(AdminArea == "County") %>%
  select(County, Vision_Total, Hearing_Total, Mobility_Total,
         Cognition_Total, SelfCare_Total, Communication_Total) %>%
  pivot_longer(-c(County), names_to = "Disability") %>%
  mutate(sum = sum(value),
         prop = value/sum(value),
         County = str_to_title(County)) %>%
  group_by(County) %>%
  filter(Disability != "Mobility_Total") %>%
  filter(prop == max(prop))

sumDisability <- disability %>% filter(AdminArea == "County") %>%
  mutate(sumDisability = sum(Vision_Total, Hearing_Total, Mobility_Total,
         Cognition_Total, SelfCare_Total, Communication_Total),
         County = str_to_title(County)) %>%
  select(County, sumDisability) %>%
  filter(County != "Kenya")
  
see <- merge(sumDisability, pop, by="County", all.y = TRUE) %>%
  mutate(prop = sumDisability/Population)

BEST <- st_as_sf(merge(see, new_df3, by = "County", all.y = TRUE))

done <- see %>% mutate(prop = sumDisability/Population)

pop <- population %>% mutate(County = case_when(County == "TanaRiver" ~ "Tana River",
                                            County == "UasinGishu" ~ "Uasin Gishu",
                                            County == "WestPokot" ~ "West Pokot",
                                            County == "TransNzoia" ~ "Trans Nzoia",
                                            County == "NairobiCity" ~ "Nairobi City",
                                            County == "HomaBay" ~ "Homa Bay", 
                                            TRUE ~ County))
population <- population %>% filter(County != "Kenya")

next_127 <- see %>% mutate(above_below = mean(prop) - prop)

sum(BEST127$sumDisability)/sum(BEST127$Population)
mean(BEST127$prop)

overall_prop <- sum(next_127$sumDisability) / sum(next_127$Population)

next_127 <- see %>% mutate(percent_diff = ((prop - overall_prop) / overall_prop)*100)
next_128 <- see %>% mutate(percent_dis = prop * 100,
                           percent_diff = percent_dis - (overall_prop *100)) 

BEST128 <- BEST128 %>% select(-Disability, prop.x, prop.y)



max(next_127$prop) *100
min(next_127$prop) *100
overall_prop *100

bla <- see %>% mutate(sd = sqrt((prop - (mean(prop)^2)) / 46))
bla %>% ggplot(aes(x = sd)) + geom_histogram()

  
BEST128 <- st_as_sf(merge(next_128, new_df3, by = "County"))

font_add_google("Alfa Slab One", "Alice")
showtext_auto()
library(ggtext)
       
colors <- c("#052955","white","#ae9c45")    

BEST128 %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = percent_diff), lwd = 0, color = "white") +
  theme_void() +
  scale_fill_gradient2(legend_title,
                      low = "#052955", 
                      mid = "white",
                      high = "#ae9c45",
                      limits = c(-2.0, 3.18),
                      labels = c("0.88%", "", "2.83%", " ", " ", "5.96%")) +
  labs(title = "Kenya Disability Rate by County",
       subtitle = "Over 1 million Kenyans (2.83%) are afflicted with impairments to their\nsight, vision, cognition, mobility, ability to communicate, or ability to self-care.",
       caption = "Created by @elianemitchll | Source rKenyaCensus | #TidyTuesday Week 4") +
  guides(fill = guide_legend(barwidth = 1, barheight = 4, reverse=TRUE)) +
  theme(plot.title = element_text(family = "Alice", face = "bold"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 7, lineheight = 1.2),
        legend.direction = "vertical",
        legend.position = c(1.2, 0.56),
        legend.title = element_blank(),
        plot.caption = element_text(size = 6, family = "Roboto Condensed", vjust=5, hjust=0.5),
        legend.text = element_text(family = "Roboto Condensed")) 

ggsave("Tidy Tuesday Kenya Disability.png")

 library(ggtext)

sum(next_128$sumDisability)

legend_title <- "Percent Difference"

font_add_google("Roboto", "Roboto Condensed")

# Darker colors show more extreme values from the mean

max(next_128$percent_diff)
min(next_128$percent_diff)


BEST %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop.x)) +
  theme_void() +
  scale_fill_gradient(low = "#fbd0d2", high = "#ee1f25")

# Another option: above and below?

class(see$prop)


View(population %>% arrange(County))
View(sumDisability %>% arrange(County))
population %>% 
merge(sumDisability, population, by = "County", all.y = TRUE)

  pivot_longer(-c(County), names_to = "Disability") %>%
  mutate(sum = sum(value),
         prop = value/sum(value),
         County = str_to_title(County))

dis_pop <- merge(disWithAllProps, population, by = "County", all.y = TRUE)


disability_data %>%
  #group_by(County) %>%
  str_to_lower(County)
  

kenya_graph_data <- kenya2 %>% mutate(County = str_to_title(County))

## MOST UPDATED JAN 27TH
new_df <- st_as_sf(merge(disability_data, kenya_graph_data, by = "County", all.y = TRUE))
new_df2 <- st_as_sf(merge(disWithAllProps, kenya_graph_data, by = "County", all.y = TRUE))
done_df <- st_as_sf(merge(disability_data_noMobility, new_df3, by = "County", all.y = TRUE))

done_df %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = "prop.x", color = Disability.x)) +
  theme_void() +
  #paletteer::scale_fill_paletteer_c("scico::tokyo") +
  #scale_color_gradient(low = "transparent", high = "white") +
  theme_void() +
  theme(
  panel.background = element_rect(fill = "black"),
  plot.background = element_rect(fill = "black"))


  

  #facet_wrap(~Disability)

# Another consideration: what proportion of people are disabled?

new_df2 %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop)) +
  theme_void() +
  #paletteer::scale_fill_paletteer_c("scico::tokyo") +
  scale_fill_gradient(low = "#fbd0d2", high = "#ee1f25") +
  theme_void() +
  facet_wrap(~Disability)

new_df3 <- new_df2 %>% mutate(County = case_when(County == "Nairobi" ~ "Nairobi City",
                                County == "Murang?A" ~ "Murang'a",
                                TRUE ~ County))
  


disability_data %>% pivot_longer() %>%
  summmarize(value = sum(value)) %>%
  mutate(prop = value / sum(value))

  group_by(County) %>%
  summarize(value = sum(value)) %>%
  mutate(prop = value/sum(value))

  mutate(total = sum(Vision_Total, Hearing_Total, Mobility_Total,
                     Cognition_Total, SelfCare_Total, Communication_Total),
         prop = n / total)





religions2 <- rKenyaCensus::V4_T2.30 %>% 
  janitor::clean_names() %>% 
  filter(county != "KENYA") %>% 
  pivot_longer(-c(county, total), names_to = "religion") %>% 
  mutate(prop = value / total) %>% 
  group_by(county) %>% 
  filter(prop == max(prop)) %>% 
  ungroup()

library(paletteer)

  scale_color_viridis()

library(scaleColour)


# Attempt for plotting shape file with population
ggplot(plot) +
  geom_sf(mapping = aes(fill = Population))


# Returns an error
table[is.na(match(countiesFromCountry2$COUNTY, countiesFromPopulation$County)),]

# Gives a bunch of Trues and Falses
is.na(match(countiesFromCountry2$COUNTY, countiesFromPopulation$County))

  
  ----------------------

shp <- st_as_sf(rKenyaCensus::KenyaCounties_SHP)

st_crs(kenya)

plot(kenya)


tm_shape(kenya)






