library(tidyverse)
library(countrycode)
library(png)
library(grid)
library('showtext')
font_add_google("Abril Fatface", "abril")
showtext_auto()
font_add_google("Roboto", "roboto slab")


#Load Data
tuesdata <- tidytuesdayR::tt_load('2020-12-08')
women <- tuesdata$women

#Add index numbers to data frame
data <- women %>% mutate(index = row_number())

#Add continents to data set
continent <- data %>% mutate(continent = countrycode(country, 'country.name', 'continent'))

#Change NAs manually to correct values
missing_continent <- continent %>% filter(is.na(continent))
continent[1, 8] = "Worldwide"
continent[63, 8] = "Europe"
continent[42, 8] = "Asia"
continent[95, 8] = "Europe"

#Assign numbers to categories because that's easier for some reason
num_cat <- continent %>% mutate(category_num = ifelse(category == "Leadership", 1,
                     ifelse(category == "Knowledge", 2,
                     ifelse(category == "Identity", 3,
                     ifelse(category == "Creativity", 4, 5)))))

colors <- c("#ffa600", "#ff6e54", "#dd5182", "#955196", "#444e86", "#003f5c")
#Palette from: https://learnui.design/tools/data-color-picker.html

p2 <- ggplot(num_cat, aes(x = category_num, y = 1, fill = continent, width = .5)) + 
      geom_bar(stat = "identity", size = 1.5, color="papayawhip") +
      xlim(0.75,5.25) +
      ylim(0, 37) +
      scale_fill_manual(values = colors) +
      theme(aspect.ratio = 1) +
      theme(panel.background = element_rect(fill = "papayawhip"),
            plot.background = element_rect("papayawhip"))

p3 <- ggplot(num_cat, aes(x = category_num, y = 1, fill = continent, width = .5)) + 
  geom_bar(stat = "identity", size = 1.5) 

#Add icons to graph
image3 <- readPNG("/Users/eliane/Desktop/R Folder/Identity Icon.png") 
id_icon <- rasterGrob(image3, interpolate=FALSE)
image1 <- readPNG("/Users/eliane/Desktop/R Folder/Leadership Icon.png")
leadership_icon <- rasterGrob(image1, interpolate=FALSE)
image2 <- readPNG("/Users/eliane/Desktop/R Folder/Knowledge icon.png")
knowledge_icon <- rasterGrob(image2, interpolate=FALSE)
image4 <- readPNG("/Users/eliane/Desktop/R Folder/Creativity Icon .png")
creativity_icon <- rasterGrob(image4, interpolate=FALSE)
image5 <- readPNG("/Users/eliane/Desktop/R Folder/Wonderwoman.png")
allwomen_icon <- rasterGrob(image5, interpolate=FALSE)

#Add icons to graph and lines
p3 <- p2 + annotation_custom(id_icon, xmin=2.5, xmax=3.5, ymin=16, ymax=21) +
  annotation_custom(leadership_icon, xmin=0.5, xmax=1.5, ymin=28, ymax=33) +
  annotation_custom(knowledge_icon, xmin=1.5, xmax=2.5, ymin=31.4, ymax=36.4) +
  annotation_custom(creativity_icon, xmin=3.5, xmax=4.5, ymin=20.7, ymax=25.7) +
  annotation_custom(allwomen_icon, xmin=4.5, xmax=5.5, ymin=23, ymax=28) +
  geom_segment(aes(x=4.8, y=2, xend=4.8, yend=22), color="#bc5090") +
  geom_segment(aes(x=4.8, y=22, xend = 5.2, yend=22), color="#bc5090", 
               arrow = arrow(length = unit(0.1, "cm"))) 

#Add labels to plot 
p3 + labs(title = "100 Influential & Inspiring Women in 2020",
       subtitle = "This year, BBC News honored 100 women leaders, intellectuals, activists, and creatives.\nThey left a space, called Unsung Hero, to recognize the unsung heroines worldwide.",
       caption = "Created by @eliane_mitchll | Source: BBC News | #TidyTuesday Week 50 | See Github for Icons") 
     theme(axis.title.x = element_blank(),
           axis.text.y = element_text(size=7),
           axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(family = "abril", size = 15)) +
    theme(plot.subtitle = element_text(family = "roboto slab", size = 8, hjust=0),
          plot.title = element_text(hjust = 0),
          plot.caption = element_text(size = 8),
          legend.position = c(0.70, 0.90),
          legend.text=element_text(size=7),
          legend.title = element_blank(),
          legend.key.size = unit(0.5, "cm"),
          legend.direction = "horizontal",
          legend.background = element_rect(fill = "papayawhip"))
          
# Icon Citations
# creativity by mynamepong from the Noun Project: https://thenounproject.com/icon/3017281/
# Wonder Woman by Éléonore Sabaté from the Noun Project: https://thenounproject.com/icon/638563/
# knowledge by Alena from the Noun Project: https://thenounproject.com/icon/2824220/

      
        

  
  
  
  
  


