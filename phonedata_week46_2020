# read in Data
tuesdata <- tidytuesdayR::tt_load('2020-11-10')
mobile <- tuesdata$mobile
landline <- tuesdata$landline

# install packages and libraries
install.packages("png")
library(png)
library(ggplot2)
library(extrafont)

# save images of mobile phone and landline
img1 <- readPNG("telephone.png") #image of landline
image1 <- rasterGrob(img1, interpolate=FALSE)
img2 <- readPNG("mobile-phone.png") # image of iphone
image2 <- rasterGrob(img2, interpolate=FALSE)

# filter US Data 
us_mobile <- mobile %>% filter(code == "USA")
us_landline <- landline %>% filter(code == "USA")
# merge US data in each set
us_data <- inner_join(us_mobile, select(us_landline, year, landline_subs))

# plot line graph
plot <- ggplot(us_data, aes(x = year)) + 
  # add images and place them 
  annotation_custom(image2, xmin=2010, xmax=2022, ymin=105, ymax=117) +
  annotation_custom(image1, xmin=2010, xmax=2022, ymin=22, ymax=34) +
  # draw lines for each kind of phone subscription
  geom_line(aes(y = landline_subs, color = "blue", size = 0.10), show.legend = FALSE) +
  geom_line(aes(y = mobile_subs, color = "red", size = 0.10), show.legend = FALSE) +
  theme_bw() +
  # add breaks 
  scale_x_continuous(breaks = seq(1990,2017, 4)) +
  labs(title = "Mobile Takeover in the US",
       subtitle = "Phone subscriptions in US per 100 people",
       caption = "Created by @eliane_mitchell | #TidyTuesday Week 46") +
  theme(
    text = element_text(size = 12, family = "Arial"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    plot.title = element_text(size = 12,face ="bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 7, hjust = 0.5))

ggsave("Tidy Tuesday Phone Plot.png")

  
  
  
