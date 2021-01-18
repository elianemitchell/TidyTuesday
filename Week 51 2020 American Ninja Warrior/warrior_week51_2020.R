library(tidyverse)
library(grid)
library(png)
library(extrafont)
library(cowplot)
library(showtext)

# Add Impact font using showtext function
font_add_google("Impact", "sans serif")
showtext_auto()

top10 <- ninja_warrior %>% 
  group_by(obstacle_name) %>%
  count(obstacle_name) %>%
  arrange(-n) %>%
  head(13) # Took thirteen rows because there is one two-way and one three-way tie in the top ten

# Gave numbers to each obstacle for easier manipulation
top10$obstacle_num <- seq.int(nrow(top10))

p <- ggplot(top10, aes(x = reorder(-obstacle_num, n), y = n)) + # Order so that obstacles in descending order
  geom_bar(stat = "identity", fill = "#d6482f") +
  coord_polar(theta = "y") +
  ylim(0,90) +
  labs(title = "American Ninja Warrior",
  subtitle = "Top 10 Obstacles", 
  caption = c(" 1. Warped Wall [86]\n 2. Salmon Ladder [41]\n 3. Quintuple Steps [32]\n 4. Floating Steps [28]\n 5. Log Grip [21]\n 6. Jump Hang [18]\n 7. Quad Steps [16]\n 8. Jumping Spider [14]\n 9. Invisible Ladder [11]\n 10. Rolling Log [11]\n 11. Wall Lift [11]\n 12. Bridge of Blades[10]\n 13. Rope Ladder [10]")) +
  xlab("") +
  geom_text(hjust = 1, size = 2.5, color = "white", family = "Georgia", aes(y = 0, label = paste(obstacle_num," "))) +
  theme(
    # Background color inspired by American Ninja Warrior Logo
    plot.background = element_rect(fill = "#0053ab"),
    panel.background = element_rect(fill = "#0053ab"),
    panel.grid = element_blank(),
    # Text on Plot 
    plot.title = element_markdown(color = "white", family = "Impact", size = 20, vjust = -27), 
    plot.subtitle = element_text(color = "white", family = "Impact"),
    plot.caption = element_text(color = "white", family = "Georgia", lineheight = 1.2, size = 7, hjust = 0, vjust = 15),
    # Items to leave blank 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_blank()) 

# Upload running icon from The Noun Project
running_icon <- readPNG("Noun Run.png") 
icon <- grid::rasterGrob(running_icon, interpolate=TRUE)

# Add icon to plot using cowplot functions
h <- ggdraw(p)
h + draw_grob(icon, 0.21, 0.42, 0.30, 0.06)

