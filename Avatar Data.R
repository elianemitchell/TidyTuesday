# Get Data
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar

# Packages
library(tidyverse)
devtools::install_github("averyrobbins1/appa")
install.packages("gt")
library(gt)
install.packages("showtext")
library(showtext)
library(appa)
avatar <- appa::appa
glimpse(avatar)
install.packages("data.table")
library(data.table)
install.packages("skimr")
library(skimr)
library(ggplot2)
font_add_google(name = "Linotype Syntax Lapidar Serif Display", 
                family = "linotype")
install.packages("extrafont")
library(extrafont)
y
#Install Emojis package
devtools::install_github("hadley/emo")


avatar %>% head() %>% gt()

#Proposed Steps
# 1. keep columns for book and book num, chapter num, director,
# and IMDB rating
# 2. May need to group books together? so that 
# there aren't so many repeats?
# 3. check that gt table makes a simple table correctly with
# the book title, its number, the director and the avg IMDB
# rating 
# 4. add in bar charts of IMDB ratings by chapter num

drop <- c("chapter", "character", "full_text",
          "character_words", "scene_description",
          "writer", "id")
df = avatar[,!(names(avatar) %in% drop)]
duplicated(df)
clean_avatar <- df[!duplicated(df), ]
clean_avatar %>% gt()

de <- clean_avatar %>% group_by(book_num) %>%
  mutate(mean_rating = mean(as.numeric(imdb_rating), na.rm = TRUE)) %>% ungroup()


drop2 <- c("chapter_num", "director", "imdb_rating")
gt = de[,!(names(de) %in% drop2)]
clean_gt <- gt[!duplicated(gt), ]
clean_gt %>% gt()

fire <- emo::ji("fire")
water <- emo::ji("splashing")
earth <- emo::ji("wood")

emojis <- c(water, earth, fire)
clean_gt$new_col <- emojis
small_gt <- clean_gt[,c("book", "new_col", "book_num", "mean_rating")]

small_gt %>% gt() %>%
  tab_header(
    title = "Avatar: The Last Airbender",
    subtitle = "IMDB Ratings by Season") %>%
  cols_label(book = "Book", 
             new_col = "",
             book_num = "#",
             mean_rating = "Avg. Rating") %>% 
  opt_table_font(font = google_font(name = "Herculanum")) %>%
  tab_source_note(source = "@eliane_mitchell | #TidyTuesday | Data: appa")


