# Get Data
install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar
devtools::install_github("averyrobbins1/appa")

# Packages
library(tidyverse)
library(gt)
library(showtext)

# Install Emojis package
devtools::install_github("hadley/emo")

# Drop Unwanted Columns 
drop <- c("chapter", "character", "full_text",
          "character_words", "scene_description",
          "writer", "id")
df = avatar[,!(names(avatar) %in% drop)]

# Drop Duplicated Rows
duplicated(df)
clean_avatar <- df[!duplicated(df), ]

# Create column for the average IMDB ratings for each season
with_average <- clean_avatar %>% group_by(book_num) %>%
  mutate(mean_rating = mean(as.numeric(imdb_rating), na.rm = TRUE)) %>% ungroup()

# Drop Unwanted Columns and Duplicates
drop2 <- c("chapter_num", "director", "imdb_rating") # Initially I wanted to have a column for the director most used for each season, but ultimately decided to drop
gt = de[,!(names(de) %in% drop2)]
clean_gt <- gt[!duplicated(gt), ]

# Add column with emojis for each book
fire <- emo::ji("fire")
water <- emo::ji("splashing")
earth <- emo::ji("wood")
emojis <- c(water, earth, fire) # Put emojis in vector
clean_gt$new_col <- emojis # Add vector as column to data frame
gt_table <- clean_gt[,c("book", "new_col", "book_num", "mean_rating")]

# Make GT Table
gt_table %>% gt() %>%
  tab_header(
    title = "Avatar: The Last Airbender",
    subtitle = "IMDB Ratings by Season") %>%
  cols_label(book = "Book", 
             new_col = "",
             book_num = "#",
             mean_rating = "Avg. Rating") %>% 
  opt_table_font(font = google_font(name = "Herculanum")) %>%
  tab_source_note(source = "@eliane_mitchell | #TidyTuesday | Data: appa")


