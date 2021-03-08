library(tidyverse)
library(gt)
library(fontawesome)
library(htmltools)


tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube


top_views <- youtube %>% arrange(-view_count) %>% 
  mutate(ratio_likes = round(like_count/dislike_count, 1)) %>% # ratio of dislikes to likes
  select(thumbnail, title, brand, year, funny, show_product_quickly,
         patriotic, celebrity, danger, animals,
         use_sex, view_count, like_count, dislike_count, comment_count,
         #youtube_url, 
         ratio_likes) %>% 
 slice_max(view_count, n=5)


# functions for adding font awesome icons
laugh_beam <- function(value) {
  icon <- fa("laugh-beam", fill = "#b9061f")
  div(icon) %>% 
    as.character() %>% 
    html()
}

animals <- function(value) {
  icon <- fa("paw", fill = "#b9061f")
  div(icon) %>% 
    as.character() %>% 
    html()
}

usa <- function(value) {
  icon <- fa("flag-usa", fill = "#b9061f")
  div(icon) %>% 
    as.character() %>% 
    html()
}

celebrity <- function(value) {
  icon <- fa("grin-stars", fill = "#b9061f")
  div(icon) %>% 
    as.character() %>% 
    html()
}

danger <- function(value) {
  icon <- fa("ghost", fill = "#b9061f")
  div(icon) %>% 
    as.character() %>% 
    html()
}


try <- top_views %>% 
  pivot_longer(5:11, names_to = "feature",
               values_to = "presence") %>%
  filter(presence == TRUE) %>%
  mutate(feature_present = "",
    feature_present = ifelse(feature == "funny", map(feature_present, laugh_beam), feature_present),
    feature_present = ifelse(feature == "animals", map(feature_present, animals), feature_present),
    feature_present = ifelse(feature == "patriotic", map(feature_present, usa), feature_present),
    feature_present = ifelse(feature == "celebrity", map(feature_present, celebrity), feature_present),
    feature_present = ifelse(feature == "use_sex", map(feature_present, heart), feature_present),
    feature_present = ifelse(feature == "danger", map(feature_present, danger), feature_present)
    ) %>%
  pivot_wider(names_from = feature, values_from = feature_present) %>%
  select(
    thumbnail, 
    title, brand, year, 
    view_count, like_count, dislike_count,
    ratio_likes,  comment_count,
    funny, patriotic, celebrity, danger, animals, show_product_quickly

table <- try %>%
  gt() %>%
   fmt_number(
    columns = c("view_count", "like_count", "dislike_count", "comment_count"),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  opt_table_font(font = google_font(name = "Staatliches")) %>% #
  tab_header(
    title = "Most Viewed Superbowl Ads on YouTube",
    subtitle = "The top among 233 ads made by the ten brands that have aired the most Superbowl spots since 2000.") %>%
  tab_source_note(
    source_note = "Created by: @elianemitchll | Source: FiveThirtyEight | #TidyTuesday Week 10"
  ) %>%
  cols_label(thumbnail = "",
             title = "Title",
             brand = "Brand",
             year = "Year",
            view_count = "Views",
             like_count = "Likes",
             dislike_count = "Dislikes",
            funny = "Funny",
            patriotic = "Patriotic",
            celebrity = "Celebrity",
            danger = "Danger",
            animals = "Animals",
            comment_count = "Comments",
            ratio_likes = "Likes:Dislike") %>%
  text_transform(
    locations = cells_body(columns = vars(thumbnail)),
    fn = function(x){
      gt::web_image(x)
    }
  ) %>%
  tab_spanner(
    label = "Features",
    columns = 10:last_col()
  ) %>% 
  tab_style(
    style = cell_text(font = google_font(name = "Teko")),
    locations = cells_body(columns = c(2:last_col()))
    ) %>%
  opt_align_table_header(align = "left") %>%
  tab_options(
    table.background.color = "#013268", # #013268 = blue color
    heading.background.color = "#fbfdff", 
    source_notes.background.color = "#fbfdff", # #fbfdff = white color
    source_notes.font.size = 10,
    footnotes.background.color = "#fbfdff",
    footnotes.font.size = 12
  ) %>%
  tab_footnote(footnote = "A lower likes to dislike ratio suggests greater controversiality.",
               locations = cells_column_labels(
                 columns = vars(ratio_likes)
               ))

gtsave(table, "tabletryBLUE.png")
