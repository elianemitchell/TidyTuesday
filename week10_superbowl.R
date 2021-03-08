library(tidyverse)
library(gt)
library(fontawesome)
library(htmltools)


tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube
 

## networking, applying to jobs, wonsulting, + linkedin posting
## projects: NLP project, tidytuesday, common app
## employment: escape room job (20 hours / week)
## classes: beginnr, movement school, bootcamp (soon), stats class

table(youtube$celebrity, youtube$funny)

# Two options: make a table of top 10 likes and top 10 controversial
# could heat by controversiality

top_views <- youtube %>% arrange(-view_count) %>% 
  mutate(ratio_likes = round(like_count/dislike_count, 1)) %>% # ratio of dislikes to likes
  select(thumbnail, title, brand, year, funny, show_product_quickly,
         patriotic, celebrity, danger, animals,
         use_sex, view_count, like_count, dislike_count, comment_count,
         #youtube_url, 
         ratio_likes) %>% 
 slice_max(view_count, n=5)

# Add thumbnail
top_views[7, 1] = "https://media-assets-05.thedrum.com/cache/images/thedrum-prod/s3-news-tmp-108565-bud_wassup_2--default--1125.png"


clean_names <- gsub("\\s*\\([^\\)]+\\)","",as.character(colnames(youtube)))


#let's do top likes and most controversial videos
# 1. top likes. create gt table

laugh_beam <- function(value) {
  icon <- fa("laugh-beam", fill = "#b9061f")
  div(icon) %>% 
    as.character() %>% 
    html()
}

clock <- function(value) {
  icon <- fa("clock", fill = "#b9061f")
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

heart <- function(value) {
  icon <- fa("heart", fill = "#b9061f")
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

nothing <- function(value) {
  icon <- fa("times", fill = "black")
  div(icon) %>% 
    as.character() %>% 
    html()
}

dislike_colours <- c("lightblue", "white", "thistle2")
dislike_scale <- scales::col_numeric(dislike_colours, 
                                    domain = c(min(top_views$ratio_likes),
                                               max(top_views$ratio_likes)))

try <- top_views %>% 
  pivot_longer(5:11, names_to = "feature",
               values_to = "presence") %>%
  filter(presence == TRUE) %>%
  mutate(feature_present = "",
         #feature_present = map(feature_present, nothing),
    feature_present = ifelse(feature == "funny", map(feature_present, laugh_beam), feature_present),
    feature_present = ifelse(feature == "show_product_quicky", map(feature_present, clock), feature_present),
    feature_present = ifelse(feature == "animals", map(feature_present, animals), feature_present),
    feature_present = ifelse(feature == "patriotic", map(feature_present, usa), feature_present),
    feature_present = ifelse(feature == "celebrity", map(feature_present, celebrity), feature_present),
    feature_present = ifelse(feature == "use_sex", map(feature_present, heart), feature_present),
    feature_present = ifelse(feature == "danger", map(feature_present, danger), feature_present)
    #ratio_likes = paste(as.character(ratio_likes), "likes per dislike")
    #title2 = paste('<a href="', youtube_url, '">',title,'</a>', sep="")
    ) %>%
  pivot_wider(names_from = feature, values_from = feature_present) %>%
  select(
    thumbnail, 
    title, brand, year, 
    view_count, like_count, dislike_count,
    ratio_likes,  comment_count,
    funny, patriotic, celebrity, danger, animals, show_product_quickly
         #use_sex, 
    #, youtube_url
         )
  
#try2 <- inner_join(try, trial_transmute, by= "title")


table <- try %>%
  gt() %>%
  #cols_label(brand = "Brand", title2 = "Title", like_count = "Like Count") %>%
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
    #table.font.color = "#b9061f" #changes header text
  ) %>%
  tab_footnote(footnote = "A lower likes to dislike ratio suggests greater controversiality.",
               locations = cells_column_labels(
                 columns = vars(ratio_likes)
               ))

gtsave(table, "tabletryBLUE.png", "/Users/eliane/Desktop/R Folder")

             
my_theme <- function(data) {
  tab_options(
    data = data,
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    column_labels.border.bottom.color = "black",
  )
}  


min_view <- min(top_views$view_count)
max_view <- max(top_views$view_count)

try$title2

top_ratio <- max(top_views$ratio_likes)
bottom_ratio <- min(top_views$ratio_likes)

top_dislike <- max(top_views$dislike_count)
bottom_dislike <- min(top_views$dislike_count)


min(top_views$dislike_count)
max(top_views$dislike_count)

#clean_names <- gsub("\\s*\\([^\\)]+\\)","",as.character(colnames(try)))
#col_names <- clean_names %>% str_remove_all("character")

clean_names <- gsub("\\_"," ",as.character(colnames(try)))



top_likes %>% across(7:12)

library(stringr)

ggsave("/Users/eliane/Desktop/R Folder/superbowl table.png")
  