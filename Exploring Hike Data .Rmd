---
title: "Exploring Hike Data"
author: "Eliane Mitchell"
date: "11/27/2020"
output:
  pdf_document: default
  html_document: default
---

This week, I want to perform some basic exploratory data analysis on data from hikes in Washington state. To begin, let's load the data and take a look at what's there.

```{r}
library(tidyverse)
```

```{r}
tuesdata <- tidytuesdayR::tt_load('2020-11-24')
hike_data_raw <- tuesdata$hike_data
glimpse(hike_data_raw)

```
Let's take out extra text from these columns (especially "length") so that numerical variables are easier to manipulate later.

```{r}
hike_data <- hike_data_raw %>% mutate(across(.cols = c(length, gain, highpoint, rating), parse_number))
glimpse(hike_data)
```

I'm interested in looking more at the ratings that users have given for hikes. I'll create a histogram to see how the distribution of ratings fall.

```{r}
hike_data %>% ggplot(aes(rating)) + geom_histogram()
```

We find that there are many hikes (close to 300) that have been given exactly a 0 rating. Looking at the website where you can only search for hikes with at least a 1-star rating, it seems that these 0-rated hikes in fact are NA, they lack a rating. But we don't want to keep the 0s around, again which are technically NAs, as they would throw off other summary statistics. So I'll replace the 0s with NAs. (Ideally I would do add this command in the "chunk above")

```{r}
data <- hike_data %>% mutate(rating = ifelse(rating == 0, NA, rating))
glimpse(data)
```
Now let's look at the distribution of ratings, now without the NAs.

```{r}
rating_dist <- data %>% ggplot(aes(x = rating)) + geom_histogram()
rating_dist
```
Note that the error tells us that this histogram excludes hikes with NA ratings, which we wanted.

Although this histogram does not tell us much, what it does show is that the distribution of ratings that users give to hikes is not centered at 3, as we would hope for from a 5-star rating system. Ideally, in 5-star rating system, half of the ratings would be above 3; half of the ratings below. Here, the center is around 3.5. 

```{r}
summary(data$rating, na.rm = TRUE)

```

Looking at the summary statistics for ratings, our observation from the histogram is confirmed: 3.5 is the median rating given to a hike. The average rating is 3.509. Let's draw a vertical line demarcating the median of the ratings. 

```{r}
rating_dist + geom_vline(xintercept = median(data$rating, na.rm = TRUE), size = 0.5, color = "orange", linetype = "dashed")
```
And now the average.

```{r}
rating_dist + geom_vline(xintercept = median(data$rating, na.rm = TRUE), size = 0.5, color = "orange", linetype = "dashed") + geom_vline(xintercept = mean(data$rating, na.rm = TRUE), size = 0.5, color = "green", linetype = "dashed")
```
Note that these lines (orange for the median and green for the average) are effectively on top of each other, with the mean slightly greater than the median. Overall, we can say that the center of user ratings falls above a 3-star rating and the distribution is left-skewed.

But I am still curious about the hikes without ratings. Is there some explanation for why those hikes don't have ratings? Perhaps those hikes are more challenging, meaning that there are fewer people who have embarked on them to rate those hikes. My hypothesis is that the hikes without ratings are longer or have larger gains in feet. Let's look. 

First, we'll create another variable (a factor) that tells us whether the specified hike has a rating or doesn't.
```{r}
comp_data <- data %>% mutate(rating_exists = ifelse(is.na(rating) == TRUE,
                                                     "no_rating", "yes_rating"))
glimpse(comp_data)
```
Although we cannot see through glimpse() if the mutate function worked properly, taking a closer look at the data frame, we can see that the hikes with NA ratings indeed say "no_rating" in the rating_exists column.

Next, let's explore the distributions for hikes with ratings and without ratings. I'll first create a boxplot to show the distribution of hike lengths for hikes without and without ratings. 

```{r}
ggplot(data = comp_data, aes(x = rating_exists, y = length)) + geom_boxplot()
```
With all of the data points scrunched together, let's transform the variable into a natural logarithm so that we can see these boxplots' more subtle structure. 

```{r}
ggplot(data = comp_data, aes(x = rating_exists, y = log(length))) + geom_boxplot()

```
Comparing these boxplots, it seems that hikes without ratings are not particularly longer or shorter than hikes with ratings. We can check their summary statistics for precise numbers. 

```{r}
# summary of hike lengths within rated hikes
summary(subset(comp_data, rating_exists == "yes_rating")$length)
# summary of hike lengths within non-rated hikes
summary(subset(comp_data, rating_exists == "no_rating")$length)
```
Note that the above summary statistics are not in natural logs. 

Let's now look at gain in feet; the other half of my hypothesis that hikes without ratings may be more difficult than hikes with ratings.

```{r}
ggplot(data = comp_data, aes(x = rating_exists, y = log(gain))) + geom_boxplot()

```
Similarly, there does not seem to be a great difference in the gains in feet a hike without a rating and a hike with a rating has. 








