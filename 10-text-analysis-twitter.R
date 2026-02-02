#install packages
install.packages("textdata")

# Import library
library(tidyverse)
library(here)
library(dataedu)
library(tidytext)

raw_tweets <- dataedu::tt_tweets
# quickly preview rows
glimpse(raw_tweets)

# Process and clean data

tweets <-
  raw_tweets %>% 
  #filter for English tweets
  filter(lang == "en") %>% 
  select(status_id, text) %>% 
  # Convert the ID field to the character data type
  mutate(status_id = as.character(status_id))

tokens <-
  tweets %>% 
  unnest_tokens(output = word, input = text)

tokens

data(stop_words)

tokens <-
  tokens %>% 
  anti_join(stop_words, by = "word")

# Begin analysis counting words
tokens %>% 
  count(word, sort = TRUE)

#Calculate percentages of words in the data set
tokens %>% 
  count(word, sort = TRUE) %>% 
  # n as a percent of total words
  mutate(percent = n / sum(n) * 100)

#view some words related to sentiment analysis
nrc_sentiments <- get_sentiments("nrc")

# Only positive in the NRC data set
nrc_pos <-
  nrc_sentiments %>% 
  filter(sentiment == "positive")

# Match to tokens
pos_tokens_count <-
  tokens %>% 
  inner_join(nrc_pos, by = "word") %>% 
  count(word, sort = TRUE)

pos_tokens_count

pos_tokens_count %>% 
  #only words that appear 75 times or more
  filter(n >= 75) %>% 
  ggplot(., aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = dataedu_colors("darkblue")) +
  labs(
    title = "Count of Words Associated with Positivity",
    subtitle = "Tweets with the hashtag #tidytuesday",
    caption = "Data: Twitter and NRC",
    x = "",
    y = "Count"
  ) +
  theme_dataedu()

# look for word "dataviz" in tokens and filter
dv_tokens <-
  tokens %>% 
  filter(word == "dataviz")
dv_tokens

#extract status_id

head(dv_tokens$status_id)

# create a vector that only has positive words using filter
pos_tokens <-
  tokens %>% 
  filter(word %in% nrc_pos$word)

#extract status_id
head(pos_tokens$status_id)

#remove duplicates using distinct
pos_tokens <-
  pos_tokens %>% 
  distinct(status_id)

#remove duplicates from dv_tokens also

dv_tokens <- 
  dv_tokens %>% 
  distinct(status_id)

dv_pos <- 
  tweets %>% 
  # Only tweets that have the dataviz status_id
  filter(status_id %in% dv_tokens$status_id) %>% 
  # Is the status_id from our vector of positive word?
  mutate(positive = if_else(status_id %in% pos_tokens$status_id, 1, 0))

# what percent of tweets had "dataviz" and also a positive word?
dv_pos %>% 
  count(positive) %>% 
  mutate(perc = n / sum(n))

# Filter and keep only positive tweets
pos_tweets <- 
  tweets %>% 
  mutate(positive = if_else(status_id %in% pos_tokens$status_id, 1, 0)) %>% 
  filter(positive == 1)

tweets %>% 
  slice(1, 3)

# Random selection in base R example
sample(x = 1:50, size = 5)

#random selection using new versions of dplyr
set.seed(2020)

pos_tweets %>% 
  slice_sample(n = 10)
