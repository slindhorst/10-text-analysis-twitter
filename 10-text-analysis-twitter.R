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