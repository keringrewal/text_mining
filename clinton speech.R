library(readtext)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
clinton.1 <- readtext("clinton.txt") 
clinton.1 <- as.data.frame(clinton.1) %>% unnest_tokens(word, text) 
data(stop_words)
clinton.2 <- clinton.1 %>% anti_join(stop_words) 
clinton.3 <- clinton.2 %>% count(word, sort = TRUE)
clinton.4 <- as_tibble(clinton.3)
clinton.4 %>%
  filter(n >= 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

clinton.sentiment1 <- clinton.4 %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(clinton.sentiment1, aes(word, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)

clinton.sentiment2 <- clinton.4 %>%
  inner_join(get_sentiments("nrc")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(clinton.sentiment2, aes(word, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)

clinton.sentiment3 <- clinton.4 %>%
  inner_join(get_sentiments("afinn")) %>%
  spread(score, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(clinton.sentiment3, aes(word, score, fill = word)) +
  geom_col(show.legend = FALSE)
