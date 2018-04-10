library(readtext)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(reshape2)

eisenhower.1 <- readtext("eisenhower.txt") 
eisenhower.1 <- as.data.frame(eisenhower.1) %>% unnest_tokens(word, text)
data(stop_words)
eisenhower.2 <- eisenhower.1 %>% anti_join(stop_words) 
eisenhower.3 <- eisenhower.2 %>% count(word, sort = TRUE)
eisenhower.4 <- as_tibble(eisenhower.3)
eisenhower.4 %>%
  filter(n >= 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

eisenhower.sentiment1 <- eisenhower.4 %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(eisenhower.sentiment1, aes(word, sentiment, fill = word)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

eisenhower.sentiment2 <- eisenhower.4 %>%
  inner_join(get_sentiments("nrc")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(eisenhower.sentiment2, aes(word, sentiment, fill = word)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

eisenhower.sentiment3 <- eisenhower.4 %>%
  inner_join(get_sentiments("afinn"))

ggplot(eisenhower.sentiment3, aes(word, score, fill = word)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

eisenhower.sentiment4 <- eisenhower.4 %>%
  inner_join(get_sentiments("nrc")) %>%
  spread(sentiment, n, fill = 0) %>% 
  select("word", "joy", "anger", "anticipation")