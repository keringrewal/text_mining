library(tidyr)
library(tidytext)
library(syuzhet)



nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_anger <- get_sentiments("nrc") %>%
  filter("sentiment" == "anger") %>%
  filter("word" != "money")

nrc_anticipation <- get_sentiments("nrc") %>%
  filter("sentiment" == "anticipation")

