# SETUP

library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(gridExtra)
library(igraph)
library(ggraph)

# TO-DO

"
1. n.a.

"
# global variable definitions

wines0 <- read_csv("wine-data-tidied.csv") %>%
  rename('id' = 'X1')

wines <- wines0

custom_stop_words <- c(
  "wine"
)
  
new_stop_words <- bind_rows(
  stop_words,
  tibble(
    word = custom_stop_words, 
    lexicon = rep("CUSTOM", length(custom_stop_words))
    )
)

# generic functions

prep_bigrams <- function(df) {
  df <- df %>%
    unnest_tokens(word, description, token = "ngrams", n = 2) %>%
    filter(!str_detect(word, "\\d")) %>%
    separate(word, sep = " ", into = c("word1", "word2")) %>%
    anti_join(new_stop_words, by = c("word1" = "word")) %>%
    anti_join(new_stop_words, by = c("word2" = "word")) %>%
    unite('word', word1, word2, sep = " ")
}

prep_words <- function(df) {
  df <- df %>%
    unnest_tokens(word, description) %>%
    filter(!str_detect(word, "\\d")) %>%
    anti_join(new_stop_words)
}

top_n_words <- function(df, num, var, col) {
  graph <- df %>%
    filter(variety %in% var) %>%
    select(id, word) %>%
    count(word) %>%
    top_n(num) %>%
    ggplot(aes(reorder(word, n), n)) +
    geom_col(fill = col) +
    coord_flip()
  return(graph)
}

### ! text EDA ! ###

red_reviews <- filter(wines, type == "red")
white_reviews <- filter(wines, type == "white")

red_words <- red_reviews %>% prep_words
white_words <- white_reviews %>% prep_words

library(wordcloud)

red_cloud <- red_words %>%
  select(word) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 40, scale = c(1,3), colors = "darkred"))

white_cloud <- white_words %>%
  select(word) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 40, scale = c(1,3), colors = "goldenrod2"))

#bigrams node graphs

#red
red_node_graph <- red_reviews %>%
  prep_bigrams %>%
  count(word) %>%
  select(word, n) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(n > 400) %>%
  arrange(desc(n)) %>%
  graph_from_data_frame()

set.seed(2019)

a <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

ggraph(red_node_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a, end_cap = circle(0.03, "inches")) +
  geom_node_point(colour = "darkred", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void()

#white
white_node_graph <- white_reviews %>%
  prep_bigrams %>%
  count(word) %>%
  select(word, n) %>%
  separate(word, into = c("word1", "word2"), sep = " ") %>%
  filter(n > 150) %>%
  arrange(desc(n)) %>%
  graph_from_data_frame()

set.seed(2019)

a <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

ggraph(white_node_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a, end_cap = circle(0.03, "inches")) +
  geom_node_point(colour = "goldenrod2", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void()

## NOTES
"
REMEMBER: sort out wine's own title when analysing its text
"
  