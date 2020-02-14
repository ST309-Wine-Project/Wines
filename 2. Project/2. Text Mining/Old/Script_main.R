# SETUP

library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(gridExtra)
library(tm)
library(proxy)
library(cluster)
library(ggrepel)
library(corpus)
library(rvest)

# TO-DO

"
1. n.a.

"
# global variable definitions

wines0 <- read_csv("wine-data-tidied.csv") %>%
  rename('id' = 'X1')

wines <- wines0

custom_stop_words <- c(
  "wine",
  "bottle",
  "aromas",
  "aroma",
  "flavors",
  "finish",
  "palate",
  "drink",
  "nose",
  "notes",
  "offers",
  "hint",
  "slightly",
  "character",
  "hint",
  "mouth",
  "note",
  "mouthfeel",
  "touch",
  "texture",
  "flavor",
  "color",
  "structure",
  "delicious",
  "makes",
  "scents",
  "pair",
  "glass"
)
 
new_stop_words <- bind_rows(
  stop_words,
  tibble(
    word = custom_stop_words, 
    lexicon = rep("CUSTOM", length(custom_stop_words))
    )
)

explicit_descriptors <- wines %>%
  unite("text", variety, type, province, region_1, region_2, sep = " ") %>%
  select(text) %>%
  unnest_tokens(word, text)

# generic functions

cast_text_to <- function(df, by, output = "df") {
  df <- df %>%
    select(by, description) %>%
    unnest_tokens(word, description) %>%
    rename(document = by) %>%
    anti_join(new_stop_words) %>%
    anti_join(explicit_descriptors) %>%
    filter(!str_detect(word, "\\d")) %>%
    count(document, word) %>%
    arrange(document, desc(n)) 
  
  if (output == "dtm") {
    df <- df %>%
      cast_dtm(document, word, n)
  } else {
      return(df)
    }
}

cast_text_to_ <- function(df, by, output = "df") {
  df <- df %>%
    select(by, description) %>%
    unnest_tokens(word, description) %>%
    rename(document = by) %>%
    anti_join(new_stop_words) %>%
    anti_join(explicit_descriptors) %>%
    filter(!str_detect(word, "\\d")) %>%
    mutate(word = as.character(text_tokens(word, stemmer = "en"))) %>%
    count(document, word) %>%
    arrange(document, desc(n))
  
  if (output == "dtm") {
    df <- df %>%
      cast_dtm(document, word, n)
  } else {
    return(df)
  }
}

## Prepping text objects ##

red_reviews <- filter(wines, type == "red")
white_reviews <- filter(wines, type == "white")

red_white_dtm <- red_reviews %>%
  bind_rows(white_reviews) %>%
  cast_text_to('variety', "dtm")

rw_stems_dtm <- red_reviews %>%
  bind_rows(white_reviews) %>%
  cast_text_to_('variety', 'dtm')

test <- bind_rows(red_reviews, white_reviews) %>%
  cast_text_to('variety', 'df') %>%
  select(word) %>%
  count(word)

## LDA ##

red_white_lda <- LDA(red_white_dtm, k = 2, control = list(seed = 2020))
red_white_topics <- tidy(red_white_lda, matrix = "beta")

rw_stems_lda <- LDA(rw_stems_dtm, k = 2, control = list(seed = 2020))
rw_stems_topics <- tidy(rw_stems_lda, matrix = "beta")

# graph top terms

plot_lda_top <- function(df) {
  df %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered()
}

plot_lda_top(red_white_topics)
plot_lda_top(rw_stems_topics)

#graph by term spread

plot_lda_spread <- function(df) {
  df %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1)) %>%
    arrange(log_ratio) %>%
    top_n(30, abs(log_ratio)) %>%
    ggplot(aes(reorder(term, log_ratio), log_ratio)) +
    geom_col() +
    coord_flip()
}

plot_lda_spread(red_white_topics)
plot_lda_spread(rw_stems_topics)

#plotting LDA betas

plot_lda_betas <- function(df) {
  df %>%
    top_n(100, abs(beta)) %>%
    spread(topic, beta) %>%
    rename(topic1 = '1', topic2 = '2') %>%
    drop_na() %>%
    mutate(type = ifelse(topic1 > topic2, 'red', 'white')) %>%
    ggplot(aes(topic1, topic2, colour = type)) +
    geom_point() +
    geom_text_repel(aes(label = term))
}

plot_lda_betas(red_white_topics)
plot_lda_betas(rw_stems_topics)

## K-Means


rw_dtm_tfidf <- weightTfIdf(red_white_dtm)

rw_dtm_tfidf <- removeSparseTerms(rw_dtm_tfidf, 0.999)

tfidf_matrix <- as.matrix(rw_dtm_tfidf)

dist_matrix <- dist(tfidf_matrix, method = "cosine")

clust_kmeans <- kmeans(tfidf_matrix, 2)

points <- cmdscale(dist_matrix, k = 2) 
palette <- colorspace::diverge_hcl(2)

plot(points, main = 'K-Means clustering', col = as.factor(clust_kmeans$cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

prediction <- tibble(variety = names(clust_kmeans$cluster), 
                     cluster = clust_kmeans$cluster) %>%
  left_join(
    wines %>%
      select(variety, type) %>%
      distinct()
  ) %>%
  mutate(true_type = ifelse(type == "red", 2, 1))

table(prediction$cluster, prediction$true_type)

red_white_docs <- tidy(red_white_lda, matrix = "gamma") 

red_white_docs <- red_white_docs %>%
  mutate(topic = str_c("topic", topic)) %>%
  spread(topic, gamma) %>%
  mutate(cluster = ifelse(topic1 > topic2, 1, 2)) %>%
  mutate(spread = abs(topic1 - topic2))

points <- cmdscale(dist_matrix, k = 2) 
palette <- colorspace::diverge_hcl(2)

plot(points, main = 'Latent Dirchlet Allocation', col = as.factor(as.factor(red_white_docs$cluster)), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

points = tibble(document = row.names(points), x = points[,1], y = points[,2])

ggplot(points, aes(x = x, y = y, colour = red_white_docs$cluster, alpha = red_white_docs$spread)) +
  geom_point()

prediction2 <- tibble(variety = red_white_docs$document, 
                     cluster = red_white_docs$cluster) %>%
  left_join(
    wines %>%
      select(variety, type) %>%
      distinct()
  ) %>%
  mutate(true_type = ifelse(type == "red", 1, 2))

table(prediction2$cluster, prediction2$true_type)
