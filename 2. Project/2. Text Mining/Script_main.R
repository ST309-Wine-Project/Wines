# SETUP

library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(gridExtra)
library(igraph)
library(ggraph)
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
  "bottle"
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

### ! text EDA ! ###

red_reviews <- filter(wines, type == "red")
white_reviews <- filter(wines, type == "white")

red_words <- red_reviews %>% prep_words
white_words <- white_reviews %>% prep_words



# LDA - Latent Dirchlet Allocation

red_white_key <- wines %>%
  select(variety, type) %>%
  filter(str_detect(type, "red|white"))

red_white_words <- red_reviews %>%
  bind_rows(white_reviews) %>%
  select(variety, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(new_stop_words) %>%
  anti_join(explicit_descriptors) %>%
  filter(!str_detect(word, "\\d")) %>%
  count(variety, word) %>%
  arrange(variety, desc(n)) %>%
  rename(document = variety)

red_white_stems <- red_reviews %>%
  bind_rows(white_reviews) %>%
  select(variety, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(new_stop_words) %>%
  anti_join(explicit_descriptors) %>%
  filter(!str_detect(word, "\\d")) %>%
  rename(document = variety, text = word)

red_white_stems$text = as.character(text_tokens(red_white_stems, stemmer = "en")) 

red_white_stems <- red_white_stems %>%
  rename(word = text) %>%
  count(document, word) %>%
  arrange(document, desc(n))

red_white_dtm <- red_white_words %>%
  cast_dtm(document, word, n)

red_white_lda <- LDA(red_white_dtm, k = 2, control = list(seed = 2020))

red_white_topics <- tidy(red_white_lda, matrix = "beta")

# graph top terms
red_white_topics %>%
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

#graph by term spread
red_white_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(log_ratio) %>%
  top_n(30, abs(log_ratio)) %>%
  ggplot(aes(reorder(term, log_ratio), log_ratio)) +
  geom_col() +
  coord_flip()

#plotting LDA betas

red_white_topics %>%
  top_n(100, abs(beta)) %>%
  spread(topic, beta) %>%
  rename(topic1 = '1', topic2 = '2') %>%
  drop_na() %>%
  mutate(type = ifelse(topic1 > topic2, 'red', 'white')) %>%
  ggplot(aes(topic1, topic2, colour = type)) +
  geom_point() +
  geom_text_repel(aes(label = term))


## K-Means

rw_dtm_tfidf <- weightTfIdf(red_white_dtm)

rw_dtm_tfidf <- removeSparseTerms(rw_dtm_tfidf, 0.999)

tfidf_matrix <- as.matrix(rw_dtm_tfidf)

dist_matrix <- dist(tfidf_matrix, method = "cosine")

clust_kmeans <- kmeans(tfidf_matrix, 5)

points <- cmdscale(dist_matrix, k = 5) 
palette <- colorspace::diverge_hcl(5)

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

## NOTES
"
REMEMBER: sort out wine's own title when analysing its text
"
