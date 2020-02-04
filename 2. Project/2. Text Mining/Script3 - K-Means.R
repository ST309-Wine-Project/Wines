library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(tm)
library(proxy)
library(cluster)
library(factoextra)

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

red_reviews <- filter(wines, type == "red")
white_reviews <- filter(wines, type == "white")

red_words <- red_reviews %>%
  select(variety, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(new_stop_words) %>%
  anti_join(explicit_descriptors) %>%
  filter(!str_detect(word, "\\d")) %>%
  ungroup()

white_words <- white_reviews %>%
  select(variety, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(new_stop_words) %>%
  anti_join(explicit_descriptors) %>%
  filter(!str_detect(word, "\\d")) %>%
  ungroup()

#red_words_sample <- red_words %>%
#  filter(id %in% sample(
#    distinct(red_words, id)$id,
#    round(nrow(distinct(red_words, id)) / 60))
#  )

#white_words_sample <- white_words %>%
#  filter(id %in% sample(
#    distinct(white_words, id)$id,
#    round(nrow(distinct(white_words, id)) / 30))
#  )

red_white_words <- bind_rows(red_words, white_words) %>%
  count(variety, word) %>%
  arrange(variety, desc(n)) %>%
  rename(document = variety)

red_white_dtm <- red_white_words %>%
  cast_dtm(document, word, n)

red_white_dtm

rw_dtm_tfidf <- weightTfIdf(red_white_dtm)

rw_dtm_tfidf <- removeSparseTerms(rw_dtm_tfidf, 0.999)

tfidf_matrix <- as.matrix(rw_dtm_tfidf)

dist_matrix <- dist(tfidf_matrix, method = "cosine")

clust_kmeans <- kmeans(tfidf_matrix, 3)

points <- cmdscale(dist_matrix, k = 3) 
palette <- colorspace::diverge_hcl(3)

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
