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
library(textstem)

# TO-DO

"
1. n.a.

"
# global variable definitions

wines <- read_csv("wine-data-tidied.csv") %>%
  rename('id' = 'X1')

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

cast_text_to <- function(df, by, output = "df", clusters = "") {
  
  if(class(by) == "character") {
    df <- df %>%
      select(by, description) %>%
      unnest_tokens(word, description) %>%
      rename(document = by) %>%
      anti_join(new_stop_words) %>%
      anti_join(explicit_descriptors) %>%
      filter(!str_detect(word, "\\d")) %>%
      mutate(word = lemmatize_words(word)) %>%
      count(document, word) %>%
      arrange(document, desc(n))
  }
  
  if(class(by) == "numeric") {
    
    df <- df %>%
      select(id, description, clusters) %>%
      unnest_tokens(word, description) %>%
      rename(document = id) %>%
      rename(cluster = clusters) %>%
      anti_join(new_stop_words) %>%
      anti_join(explicit_descriptors) %>%
      filter(!str_detect(word, "\\d")) %>%
      mutate(word = lemmatize_words(word))
    
    df_tally <- df %>%
      select(cluster, document) %>%
      distinct() %>%
      group_by(cluster) %>%
      mutate(rank = row_number()) %>%
      mutate(new_rank = (rank + by - 1) %/% by) %>%
      select(-rank)
    
    df <- df %>%
      left_join(df_tally) %>%
      mutate(document = str_c(cluster, new_rank)) %>%
      select(document, word) %>%
      count(document, word) %>%
      arrange(document, desc(n))
  }
  
  if (output == "dtm") {
    df <- df %>%
      cast_dtm(document, word, n)
  } else {
    return(df)
  }
}

## Prepping text objects ##

red_reviews <- filter(wines, type == "red")

variety_dtm <- red_reviews %>%
  filter(variety %in% c("Pinot Noir", "Cabernet Sauvignon", "Syrah", "Merlot")) %>%
  cast_text_to(1000, clusters = "variety", "dtm")

variety_key <- red_reviews %>%
  filter(variety %in% c("Pinot Noir", "Cabernet Sauvignon", "Syrah", "Merlot")) %>%
  select(province, variety) %>%
  distinct()

## LDA ##

keep_v = c('keep_v', 'variety_dtm', 'variety_key')
rm(list = ls()[!(ls() %in% keep_v)])

variety_lda <- LDA(variety_dtm, k = 4, control = list(seed = 2020))
variety_topics0 <- tidy(variety_lda, matrix = "beta")

# for creating our dictionary
View(variety_topics %>%
       arrange(desc(beta))
     )

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

plot_lda_top(variety_topics)

keep_v <- c(keep_v, 'variety_lda')
rm(list = ls()[!(ls() %in% keep_v)])


## K-Means

variety_dtm_tfidf <- weightTfIdf(variety_dtm)

variety_dtm_tfidf <- removeSparseTerms(variety_dtm_tfidf, 0.999)

tfidf_matrix <- as.matrix(variety_dtm_tfidf)

keep_v <- c(keep_v, 'tfidf_matrix')
keep_v <- keep_v[!keep_v %in% 'variety_dtm']
rm(list = ls()[!(ls() %in% keep_v)])


dist_matrix <- dist(tfidf_matrix, method = "cosine")

clust_kmeans <- kmeans(tfidf_matrix, 4)
clust_hierarchical <- hclust(dist_matrix, method = "ward.D2") 
clust_dbscan <- dbscan::hdbscan(dist_matrix, minPts = 10)

points <- cmdscale(dist_matrix, k = 4)
palette <- colorspace::diverge_hcl(4)

plot(points, main = 'K-Means clustering', 
     col = as.factor(clust_kmeans$cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

plot(points, main = 'Hierarchical clustering', 
     col = as.factor(cutree(clust_hierarchical, 4)), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

plot(points, main = 'Density-based clustering', 
     col = as.factor(clust_dbscan$cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')


variety_docs <- tidy(variety_lda, matrix = "gamma")

variety_docs <- variety_docs %>%
  group_by(document) %>%
  mutate(top_gamma = max(gamma)) %>%
  select(document, top_gamma) %>%
  distinct() %>%
  left_join(select(variety_docs, -document), by = c('top_gamma' = 'gamma'))

points <- cmdscale(dist_matrix, k = 4) 
palette <- colorspace::diverge_hcl(4)

plot(points, main = 'Latent Dirchlet Allocation', col = as.factor(variety_docs$topic), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

points = tibble(document = row.names(points), x = points[,1], y = points[,2])

ggplot(points, aes(x = x, y = y, colour = as.factor(variety_docs$topic))) +
  geom_point()

# hierarchical clustering

plot(clust_hierarchical, hang = -1, cex = 0.6)

# performance testing 

predictions <- tibble(
  document = variety_docs$document,
  hierarchical = cutree(clust_hierarchical, 4),
  kmeans = clust_kmeans$cluster,
  dbscan = clust_dbscan$cluster,
  lda = variety_docs$topic
)
