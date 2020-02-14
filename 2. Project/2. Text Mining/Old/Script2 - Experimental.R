# LDA - Latent Dirchlet Allocation

red_white_key <- wines %>%
  select(id, type) %>%
  filter(str_detect(type, "red|white"))

red_white_dtm <- red_reviews %>%
  bind_rows(white_reviews) %>%
  select(id, description) %>%
  prep_bigrams() %>%
  count(id, word) %>%
  arrange(id, desc(n)) %>%
  rename(document = id) %>%
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