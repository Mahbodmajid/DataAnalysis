library(wordcloud2)
str_replace_all(movies$Title,"(?!')[[:punct:]]", " ") %>% str_trim() -> punct_free

punct_free %>%
  str_split("\\s+") %>%
  unlist() -> words_vec

words_vec[!(str_to_lower(words_vec) %in% stop_words$word)] %>% 
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("[:digit:]{4}", " ") %>%
  str_trim() -> punct_free_2

punct_free_2 %>%
  str_split("\\s+") %>%
  unlist() %>%
  table() %>%
  as.data.frame() -> words_df

dplyr::select(words_df, word = `.`, freq = Freq)%>%
  filter(str_length(word) >= 3) %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(!str_detect(word, "\\d")) %>% 
  arrange(desc(freq))-> all_text

wordcloud2(all_text %>% top_n(n = 200))
