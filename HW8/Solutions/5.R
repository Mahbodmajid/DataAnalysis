book <- LeMiserables
str_replace_all(book$text,"(?!')[[:punct:]]", " ") %>% str_trim() -> book_punct_free

book_punct_free %>%
  str_split("\\s+") %>%
  unlist() -> book_words_vector
books_words_vec <- c(books_words_vec, book_words_vector)
book_words_vector[!(str_to_lower(book_words_vector) %in% stop_words$word)] %>% 
  str_replace_all("[[:punct:]]", " ") %>% str_trim() -> book_punct_free_2

book_punct_free_2 %>%
  str_split("\\s+") %>%
  unlist() %>%
  as.data.frame() -> book_words_df

wcount <- nrow(book_words_df)

dplyr::select(book_words_df, word = `.`)%>%
  filter(str_length(word) >= 3) %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(!str_detect(word, "\\d")) %>% 
  mutate(word = as.character(word)) %>% 
  mutate(part =  ceiling(200 * row_number() / wcount)) -> book_text

book_text %>% 
  mutate(word = tolower(word)) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(part) %>% 
  summarize(positive = sum(sentiment == "positive"), negative = sum(sentiment == "negative")) -> scores

ggplot(scores,aes(x = part)) +
  geom_bar(aes(y= positive, fill ="postive"), stat = "identity")+
  geom_bar(aes(y =  -negative, fill = "negative"), stat = "identity")+
  xlab("count") +
  ylab("part")+
  ggtitle("Le Miserables storyline")
