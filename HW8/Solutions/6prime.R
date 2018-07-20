for (book in books) {
  str_replace_all(book$text,"(?!')[[:punct:]]", " ") %>% str_trim() -> book_punct_free
  
  book_punct_free %>%
    str_split("\\s+") %>%
    unlist() -> book_words_vector
  
  book_words_vector[!(str_to_lower(book_words_vector) %in% stop_words$word)] %>% 
    str_replace_all("[[:punct:]]", " ") %>% str_trim() -> book_punct_free_2
  
  book_punct_free_2 %>%
    str_split("\\s+") %>%
    unlist() %>%
    as.data.frame() -> book_words_df
  
  book$gutenberg_id[1] -> id
  book_name <- book_meta %>% filter(gutenberg_id == id) %>% .$title
  
  rbind(all_dickens,dplyr::select(book_words_df, word = `.`)%>%
          filter(str_length(word) >= 3) %>% 
          filter(!str_to_lower(word) %in% stop_words$word) %>% 
          filter(!str_detect(word, "\\d")) %>% 
          mutate(word = as.character(word)) %>% 
          mutate(name = book_name)) -> all_dickens
}