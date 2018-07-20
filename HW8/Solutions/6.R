
all_dickens <- data.frame()
for (book in books) {
  
  book$gutenberg_id[1] -> id
  book_name <- book_meta %>% filter(gutenberg_id == id) %>% .$title
  
  rbind(all_dickens,book %>% 
          mutate(book = book_name)) -> all_dickens
}

dickens_bigrams <- all_dickens %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- dickens_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_counts <- bigrams_united %>% 
  count(bigram, sort = TRUE) %>% 
  head(30) -> top_bigrams

ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n, fill = n))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Bigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Charles Dickens Novels Bigrams")+
  coord_flip()
