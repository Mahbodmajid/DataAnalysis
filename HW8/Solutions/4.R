library(tidyr)
for(id in book_ids){
  book <- books_text[[id]]
  bing_word_counts <- book %>%
    inner_join(get_sentiments("bing")) %>% group_by(sentiment) %>% 
    top_n(wt = freq, n = 20) %>% 
    ungroup()
  bing_word_counts
  book_name <- book_meta %>% filter(gutenberg_id == id) %>% .$title
  print(ggplot(bing_word_counts, aes(x = reorder(word, freq), y = freq, fill = sentiment))+
    geom_bar(stat = "identity")+
    ggtitle(book_name)+
    theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                                          size = 8, hjust = 1))+
    xlab("Word") +
    ylab("Frequency") +coord_flip()+  facet_wrap(~sentiment, scales = "free_y"))
}
