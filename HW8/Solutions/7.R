bigrams_separated %>% filter(word1 == "he" | word1 == "she") -> bigrams_he_she
bigram_he_she <- bigrams_he_she %>%
  count(word1, word2, sort = TRUE) %>% 
  group_by(word1) %>% 
  top_n(wt = n, n = 20)-> top_verbs

ggplot(top_verbs, aes(x = reorder(word2, n), y = n, fill = word1))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Verb") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Charles Dickens Novels Verbs")+
  coord_flip()+  facet_wrap(~word1, scales = "free")
