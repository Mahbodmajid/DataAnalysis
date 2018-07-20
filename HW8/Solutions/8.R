by_chapter <- all_dickens %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0)



###Unigrams

Dickens_unigrams <- by_chapter %>%
  unnest_tokens(word, text, token = "ngrams", n = 1)

unigrams_filtered <- Dickens_unigrams %>%
  filter(!word %in% stop_words$word)

top_unigrams <- unigrams_filtered %>% 
  count(word, sort = TRUE) %>% 
  rename(count = n) %>% 
  top_n(wt = count, n = 20)

unigrams_counts <- unigrams_filtered %>% 
  filter(word %in% top_unigrams$word) %>% 
  group_by(book, chapter) %>% 
  count(word, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(word, count) %>%
  unite(document, book , chapter)
unigrams_counts[is.na(unigrams_counts)] <- 0

chisq.test(as.matrix(unigrams_counts %>% select(-document)))

unigrams_counts_gathered <- unigrams_counts %>%
  gather(key = word, value = count, -document)

top_documents <- unigrams_counts_gathered %>% 
  group_by(document) %>% 
  summarize(count = sum(count)) %>%
  ungroup() %>% top_n(n = 20) %>% .$document

ggplot(unigrams_counts_gathered %>% filter(document %in% top_documents),
       aes(x = reorder(word, count), y = count, fill = document))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Unigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Unigrams In Top Dickens Chapters")+
  coord_flip()+  facet_wrap(~document, scales = "free")

####Bigrams

Dickens_bigrams <- by_chapter %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- Dickens_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

top_bigrams <- bigrams_united %>% 
  count(bigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  top_n(wt = count, n = 20)

bigrams_counts <- bigrams_united%>% 
  filter(bigram %in% top_bigrams$bigram) %>% 
  group_by(book, chapter) %>% 
  count(bigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(bigram, count) %>%
  unite(document, book , chapter)
bigrams_counts[is.na(bigrams_counts)] <- 0

chisq.test(as.matrix(bigrams_counts %>% select(-document)))

bigrams_counts_gathered <- bigrams_counts %>%
  gather(key = bigram, value = count, -document)

top_documents <- bigrams_counts_gathered %>% 
  group_by(document) %>% 
  summarize(count = sum(count)) %>%
  ungroup() %>% top_n(n = 20) %>% .$document

ggplot(bigrams_counts_gathered %>% filter(document %in% top_documents),
       aes(x = reorder(bigram, count), y = count, fill = document))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Bigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Bigrams In Top Dickens Chapters")+
  coord_flip()+  facet_wrap(~document, scales = "free_x")




####Trigrams

Dickens_trigrams <- by_chapter %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

trigrams_separated <- Dickens_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)
  

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")
top_trigrams <- trigrams_united %>% 
  count(trigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  arrange(desc(count)) %>% 
  head(20)

trigrams_counts <- trigrams_united%>% 
  filter(trigram %in% top_trigrams$trigram) %>% 
  group_by(book, chapter) %>% 
  count(trigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(trigram, count) %>%
  unite(document, book , chapter)
trigrams_counts[is.na(trigrams_counts)] <- 0

chisq.test(as.matrix(trigrams_counts %>% select(-document)))

trigrams_counts_gathered <- trigrams_counts %>%
  gather(key = trigram, value = count, -document)

top_documents <- trigrams_counts_gathered %>% 
  group_by(document) %>% 
  summarize(count = sum(count)) %>%
  ungroup() %>% top_n(n = 20) %>% .$document

ggplot(trigrams_counts_gathered %>% filter(document %in% top_documents),
       aes(x = reorder(trigram, count), y = count, fill = document))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Trigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Trigrams In Top Dickens Chapters")+
  coord_flip()+  facet_wrap(~document, scales = "free_x")

