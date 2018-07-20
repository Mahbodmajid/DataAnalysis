###Jane Austen Books
library(janeaustenr)

all_Austin <- austen_books()
by_chapter <- all_Austin %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0)



###Unigrams

Austin_unigrams <- by_chapter %>%
  unnest_tokens(word, text, token = "ngrams", n = 1)

unigrams_filtered <- Austin_unigrams %>%
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
  ungroup() %>% top_n(n = 19) %>% .$document

ggplot(unigrams_counts_gathered %>% filter(document %in% top_documents),
       aes(x = reorder(word, count), y = count, fill = document))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Unigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Unigrams In Top Austin Chapters")+
  coord_flip()+  facet_wrap(~document, scales = "free")

####Bigrams

Austin_bigrams <- by_chapter %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- Austin_bigrams %>%
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
  ungroup() %>% top_n(n = 19) %>% .$document

ggplot(bigrams_counts_gathered %>% filter(document %in% top_documents),
       aes(x = reorder(bigram, count), y = count, fill = document))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Bxigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Bigrams In Top Austin Chapters")+
  coord_flip()+  facet_wrap(~document, scales = "free_x")











###### versus

together <- rbind(all_Austin %>% mutate(writer = "Austin"),
                  all_dickens %>% mutate(writer = "Dickens") %>% select(-gutenberg_id))


###Unigrams

Versus_unigrams <- together %>%
  unnest_tokens(word, text, token = "ngrams", n = 1)

unigrams_filtered <- Versus_unigrams %>%
  filter(!word %in% stop_words$word)

top_unigrams <- unigrams_filtered %>% 
  count(word, sort = TRUE) %>% 
  rename(count = n) %>% 
  top_n(wt = count, n = 20)

unigrams_counts <- unigrams_filtered %>% 
  filter(word %in% top_unigrams$word) %>% 
  group_by(writer) %>% 
  count(word, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(word, count) %>% 
  ungroup()

unigrams_counts[is.na(unigrams_counts)] <- 0
chisq.test(as.matrix(unigrams_counts %>% select(-writer)))

unigrams_counts_gathered <- unigrams_counts %>%
  gather(key = word, value = count, -writer)

ggplot(unigrams_counts_gathered,
       aes(x = reorder(word, count), y = count, fill = writer))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Unigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Unigrams")+
  coord_flip()+  facet_wrap(~writer, scales = "free")

####Bigrams

Versus_bigrams <- together %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- Versus_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

top_bigrams <- bigrams_united %>% 
  count(bigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  top_n(wt = count, n = 50)

bigrams_counts <- bigrams_united%>% 
  filter(bigram %in% top_bigrams$bigram) %>% 
  group_by(writer) %>% 
  count(bigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(bigram, count) %>%
  ungroup()
bigrams_counts[is.na(bigrams_counts)] <- 0

chisq.test(as.matrix(bigrams_counts %>% select(-writer)))

bigrams_counts_gathered <- bigrams_counts %>%
  gather(key = bigram, value = count, -writer)

ggplot(bigrams_counts_gathered,
       aes(x = reorder(bigram, count), y = count, fill = writer))+
  geom_bar(stat = "identity")+
  theme(axis.text.y = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  xlab("Bigram") +
  guides(fill = "none")+
  ylab("Frequency") +
  ggtitle("Bigrams")+
  coord_flip()+  facet_wrap(~writer, scales = "free_x")
