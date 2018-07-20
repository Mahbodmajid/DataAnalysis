books = list(BarnabyRudge, BleakHouse, DavidCopperfield, DombeyandSon, GreatExpectations,
             HardTimes, LittleDorrit, MartinChuzzlewit, NicholasNickleby, OliverTwist, OurMutualFriend,
             TheMysteryofEdwinDrood, TheOldCuriosityShop, ThePickwickPapers, ATaleofTwoCities
             )

books_text <- list()
books_words_vec <- c()
for (book in books) {
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
    table() %>%
    as.data.frame() -> book_words_df
  book$gutenberg_id[1] -> id
  
  dplyr::select(book_words_df, word = `.`, freq = Freq)%>%
    filter(str_length(word) >= 3) %>% 
    filter(!str_to_lower(word) %in% stop_words$word) %>% 
    filter(!str_detect(word, "\\d")) %>% 
    mutate(word = as.character(word)) %>% 
    arrange(desc(freq))-> books_text[[id]]
}


books_words_vec[!(str_to_lower(books_words_vec) %in% stop_words$word)] %>% 
  str_replace_all("[[:punct:]]", " ") %>% str_trim() -> book_punct_free_2

book_punct_free_2 %>%
  str_split("\\s+") %>%
  unlist() %>%
  table() %>%
  as.data.frame() -> book_words_df

dplyr::select(book_words_df, word = `.`, freq = Freq)%>%
  filter(str_length(word) >= 3) %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(!str_detect(word, "\\d")) %>% 
  arrange(desc(freq))-> all_books_text

all_books_text %>% top_n(n = 20) %>% arrange(desc(freq)) %>% as.matrix() %>% as.data.frame()-> top_words
top_words$freq %>% as.character() %>% as.numeric() -> top_words$freq
ggplot(data = top_words,
       aes(
          x = reorder(word, freq),
          y = freq,
          fill = freq
))+
  geom_bar(stat = "identity")+
  guides(fill = F) +
  xlab("Word Count") +
  ylab("Frequency") +
  ggtitle("Words") + coord_flip()
