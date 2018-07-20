char_names <- data.frame()
for(id in book_ids){
  book <- books_text[[id]]
  top_guys <- book[str_which(book$word, "[A-Z ,.'-]"), ]
  new_char <- top_guys %>% filter(!word %in% c("Sir", "Lady", "Miss")) %>% top_n(5) %>% mutate(book_id = id)
  new_char$book_name <- book_meta %>% filter(gutenberg_id == id) %>% .$title
  char_names <- rbind(char_names, new_char)
}
hchart(char_names,
       type= "column",
       hcaes(group= word,
             x = book_name, y = freq))
