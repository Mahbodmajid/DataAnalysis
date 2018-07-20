rbinded %>%
  group_by(Date) %>%
  summarize(value = sum(`Adj Close` * Volume)) %>% 
  top_n(wt = value, n = 1) %>% 
  kable()
