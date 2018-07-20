la_2012 <- la %>% filter(Season == 2012) %>% select(home, visitor, FT)
la_2012 %>% tidyr::spread(visitor, FT) -> chart_12

  