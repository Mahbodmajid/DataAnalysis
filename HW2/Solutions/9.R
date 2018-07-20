samsung <- mobile  %>% 
  filter(company == "Samsung") %>% 
  group_by(year) %>% 
  top_n(n = 4, wt = price) %>% 
  ungroup()

ggplot(samsung, aes(x = year, y = price, label = device))+
  geom_text(check_overlap = T)+
  scale_x_continuous(limits  = c(2004, 2018))+
  xlab("Year")+
  ylab("Price")
