mobile_ram_year <- mobile %>% 
  filter(!is.na(ram), !is.na(year)) %>% 
  group_by(year) %>%
  summarize(log_avg_ram = log2(mean(ram)))
ggplot(mobile_ram_year, aes(x = year, y = log_avg_ram)) +
  geom_point()+
  geom_smooth(method = "lm")+
  ylab(expression(log(average(ram))))+
  xlab("Year")
