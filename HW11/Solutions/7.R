ww %>%
  mutate(lat = round(latitude, 0), long = round(longitude, 0)) %>% 
  select(time, lat, long, mag) %>% 
  arrange(time) -> ww7

ww7 %>% mutate(days_from_beginning =
                 round(as.numeric(time- ww7$time[1])/(24*60*60*3),0)) %>%  
  group_by(lat, long, round(days_from_beginning/3, 0)) %>% 
  summarise(greatest = max(mag),
            greatest_time= time[which.max(mag)],
            first = min(time),
            count = n()) %>% 
  ungroup() %>% 
  filter(greatest >= 4.5) %>% 
  filter(count > 6) %>% 
  mutate(distance = as.numeric(greatest_time - first)) %>% 
  select(distance) %>%
  as.data.frame() -> pre_post

ggplot(pre_post, aes(x = distance / 60 / 60 )) + 
  geom_density() +
  ggtitle("Distance Distribution") + 
  xlab("Distance (hours)") + 
  ylab("density")
