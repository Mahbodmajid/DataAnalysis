mobile_most_durable <- mobile %>%
  mutate(durability = battery_mah / (display_size ^ 2)) %>%
  filter(os_type == "Android" | os_type == "iOS", display_size > 4) %>% 
  group_by(year) %>%
  top_n(n = 3, wt  = durability) %>% 
  ungroup()
