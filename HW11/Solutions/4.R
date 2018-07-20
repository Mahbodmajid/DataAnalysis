iequake %>% 
  select(time = OriginTime, mag = Mag) %>%
  as.data.frame() %>% 
  filter(mag >= 7) %>% 
  mutate(year = as.numeric(format(time,"%Y")), month = as.numeric(format(time,"%m"))) %>% 
  select(-mag, -time)-> iequake_big

disaster %>% 
  filter(COUNTRY == "IRAN") %>% 
  select(year = YEAR, month = MONTH, mag = EQ_PRIMARY) %>% 
  filter(mag >= 7) %>% 
  as.data.frame() %>% 
  select(-mag)-> dis_iran

union(dis_iran, iequake_big) %>%
  as.data.frame() %>% 
  arrange(-year, -month) %>% 
  mutate(month_from_before = month - lead(month) + 12 * (year - lead(year))) %>%
  filter(year >= 1900) -> last_eqs

last_eqs[1,] %>% kable()

12*(as.numeric(format(Sys.time(),"%Y"))- last_eqs[1,]$year) + 
  as.numeric(format(Sys.time(),"%m")) - last_eqs[1,]$month -> months_past_from_last

(months_past_from_last + 60) -> desired_distance

last_eqs$month_from_before -> distances
paste0("probability: ", round(sum(distances <= desired_distance) / sum(!is.na(distances)),2))

       