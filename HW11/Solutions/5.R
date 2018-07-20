disaster %>%
  group_by(COUNTRY) %>%
  filter(!is.na(TOTAL_DEATHS)) %>% 
  summarise(sum_death = sum(TOTAL_DEATHS), mean_death = round(mean(TOTAL_DEATHS),2)) %>% 
  mutate(COUNTRY = countrycode(COUNTRY, "country.name", "iso2c" )) -> countries_death_toll

hcmap("custom/world", data = countries_death_toll, value = "sum_death",
      joinBy = c("iso-a2","COUNTRY"), name = "Total Death Toll",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1) %>% 
  hc_mapNavigation(enabled = TRUE)

hcmap("custom/world", data = countries_death_toll, value = "mean_death",
      joinBy = c("iso-a2","COUNTRY"), name = "Mean Death Toll",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1) %>% 
  hc_mapNavigation(enabled = TRUE)


