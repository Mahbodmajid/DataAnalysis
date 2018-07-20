library(sp)
library(rworldmap)

(ww$time %>% max() - ww$time %>% min()) %>% as.numeric() / 365.4 -> period

ww_country <- ww %>% filter(mag > 4)

ww_country$country <-
  ww_country %>%
  select(longitude, latitude) %>%
  coords2country() %>%
  as.character()

ww_country %>%
  group_by(country) %>%
  summarize(mean = round(n()/period, 2)) %>%
  ungroup() %>% 
  mutate(code = countrycode(country, "country.name", "iso3c")) %>%
  arrange(desc(mean)) -> countries_year

hcmap(
  "custom/world",
  data = countries_year %>% na.omit(),
  value = "mean",
  joinBy = c("iso-a3", "code"),
  name = "Avg. > 4 Richters Earthquakes in a year",
  dataLabels = list(enabled = TRUE, format = '{point.name}'),
  borderColor = "#FAFAFA",
  borderWidth = 0.1
) %>%
  hc_mapNavigation(enabled = TRUE)
# ww_country %>% select(time, mag, place, country) %>% View()

#  OTHER METHOD
########################

(ww$time %>% max() - ww$time %>% min()) %>% as.numeric() / 365.4 -> period

ww %>% filter(mag > 4) %>% 
  mutate(country = place2country(place = place)) %>% 
  mutate(code = countrycode(country, "country.name", "iso3c")) %>%
  group_by(code) %>%
  summarize(mean = round(n()/period, 2)) %>%
  ungroup() %>% 
  arrange(desc(mean)) %>% 
  mutate(country = countrycode(code, "iso3c", "country.name"))-> countries_year

hcmap(
  "custom/world",
  data = countries_year %>% na.omit(),
  value = "mean",
  joinBy = c("iso-a3", "code"),
  name = "Avg. > 4 Richters Earthquakes in a year",
  dataLabels = list(enabled = TRUE, format = '{point.name}'),
  borderColor = "#FAFAFA",
  borderWidth = 0.1
) %>%
  hc_mapNavigation(enabled = TRUE)

## HARP
############

worldmap <- get_world_map()
worldmap +
  geom_point(
    data = ww,
    aes(longitude, latitude, color = mag),
    pch = 19,
    size = 1,
    alpha = .4
  ) +
  scale_color_distiller(palette = "Spectral")+
  ggtitle("> 4.0 Ritchters Earthquakes")

