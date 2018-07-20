get_indicator_gathered("NY.GDP.PCAP.PP.CD")$most_recent %>%
  arrange(value) %>% head(10) -> poorest

poorest %>% mutate(daily = round(value / 365, 2)) %>% hchart(
  type = "bar",
  hcaes(x = Country, y = daily),
  tooltip = list(pointFormat =
                   "Daily Income per Person: {point.daily}$<br/>")
) %>%
  hc_title(text = "Poorest") %>%
  hc_xAxis(title = list(text = "Country")) %>%
  hc_yAxis(title = list(text = "Daily Income"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())


get_indicator_gathered("SI.POV.NAHC")$most_recent %>%
  select(-Country) %>%
  right_join(poorest %>% head(10), by = c("Country_Code")) %>%
  mutate(daily = round(value.y / 365, 2), UnderPovertyLine = value.x, uplyear =year.x, pvyear = year.y) %>%
  hchart(
    type = "bar",
    hcaes(x = Country, y = UnderPovertyLine),
    tooltip = list(pointFormat =
                     "Daily Income per Person ({point.pvyear}): {point.daily}$ <br/> 
                      Under Poverty line Percent ({point.uplyear}): {point.UnderPovertyLine}%<br/>")
  ) %>%
  hc_title(text = "Poorest (Under Poverty Line Percent)") %>%
  hc_xAxis(title = list(text = "Country")) %>%
  hc_yAxis(title = list(text = "Percent"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

get_indicator_gathered("SP.DYN.LE00.IN")$most_recent %>% select(-Country) %>%
  right_join(poorest %>% head(10), by = c("Country_Code")) %>%
  mutate(daily = round(value.y / 365, 2), LE = value.x, LEyear =year.x, pvyear = year.y) %>%
  hchart(
    type = "bar",
    hcaes(x = Country, y = LE),
    tooltip = list(pointFormat =
                     "Daily Income per Person ({point.pvyear}): {point.daily}$ <br/> 
                   Life Expectancy ({point.LEyear}): {point.LE} years<br/>")
    ) %>%
  hc_title(text = "Poorest (Life Expectancy)") %>%
  hc_xAxis(title = list(text = "Country")) %>%
  hc_yAxis(title = list(text = "years"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())
