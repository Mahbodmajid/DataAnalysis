# Household final consumption expenditure per capita (constant 2010 US$)
get_indicator_gathered("NE.CON.PRVT.PC.KD", Country_Code = c("IRN"))$gathered -> iran_cfcepc
iran_cfcepc %>% 
  mutate(value = round(value, 2)) %>% 
  hchart(
  type = "line",
  hcaes(x = year, y = value),
  tooltip = list(pointFormat ="Household expenditure per capita: {point.value}  (constant 2010 US$)<br/>")
  ) %>%
  hc_title(text = "Iran's Household final consumption expenditure per capita") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Household Expenditure per capita (constant 2010 US$)"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

iran_cfcepc %>% filter(year == 2016) %>% .$value / iran_cfcepc %>% filter(year == 1966) %>% .$value

### Almost 3 times