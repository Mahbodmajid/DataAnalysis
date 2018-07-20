get_indicator_gathered("SP.DYN.LE00.IN")$most_recent %>% select(-Country) %>%
  right_join(get_indicator_gathered("SH.XPD.CHEX.PP.CD")$most_recent,
             by = c("Country_Code")) %>% 
mutate(LEyear = year.x, LE = round(value.x,2), HEyear = year.y, HE = round(value.y,2)) -> LE_HE

LE_HE %>% 
hchart(
  type = "scatter",
  hcaes(x = HE, y = LE),
  name = "Health Expenditture per Capita / Life Expectancy",
  tooltip = list(pointFormat ="<b> {point.Country} <b/> <br/>
                 Life Expectancy ({point.LEyear}): {point.LE} years<br/>
                 Health Expenditture per Capita ({point.HEyear}): {point.HE}$<br/>")
  ) %>%
  hc_title(text = "Life Expectancy / Health Expenditture per Capita") %>%
  hc_xAxis(title = list(text = "Health Expenditure")) %>%
  hc_yAxis(title = list(text = "Life Expectancy"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

LE_HE %>% 
  hchart(
    type = "scatter",
    hcaes(x = round(log(HE),2), y = LE),
    name = "Logarithm of Health Expenditture per Capita / Life Expectancy",
    tooltip = list(pointFormat ="<b> {point.Country} <b/> <br/>
                   Life Expectancy ({point.LEyear}): {point.LE} years<br/>
                   Logarithm of Health Expenditture per Capita ({point.HEyear}): {point.HE} log($)<br/>")
    ) %>%
  hc_title(text = "Life Expectancy / Logarithm of Health Expenditture per Capita") %>%
  hc_xAxis(title = list(text = "Logarithm of Health Expenditure")) %>%
  hc_yAxis(title = list(text = "Life Expectancy"))  %>%
  hc_tooltip(crosshairs = T) %>%
  hc_add_theme(hc_theme_sandsignika())

cor.test(log(LE_HE$HE), LE_HE$LE)

### Life Expectancy andLogarithm of Health Expenditture per Capita have linear relation
