#### Tsunamis kill more people
##############################

disaster %>% filter(!is.na(FLAG_TSUNAMI), !is.na(TOTAL_DEATHS)) -> tsu
disaster %>% filter(is.na(FLAG_TSUNAMI), !is.na(TOTAL_DEATHS)) -> ntsu
mean(tsu$TOTAL_DEATHS)
mean(ntsu$TOTAL_DEATHS)
wilcox.test(tsu$TOTAL_DEATHS, ntsu$TOTAL_DEATHS, alternative = "greater")

#### Greatest Twin Attacks!
#### (Two EQs that oocured together and killed the most)
########################################################

disaster %>%
  select(YEAR, COUNTRY, EQ_PRIMARY, TOTAL_DEATHS) %>% 
  filter(!is.na(TOTAL_DEATHS)) %>%
  group_by(COUNTRY) %>%
  arrange(YEAR) %>%
  mutate(PREV_YEAR = lag(YEAR)) %>% 
  mutate(PREV_DEATH = lag(TOTAL_DEATHS)) %>% 
  mutate(DEATH_TOGETHER = TOTAL_DEATHS + PREV_DEATH)%>%
  mutate(PREV_EQ_PRIMARY = lag(EQ_PRIMARY)) %>% 
  filter(YEAR - PREV_YEAR <= 1) %>% 
  ungroup() %>% arrange(desc(DEATH_TOGETHER)) %>%  
  select(COUNTRY, YEAR, PREV_YEAR, DEATH_TOGETHER,
         CURR_DEATH = TOTAL_DEATHS, PREV_DEATH,
         CURR_MAG = EQ_PRIMARY, PREV_MAG = PREV_EQ_PRIMARY) %>% 
  arrange(desc(DEATH_TOGETHER)) %>% 
  head(15) %>% 
  hchart(type = "bar",
         hcaes(x = paste(COUNTRY, "(",YEAR, ")") , y = DEATH_TOGETHER),
         name = "Total deaths in two eqs",
         tooltip = list(pointFormat = "Country: {point.COUNTRY} <br/>
                        EQ1 Year: {point.PREV_YEAR} <br/>
                        EQ2 Year: {point.YEAR} <br/>
                        Death Toll: {point.DEATH_TOGETHER} <br/>
                        EQ1 Death Toll: {point.PREV_DEATH} <br/>
                        EQ2 Death Toll: {point.CURR_DEATH} <br/>
                        EQ1 Mag.: {point.PREV_MAG} <br/>
                        EQ2 Mag: {point.CURR_MAG}")) %>%
  hc_title(text = "Greatest Twin Attacks") %>% 
  hc_xAxis(title = list(text = "EQ")) %>%
  hc_yAxis(title = list(text = "Death Toll"))  %>%
  hc_add_theme(hc_theme_sandsignika())

#### Countries With Higher EQ rate have higher populations
##########################################################

library(wpp2017)
data(pop)
inner_join(x = pop %>%
            select(country_code,population = `2015`),
          y = countries_year %>%
            mutate(country_code = countrycode(country, "country.name", "iso3n")))%>% 
  select(country,population, rate = mean) -> population_rate

population_rate %>% mutate(population = round(population / 1000, 2))  %>% 
  hchart(hcaes(x= population, y = rate, size = population),name = "EQ Rate / Pop.", type = "scatter",
         tooltip = list(pointFormat = "Country: {point.country} <br/>
                        Eq Rate: {point.rate} <br/>
                        Pop.: {point.population} <br/>")) %>% 
  hc_title(text = "EQ Rate / Pop.") %>%
  hc_xAxis(title = list(text = "Population (million)")) %>%
  hc_yAxis(title = list(text = "The Same?"))  %>%
  hc_add_theme(hc_theme_sandsignika())

population_rate %>%
  mutate(population = round(population / 1000, 2)) %>% 
  group_by(rate >= 100) %>% 
  summarize(population = sum(population)) %>% 
  ungroup() %>% 
  hchart(type = "pie",
         hcaes(
           x = `rate >= 100`,
           y = population,
           name = as.character(`rate >= 100`)
         ),
         name = "Pop. (millions)") %>%
  hc_title(text = "People Living in Countries with >= 100 EQs in a year") %>%
  hc_add_theme(hc_theme_sandsignika())

cor.test(population_rate$population,
         population_rate$rate,
         method = "pearson",
         alternative = "greater")
