WDIData %>%
  gather("year", "value", 5:62) %>%
  select(Country = `Country Name`, Country_Code = `Country Code`,Indicator = `Indicator Code`, year, value) %>%
  filter(Country_Code == "IRN") %>%
  group_by(Indicator) %>%
  summarize(how_good = sum(!is.na(value)), has_20_years = sum(!is.na(value[year <= 2016 & year >= 1997]))) %>%
  ungroup() %>% 
  left_join(WDISeries, by = c("Indicator" ="Series Code")) %>%
  .[,1:7] %>%
  filter(str_detect(Topic, "Health")) %>% 
  filter(has_20_years == 20) %>% 
  filter(!str_detect(Indicator, "SP.POP")) %>% 
  filter(Indicator != "SH.VAC.TTNS.ZS") %>% 
  arrange(desc(has_20_years),desc(how_good)) %>% 
  .$Indicator %>% 
  .[1:20]-> health_indicators


plots <- list()
for(i in health_indicators){
  indicator_name <- WDISeries %>% filter(`Series Code` == i) %>% .$`Indicator Name`
  get_indicator_gathered(i)$gathered %>% .$value %>% na.omit() -> dfry 
  ylim <- boxplot.stats(dfry)$stats[c(1, 5)]
  ggplot(get_indicator_gathered(i)$gathered, aes(x = year, y = value)) +
    geom_boxplot(fill = "3", outlier.shape = NA)+
    coord_cartesian(ylim = quantile(dfry, c(0.1, 0.9)))+
    geom_line(data = get_indicator_gathered(i)$gathered %>%
                filter(Country_Code == "IRN"), aes(x = year, y = value, group ="1"), color = "4")+
    xlab("Year") +
    ggtitle(indicator_name)+
    ggpubr::theme_classic2() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 5, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 1, 
                                     size = 5, hjust = 1),
          title = element_text(angle = 0, vjust = 1, 
                               size = 6, hjust = 1)) -> plots[[i]]
}
ggpubr::ggarrange(plotlist = plots, ncol = 5, nrow = 4)
