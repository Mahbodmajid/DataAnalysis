WDIData %>%
  gather("year", "value", 5:62) %>%
  select(Country = `Country Name`, Country_Code = `Country Code`,Indicator = `Indicator Code`, year, value) %>% 
  filter(Country_Code == "IRN") %>%
  group_by(Indicator) %>% 
  summarize(how_good = sum(!is.na(value)), has_20_years = sum(!is.na(value[year <= 2016 & year >= 1997]))) %>%
  ungroup() %>% 
  left_join(WDISeries, by = c("Indicator" ="Series Code")) %>% 
  .[,1:7] %>% 
  filter(str_detect(`Indicator Name`, "(annual %)")) %>%
  filter(!str_detect(`Indicator Name`, "per")) %>% 
  arrange(desc(has_20_years),desc(how_good)) %>%
  select(Indicator, `Indicator Name`) -> selected_indicators

#selected_indicators %>% View()

selected_indicators$Indicator[c(-11)] %>% .[1:20]-> economic_indicators

#economic_indicators
## "NY.GDP.MKTP.KD.ZG" : GDP annual growth
## ATTENTION: (annual % growth) 
## Economic Growth is NY.GDP.MKTP.KD.ZG
## Economic growth is the increase in the inflation-adjusted market value of the
## goods and services produced by an economy over time.
## It is conventionally measured as the percent rate of increase in real gross domestic product,
## or real GDP

# c("FP.CPI.TOTL",
# "BX.GRT.EXTA.CD.WD",
# "DT.ODA.ALLD.KD",
# "FP.CPI.TOTL.ZG",
# "NE.CON.GOVT.KD",
# "NE.DAB.TOTL.KD",
# "NE.CON.TOTL.KD",
# "NE.EXP.GNFS.KD",
# "NE.GDI.FTOT.KD",
# "NE.IMP.GNFS.KD",
# "NV.AGR.TOTL.KD",
# "NV.IND.MANF.KD",
# "NV.IND.TOTL.KD",
# "NV.SRV.TETC.KD",
# "NY.GDP.FCST.KD",
# "NY.GDP.MKTP.KD",
# "NY.GNP.MKTP.KD.ZG",
# "FS.AST.PRVT.GD.ZS",
# "NY.ADJ.NNTY.PC.KD.ZG",
# "NE.GDI.TOTL.KD.ZG") -> economic_indicators

plots <- list()
for(i in economic_indicators){
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
