ggplot(get_indicator_gathered("SP.DYN.LE00.IN")$gathered, aes(x = year, y = value)) +
  geom_boxplot(fill = "3")+
  geom_line(data = get_indicator_gathered("SP.DYN.LE00.IN")$gathered %>%
              filter(Country_Code == "RWA"), aes(x = year, y = value, group ="1"), color = "4")+
  xlab("Year") +
  ylab("Years") +
  ggtitle("Life Expectancy")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 1, 
                                   size = 8, hjust = 1))

## From Wikipedia: Death toll is about 500k to 1m
