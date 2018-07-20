mobile_ppi <- mobile
mobile_ppi$ppi <- sqrt((mobile$px_col * mobile$px_col) +
                         (mobile$px_row * mobile$px_row)) / mobile$display_size
ggplot(mobile_ppi, aes(x = ppi, y = ..count..)) +
  geom_histogram(bins = 40)

mobile_ppi_year <- mobile_ppi %>%
  group_by(year) %>%
  summarise(avg_ppi = mean(ppi, na.rm = T))

ggplot(mobile_ppi_year, aes(x = year, y = avg_ppi)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Avg. PPI") +
  ggtitle("Avg. PPI ~ Year")

highest_ppi <- mobile_ppi[which.max(mobile_ppi$ppi),]
highest_ppi$device
highest_ppi$ppi
