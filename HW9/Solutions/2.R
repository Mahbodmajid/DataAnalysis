rbinded_profit <- rbinded %>%
  mutate(profit = Close - Open)
rbinded_profit %>%
  filter(as.integer(format(Date,"%d")) == 13) -> day13
day13$profit -> profits
mean(profits)

### We're not sure if the profits are distributed normally
wilcox.test(profits, mu = 0, alternative = "greater")
## There is no sufficient evidence to reject null hypothesis
  
