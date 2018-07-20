mob_sim_lte = mobile %>%
  group_by(LTE, sim_no) %>%
  summarise(avg_price = mean(price, na.rm = T))
ggplot(mob_sim_lte, aes(x = sim_no, y = avg_price, fill = LTE)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("SIM No.") +
  ylab("Avg. Price") +
  ggtitle("Avg. Price ~ SIM No. & LTE")
