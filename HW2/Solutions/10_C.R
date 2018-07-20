mob_worth <- mobile
mob_worth <- mob_worth %>%
  filter(!is.na(price),
         !is.na(weight),
         year < 2017)
mob_worth$worth <- mob_worth$price / mob_worth$weight
top_mob_worth <- mob_worth %>%
  arrange(desc(worth)) %>%
  head(20) %>%
  select(company, device, worth)
top_mob_worth$company_device <-
  paste(top_mob_worth$company, top_mob_worth$device)

top_mob_worth$company_device <-
  factor(top_mob_worth$company_device,
         levels = top_mob_worth$company_device[order(top_mob_worth$worth)])

ggplot(top_mob_worth, aes(fill = as.factor((worth > 34.934) + (worth > 26.03006)),
                          y = worth,
                          x = company_device)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 34.934,
             color = "gold",
             size = 1) +   #gold
  geom_text(
    aes(x = 1, label = "Gold", y = 34.934),
    colour = "black",
    angle = 0,
    size = 3
  ) +
  geom_hline(yintercept = 26.03006,
             color = "grey",
             size = 1) + #platinum
  geom_text(
    aes(x = 2, label = "Platinum", y = 26.03006),
    colour = "black",
    angle = 0,
    size = 3
  ) +
  ylab("€ / gr") +
  xlab("Device Name") +
  guides(fill=FALSE)+
  coord_flip()