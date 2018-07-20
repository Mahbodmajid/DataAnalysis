valid_mob <- mobile %>%
  filter(!is.na(battery_mah), !is.na(weight))

ggplot(valid_mob, aes(x = battery_mah, y = weight)) +
  geom_point(na.rm = T, size = 0.5)+
  xlab("Battery (m A.h)")+
  ylab("Weight (g)")

cor(valid_mob$battery_mah, valid_mob$weight)
