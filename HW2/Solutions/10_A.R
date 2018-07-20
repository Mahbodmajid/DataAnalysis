mob_ratio <- mobile
mob_ratio %>%
  filter(
    !is.na(mob_ratio$dim_length),
    !is.na(mob_ratio$display_size),
    !is.na(mob_ratio$dim_breadth),
    mob_ratio$dim_length != 0,
    mob_ratio$display_size != 0,
    mob_ratio$dim_breadth != 0,
    !is.na(year)
  ) -> mob_ratio
mob_ratio$ratio <-
  ((
    mob_ratio$display_size / sqrt(
      mob_ratio$dim_length * mob_ratio$dim_length +
        mob_ratio$dim_breadth * mob_ratio$dim_breadth
    )
  ) ^ 2) * 645.16
mob_ratio %>%
  group_by(year) %>%
  summarise(avg_ratio = mean(ratio, na.rm = T),
            highest_ratio = max(ratio, na.rm = T)) -> mob_ratio

ggplot(mob_ratio, aes(x = year)) +
  geom_point(
    aes(y = highest_ratio),
    color = "darkblue",
    alpha = 0.7,
    stat = "identity"
  ) +
  geom_point(aes(y = avg_ratio),
             shape = 15,
             color = "red",
             alpha = 0.7) +
  xlab("Year") +
  ylab(expression(frac("Display Area", "Total Area"))) +
  theme(axis.title.y = element_text(angle = 360, vjust = 0.5)) +
  stat_smooth(
    method = 'lm',
    formula = y ~ x,
    aes(y = highest_ratio),
    fullrange = T,
    color = "darkblue",
    alpha = 0.7
  ) +
  stat_smooth(
    method = 'lm',
    formula = y ~ x,
    aes(y = avg_ratio),
    fullrange = T,
    color = "red",
    alpha = 0.7
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(1998, 2022))
