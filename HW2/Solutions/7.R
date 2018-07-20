mob_density <- mobile
mob_density$density <-
  1000  * mob_density$weight / (mob_density$dim_length *
                                  mob_density$dim_breadth *
                                  mob_density$dim_thickness) # in gr / cm^3 (water is  1)
mob_float <- mob_density %>%
  filter(density < 1)

ggplot(mob_density, aes(x = density, y = ..count.., fill = density <1)) +
  geom_histogram(breaks = seq(0,4,0.1)) +
  geom_vline(xintercept = 1) +
  scale_x_continuous(limits = c(0, 4))+
  coord_flip()