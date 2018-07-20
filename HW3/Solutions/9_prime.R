la_2012_other <-
  la %>% filter(Season == 2012) %>% select(home, visitor, FT, hgoal)
teams_12 <- unique(la_2012 %>% select(home))
la_2012_self <- teams_12 %>%
  mutate(visitor = home,
         FT = "",
         hgoal = 0)
la_2012 <- rbind(la_2012_other, la_2012_self)

scale_fill_Matrix <- function(...) {
  library(scales)
  discrete_scale("fill", "Publication", manual_pal(values = c("white",
                                                              "lightBlue")), ...)
}

ggplot(la_2012, aes(reorder(home, -desc(home)), reorder(visitor, desc(visitor)))) +
  # x and y axes => Var1 and Var2
  geom_tile(aes(fill = as.factor((
    as.numeric(as.factor(home)) +
      as.numeric(as.factor(visitor))
  ) %% 2)), color = "black") + # background colours are mapped according to the value column
  geom_text(aes(label = FT)) + # write the values
  theme(
    panel.grid.major.x = element_blank(),
    #no gridlines
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    # background=white
    axis.text.x = element_text(
      angle = 40,
      hjust = 0,
      vjust = -2,
      size = 12,
      face = "bold"
    ),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  ggtitle("1998 League") +
  theme(legend.title = element_text(face = "bold", size = 14)) +
  scale_x_discrete(name = "", position = "top") +
  scale_y_discrete(name = "") +
  labs(fill = "") +
  geom_abline(slope = -1, intercept = 21) +
  guides(fill = F) + scale_fill_Matrix()

la_2012 %>%
  hchart(type = "scatter",
         hcaes(x = hgoal,
               y = home,
               value = hgoal),
         name = "FT")
