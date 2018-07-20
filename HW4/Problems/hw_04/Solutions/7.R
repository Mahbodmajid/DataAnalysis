## only breakfast exists in data
BSG %>%
  select(idcntry:idstud, bsmmat01:bsssci05 , food = bsbg12) %>%
  mutate(
    food = 4 - food,
    score = (
      bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05 +
        bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05
    ) / 10
  ) %>%
  select(idcntry:idstud, food, score) %>%
  filter(!is.na(food), !is.na(score)) -> sb

kable(unlist(summary.aov(
  aov(formula = score ~ food,
      data = sb)
)))

kable(unlist(t.test((sb %>% filter(food == 3))$score,
                    (sb %>% filter(food != 3))$score,
                    alternative = "greater"
)))

kable(unlist(t.test((sb %>% filter(food == 0))$score,
                    (sb %>% filter(food != 0))$score,
                    alternative = "less"
)))

sb$food <- as.character(sb$food)
sb %>% group_by(food) %>% summarize(mean_score = mean(score)) %>% filter(!is.na(food)) -> sb_m
sb_m$food <- as.numeric(sb_m$food)
sb$food <- as.numeric(sb$food)

sb %>% filter(!is.na(food), !is.na(score)) %>% sample_n(5000) -> sb_sample_5000

hchart(
  sb_sample_5000,
  type = "scatter",
  hcaes(food, round(score, 2)),
  color = hex_to_rgba(x <- "#386cb0", alpha = 0.5)
) %>%
  hc_add_series(
    data = sb_m,
    type = "line",
    hcaes(food, round(mean_score, 2)),
    color = hex_to_rgba(x <- "#fdb462")
  ) %>% 
  hc_title(text = "Eating Breakfast and Score Relation") %>%
  hc_xAxis(title = list(text = "Eating Breakfast")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

hcboxplot(x = sb_sample_5000$score,
          var = sb_sample_5000$food,
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Eating Breakfast and Score Relation") %>%
  hc_xAxis(title = list(text = "Eating Breakfast")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(sb %>% sample_n(20000), aes(x = food, y = score)) +
  geom_boxplot(aes(group = food, y = score, fill = food)) +
  geom_line(data = sb_m, aes(x = food, y = mean_score), color = "#fdb462")+
  xlab("Eating Breakfast") +
  ylab("Score") +
  ggtitle("Eating Breakfast and Score Relation")
