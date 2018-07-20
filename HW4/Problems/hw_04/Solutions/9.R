BSG %>%
  select(idcntry:idstud, bsmmat01:bsssci05 , presence  = bsbg11) %>%
  mutate(
    score = (
      bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05 +
        bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05
    ) / 10
  ) %>%
  select(idcntry:idstud, presence, score) %>%
  filter(!is.na(presence), !is.na(score)) -> sp

kable(unlist(summary.aov(aov(
  formula = score ~ presence,
  data = sp
))))

kable(unlist(t.test((sp %>% filter(presence == 4))$score,
                    (sp %>% filter(presence != 4))$score,
                    alternative = "greater"
)))

kable(unlist(t.test((sp %>% filter(presence == 1))$score,
                    (sp %>% filter(presence != 1))$score,
                    alternative = "less"
)))

sp$presence <- as.character(sp$presence)
sp %>% group_by(presence) %>% summarize(mean_score = mean(score)) %>% filter(!is.na(presence)) -> sp_m
sp_m$presence <- as.numeric(sp_m$presence)
sp$presence <- as.numeric(sp$presence)


sp %>% filter(!is.na(presence), !is.na(score)) %>% sample_n(5000) -> sp_sample_5000

hchart(
  sp_sample_5000,
  type = "scatter",
  hcaes(presence, round(score, 2)),
  color = hex_to_rgba(x <- "#386cb0", alpha = 0.5)
) %>%
  hc_add_series(
    data = sp_m,
    type = "line",
    hcaes(presence, round(mean_score, 2)),
    color = hex_to_rgba(x <- "#fdb462")
  ) %>%
  hc_title(text = "Presence in Class and Score Relation") %>%
  hc_xAxis(title = list(text = "Presence in Class")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

hcboxplot(x = sp_sample_5000$score,
          var = sp_sample_5000$presence,
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Presence in Class and Score Relation") %>%
  hc_xAxis(title = list(text = "Presence in Class")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(sp %>% sample_n(50000), aes(x = presence, y = score)) +
  geom_boxplot(aes(group = presence, y = score, fill = presence)) +
  geom_line(data = sp_m,
            aes(x = presence, y = mean_score),
            color = "#fdb462") +
  xlab("Presence in Class") +
  ylab("Score") +
  ggtitle("Presence in Class and Score Relation")+
  guides(fill = F)
