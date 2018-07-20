BSG %>%
  select(idcntry:idstud, bsmmat01:bsssci05 , bsbg06a:bsbg06g) %>%
  mutate(
    score = (
      bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05 +
        bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05
    ) / 10
  ) %>%
  mutate(Value = -(bsbg06a + bsbg06b + bsbg06c + bsbg06d + bsbg06e + bsbg06g) + 12) %>%
  select(c(idcntry:idstud, score, Value)) %>%
  filter(!is.na(Value), !is.na(score)) -> WF

kable(unlist(summary.aov(
  aov(formula = score ~ Value,
      data = WF)
)))

kable(unlist(t.test((WF %>% filter(Value == 6))$score,
                    (WF %>% filter(Value != 6))$score,
                    alternative = "greater"
)))

kable(unlist(t.test((WF %>% filter(Value == 0))$score,
                    (WF %>% filter(Value != 0))$score,
                    alternative = "less"
)))

WF$Value <- as.character(WF$Value)
WF %>% group_by(Value) %>% summarize(mean_score = mean(score)) %>% filter(!is.na(Value)) -> WF_m
WF_m$Value <- as.numeric(WF_m$Value)
WF$Value <- as.numeric(WF$Value)

WF %>% filter(!is.na(Value), !is.na(score)) %>% sample_n(5000) -> wf_sample_5000

hchart(
  WF %>% sample_n(5000),
  type = "scatter",
  hcaes(Value, round(score, 2)),
  color = hex_to_rgba(x <- "#386cb0", alpha = 0.5)
) %>%
  hc_add_series(
    data = WF_m,
    type = "line",
    hcaes(Value, round(mean_score, 2)),
    color = hex_to_rgba(x <- "#fdb462")
  )  %>% 
  hc_title(text = "Home Wellfare and Score Relation") %>%
  hc_xAxis(title = list(text = "Home Wellfare Level")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

hcboxplot(x = wf_sample_5000$score,
          var = wf_sample_5000$Value,
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Home Wellfare and Score Relation") %>%
  hc_xAxis(title = list(text = "Home Wellfare Level")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(WF %>% sample_n(20000), aes(x = Value, y = score)) +
  geom_boxplot(aes(group = Value, y = score, fill = Value)) +
  geom_line(data = WF_m, aes(x = Value, y = mean_score), color = "#fdb462")+
  xlab("Home Wellfare Level") +
  ylab("Score") +
  ggtitle("Home Wellfare and Score Relation")
