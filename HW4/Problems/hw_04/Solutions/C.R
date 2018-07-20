## Comparing immigrants who have immigrated earlier to  others

BSG %>%
  select(idcntry:idstud, bsmmat01:bsssci05 , bsbg10a) %>%
  filter(!is.na(bsbg10a)) %>%
  mutate(
    score = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5,
    immigrated = (bsbg10a == 2)
  ) %>%
  select(c(idcntry:idstud, score, immigrated)) %>%
  filter(!is.na(immigrated), !is.na(score)) -> immigration_score


kable(unlist(t.test((immigration_score %>% filter(immigrated == T))$score,
       (immigration_score %>% filter(immigrated == F))$score,
       alternative = "greater"
)))

immigration_score_mean <-  immigration_score %>%
  group_by(immigrated) %>%
  summarize(mean_score = mean(score))

ggplot(
  immigration_score_mean,
  aes(x = immigrated, y = mean_score, fill = immigrated)
) +
  geom_bar(position = "dodge", stat = "identity") +
  guides(fill = F) +
  xlab("Immigrated") +
  ylab("Avg. Score") +
  ggtitle("Immigration and Score") + coord_flip()


immigration_score_mean %>% mutate(mean_score = round(mean_score, 2)) %>%
  hchart(type = "bar", hcaes(
    x = as.factor(immigrated),
    y = mean_score,
    color = as.factor(immigrated)
  )) %>%
  hc_title(text = "Immigration and Score") %>%
  hc_xAxis(title = list(text = "Immigrated")) %>%
  hc_yAxis(title = list(text = "Avg. Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())
