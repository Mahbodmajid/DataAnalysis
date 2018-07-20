BSG %>%
  select(idcntry:idstud, bsmmat01:bsssci05 , bsbg07a, bsbg07b) %>%
  mutate(
    score = (
      bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05 +
        bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05
    ) / 10
  ) %>%
  gather(Parent_Type, Education, c(bsbg07a, bsbg07b)) %>%
  select(c(idcntry:idstud, score, Parent_Type, Education)) %>%
  filter(!is.na(Education), !is.na(score), Education != 8) -> parent_education


kable(unlist(summary.aov(
  aov(formula = score ~ Education,
      data = parent_education)
)))

kable(unlist(t.test((parent_education %>% filter(Education == 7))$score,
                    (parent_education %>% filter(Education != 7))$score,
                    alternative = "greater"
)))

kable(unlist(t.test((parent_education %>% filter(Education == 1))$score,
                    (parent_education %>% filter(Education != 1))$score,
                    alternative = "less"
)))

parent_education %>% group_by(Education) %>% summarize(mean_score = mean(score)) -> education_mean_score

parent_education %>% filter(!is.na(Education), !is.na(score)) %>% sample_n(5000) -> pe_sample_5000

hchart(
  pe_sample_5000,
  type = "scatter",
  hcaes(Education, round(score, 2)),
  color = hex_to_rgba(x <- "#386cb0", alpha = 0.1)
) %>%
  hc_add_series(
    data = education_mean_score,
    type = "line",
    hcaes(Education, round(mean_score, 2)),
    color = hex_to_rgba(x <-
                          "#fdb462")
  ) %>%
  hc_title(text = "Parent's Education and Score Relation") %>%
  hc_xAxis(title = list(text = "Education Level")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

hcboxplot(x = pe_sample_5000$score,
          var = pe_sample_5000$Education,
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Parent's Education and Score Relation") %>%
  hc_xAxis(title = list(text = "Education Level")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(parent_education %>% sample_n(20000),
       aes(x = Education, y = score)) +
  geom_boxplot(aes(group = Education, y = score, fill = Education)) +
  geom_line(data = education_mean_score,
            aes(x = Education, y = mean_score),
            color = "#fdb462") +
  xlab("Education Level") +
  ylab("Score") +
  ggtitle("Parent's Education and Score Relation")
