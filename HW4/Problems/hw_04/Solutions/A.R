st <- BST %>%
  select(c(idcntry:idlink))

teachersM <-
  BTM %>%
  mutate(assignments =  btbm22a) %>%
  select(c(idcntry:idlink, assignments))

full_join(teachersM, st) -> st_M

bsa_m <-
  BSA %>% select(idcntry:idstud, bsmmat01:bsmmat05) %>%
  group_by(idcntry, idbook, idschool, idclass, idstud) %>%
  summarize(score = mean(bsmmat01:bsmmat05)) %>%
  ungroup()

full_join(st_M, bsa_m) -> sts_M

kable(unlist(t.test((sts_M %>% filter(assignments == 5))$score,
                    (sts_M %>% filter(assignments != 5))$score,
                    alternative = "less"
)))

sts_M$assignments <- as.character(sts_M$assignments)
sts_M %>%
  group_by(assignments) %>%
  summarize(mean_score = mean(score)) %>% filter(!is.na(assignments)) -> sts_M_m
sts_M_m$assignments <- as.numeric(sts_M_m$assignments)
sts_M$assignments <- as.numeric(sts_M$assignments)

sts_M %>% filter(!is.na(assignments), !is.na(score)) %>% sample_n(5000) -> sts_M_sample_5000

hchart(
  sts_M_sample_5000,
  type = "scatter",
  hcaes(assignments, round(score, 2)),
  color = hex_to_rgba(x <- "#386cb0", alpha = 0.1)
) %>%
  hc_add_series(
    data = sts_M_m,
    type = "line",
    hcaes(assignments, round(mean_score, 2)),
    color = hex_to_rgba(x <- "#fdb462", alpha = 1)
  ) %>%
  hc_title(text = "Homework Frequency and Score Relation") %>%
  hc_xAxis(title = list(text = "Homework Frequency")) %>%
  hc_yAxis(title = list(text = "Score")) %>%
  hc_add_theme(hc_theme_sandsignika())

hcboxplot(x = sts_M_sample_5000$score,
          var = sts_M_sample_5000$assignments,
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Homework Frequency and Score Relation") %>%
  hc_xAxis(title = list(text = "Homework Frequency")) %>%
  hc_yAxis(title = list(text = "Score")) %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(sts_M %>% filter(!is.na(assignments), !is.na(score)) %>% sample_n(50000),
       aes(x = assignments, y = score)) +
  geom_boxplot(aes(group = assignments, y = score, fill = assignments)) +
  geom_line(data = sts_M_m, aes(x = assignments, y = mean_score), color = "#fdb462")+
  xlab("Homework Frequency") +
  ylab("Score") +
  ggtitle("Homework Frequency and Score Relation")+
  guides(fill = F)
