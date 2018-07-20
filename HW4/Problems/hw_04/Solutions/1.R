st <- BST %>%
  select(c(idcntry:idlink))

teachersM <-
  BTM %>%
  mutate(sat = (28 - btbg10a + btbg10b + btbg10c + btbg10d +
                  btbg10e + btbg10f + btbg10g)) %>%
  select(c(idcntry:idlink, sat))

teachersS <-
  BTS %>%
  mutate(sat = (28 - btbg10a + btbg10b + btbg10c + btbg10d +
                  btbg10e + btbg10f + btbg10g)) %>%
  select(c(idcntry:idlink, sat))

full_join(teachersM, st) -> st_M
full_join(teachersS, st) -> st_S

bsa_m <-
  BSA %>% select(idcntry:idstud, bsmmat01:bsmmat05) %>%
  group_by(idcntry, idbook, idschool, idclass, idstud) %>%
  summarize(score = mean(bsmmat01:bsmmat05)) %>%
  ungroup()

bsa_s <-
  BSA %>% select(idcntry:idstud, bsssci01:bsssci05) %>%
  group_by(idcntry, idbook, idschool, idclass, idstud) %>%
  summarize(score = mean(bsssci01:bsssci05)) %>%
  ungroup()

full_join(st_M, bsa_m) -> sts_M
full_join(st_S, bsa_s) -> sts_S

rbind(sts_M, sts_S) %>% filter(!is.na(score)) -> sts


kable(unlist(cor.test(sts$sat, sts$score, alternative = "greater", method = "spearman")))


sts$sat <- as.character(sts$sat)
sts %>% group_by(sat) %>% summarize(mean_score = mean(score)) %>% filter(!is.na(sat)) -> sts_m
sts_m$sat <- as.numeric(sts_m$sat)
sts$sat <- as.numeric(sts$sat)

sts %>% filter(!is.na(sat), !is.na(score))%>% sample_n(5000) -> sts_sample_5000
hchart(
  sts_sample_5000,
  type = "scatter",
  hcaes(sat, round(score, 2)),
  color = hex_to_rgba(x <- "#386cb0", alpha = 0.1)
) %>%
  hc_add_series(
    data = sts_m,
    type = "line",
    hcaes(sat, round(mean_score, 2)),
    color = hex_to_rgba(x <- "#fdb462", alpha = 1)
  ) %>%
  hc_title(text = "Teachers' Satisfaction Level and Score Relation") %>%
  hc_xAxis(title = list(text = "Satisfaction Level")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

hcboxplot( x = sts_sample_5000$score, var = sts_sample_5000$sat, outliers = F) %>% 
  hc_chart(type = "column") %>%
  hc_title(text = "Teachers' Satisfaction Level and Score Relation") %>%
  hc_xAxis(title = list(text = "Satisfaction Level")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())


ggplot(sts %>% filter(!is.na(sat), !is.na(score)), aes(x = sat, y = score)) +
  geom_boxplot(aes(group = sat, y = score, fill = sat))+
  geom_line(data = sts_m, aes(x = sat, y = mean_score), color = "#fdb462")+
  xlab("Satisfaction Level") +
  ylab("Score") +
  ggtitle("Teachers' Satisfaction Level and Score Relation")
