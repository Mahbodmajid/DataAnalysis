## Taking classes to excel in class

BSG %>%
  select(idcntry:idstud, bsmmat01:bsssci05 , bsbm39aa) %>%
  filter(!is.na(bsbm39aa)) %>% 
  filter(bsbm39aa != 2) %>% 
  mutate(
    score = (
      bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05
    ) / 5,
    to_excel = (bsbm39aa == 1)
  ) %>%
  select(c(idcntry:idstud, score, to_excel)) %>%
  filter(!is.na(to_excel), !is.na(score)) -> math_class_purpose


kable(unlist(t.test((math_class_purpose %>% filter(to_excel == T))$score,
       (math_class_purpose %>% filter(to_excel == F))$score,
       alternative = "less")))

math_class_purpose_mean <-  math_class_purpose%>% 
  group_by(to_excel) %>% 
  summarize(mean_score = mean(score))

ggplot(math_class_purpose_mean,
       aes(x = to_excel, y = mean_score, fill = to_excel)) +
  geom_bar(position = "dodge", stat = "identity") +
  guides(fill = F) +
  xlab("To Excel and not Taken") +
  ylab("Avg. Score") +
  ggtitle("To Excel and not Taken and Score") + coord_flip()


math_class_purpose_mean %>% mutate(mean_score = round(mean_score, 2)) %>%
  hchart(type = "bar", hcaes(x = as.factor(to_excel), y = mean_score, color = as.factor(to_excel))) %>%
  hc_title(text = "Score ~ To Excel and not Taken") %>%
  hc_xAxis(title = list(text = "To Excel and not Taken")) %>%
  hc_yAxis(title = list(text = "Avg. Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

