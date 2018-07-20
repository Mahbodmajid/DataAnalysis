iran_app_rea <-
  BSA %>% select(idcntry:idstud, bsmapp01:bsmrea05, bssapp01:bssrea05) %>%
  filter(idcntry == 364) %>%
  group_by(idcntry, idbook, idschool, idclass, idstud) %>%
  summarize(Applying = mean(c(bsmapp01:bsmapp05, bssapp01:bssapp05)),
            Reasoning = mean(c(bsmrea01:bsmrea05, bssrea01:bssrea05))) %>%
  ungroup() %>%
  gather(Section, Score, Applying, Reasoning)

kable(unlist(t.test((iran_app_rea %>% filter(Section == "Applying"))$Score,
                    (iran_app_rea %>% filter(Section == "Reasoning"))$Score,
                    alternative = "greater"
)))

hcboxplot(x = round(iran_app_rea$Score,2),
          var = iran_app_rea$Section,
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Iranians in Applying vs Reasoning") %>%
  hc_xAxis(title = list(text = "Section")) %>%
  hc_yAxis(title = list(text = "Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())

ggplot(iran_app_rea, aes(x = Section, y = Score)) +
  geom_boxplot(aes(group = Section, y = Score, fill = Section)) +
  xlab("Section") +
  ylab("Score") +
  ggtitle("Iranians in Applying vs Reasoning")+
  guides(fill = F)
