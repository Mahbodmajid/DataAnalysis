geo_app_qi <- questions_info_f %>%
  filter(`Content Domain` == "Geometry",
         `Cognitive Domain` == "Applying")

applying_geometry_questions <- geo_app_qi$`Item ID`

BSA_math <- BSA %>%
  select(idcntry:m062120, itsex)

BSA_math %>%
  gather(Question, Value , m042182:m062120) -> BSA_math_gathered

BSA_math_gathered %>%
  filter(Question %in% applying_geometry_questions,
         !is.na(Value),
         !is.na(itsex)) -> student_app_geo

marx <-
  getMark(student_app_geo %>% select(Question), student_app_geo$Value)
sex_marx <-
  data.frame(cbind(sex = as.character(student_app_geo$itsex), marx))
sex_marx$sex <- as.factor(sex_marx$sex)

library(perm)                  # for Permutation Test
kable(unlist(permTS(
  (sex_marx$score * sex_marx$weight) ~ sex_marx$sex,
  alternative = "less"
)))

student_app_geo <- cbind(student_app_geo, marx)

sex_app_geo <- student_app_geo %>%
  group_by(itsex) %>%
  summarize(mean_score = weighted.mean(score, weight)) %>%
  ungroup()
ggplot(sex_app_geo,
       aes(x = itsex, y = mean_score, fill = itsex)) +
  geom_bar(position = "dodge", stat = "identity") +
  guides(fill = F) +
  xlab("Gender") +
  ylab("Avg. Score") +
  ggtitle("Male vs. Female Performance in Applying Geometry") + coord_flip()


sex_app_geo %>% mutate(mean_score = round(mean_score, 3)) %>%
  hchart(type = "bar", hcaes(x = itsex, y = mean_score, color = itsex)) %>%
  hc_title(text = "Male vs. Female Performance in Applying Geometry") %>%
  hc_xAxis(title = list(text = "Gender")) %>%
  hc_yAxis(title = list(text = "Avg. Score"))  %>%
  hc_add_theme(hc_theme_sandsignika())
