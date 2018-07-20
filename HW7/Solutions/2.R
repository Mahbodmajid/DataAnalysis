library(cramer)

ms_2 <- ms %>%
  filter(EducationReportingFlag == 1) %>% 
  select(MannerOfDeath, Sex, Education2003Revision,
         Race, Age, MethodOfDisposition)  %>%
  mutate(MannerOfDeath = as.factor((MannerOfDeath == 2)* 1))

ms_2$Education2003Revision <- as.factor(ms_2$Education2003Revision)
ms_2$Sex <- as.factor(ms_2$Sex)
ms_2$MethodOfDisposition <- as.factor(ms_2$MethodOfDisposition)
ms_2$Race <- as.factor(ms_2$Race)

chisq.test(ms_2$MannerOfDeath, ms_2$Sex)
chisq.test(ms_2$MannerOfDeath, ms_2$Education2003Revision)
chisq.test(ms_2$MannerOfDeath, ms_2$Race)
summary(glm(MannerOfDeath ~ Age,  data = ms_2, family = binomial))
chisq.test(ms_2$MannerOfDeath, ms_2$MethodOfDisposition)

