ms_1 %>%
  mutate(MannerOfDeath = as.factor((MannerOfDeath == 2)* 1)) -> ms_3
ms_3$MethodOfDisposition[ms_3$MethodOfDisposition == 'U'] <- NA
ms_3$MaritalStatus[ms_3$MaritalStatus == 'U'] <- NA
ms_3$InjuryAtWork[ms_3$InjuryAtWork == 'U'] <- NA
ms_3$Autopsy[ms_3$Autopsy == 'U'] <- NA
ms_3$PlaceOfInjury[ms_3$PlaceOfInjury == 9 ] <- NA
ms_3$ActivityCode[ms_3$ActivityCode == 99 ] <- NA
as.factor(ms_3$Race) -> ms_3$Race 
as.factor(ms_3$ResidentStatus) -> ms_3$ResidentStatus
as.factor(ms_3$Education2003Revision) -> ms_3$Education2003Revision
as.factor(ms_3$PlaceOfDeathAndDecedentsStatus) -> ms_3$PlaceOfDeathAndDecedentsStatus
as.factor(ms_3$Cause) -> ms_3$Cause
as.factor(ms_3$PlaceOfInjury) -> ms_3$PlaceOfInjury
as.factor(ms_3$ActivityCode) -> ms_3$ActivityCode
as.factor(ms_3$DayOfWeekOfDeath) -> ms_3$DayOfWeekOfDeath
as.factor(ms_3$MonthOfDeath) -> ms_3$MonthOfDeath
as.factor(ms_3$Sex) -> ms_3$Sex
as.factor(ms_3$InjuryAtWork) -> ms_3$InjuryAtWork
as.factor(ms_3$MaritalStatus) -> ms_3$MaritalStatus
as.factor(ms_3$Autopsy) -> ms_3$Autopsy
na.omit(ms_3) -> ms_3


#model <- glm(MannerOfDeath ~.-Id, data = ms_3, family = binomial)
#summary(model)

model <- glm(
  MannerOfDeath ~.-Id-Cause-ActivityCode-MonthOfDeath-MethodOfDisposition-PlaceOfInjury,
  data = ms_3,
  family = binomial)
summary(model)

par(mar=c(1,1,1,1))
library(boot)
glm.diag.plots(model, glmdiag = glm.diag(model))

library(ResourceSelection)
hoslem.test(ms_3$MannerOfDeath, fitted(model))

