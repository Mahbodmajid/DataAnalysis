rbind(ms %>% 
  filter(EducationReportingFlag == 1, AgeType == 1) %>% 
  select(Id, ResidentStatus, Education2003Revision, MonthOfDeath, Age, Sex,
         PlaceOfDeathAndDecedentsStatus, DayOfWeekOfDeath, MannerOfDeath,
         ActivityCode, PlaceOfInjury, Cause = CauseRecode39,Race =  RaceRecode5, MaritalStatus, InjuryAtWork,
         MethodOfDisposition, Autopsy,
         NumberOfEntityAxisConditions, NumberOfRecordAxisConditions),
  ms %>% filter(EducationReportingFlag == 1, AgeType != 9, AgeType != 1) %>%
    mutate(Age = (AgeType == 2) * 1 /12 + (AgeType == 3) * 1 /365) %>% 
    select(Id, ResidentStatus, Education2003Revision, MonthOfDeath, Age, Sex,
           PlaceOfDeathAndDecedentsStatus, DayOfWeekOfDeath, MannerOfDeath,
           ActivityCode, PlaceOfInjury,Cause = CauseRecode39,Race =  RaceRecode5, MaritalStatus, InjuryAtWork,
           MethodOfDisposition, Autopsy,
           NumberOfEntityAxisConditions, NumberOfRecordAxisConditions))-> ms_1

# ms_1$Education2003Revision[ms_1$Education2003Revision == 9] <- NA
# ms_1$AgeRecode52[ms_1$AgeRecode52 == 52] <- NA
# ms_1$MaritalStatus[ms_1$MaritalStatus == 'U'] <- NA
# ms_1$MannerOfDeath[ms_1$MannerOfDeath == 0] <- NA
# ms_1$MethodOfDisposition[ms_1$MethodOfDisposition == 'U'] <- NA
# ms_1$Autopsy[ms_1$Autopsy == 'U'] <- NA
# 
# ms_reduced <- ms_1 %>% filter(!is.na(Education2003Revision), is.na(AgeRecode52),
#                               is.na(MaritalStatus), is.na(MannerOfDeath),
#                               is.na(MethodOfDisposition), is.na(Autopsy))
# 
ms_reduced <- ms_1
ms_reduced$MaritalStatus <- as.factor(ms_reduced$MaritalStatus)
ms_reduced$InjuryAtWork <- as.factor(ms_reduced$InjuryAtWork)
ms_reduced$MethodOfDisposition <- as.factor(ms_reduced$MethodOfDisposition)
ms_reduced$Autopsy <- as.factor(ms_reduced$Autopsy)
ms_reduced$Race <- as.factor(ms_reduced$Race)
ms_reduced$Cause <- as.factor(ms_reduced$Cause)



ms_reduced[] <- lapply(ms_reduced, function(x) {
  if(is.factor(x)) as.numeric(x) else x
})
#sapply(ms_reduced, class)

ms_reduced_2 <- (ms_reduced %>% select(-Id))

cormat <- round(cor(ms_reduced_2), 4)

melted_cormat <- melt(cormat, na.rm = TRUE)

ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()

ms_reduced_3 <- (ms_reduced %>% select(-Id))[sample(1:nrow(ms_reduced), 1000),]
library(GGally)
ggpairs(ms_reduced_3)

