library(h2o)
localH2O = h2o.init()
hms = as.h2o(ms_3)
chglm = h2o.glm(y = "MannerOfDeath", x= c("ResidentStatus", "Education2003Revision", "Age", "Sex",
                                          "PlaceOfDeathAndDecedentsStatus", "DayOfWeekOfDeath", "Race",
                                          "MaritalStatus", "InjuryAtWork", "Autopsy", 
                                          "NumberOfEntityAxisConditions",
                                          "NumberOfRecordAxisConditions"),
                training_frame = hms, family="binomial",nfolds = 5)
chglm
kable(h2o.confusionMatrix(chglm))
