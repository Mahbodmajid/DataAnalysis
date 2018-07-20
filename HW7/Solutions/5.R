set.seed(200)
index = sample(x= 1:nrow(ms_3),size = 0.8*nrow(ms_3),replace = F)
train = ms_3[index,] 
test =  ms_3[-index,]
model_glm = glm(
  MannerOfDeath ~.-Id-Cause-ActivityCode-MonthOfDeath-MethodOfDisposition-PlaceOfInjury,
  data = train,
  family = "binomial")
# prediction
train$prediction = predict( model_glm, newdata = train, type = "response" )
test$prediction  = predict( model_glm, newdata = test , type = "response" )

co <- 0.5
cm_info = ConfusionMatrixInfo( data = test, predict = "prediction", 
                               actual = "MannerOfDeath", cutoff = co)
cm_info$plot


P <- test %>% filter(prediction > co) %>% count()
N <- test %>% filter(prediction <= co) %>% count()
TP <- test %>% filter(prediction > co, MannerOfDeath == 1) %>% count()
TN <- test %>% filter(prediction <= co, MannerOfDeath == 0) %>% count()
FP <- test %>% filter(prediction > co, MannerOfDeath == 0) %>% count()
FN <- test %>% filter(prediction <= co, MannerOfDeath == 1) %>% count()

sprintf("positive samples: %.0f", P)
sprintf("negative samples: %.0f", N)
sprintf("true positive: %.0f", TP)
sprintf("true negative: %.0f", TN)
sprintf("false positive: %.0f", FP)
sprintf("false negative: %.0f", FN)

ACC <- (TP + TN)/ (P + N)
FPR <- 1 - (TN / N)
TPR <- TP / P

sprintf("Accuracy (ACC): %.3f", ACC)
sprintf("False positive rate (FPR): %.3f", FPR)
sprintf("True positive rate (TPR): %.3f", TPR)
