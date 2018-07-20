set.seed(200)
#shuffling
train_sig_cv <- train_sig[sample(nrow(train_sig)),]

train_sig_cv_train <-
  train_sig_cv  %>% filter(row_number() < 0.8 * n())
train_sig_cv_test <-
  train_sig_cv  %>%  filter(row_number() >= 0.8 * n())

testmodel = lm(SalePrice ~ ., data= train_sig_cv_train)


train_sig_cv_test_pre_err <-
  train_sig_cv_test %>%
  mutate(pred =  predict.lm(testmodel, train_sig_cv_test, type = "response")) %>% 
  mutate(err = (SalePrice - pred) * (SalePrice - pred)) %>% 
  dplyr::select(SalePrice, pred, err)

model_error <- mean(train_sig_cv_test_pre_err$err)
model_error
sqrt(model_error)
