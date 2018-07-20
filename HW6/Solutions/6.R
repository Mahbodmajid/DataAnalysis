train_top %>% 
  select(-GarageArea, -TotRmsAbvGrd) -> train_sig

fit_sig <- lm(SalePrice ~ ., data=train_sig)
summary(fit_sig)