train_top %>% 
  mutate(PricePredict = predict(fit)) -> train_top_pre

ggplot(train_top_pre) +
  geom_point(aes(x = SalePrice, y = PricePredict), alpha = 0.3, colour = "blue")+
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1, alpha = 0.5)
