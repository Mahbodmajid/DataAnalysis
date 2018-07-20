get_multiple_indicator_gathered(indicators = economic_indicators, Country_Code = "IRN")$gathered %>% 
  spread(key = Indicator, value = value) %>% 
  select(-Country_Code, -Country) %>% 
  arrange(year) %>% 
  mutate( next_year_growth = lead(NY.GDP.MKTP.KD.ZG)) %>% 
  select(-year) %>% 
  drop_na()-> iran_data

library(h2o)
localH2O = h2o.init()
h2o.no_progress()

new_matrix <- iran_data %>% 
  as.h2o()
           
model<- h2o.glm(y = "next_year_growth",
                training_frame = new_matrix, nfolds = 5)
model
h2o.mse(model)

