indexes%>% mutate(growth = SP500 / lag(SP500)) %>% .$growth -> growths
indexes%>% mutate(growth = (SP500 - lag(SP500)) / lag(SP500)) %>% .$growth -> growths2
indexes%>% mutate(growth = (SP500 - lag(SP500))) %>% .$growth -> growths3

qqnorm(growths)
qqline(growths)

qqnorm(growths2)
qqline(growths2)

qqnorm(growths3)
qqline(growths3)

shapiro.test(growths)
shapiro.test(growths2)
shapiro.test(growths3)

####none of them are normal 

spread(rbinded %>% select(Date, Open, Company), Company, Open) -> new_df
no_na_df <- new_df %>% select_if((function(col) sum(is.na(col)) < 500)) %>% na.omit()
nrow(no_na_df)
pca = prcomp(no_na_df %>% select(-Date), center = T)

pca$x[,1:10] %>%  as.data.frame() %>% mutate(Date = no_na_df$Date) %>% 
  inner_join(indexes %>% select(Date, SP500)) %>% 
  mutate(growth = SP500 >= lag(SP500)) %>% 
  select(-Date, SP500) %>% as.h2o( )-> final_h2o

model<- h2o.glm(y = "growth",
                training_frame = final_h2o,nfolds = 5, family = "binomial")

h2o.confusionMatrix(model)
