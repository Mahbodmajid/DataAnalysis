spread(rbinded %>% select(Date, Open, Company), Company, Open) %>% 
  select(-Date) -> new_df
no_na_df <- new_df %>% select_if((function(col) sum(is.na(col)) < 500)) %>% na.omit()
nrow(no_na_df)
pca = prcomp(no_na_df, center = T)
vars = pca$sdev^2


qcc::pareto.chart(vars[1:20])
vars %>% str()
sum(vars[1:3])/ sum(vars)
