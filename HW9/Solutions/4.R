rbinded %>% filter(Company == "AAPL") -> AAPL
AAPL %>% arrange(Date) -> AAPL_sorted
interval <- 1:140
library(h2o)
localH2O = h2o.init()
h2o.no_progress()
new_matrix <- matrix(data = AAPL_sorted$Open, byrow = T, ncol = 5) %>% 
  as.data.frame() %>% 
  as.h2o()

mses <- c()
for(k in interval){
  new_matrix <- matrix(data = AAPL_sorted$Open, byrow = T, ncol = k + 1) %>% 
    as.data.frame() %>%
    as.h2o()
  model<- h2o.glm(y = paste0("V", k+1),
                          training_frame = new_matrix,nfolds = 5)
  c(mses, h2o.mse(model)) -> mses
 # print(k)
}
data.frame(mses = mses, k = interval) -> mses_df
ggplot(mses_df, aes(x = k, y = mses))+
  geom_point()

mses_df %>% .[which.min(mses_df$mses), ] %>% kable()
