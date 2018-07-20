AAPL_sorted %>%na.omit() %>% select(-`Adj Close`) %>% arrange(Date) %>% select(-Date, -Company) -> apple

apple %>% as.matrix() -> mymatrix
pca = prcomp(mymatrix, center = T, scale. = T)
pca$x[,1] -> pca_open

new_matrix <- matrix(data = pca_open, byrow = T, ncol = 5)
other_matrix <- matrix(data = apple$Open, byrow = T, ncol = 5)
new_matrix[,5] <- other_matrix[,5]
new_h2o <-
  new_matrix %>% 
  as.data.frame() %>% 
  as.h2o()

interval <- 1:50
mses <- c()
for(k in interval){
  new_matrix <- matrix(data = pca_open, byrow = T, ncol = k + 1)
  other_matrix <- matrix(data = apple$Open, byrow = T, ncol = k + 1)
  new_matrix[,k + 1] <- other_matrix[,k + 1]
  new_h2o <-
    new_matrix %>% 
    as.data.frame() %>% 
    as.h2o()
  
  model<- h2o.glm(y = paste0("V", k+1),
                  training_frame = new_h2o,nfolds = 5)
  c(mses, h2o.mse(model)) -> mses
  # print(k)
}
data.frame(mses = mses, k = interval) -> mses_df
ggplot(mses_df, aes(x = k, y = mses))+
  geom_point()


mses_df %>% .[which.min(mses_df$mses), ] %>% kable()
