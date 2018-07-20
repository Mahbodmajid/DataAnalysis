rbinded %>%
  add_name_sector() %>%
  filter(!is.na(Sector)) %>% 
  group_by(Date, Sector) %>% 
  summarize(Open = mean(Open)) %>%
  ungroup() %>% 
  spread(Sector, Open) -> new_df

full_df <- new_df %>% full_join(indexes) %>%  na.omit() 
full_df %>% select(-Date) %>%  as.matrix() -> mymatrix
rownames(mymatrix) <- full_df$Date %>% as.character()
pca = prcomp(mymatrix, center = T, scale. = T)
vars = pca$sdev^2
biplot(pca,cex=0.8)
ggbiplot::ggbiplot(pca, 1:2, groups =full_df$Date, labels = full_df$Date %>% as.character())
