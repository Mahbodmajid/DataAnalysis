train %>% select_if(is.numeric)  -> train_nuemric
cormat <- round(cor(train_nuemric), 4)

melted_cormat <- melt(cormat, na.rm = TRUE)

ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()

library("Hmisc")
res2 <- rcorr(as.matrix(train_nuemric))
# Extract the correlation coefficients
kable(res2$r, digits = 3)
# Extract p-values
kable(res2$P, digits = 3)

melted_cormat %>%
  filter(Var1 == "SalePrice") %>%
  filter(Var2 != "SalePrice") %>% 
  top_n(n = 10, wt = value) %>%
  arrange(-value) -> top_cor
kable(top_cor)

train_nuemric %>% select(SalePrice, OverallQual, GrLivArea, GarageCars, GarageArea,
                         TotalBsmtSF, `1stFlrSF`, FullBath, TotRmsAbvGrd, YearBuilt,
                         YearRemodAdd) -> train_top
