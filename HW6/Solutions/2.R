library(car)
scatterplotMatrix(train_top)
library(GGally)
ggpairs(train_top)

library(mctest)
omcdiag(train_top %>% select(-SalePrice), train_top$SalePrice)
imcdiag(train_top %>% select(-SalePrice), train_top$SalePrice)
