sp <- train_top$SalePrice
for(i in 2:11){
  other <- train_top[, i]
  sp_other <- data.frame(sp = sp,
                   xaxis = other)
  colnames(sp_other) <- c("sp", "xaxis")
  ggplot(sp_other, aes(x=xaxis, y=sp)) + 
    geom_point(alpha = 0.3)+
    geom_smooth(method = "loess")+
    geom_smooth(method = "lm", color = "red", alpha = 0.5)+
    xlab(colnames(other))+
    ylab(colnames(sp)) -> graph
  print(graph)
}

#####
#####   OverallQual
other <- train_top[, 2]
sp_other <- data.frame(sp = sp,
                 xaxis = other*other)
colnames(sp_other) <- c("sp", "xaxis")
ggplot(sp_other, aes(x=xaxis, y=sp)) + 
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  geom_smooth(method = "lm", color = "red", alpha = 0.5)+
  xlab(paste(colnames(other), "^2"))+
  ylab(colnames(sp)) -> graph
print(graph)

#####
#####   TotalBsmtSF
other <- train_top[, 6]
sp_other <- data.frame(sp = sp,
                       xaxis = other *other)
colnames(sp_other) <- c("sp", "xaxis")
ggplot(sp_other %>% filter(xaxis < 30000000), aes(x=xaxis, y=sp)) + 
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  geom_smooth(method = "lm", color = "red", alpha = 0.5)+
  xlab(paste(colnames(other), "^2"))+
  ylab(colnames(sp)) -> graph
print(graph)

#####
#####   improving model

train_top_improved <- train_top %>% 
  mutate(TotalBsmtSF2 = TotalBsmtSF ** 2,
         TotalBsmtSF3 = TotalBsmtSF ** 3,
         OverallQual2 = OverallQual ** 2,
         OverallQual3 = OverallQual ** 3,
         GrLivArea2 = GrLivArea ** 2,
         GarageArea2 = GarageArea ** 2,
         YearBuilt2 = YearBuilt ** 2)

model_improved <- lm(log2(SalePrice) ~ ., data = train_top_improved %>%
                       dplyr::select(-TotRmsAbvGrd, -GarageArea, -TotalBsmtSF, -`1stFlrSF`))
summary(model_improved)
