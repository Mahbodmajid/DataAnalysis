read_csv("./../house/test.csv") -> test_data

test_data_2 <- test_data %>% 
  mutate(TotalBsmtSF2 = TotalBsmtSF ** 2,
         TotalBsmtSF3 = TotalBsmtSF ** 3,
         OverallQual2 = OverallQual ** 2,
         OverallQual3 = OverallQual ** 3,
         OverallQual4 = OverallQual ** 3,
         GrLivArea2 = GrLivArea ** 2,
         GarageArea2 = GarageArea ** 2,
         YearBuilt2 = YearBuilt ** 2)
test_data_2[is.na(test_data_2)] <- 0

test_data_2 %>% 
  mutate(SalePrice = 2 ** predict.lm(model_improved, test_data_2)) %>% 
  dplyr::select(Id, SalePrice) -> result

write_csv(result, "./../result.csv")
