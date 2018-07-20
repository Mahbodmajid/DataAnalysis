model_df <- disaster %>%
  select(LONGITUDE, LATITUDE, EQ_PRIMARY, FOCAL_DEPTH, TOTAL_DEATHS)
model<- glm(TOTAL_DEATHS ~., model_df, family = "poisson")
model %>% summary()
