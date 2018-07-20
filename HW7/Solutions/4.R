ms_4 = ms_3 %>% mutate(pred = fitted(model))

ggplot(ms_4,aes(x = Age,y = pred,col = MannerOfDeath))+
  geom_point(alpha = 0.5)

ggplot() + 
  geom_line(aes(x = model$linear.predictors, y = model$fitted.values), color = "red", alpha = 0.8) + 
  geom_point(aes(color = ms_4$MannerOfDeath, x = model$linear.predictors, y = as.numeric(as.character(ms_4$MannerOfDeath))))+
  xlab("Age")+
  ylab("MannerOfDeath")

ggplot(ms_4, aes(pred, color = MannerOfDeath )) + 
  geom_density( size = 1 ) +
  ggtitle( "Training Set's Predicted Score")

cm_info = ConfusionMatrixInfo(data = ms_4, predict = "pred", 
                               actual = "MannerOfDeath", cutoff = .5 )
cm_info$plot
