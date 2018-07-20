accuracy_info = AccuracyCutoffInfo(train = train , test = test, 
                                    predict = "prediction", actual = "MannerOfDeath" )
accuracy_info$plot
(accuracy_info$data)[which.max((accuracy_info$data)$test),] -> best_co_data
best_co_data
best_co <- best_co_data$cutoff
best_co
