cost_fp = 100;cost_fn = 100
roc_info = ROCInfo( data = test, predict = "prediction", 
                    actual = "MannerOfDeath", cost.fp = cost_fp, cost.fn = cost_fn )
roc_info$roc_plot
roc_info$cutoff
