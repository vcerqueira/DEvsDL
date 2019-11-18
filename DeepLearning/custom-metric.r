require(keras)

nrmse <-
  function(y_true, y_pred) {
    require(keras)
    K <- backend()
    #y_true1<<-y_true#<-y_true1
    #y_pred1<<-y_pred#<-y_pred1
    
    avg_y <- K$mean(y_true, keepdims = TRUE)
    
    rmse_avg <- K$sqrt(K$mean((y_true - avg_y) ^ 2, keepdims = TRUE))
    
    rmse <- K$sqrt(K$mean((y_true - y_pred) ^ 2, keepdims = TRUE))
    
    nrmse <- rmse / rmse_avg
    
    nrmse
  }

metric_nrmse <-
  custom_metric("metric_nrmse",
                function(y_true, y_pred) {
                  nrmse(y_true, y_pred)
                })