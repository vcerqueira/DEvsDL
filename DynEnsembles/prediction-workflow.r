get_predictions <- 
  function(form, train, test, specs) {
    M0 <-
      build_base_ensemble(
        form = form,
        data = train,
        specs = specs,
        num_cores = 1
      )
    
    MODELSIZE <- 
      sapply(M0@base_models,
             function(x) {
               object.size(x)
             })
    
    Y_hat <- predict(M0, test)
    ttimecost <- attr(Y_hat,"Times")
    
    Y <- test[,1,drop=TRUE]
    Y_tr <- train[,1,drop=TRUE]
    
    Y_hat_tr <- predict(M0, train)
    ttimecost_tr <- attr(Y_hat_tr,"Times")
    
    # Y_hat[] <- 
    #   lapply(Y_hat,
    #          function(x) {
    #            posp_ts(
    #              pointf = x,
    #              trend_xi = TRprocessed$train_adj_xi,
    #              lambda = TRprocessed$lambda,
    #              seasonal_factor = TRprocessed$seasonal_factor
    #            )
    #          })
    
    list(
      Y_hat = Y_hat,
      Y = Y,
      Y_hat_tr=Y_hat_tr,
      Y_tr=Y_tr,
      TRAINK = train,
      TESTK = test,
      MODELSIZE = MODELSIZE,
      TESTTIME = ttimecost,
      TRAINTIME = ttimecost_tr
    )
  }