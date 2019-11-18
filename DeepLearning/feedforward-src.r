library(keras)
library(Metrics)

source("DeepLearning/custom-metric.r")

####### GRID SEARCH


FEEDFORWARD_GRIDSEARCH <- 
  function(train, test, vld_train, vld_test) {
    GS <-
      expand.grid(
        list(nunits_1l = c(64,128,256),
             nunits_2l = c(32,64),
             nepochs = c(500,1000),
             activation_fun = c("relu"),
             dropout = c(.25,.5),
             batch_size = c(50, 100),
             patience = c(50),
             model_arch = c(FF_1L,FF_2L))
      )
    
    GS_CHAR <-
      expand.grid(
        list(nunits_1l = c(64,128,256),
             nunits_2l = c(32,64),
             nepochs = c(500,1000),
             activation_fun = c("relu"),
             dropout = c(.25,.5),
             batch_size = c(50, 100),
             patience = c(50),
             model_arch = c("FF_1L","FF_2L"))
      )
    
    GS_CHAR$val_loss <- NA_real_
    
    VALIDATION_RESULTS <- vector("list", nrow(GS))
    for (j in 1:nrow(GS)) {
      PL_j <- GS[j,]
      
      RES_j <-
        FF_INNER_WF(
          train = vld_train,
          test = vld_test,
          m_arch = GS[j, "model_arch"][[1]],
          PL = PL_j)
      
      LOSS_j <- rmse(RES_j$y, RES_j$y_hat)
      
      GS_CHAR$val_loss[j] <- LOSS_j
    }
    
    cat("Running best config\n")
    best_config <- which.min(GS_CHAR[,"val_loss"])
    PL_bc <- GS[best_config,]
    
    RESULT <-
      FF_INNER_WF(
        train = train,
        test = test,
        m_arch = GS[best_config, "model_arch"][[1]],
        PL = PL_bc)
    
    Results <- list(y = RESULT$y, y_hat = RESULT$y_hat)
    Config <- GS_CHAR[best_config,]
    
    list(Results=Results,Config=Config)
  }



####### PREDICTION WORKFLOW

FF_INNER_WF <- 
  function(train,test,m_arch,PL) {
    model <-
      m_arch(
        npredictors = dim(train$X)[[2]],
        nunits_1l = PL$nunits_1l,
        nunits_2l = PL$nunits_2l,
        activation_fun = as.character(PL$activation_fun),
        optimizer = "adam",
        loss = "mse",
        metrics = c(metric_nrmse,"mse")
      )
    
    early_stopping <-
      callback_early_stopping(monitor = 'val_metric_nrmse',
                              patience = PL$patience, 
                              min_delta = .01)
    
    model %>% fit(x          = train$X, 
                  y          = train$y, 
                  batch_size = PL$batch_size,
                  epochs     = PL$nepochs, 
                  validation_split = .2,
                  verbose    = 1, 
                  shuffle    = FALSE,
                  callbacks = c(early_stopping))
    
    y_hat <- model %>% 
      predict(test$X) %>% .[,1] 
    
    y <- test$y
    
    list(y_hat=y_hat, y=y)
  }



######## ARCHITECTURES

FF_2L <-
  function(npredictors, 
           nunits_1l=32, 
           nunits_2l=16, 
           activation_fun="relu", 
           dropout=.4,
           optimizer = "adam", 
           loss="mse", 
           metrics="mse") {
    require(keras)
    
    model <- keras_model_sequential() %>%
      layer_dense(
        units = nunits_1l,
        activation = activation_fun,
        input_shape = npredictors
      ) %>%
      layer_dropout(rate = dropout) %>% 
      layer_dense(units = nunits_2l, 
                  activation = activation_fun) %>%
      layer_dropout(rate = dropout) %>% 
      layer_dense(units = 1)
    
    model %>% compile(optimizer = optimizer,
                      loss = loss,
                      metrics = metrics)
    
    model
  }

FF_1L <-
  function(npredictors, 
           nunits_1l=32, 
           nunits_2l=16, 
           activation_fun="relu", 
           dropout=.4,
           optimizer = "adam", 
           loss="mse", 
           metrics="mse") {
    require(keras)
    
    model <- keras_model_sequential() %>%
      layer_dense(
        units = nunits_1l,
        activation = activation_fun,
        input_shape = npredictors
      ) %>%
      layer_dropout(rate = dropout) %>% 
      layer_dense(units = 1)
    
    model %>% compile(optimizer = optimizer,
                      loss = loss,
                      metrics = metrics)
    
    model
  }
