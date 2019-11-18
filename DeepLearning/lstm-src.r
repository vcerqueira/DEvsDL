require(keras)
require(Metrics)

#### GRID SEARCH


LSTM_GS <- 
  function(train, test, vld_train,vld_test) {
    
    GS <- 
      expand.grid(
        list(nunits_1l = c(32, 128, 256),
             nunits_2l = c(16, 32),
             nepochs = c(2000),
             activation_lstm = c("tanh"),
             activation_dense = c("relu"),
             recurrent_activation=c("tanh"),
             unroll = c(F),
             stateful = c(F),
             use_bias = c(T),
             unit_forget_bias = c(F),
             dropout = c(0),
             recurrent_dropout = c(.5),
             batch_size = c(1),
             patience = c(10, 50),
             model_arch = c(LSTM_1L, LSTM_2L))
      )
    
    GS_CHAR <- 
      expand.grid(
        list(nunits_1l = c(32, 128, 256),
             nunits_2l = c(16, 32),
             nepochs = c(2000),
             activation_lstm = c("tanh"),
             activation_dense = c("relu"),
             recurrent_activation=c("tanh"),
             unroll = c(F),
             stateful = c(F),
             use_bias = c(T),
             unit_forget_bias = c(F),
             dropout = c(0),
             recurrent_dropout = c(.5),
             batch_size = c(1),
             patience = c(10, 50),
             model_arch = c("LSTM_1L", "LSTM_2L"))
      )
    #dim(GS_CHAR)
    
    if (nrow(GS_CHAR) != nrow(GS)) {
      stop("GS with diff rows")
    }
    
    GS_CHAR$val_loss <- NA_real_
    
    
    INNER_RESULTS <- vector("list", nrow(GS))
    for (j in 1:nrow(GS)) {
      PL_j <- GS[j,]
      #j<-1
      
      RES_j <-
        LSTM_INNER_WF(
          train = vld_train,
          test = vld_test,
          m_arch = GS[j, "model_arch"][[1]],
          PL = PL_j)
      
      cat("OUT now. Computing loss...\n")
      L <- rmse(RES_j$y, RES_j$y_hat) / rmse(RES_j$y, mean(RES_j$y)) 
      
      GS_CHAR$val_loss[j] <- L
    }
    
    cat("Running best config\n")
    best_config <- which.min(GS_CHAR[,"val_loss"])
    PL_bc <- GS[best_config,]
    
    RESULTS <-
      LSTM_INNER_WF(
        train = train,
        test = test,
        m_arch = GS[best_config, "model_arch"][[1]],
        PL = PL_bc)
    
    Results <- list(y=RESULTS$y, y_hat=RESULTS$y_hat)
    Config <- GS_CHAR[best_config,]
    Config["epochsrun"] = Results$epochsrun
    
    list(Results=Results,Config=Config)
  }


#### LSTM WORKFLOW

LSTM_INNER_WF <- 
  function(train,test,m_arch,PL) {
    
    p <- ncol(train$X)
    
    cat("Specifying model...\n")
    model <-
      m_arch(
        nunits_1l = PL$nunits_1l,
        nunits_2l = PL$nunits_2l,
        batch_size = PL$batch_size,
        npredictors = p,
        use_bias = PL$use_bias,
        activation_lstm = PL$activation_lstm,
        activation_dense = PL$activation_dense,
        unroll = PL$unroll,
        dropout = PL$dropout,
        recurrent_dropout=PL$recurrent_dropout,
        unit_forget_bias  = PL$unit_forget_bias,
        recurrent_activation=PL$recurrent_activation,
        stateful = PL$stateful,
        loss = "mse",
        metrics = c(metric_nrmse,"mse"),
        optimizer = "adam"
      )
    
    early_stopping <-
      callback_early_stopping(monitor = 'val_metric_nrmse',
                              patience = PL$patience, 
                              min_delta = .01)
    
    cat("Fitting...\n")
    best_loss <- 1e10
    iter <- 0
    for (i in 1:PL$nepochs) {
      cat("Epoch",i, "/", PL$nepochs, "\n")
      model %>% fit(x          = train$X, 
                    y          = train$y, 
                    batch_size = PL$batch_size,
                    epochs     = 1, 
                    validation_split = .2,
                    verbose    = 1, 
                    shuffle    = FALSE)
      
      y_hat <- model %>% 
        predict(test$X, 
                batch_size = PL$batch_size) %>% .[,1] 
      
      y <- test$y
      LOSS_j <- rmse(y, y_hat) / rmse(y, mean(y))
      
      if (is.na(LOSS_j)) LOSS_j <- 1e10
      
      if (best_loss - LOSS_j < .01) {
        cat("New best loss\n")
        best_loss <- LOSS_j
        iter <- 0
      } else {
        iter <- iter + 1
      }
      
      if (iter > PL$patience) {
        cat("Breaking\n")
        model %>% reset_states()
        break
      } else {
        model %>% reset_states()
      }
    }
    
    y_hat <- model %>% 
      predict(test$X, 
              batch_size = PL$batch_size) %>% .[,1] 
    
    y <- test$y
    
    list(y_hat = y_hat,
         y = y,
         epochsrun = i)
  }



#### ARCHITECTURES

LSTM_2L <- 
  function(nunits_1l,
           nunits_2l,
           batch_size,
           npredictors,
           use_bias = FALSE,
           activation_lstm = "relu",
           activation_dense = "relu",
           unroll = FALSE,
           dropout = .5,
           recurrent_dropout=.5,
           unit_forget_bias  = TRUE,
           recurrent_activation="hard_sigmoid",
           stateful = FALSE, 
           loss="mse", 
           metrics = "mse",
           optimizer="adam") {
    
    d3 <- 1
    
    model <- keras_model_sequential()
    model %>%
      layer_lstm(
        units = nunits_1l,
        batch_input_shape  = c(batch_size,
                               npredictors,
                               d3),
        use_bias = use_bias, 
        recurrent_dropout = recurrent_dropout,
        recurrent_activation=recurrent_activation, 
        unit_forget_bias = unit_forget_bias,
        #kernel_regularizer = regularizer_l2(l = 0.001),
        activation = activation_lstm,
        unroll = unroll,
        dropout = dropout,
        return_sequences = FALSE,
        stateful = stateful
      ) %>%
      layer_dense(units = nunits_2l,
                  activation = activation_dense) %>%
      layer_dropout(rate = dropout) %>% 
      layer_dense(units = 1)
    
    model %>% 
      compile(optimizer = optimizer,
              loss = loss, 
              metrics = metrics)
    
    model
  }


LSTM_1L <- 
  function(nunits_1l,
           nunits_2l,
           batch_size,
           npredictors,
           use_bias = FALSE,
           activation_lstm = "relu",
           activation_dense = "relu",
           unroll = FALSE,
           dropout = .5,
           recurrent_dropout=.5,
           unit_forget_bias  = TRUE,
           recurrent_activation="hard_sigmoid",
           stateful = FALSE, 
           loss="mse", 
           metrics = "mse",
           optimizer="adam") {
    
    d3 <- 1
    
    model <- keras_model_sequential()
    model %>%
      layer_lstm(
        units = nunits_1l,
        batch_input_shape  = c(batch_size,
                               npredictors,
                               d3),
        use_bias = use_bias, 
        recurrent_dropout = recurrent_dropout,
        recurrent_activation = recurrent_activation,
        unit_forget_bias = unit_forget_bias,
        #kernel_regularizer = regularizer_l2(l = 0.001),
        activation = activation_lstm,
        unroll = unroll,
        dropout = dropout,
        return_sequences = FALSE,
        stateful = stateful
      ) %>%
      layer_dense(units = 1)
    
    model %>% 
      compile(optimizer = optimizer,
              loss = loss, 
              metrics = metrics)
    
    
    model
  }