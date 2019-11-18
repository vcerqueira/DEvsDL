prep_data <- 
  function(train, test) {
    TGTCOL <- 1
    
    y_tr <- array(train[,TGTCOL,drop=TRUE])
    y_ts <- array(test[,TGTCOL,drop=TRUE])
    
    X_tr <- subset(train, select = -TGTCOL)
    X_ts <- subset(test, select = -TGTCOL)
    
    X_tr <- as.matrix(X_tr)
    X_ts <- as.matrix(X_ts)
    
    meanx <- apply(X_tr, 2, mean)
    sdevx <- apply(X_tr, 2, sd)
    
    X_tr <- scale(X_tr, center = meanx, scale = sdevx)
    X_ts <- scale(X_ts, center = meanx, scale = sdevx)
    
    TR <- list(X=X_tr, y=y_tr)
    TST <- list(X=X_ts, y=y_ts)
    
    
    list(TR=TR,TST=TST)
  }


prep_data_RNN <- 
  function(train, test, batch_size=NULL) {
    
    TGTCOL <- 1
    
    if (!is.null(batch_size)) {
      require(plyr)
      ntr <- round_any(nrow(train), batch_size, floor)
      train <- head(train, ntr)
    }
    
    y_tr <- array(train[,TGTCOL,drop=TRUE])
    y_ts <- array(test[,TGTCOL,drop=TRUE])
    
    X_tr <- subset(train, select = -TGTCOL)
    X_ts <- subset(test, select = -TGTCOL)
    
    X_tr <- as.matrix(X_tr)
    X_ts <- as.matrix(X_ts)
    
    meanx <- apply(X_tr, 2, mean)
    sdevx <- apply(X_tr, 2, sd)
    
    X_tr <- scale(X_tr, center = meanx, scale = sdevx)
    X_ts <- scale(X_ts, center = meanx, scale = sdevx)
    
    ### RNN DIMS
    npredictors <- 1 #univariate series
    d.tr <- dim(X_tr)
    d.ts <- dim(X_ts)
    
    X_tr <- array(X_tr, dim = c(d.tr[1],d.tr[2], npredictors))
    X_ts <- array(X_ts, dim = c(d.ts[1],d.ts[2], npredictors))
    #############
    
    TR <- list(X=X_tr, y=y_tr)
    TST <- list(X=X_ts, y=y_ts)
    
    
    list(TR=TR,TST=TST)
  }

# OBSOLETE
# prep_data <- 
#   function(form, train, test, is_lstm=FALSE, batch_size) {
#     require(plyr)
#     in_train <- holdout(train, .8)
#     
#     validation <- in_train$test
#     in_train <- in_train$train
#     
#     if (!is.null(batch_size)) {
#       ntr <- round_any(nrow(train), batch_size, floor)
#       nitr <- round_any(nrow(in_train), batch_size, floor)
#       nvl <- round_any(nrow(validation), batch_size, floor)
#       
#       train <- head(train,ntr)
#       in_train <- head(in_train,nitr)
#       validation <- head(validation,nvl)
#     }
#     
#     y_tr <- array(get_y(train, form))
#     y_itr <- array(get_y(in_train, form))
#     y_vl <- array(get_y(validation, form))
#     y_ts <- array(get_y(test, form))
#     
#     tgt <- get_target(form)
#     tgti <- pmatch(tgt, colnames(train))
#     
#     trainx <- subset(train, select = -tgti)
#     intrainx <- subset(in_train, select = -tgti)
#     validx <- subset(validation, select = -tgti)
#     testx <- subset(test, select = -tgti)
#     
#     trainx <- as.matrix(trainx)
#     intrainx <- as.matrix(intrainx)
#     validx <- as.matrix(validx)
#     testx <- as.matrix(testx)
#     
#     meanx <- apply(trainx, 2, mean)
#     sdevx <- apply(trainx, 2, sd)
#     
#     trainx <- scale(trainx, center = meanx, scale = sdevx)
#     intrainx <- scale(intrainx, center = meanx, scale = sdevx)
#     validx <- scale(validx, center = meanx, scale = sdevx)
#     testx <- scale(testx, center = meanx, scale = sdevx)
#     
#     if (is_lstm) {
#       np <- 1
#       d.tr <- dim(trainx)
#       d.itr <- dim(intrainx)
#       d.vl <- dim(validx)
#       d.ts <- dim(testx)
#       
#       trainx <- array(trainx, dim = c(d.tr[1],d.tr[2], np))
#       intrainx <- array(intrainx, dim = c(d.itr[1],d.itr[2], np))
#       validx <- array(validx, dim = c(d.vl[1],d.vl[2], np))
#       testx <- array(testx, dim = c(d.ts[1],d.ts[2], np))
#     }
#     
#     
#     X <- list(trainx=trainx,
#               intrainx=intrainx,
#               validx=validx,
#               testx=testx)
#     
#     y <- list(y_tr=y_tr,
#               y_itr=y_itr,
#               y_vl=y_vl,
#               y_ts=y_ts)
#     
#     list(X=X,y=y)
#   }