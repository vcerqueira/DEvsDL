ts_to_df_noPP <- 
  function(x) {
    
    # frequency
    FRQ <- frequency(x)
    # time steps (window to look back)
    k <- 20 + 1
    
    # SPLITTING INTO TRAIN/TEST (70/30)
    x <- diff(x)
    x_split <- ts_holdout(x, ratio = .7, frq = FRQ)
    
    TRAIN <- x_split$train
    TEST <- x_split$test
    
    # PRE-PROCESSING TRAIN, RETRIEVE PROCESSING INFO
    # TRAIN_pp <-
    #   pp_ts_train(tr = TRAIN,
    #               frq = FRQ,
    #               h = 1,
    #               k = k)
    
    # PRE-PROCESSING TEST WITH TRAIN INFO
    # TEST_pp <- 
    #   pp_ts_test(
    #     x = TEST,
    #     lambda = TRAIN_pp$lambda,
    #     seasonal_factor = TRAIN_pp$seasonal_factor,
    #     first_diffs = TRAIN_pp$train_adj_xi 
    #   )
    
    DS <-
      create_datasets(train = TRAIN,
                      test = TEST,
                      k = k)
    
    DS$train <- DS$train[complete.cases(DS$train),]
    
    trainx <- DS$train
    trvl_IDS <- 1:ceiling((nrow(trainx) * .7))
    in_train <- trainx[trvl_IDS,]
    validation <- trainx[-trvl_IDS,]
    
    
    list(train=DS$train, 
         test=DS$test,
         in_train = in_train,
         validation = validation,
         #TRAIN_pp = TRAIN_pp,
         #TEST_pp = TEST_pp,
         TRAIN_RAW = TRAIN,
         TEST_RAW = TEST)
  }


ts_to_df <- 
  function(x) {
    
    # frequency
    FRQ <- frequency(x)
    # time steps (window to look back)
    k <- 20 + 1
    
    # SPLITTING INTO TRAIN/TEST (70/30)
    x_split <- ts_holdout(x, ratio = .7, frq = FRQ)
    
    TRAIN <- x_split$train
    TEST <- x_split$test
    
    # PRE-PROCESSING TRAIN, RETRIEVE PROCESSING INFO
    TRAIN_pp <-
      pp_ts_train(tr = TRAIN,
                  frq = FRQ,
                  h = 1,
                  k = k)
    
    # PRE-PROCESSING TEST WITH TRAIN INFO
    TEST_pp <- 
      pp_ts_test(
        x = TEST,
        lambda = TRAIN_pp$lambda,
        seasonal_factor = TRAIN_pp$seasonal_factor,
        first_diffs = TRAIN_pp$train_adj_xi 
      )
    
    DS <-
      create_datasets(train = TRAIN_pp$train_adj,
                      test = TEST_pp,
                      k = k)
    
    DS$train <- DS$train[complete.cases(DS$train),]
    
    trainx <- DS$train
    trvl_IDS <- 1:ceiling((nrow(trainx) * .7))
    in_train <- trainx[trvl_IDS,]
    validation <- trainx[-trvl_IDS,]
    
    
    list(train=DS$train, 
         test=DS$test,
         in_train = in_train,
         validation = validation,
         TRAIN_pp = TRAIN_pp,
         TEST_pp = TEST_pp,
         TRAIN_RAW = TRAIN,
         TEST_RAW = TEST)
  }




ts_holdout <-
  function(x, ratio, frq) {
    len <- NROW(x)
    
    train <- head(x, ceiling(ratio * len))
    test <- tail(x, len - ceiling(ratio * len))
    
    train <- ts(train, frequency = frq)
    test <- ts(test, frequency = frq)
    
    stopifnot(is.ts(train))
    stopifnot(is.ts(test))
    
    list(train=train,test=test)
  }

pp_ts_train_winf <- 
  function(tr, frq, h, k, lambda, seasonal_factor=NULL, do_first_diffs=FALSE) {
    require(randtests)
    
    if (class(tr) != "ts") {
      stop("x not class ts")
    }
    
    #LAMBDA
    tr <- BoxCox(tr, lambda)
    
    len <- length(tr)
    
    ###SEASONO
    if (!is.null(seasonal_factor)) {
      sfctr <- rep(seasonal_factor, trunc(1 + len/frq))[1:len]
      #sfctr <- seasonal_factor
      tr_adj = tr / sfctr
      
      tr_adj <- ts(tr_adj, frequency = frq)
      
    } else {
      tr_adj <- tr
      seasonal_factor <- NULL
    }
    
    tr_adj[is.infinite(tr_adj)] <- NaN
    tr_adj[is.na(tr_adj)] <- mean(tr_adj, na.rm=TRUE)
    
    tr[is.infinite(tr)] <- NaN
    tr[is.na(tr)] <- mean(tr, na.rm=TRUE)
    
    ## DIFFS
    if (do_first_diffs) {
      tr_adj_xi <- tr_adj[length(tr_adj)]
      tr_adj <- diff(tr_adj, 1)
      tr_adj <- c(NA_real_, tr_adj)
      #fd_point_adj <- tail(tr_adj, 1)
    } else {
      tr_adj_xi <- NULL
    }
    
    if (do_first_diffs) {
      tr_xi <- tr[length(tr)]
      tr <- diff(tr, 1)
      tr <- c(NA_real_, tr)
      #fd_point <- tail(tr, 1)
    } else {
      tr_xi <- NULL
    }
    
    trk <- embed_timeseries(as.numeric(tr_adj), k)
    
    list(
      embedded_tr = trk,
      train_adj = tr_adj,
      train = tr,
      train_xi = tr_xi,
      train_adj_xi = tr_adj_xi,
      seasonal_factor = seasonal_factor,
      lambda = lambda
    )
  }

pp_ts_train <-
  function(tr, frq, h, k) {
    require(randtests)
    
    if (class(tr) != "ts") {
      stop("x not class ts")
    }
    
    lambda <- BoxCox.lambda(tr)
    tr <- BoxCox(tr, lambda)
    
    seas_test <- tryCatch(nsdiffs(tr), error = function(e) 0)
    if (seas_test > 0) {
      decomp <-
        tryCatch(decompose(tr, "multiplicative"),
                 error=function(e) {
                   NULL
                 })
      
      if (!is.null(decomp)) {
        tr_adj = tr / decomp$seasonal
        
        #seasonal_factor <- rep(tail(decomp$seasonal, frq), trunc(1 + h/frq))[1:h]
        seasonal_factor <- tail(decomp$seasonal, frq)
        
        tr_adj <- ts(tr_adj, frequency = frq)
      } else {
        tr_adj <- tr
        seasonal_factor <- NULL
      }
    } else {
      tr_adj <- tr
      seasonal_factor <- NULL
    }
    
    tr_adj[is.infinite(tr_adj)] <- NaN
    tr_adj[is.na(tr_adj)] <- mean(tr_adj, na.rm=TRUE)
    
    tr[is.infinite(tr)] <- NaN
    tr[is.na(tr)] <- mean(tr, na.rm=TRUE)
    
    coxstuart <- cox.stuart.test(tr_adj)$p.value
    if (coxstuart < .05) {
      tr_adj_xi <- tr_adj[length(tr_adj)]
      tr_adj <- diff(tr_adj, 1)
      tr_adj <- c(NA_real_, tr_adj)
      #fd_point_adj <- tail(tr_adj, 1)
    } else {
      tr_adj_xi <- NULL
    }
    
    coxstuart2 <- cox.stuart.test(tr)$p.value
    if (coxstuart2 < .05) {
      tr_xi <- tr[length(tr)]
      tr <- diff(tr, 1)
      tr <- c(NA_real_, tr)
      #fd_point <- tail(tr, 1)
    } else {
      tr_xi <- NULL
    }
    
    trk <- embed_timeseries(as.numeric(tr_adj), k)
    
    list(
      embedded_tr = trk,
      train_adj = tr_adj,
      train = tr,
      train_xi = tr_xi,
      train_adj_xi = tr_adj_xi,
      #fd_point_adj = fd_point_adj,
      #fd_point = fd_point,
      seasonal_factor = seasonal_factor,
      lambda = lambda,
      do_diffs = !is.null(tr_adj_xi)
    )
  }

# data("AirPassengers")
# x <- AirPassengers
# frequency(x)
# length(AirPassengers)
# a <- ts(x[1:116], frequency = 12)
# b <- ts(x[117:144], frequency = 12)
# ax <- pp_ts_train(a, frq = 12, 1,10)
# 
# seasonal_factor <- ax$seasonal_factor
# 
# ax$train_adj
# pp_ts_test(
#   b,
#   lambda = ax$lambda,
#   seasonal_factor = ax$seasonal_factor,
#   first_diffs = !is.null(ax$train_adj_xi)
# )
# 
# a<-as.numeric(ts(1:40, frequency = 1))
# b <- as.numeric(ts(41:55, frequency = 1))
# k<-10
# ak <- embed_timeseries(a, k)
# a_t <- tail(a, k-1)
# 
# b_t <- embed_timeseries(c(a_t,b), k)
# b_t$target

create_datasets <- 
  function(train, test, k) {
    train <- as.numeric(train)
    test <- as.numeric(test)
    
    tr_k <- embed_timeseries(train, k)
    
    tr_tail <- tail(train, k-1)
    
    ts_k <- embed_timeseries(c(tr_tail,test), k)
    
    list(train=tr_k,test=ts_k)
  }

pp_ts_test <- 
  function(x, lambda, seasonal_factor, first_diffs = NULL) {
    if (class(x) != "ts") {
      stop("x not class ts")
    }
    
    len <- length(x)
    frq <- frequency(x)
    
    x <- BoxCox(x, lambda)
    
    if (!is.null(seasonal_factor)) {
      sfctr <- rep(seasonal_factor, trunc(1 + len/frq))[1:len]
      x_adj = x / sfctr
    } else {
      x_adj <- x
    }
    
    if (!is.null(first_diffs)) {
      x_adj <- diff(c(first_diffs, x_adj), 1)
    }
    
    x_adj
  }

posp_ts <-
  function(pointf, trend_xi, lambda, seasonal_factor) {
    if (!is.null(trend_xi)) {
      pointf <- diffinv(pointf,xi = trend_xi)[-1]
    }
    
    if (!is.null(seasonal_factor)) {
      len <- length(pointf)
      frq <- length(seasonal_factor)
      
      sfctr <- rep(seasonal_factor, trunc(1 + len/frq))[1:len]
      
      pointf <- pointf * sfctr
    }
    
    if (!is.null(lambda)) {
      pointf <- InvBoxCox(pointf, lambda)
    }
    
    pointf
  }
