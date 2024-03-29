source("DeepLearning/lstm-src.r")
source("DeepLearning/custom-metric.r")
source("DeepLearning/prepare-data.r")

ntimeseries <- 14

IDS <- 14

LSTM_RESULTS <- vector("list", ntimeseries) 
for (i in IDS) {
  #i<-1
  tr_dir <- paste0("data/CSVs/ts",i,"/train.csv")
  tst_dir <- paste0("data/CSVs/ts",i,"/test.csv")
  vldtr_dir <- paste0("data/CSVs/ts",i,"/vld_train.csv")
  vldtst_dir <- paste0("data/CSVs/ts",i,"/vld_tst.csv")
  
  train <- read.csv(tr_dir, header=TRUE)
  test <- read.csv(tst_dir, header=TRUE)
  vldtr <- read.csv(vldtr_dir, header=TRUE)
  vldtst <- read.csv(vldtst_dir, header=TRUE)
  
  TRAIN_TEST <- prep_data_RNN(train, test)
  VALIDATION <- prep_data_RNN(vldtr, vldtst)
  
  #
  TRAIN <- TRAIN_TEST$TR
  TEST <- TRAIN_TEST$TST
  VLD_TRAIN <- VALIDATION$TR
  VLD_TST <- VALIDATION$TST
  
  LSTM_RESULTS <-
    LSTM_GS(
      train = TRAIN,
      test = TEST,
      vld_train = VLD_TRAIN,
      vld_test = VLD_TST
    )
  
  SIGN <- paste0("LSTM_",IDS[1],"_",IDS[length(IDS)],".rdata")
  save(LSTM_RESULTS, file = SIGN)
}

end <- "--"
save(end, file = paste0("FNSHD_LSTM_",IDS[1],"_",IDS[length(IDS)]))

##