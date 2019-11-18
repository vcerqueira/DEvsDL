source("DeepLearning/feedforward-src.r")
source("DeepLearning/custom-metric.r")
source("DeepLearning/prepare-data.r")

ntimeseries <- 14

IDS <- 14

FF_RESULTS <- vector("list", ntimeseries) 
for (i in IDS) {
  
  tr_dir <- paste0("data/CSVs/ts",i,"/train.csv")
  tst_dir <- paste0("data/CSVs/ts",i,"/test.csv")
  vldtr_dir <- paste0("data/CSVs/ts",i,"/vld_train.csv")
  vldtst_dir <- paste0("data/CSVs/ts",i,"/vld_tst.csv")
  
  train <- read.csv(tr_dir, header=TRUE)
  test <- read.csv(tst_dir, header=TRUE)
  vldtr <- read.csv(vldtr_dir, header=TRUE)
  vldtst <- read.csv(vldtst_dir, header=TRUE)
  
  TRAIN_TEST <- prep_data(train, test)
  VALIDATION <- prep_data(vldtr, vldtst)
  
  #
  TRAIN <- TRAIN_TEST$TR
  TEST <- TRAIN_TEST$TST
  VLD_TRAIN <- VALIDATION$TR
  VLD_TST <- VALIDATION$TST
  
  
  FF_RESULTS <-
    FEEDFORWARD_GRIDSEARCH(
      train = TRAIN,
      test = TEST,
      vld_train = VLD_TRAIN,
      vld_test = VLD_TST
    )
  
  SIGN <- paste0("FF_",IDS[1],"_",IDS[length(IDS)],".rdata")
  save(FF_RESULTS, file = SIGN)
}

end <- "--"
save(end, file = paste0("FNSHD",IDS[1],"_",IDS[length(IDS)]))

##