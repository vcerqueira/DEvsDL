load("data/timeseries_DEvDL.rdata")

source("data/pre-process-src.r")

library(forecast)
library(tsensembler)

tsdata_df <- lapply(tsdata, ts_to_df)
save(tsdata_df, file = "data/tsdata_df.rdata")

for (i in 1:length(tsdata)) {
  cat(i,"\n")
  dir <- paste0("./data/CSVs/ts", i, "/")
  
  if (!dir.exists(dir)) dir.create(dir)
  
  x <- ts_to_df(tsdata[[i]])
  
  tr <- x$train
  tst <- x$test
  vld_tr <- x$in_train
  vld_tst <- x$validation
  
  write.csv(tr, file = paste0(dir,"train.csv"), row.names = FALSE)
  write.csv(tst, file = paste0(dir,"test.csv"), row.names = FALSE)
  write.csv(vld_tr, file = paste0(dir,"vld_train.csv"), row.names = FALSE)
  write.csv(vld_tst, file = paste0(dir,"vld_tst.csv"), row.names = FALSE)
}

#vld_tr <- read.csv("data/CSVs/ts1/vld_train.csv")

