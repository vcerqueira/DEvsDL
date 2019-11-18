form <- target ~.
load("data/tsdata_df.rdata")

library(Metrics)

source("DynEnsembles/prediction-workflow.r")
source("DynEnsembles/methods-specifications.r")

IDS <- 13:14

base_yhat <- vector("list", length(tsdata_df))
for (i in IDS) {
  #i<-1
  cat(i, "\n")
  x <- tsdata_df[[i]]
  
  RES_i <-
    get_predictions(
      form = form,
      train = tail(x$train,15000),
      test = x$test,
      specs = MODELSPECS
    )
  
  base_yhat[[i]] <- RES_i
  
  SIGNATURE <- paste0("BASE_YHAT_",IDS[1],"_",IDS[length(IDS)], ".rdata")
  save(base_yhat, file=SIGNATURE)
}


#### COMBINING DIFF PROCESSES

lf <- list.files()
rgx <- "BASE_YHAT"
lf <- lf[grepl(rgx, lf)]

RESULTS <- list()
for (lfile in lf) {
  cat(lfile,"\n")
  load(lfile)
  names(base_yhat) <- paste0("TS",1:length(base_yhat))
  RESULTS <- c(RESULTS, base_yhat)
}

RESULTS <- RESULTS[!sapply(RESULTS,is.null)]
RESULTS <- RESULTS[!duplicated(names(RESULTS))]
length(RESULTS)

#save(RESULTS, file = "DynEnsembles/RESULTS_BASE.rdata")

#

load("DynEnsembles/RESULTS_BASE.rdata")

for (i in 1:length(RESULTS)) {
  cat(i,"\n")
  
  x <- RESULTS[[i]]
  
  dir <- paste0("./data/CSVs/ts", i, "/")
  
  #if (!dir.exists(dir)) dir.create(dir)
  
  err <-
    sapply(x$Y_hat,
           function(yh) {
             rmse(x$Y, yh)
           })
  
  err <- round(t(err),3)
  
  write.csv(err, file = paste0(dir,"error_by_model.csv"), row.names = FALSE)
}



