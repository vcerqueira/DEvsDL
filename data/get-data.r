nms <- c(
  "internet_traffic_data_in_bits_fr3",
  "mean_daily_saugeen_river_flows_j",
  "co_gt_",
  "no2_gt_",
  "hl_equip",
  "avg_gbl_hor",
  "preciosa_mar",
  "sign_wave_height",
  "amial",
  "aep"
)

load("data/timeseries.rda")

library(tsdl)
library(xts)

tsdata <- timeseries[sapply(timeseries, length) > 5000]
tsdata <- tsdata[names(tsdata) %in% nms]

# length(tsdata)
# sapply(tsdata, class)
# sapply(tsdata, function(x) x[1:10])
# names(tsdata)

FRQ <- c(24, 24, 288, 365, 24, 24, 48, 48, 48, 24)

for (i in 1:length(tsdata)) {
  tsdata[[i]] <- ts(as.vector(tsdata[[i]]), frequency = FRQ[i])
}

tsdata2 <- tsdl[sapply(tsdl, length) > 10000]
is_univar <- sapply(tsdata2, class) == "ts"
tsdata2 <- tsdata2[is_univar]

names(tsdata2) <- paste0("TS", 1:length(tsdata2))

tsdata <- c(tsdata, tsdata2)

# length(tsdata)
# sapply(tsdata, length)
# sapply(tsdata, class)

save(tsdata, file = "data/timeseries_DEvDL.rdata")