

# delete all objects
# rm(list = ls())


mypath = "D:/Toranj14011217/RFM_Churn_Toranj/"
setwd(mypath)

# ReadData ----------------------------------------------------------------

source("Requirements.R")

AnarTrades <- read.csv(file = "AnarAllTrades_14010101.csv", fileEncoding = "UTF-8")


# Calc 6 month Initial R, F, M ------------------------------------------------------------

DF = AnarTrades
DF$Value = as.numeric(AnarTrades$number_of_units) * as.numeric(AnarTrades$price)

DF6M <- DF %>% dplyr::filter(date_key %in% c(StartDate : EndDate))
DF6M$buyer_code_melli <- as.numeric(DF6M$buyer_code_melli)
DF6M$seller_code_melli <- as.numeric(DF6M$seller_code_melli)
