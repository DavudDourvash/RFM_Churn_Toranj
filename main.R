

# delete all objects
rm(list = ls())


mypath = "D:/Dourvash1401/Projects/Mixture/RFM_Repo/RFM_Churn_Toranj/"
setwd(mypath)

# ReadData ----------------------------------------------------------------

source("Requirements.R")

AnarTrades <- read.csv(file = "AnarAllTrades_14010101.csv", fileEncoding = "UTF-8")


# Calc 6 month Initial R, F, M ------------------------------------------------------------

# if data wasn't numeric
AnarTrades$Value = as.numeric(AnarTrades$number_of_units) * as.numeric(AnarTrades$price)

# Seperate 6 months as intial data
DF6M <- AnarTrades %>% dplyr::filter(date_key %in% c(StartDate : EndDate))

# if data wasn't numeric
DF6M$buyer_code_melli <- as.numeric(DF6M$buyer_code_melli)
DF6M$seller_code_melli <- as.numeric(DF6M$seller_code_melli)


## R
# 6 month grouped by buyers
DF_RB <- DF6M %>% dplyr::group_by(buyer_code_melli) %>%
  dplyr::summarise(minBuyDate = min(date_key), maxBuyDate = max(date_key)) %>% 
  as.data.frame()

# 6 month grouped by sellers
DF_RS <- DF6M %>% dplyr::group_by(seller_code_melli) %>%
  dplyr::summarise(maxDate = max(date_key)) %>% 
  as.data.frame()

# join buyers and sellers
DF_RBS <- DF_RB %>% left_join(DF_RS, by = c("buyer_code_melli" = "seller_code_melli"))


R_Init <- c()

# calc R
for(i in seq(nrow(DF_RBS))) {
  R_Init[i] =  
    ifelse(is.na(DF_RBS$maxDate[i]), 
           which(DatesMain == EndDate) - which(DatesMain == DF_RBS$maxBuyDate[i]) + 1,
           ifelse(DF_RBS$maxDate[i] < DF_RBS$maxBuyDate[i],
                  which(DatesMain == EndDate) - which(DatesMain == DF_RBS$maxBuyDate[i]) + 1, 
                  which(DatesMain == EndDate) - which(DatesMain == DF_RBS$maxDate[i]) + 1))
  print(i)
}

R_Init_DF <- data.frame(code_melli = DF_RBS$buyer_code_melli, R_Init = R_Init)

R_Init_DF$code_melli <- as.numeric(R_Init_DF$code_melli)

rm(DF_RB, DF_RS, DF_RBS)


## F
# 6 month grouped by buyers
DF_FB <- DF6M %>% dplyr::group_by(buyer_code_melli) %>%
  dplyr::summarise(FBuy = n()) %>% 
  as.data.frame()

# 6 month grouped by sellers
DF_FS <- DF6M %>% dplyr::group_by(seller_code_melli) %>%
  dplyr::summarise(FSell = n()) %>% 
  as.data.frame()


DF_FBS <- DF_FB %>% left_join(DF_FS, by = c("buyer_code_melli" = "seller_code_melli"))

DF_FBS$buyer_code_melli <- as.numeric(DF_FBS$buyer_code_melli)

DF_FBS <- DF_FBS %>% dplyr::group_by(buyer_code_melli) %>%
  dplyr::summarise(FBuy = sum(FBuy,na.rm = T), FSell = sum(FSell, na.rm = T))


F_Init_DF <- DF_FBS

colnames(F_Init_DF)[1] = "code_melli"

rm(DF_FB,DF_FS,DF_FBS)


## M
# 6 month grouped by buyers
DF_MB <- DF6M %>% dplyr::group_by(buyer_code_melli) %>%
  dplyr::summarise(MBuy = sum(Value, na.rm = T)) %>% 
  as.data.frame()

DF_MS <- DF6M %>% dplyr::group_by(seller_code_melli) %>%
  dplyr::summarise(MSell = sum(Value, na.rm = T)) %>% 
  as.data.frame()


DF_MBS <- DF_MB %>% left_join(DF_MS, by = c("buyer_code_melli" = "seller_code_melli"))

DF_MBS$buyer_code_melli <- as.numeric(DF_MBS$buyer_code_melli)

DF_MBS <- DF_MBS %>% dplyr::group_by(buyer_code_melli) %>%
  dplyr::summarise(MBuy = sum(MBuy,na.rm = T), MSell = sum(MSell, na.rm = T))


M_Init_DF <- DF_MBS

colnames(M_Init_DF)[1] = "code_melli"

rm(DF_MB,DF_MS,DF_MBS)



# RFM labeled DF ----------------------------------------------------------

DF6RFM <- R_Init_DF %>% dplyr::left_join(F_Init_DF) %>% dplyr::left_join(M_Init_DF)
# DF6RFM$F_Diff <- DF6RFM$FBuy - DF6RFM$FSell
# Remove outliers  ---------------------------------------------------------------

# R outliers
# DF6RFM$Routliers <- remove_outliers(DF6RFM$R_Init,0.1, 0.9)

# F outliers
# DF6RFM$FBuyoutliers <- remove_outliers(DF6RFM$FBuy, 0.05, 0.95)

# M outliers
# DF6RFM$Mbuyoutliers <- remove_outliers(DF6RFM$MBuy, 0.05, 0.95)



# Normal Scoring ----------------------------------------------------------


DF6RFM$R_Norm <- (DF6RFM$R_Init - min(DF6RFM$R_Init, na.rm = T)) / (max(DF6RFM$R_Init, na.rm = T) - min(DF6RFM$R_Init, na.rm = T))

DF6RFM$RNormscore = ifelse(DF6RFM$R_Norm <= 0.2, 1, ifelse(DF6RFM$R_Norm > 0.2 & DF6RFM$R_Norm <= 0.4, 
                                                       2, ifelse(DF6RFM$R_Norm > 0.4 & DF6RFM$R_Norm <= 0.6, 
                                                                 3, ifelse(DF6RFM$R_Norm > 0.6 & DF6RFM$R_Norm <= 0.8, 
                                                                           4, 5))))


DF6RFM$F_Norm <- (DF6RFM$FBuy - min(DF6RFM$FBuy, na.rm = T)) / (max(DF6RFM$FBuy, na.rm = T) - min(DF6RFM$FBuy, na.rm = T))

DF6RFM$FNormscore = ifelse(DF6RFM$F_Norm <= 0.2, 1, ifelse(DF6RFM$F_Norm > 0.2 & DF6RFM$F_Norm <= 0.4, 
                                                       2, ifelse(DF6RFM$F_Norm > 0.4 & DF6RFM$F_Norm <= 0.6, 
                                                                 3, ifelse(DF6RFM$F_Norm > 0.6 & DF6RFM$F_Norm <= 0.8, 
                                                                           4, 5))))

DF6RFM$M_Norm <- (DF6RFM$MBuy - min(DF6RFM$MBuy, na.rm = T)) / (max(DF6RFM$MBuy, na.rm = T) - min(DF6RFM$MBuy, na.rm = T))

DF6RFM$MNormscore = ifelse(DF6RFM$M_Norm <= 0.2, 1, ifelse(DF6RFM$M_Norm > 0.2 & DF6RFM$M_Norm <= 0.4, 
                                                       2, ifelse(DF6RFM$M_Norm > 0.4 & DF6RFM$M_Norm <= 0.6, 
                                                                 3, ifelse(DF6RFM$M_Norm > 0.6 & DF6RFM$M_Norm <= 0.8, 
                                                                           4, 5))))


# Quantile Scoring --------------------------------------------------------

DF6RFM$RQuantscore = ifelse(DF6RFM$R_Init <= quantile(DF6RFM$R_Init, 0.2),
                        1, ifelse(DF6RFM$R_Init > quantile(DF6RFM$R_Init, 0.2) & DF6RFM$R_Init <= quantile(DF6RFM$R_Init, 0.4), 
                                  2, ifelse(DF6RFM$R_Init > quantile(DF6RFM$R_Init, 0.4) & DF6RFM$R_Init <= quantile(DF6RFM$R_Init, 0.6),
                                            3, ifelse(DF6RFM$R_Init > quantile(DF6RFM$R_Init, 0.6) & DF6RFM$R_Init <= quantile(DF6RFM$R_Init, 0.8), 
                                                      4, 5))))


DF6RFM$FQuantscore = ifelse(DF6RFM$FBuy <= quantile(DF6RFM$FBuy, 0.2),
                        1, ifelse(DF6RFM$FBuy > quantile(DF6RFM$FBuy, 0.2) & DF6RFM$FBuy <= quantile(DF6RFM$FBuy, 0.4), 
                                  2, ifelse(DF6RFM$FBuy > quantile(DF6RFM$FBuy, 0.4) & DF6RFM$FBuy <= quantile(DF6RFM$FBuy, 0.6),
                                            3, ifelse(DF6RFM$FBuy > quantile(DF6RFM$FBuy, 0.6) & DF6RFM$FBuy <= quantile(DF6RFM$FBuy, 0.8), 
                                                      4, 5))))



DF6RFM$MQuantscore = ifelse(DF6RFM$MBuy <= quantile(DF6RFM$MBuy, 0.2),
                        1, ifelse(DF6RFM$MBuy > quantile(DF6RFM$MBuy, 0.2) & DF6RFM$MBuy <= quantile(DF6RFM$MBuy, 0.4), 
                                                       2, ifelse(DF6RFM$MBuy > quantile(DF6RFM$MBuy, 0.4) & DF6RFM$MBuy <= quantile(DF6RFM$MBuy, 0.6),
                                                                 3, ifelse(DF6RFM$MBuy > quantile(DF6RFM$MBuy, 0.6) & DF6RFM$MBuy <= quantile(DF6RFM$MBuy, 0.8), 
                                                                           4, 5))))
