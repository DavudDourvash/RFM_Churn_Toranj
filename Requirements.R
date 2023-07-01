


# Requirements -------------------------------------------------------------
# remove all objects
# rm(list = ls())

# set location for persian characters
Sys.setlocale(locale = "Persian")

# requirement pkgs
pkgs = c("dplyr", "tidyr", "stringr")
# load all requirement pkgs
lapply(pkgs, require, character.only = TRUE)



# Parameters --------------------------------------------------------------

StartDate = 14010701
EndDate = 14011230


Days = c("01", "02", "03", "04", "05", "06", "07", "08", "09",10:31)
Months = c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:12)
Years = c(1401:1403)

DatesMain = c(paste0(rep(Years[1], 6*31), rep(Months[1:6], 31) %>% sort(), rep(Days, 6)), 
              paste0(rep(Years[1], 6*30), rep(Months[7:12], 30) %>% sort(), rep(Days[-31], 6)), 
              paste0(rep(Years[2], 6*31), rep(Months[1:6], 31) %>% sort(), rep(Days, 6)), 
              paste0(rep(Years[2], 6*30), rep(Months[7:12], 30) %>% sort(), rep(Days[-31], 6)),
              paste0(rep(Years[3], 6*31), rep(Months[1:6], 31) %>% sort(), rep(Days, 6)), 
              paste0(rep(Years[3], 6*30), rep(Months[7:12], 30) %>% sort(), rep(Days[-31], 6))
) %>% as.numeric()

