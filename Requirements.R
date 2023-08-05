


# Requirements -------------------------------------------------------------
# remove all objects
# rm(list = ls())

# set location for persian characters
Sys.setlocale(locale = "Persian")

libs <- list("bubbles",
             "openxlsx",
             "lubridate",
             "cluster",
             "data.table",
             "dplyr",
             "tidyr",
             "DT",
             "dygraphs",
             "jsonlite",
             "leaflet",
             "packcircles",
             "purrr", 
             "htmlwidgets",
             "highcharter",
             "rCharts",
             "forecast",
             "ggiraph",
             "ggplot2", 
             "plotly", 
             "plyr", 
             "rCharts", 
             "reshape", 
             "shiny", 
             "shinydashboard", 
             "TTR", 
             "viridis",
             "xlsx"
)



# Github Libraries

# install uninstalled packages
# lapply(libs, function(x) if(!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE))

# load all libraries
lapply(libs, require, character.only = TRUE)

options(RCHART_LIB = 'nvd3')
options(digits = 5)
options(scipen = 999)




# Parameters --------------------------------------------------------------

StartDate = 14010701
EndDate = 14011230
af = 0.1 # left limit of outliers
an = 0.9 # right limit of outliers



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

# Functions ---------------------------------------------------------------

remove_outliers <- function(x, af, an, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(af, an), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


# service libs requires = c("config","RCurl")


