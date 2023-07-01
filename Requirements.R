


# Requirements -------------------------------------------------------------
# remove all objects
rm(list = ls())

# set location for persian characters
Sys.setlocale(locale = "Persian")

# requirement pkgs
pkgs = c("dplyr", "tidyr", "stringr")
# load all requirement pkgs
lapply(pkgs, require, character.only = TRUE)

