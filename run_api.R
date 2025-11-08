# run_api.R
library(plumber)
library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)

cat("Starting Data Analytics API...\n")
pr <- plumber::plumb("plumber.R")
pr$run(host = '0.0.0.0', port = 8000)
