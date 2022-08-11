# https://www.spc.noaa.gov/wcm/#data
# https://www.spc.noaa.gov/wcm/data/1955-2021_hail.csv.zip

library(tidyverse)
library(DBI)
library(duckdb)

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# setup
data_filepath <- paste0(getwd(), '/duckdb/noaa_hail_data.csv')
setwd(paste0(getwd(), '/duckdb'))

# typical utils read method
clockin()
dumb1 <- read.csv(data_filepath, 
                  stringsAsFactors = FALSE)
clockout()

# try duck **this is not even close to right
con <- dbConnect(duckdb::duckdb(), dbdir = ':memory:')
clockin()
dumb2 <- dbGetQuery(con, 
                    'SELECT * FROM read_csv_auto(noaa_hail_data.csv);')
clockout()
