# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ============================================================

# LOAD LIBRARIES ***********************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(gt)

library(rstan)
library(bayesrules)
library(bayesplot)
library(janitor)
library(rstanarm)
library(tidybayes)
library(e1071)
library(modelr)
library(broom.mixed)
library(forcats)

set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5, 
        stringsAsFactors = FALSE)
mem_used()

# basic helper functions ************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# 5 ----------------------------------------------------

# studying poisson
rpois(500, 7) %>% hist(breaks = 20)

fun_pois <- function(arg_lambda) {
  fun_pmf <- function(arg_y) {
    numerator <- ((arg_lambda^arg_y) * (exp(-arg_lambda)))
    numerator / gamma(arg_y)
  }
  y_values <- seq(1, 12, by = 1)
  run_pois <- map_dbl(y_values, fun_pmf)
  fun_output <- data.frame(y_values, 
                           poisson_pmf = run_pois)
  return(fun_output)
}
fun_pois(2) %>% ggplot() + geom_col(aes(x = y_values, y = poisson_pmf))

# studying gamma
gamma(seq(1, 5))

fun_gamma <- function(arg_s, arg_r) {
  fun_interim <- function(arg_lambda) {
    term1 <- (arg_r^arg_s) / gamma(arg_s)
    term2 <- arg_lambda^(arg_s - 1) * exp(-arg_r * arg_lambda)
    term1 * term2
  }
  lambda_vals <- seq(1, 20, by = 1)
  run_gamma <- map_dbl(lambda_vals, fun_interim)
  fun_output <- data.frame(lambda_vals, 
                           gamma_model_results = run_gamma)
  return(fun_output)
}
fun_gamma(4, 1) %>% ggplot() + geom_col(aes(x = lambda_vals, 
                                            y = gamma_model_results))

plot_gamma(4, 1)
# plucked from the internals of the above function
x_min <- qgamma(0.0000000000000000000000001, 4, 1)
x_max <- qgamma(0.99999, 4, 1)
ggplot(data = data.frame(x = c(x_min, x_max)), aes(x)) + 
  stat_function(fun = dgamma, n = 101, args = list(shape = 4, 
                                                   rate = 1))

plot_gamma_poisson(shape = 10, rate = 2, sum_y = 11, n = 4)
