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

# 3 ------------------------------------

plot_beta(45, 55)

aa <- seq(0, 1, length.out = 30)
dbeta(aa, 1, 1)
pbeta(aa, 1, 1)

data.frame(x = seq(0, 1, length.out = 30), 
           y1 = dbeta(aa, 1, 1), 
           y2 = pbeta(aa, 1, 1), 
           y3 = dbeta(aa, 5, 5)) %>% 
  ggplot(aes(x = x)) + 
  geom_point(aes(y = y1), color = 'blue') + 
  geom_point(aes(y = y2), color = 'black') + 
  geom_point(aes(y = y3), color = 'red')

# trying to hand roll a beta model to better understand it
fun_beta <- function(arg_alpha, arg_beta) {
  term1 <- gamma((arg_alpha + arg_beta)) /
    (gamma(arg_alpha) * gamma(arg_beta))
  fun_interim <- function(a0, a1 = arg_alpha, 
                          a2 = arg_beta, a3 = term1) {
    fun_out <- a3 * (a0^(a1 - 1)) * (1 - a0)^(a2 - 1)
    return(round(fun_out, 5))
  }
  pi_val <- seq(0, 1, by = 0.01)
  binom_output <- map_dbl(pi_val, 
                          fun_interim)
  binom_model <- data.frame(pi_val, binom_output)
  
  out_beta_mean <- arg_alpha / (arg_alpha + arg_beta)
  out_beta_mode <- (arg_alpha - 1) / (arg_alpha + arg_beta - 2)
  out_beta_var <- (arg_alpha * arg_beta) / 
    ((arg_alpha + arg_beta)^2 * (arg_alpha + arg_beta + 1))
  
  lout <- list(beta_mean = out_beta_mean, 
               beta_mode = out_beta_mode, 
               beta_var = out_beta_var, 
               beta_sd = sqrt(out_beta_var), 
               binom_model = binom_model)
  return(lout)
}

fun_beta(5, 5)
asdf <- fun_beta(45, 55)
asdf$binom_model %>% 
  ggplot() + 
  geom_point(aes(x = pi_val, y = binom_output))

plot_beta_binomial(alpha = 45, 
                   beta = 55, 
                   y = 30, n = 50)
summarize_beta_binomial(alpha = 45, beta = 55, y = 30, n = 50)

fart <- list(pp = 0.34, 
             nn = 1000, 
             pp2 = 0.27, 
             nn2 = 800)
plot_beta_binomial(alpha = floor(fart$pp * fart$nn), 
                   beta = floor(fart$nn * (1 - fart$pp)), 
                   y = floor(fart$pp2 * fart$nn2), 
                   n = fart$nn2) + theme(legend.position = 'top')


lapply(list(aa = c(1:10)), gamma)


