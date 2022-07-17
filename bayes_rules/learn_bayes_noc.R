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

# set.seed(metadatar$seed_set[1])
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

# 7 ----------------------------------------

# mcmc

monty <- data.frame(mu = rnorm(10000, mean = 4, sd = 0.6))

monty %>% ggplot() + 
  geom_histogram(aes(x = mu), bins = 30, color = 'white')

# metropolis hastings mcmc algorithm manual
#   corresponds to examples in the text with Y of 6.25 and sd of 0.75
mh_iteration <- function(uni_half_width, cur_chain_val, 
                         liklihood_y = 6.25, liklihood_sd = 0.75) {
  propose_chain_val <- runif(1, 
                             min = cur_chain_val - uni_half_width, 
                             max = cur_chain_val + uni_half_width)
  
  cur_plausibility <- dnorm(cur_chain_val, 0, 1) * 
    dnorm(liklihood_y, cur_chain_val, liklihood_sd)
  pro_plausibility <- dnorm(propose_chain_val, 0, 1) * 
    dnorm(liklihood_y, propose_chain_val, liklihood_sd)
  
  alpha <- min(1, pro_plausibility / cur_plausibility)
  next_chain_val <- sample(c(cur_chain_val, propose_chain_val), 
                           size = 1, 
                           prob = c(1 - alpha, alpha))
  fn_output <- data.frame(cur_chain_val, 
                          propose_chain_val, 
                          alpha, 
                          next_chain_val)
  return(fn_output)
}
mh_iteration(1, 3)

mh_simulation <- function(n_length, uni_half_width, 
                          init_chain_val = 3, 
                          liklihood_y = 6.25, liklihood_sd = 0.75) {
  sim_chain_val <- init_chain_val
  mu_sim <- rep(0, n_length)
  for(i in c(1:n_length)) {
    simulation <- mh_iteration(uni_half_width = uni_half_width, 
                               cur_chain_val = sim_chain_val, 
                               liklihood_y = liklihood_y, 
                               liklihood_sd = liklihood_sd)
    mu_sim[i] <- simulation$next_chain_val
    sim_chain_val <- simulation$next_chain_val
  }
  fn_output <- data.frame(chain_step_num = c(1:n_length), 
                          mu_sim)
  return(fn_output)
}
mh_simulation(3000, uni_half_width = 1) %>% 
  ggplot(aes(x = chain_step_num, y = mu_sim)) + 
  geom_line()

mh_simulation(3000, uni_half_width = 1) %>% 
  ggplot(aes(x = mu_sim)) + 
  geom_histogram(bins = 30, color = 'white')

# beta binomial example ...












