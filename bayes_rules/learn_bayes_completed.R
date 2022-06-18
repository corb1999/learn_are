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


# 4 ---------------------------------------

fun_beta_binom_manual <- function(arg_alpha, 
                                  arg_beta) {
  ggplot() + 
    stat_function(fun = dbeta, 
                  args = list(shape1 = arg_alpha, 
                              shape2 = arg_beta), 
                  geom = 'area', alpha = 0.25, 
                  fill = 'yellow')
}
fun_beta_binom_manual(5, 5)


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

fun_beta(1, 10)
asdf <- fun_beta(1, 10)
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

# 3.5 --

michelle_sim <- data.frame(pi = rbeta(50000, 45, 55)) %>% 
  mutate(y = rbinom(50000, size = 50, prob = pi))
michelle_sim

ggplot(michelle_sim, aes(x = pi, y = y)) + 
  geom_point(aes(color = (y == 30)), size = 0.1)

# Keep only the simulated pairs that match our data
michelle_posterior <- michelle_sim %>% 
  filter(y == 30) %>% 
  ggplot(aes(x = pi)) + 
  geom_density()

michelle_sim %>% 
  filter(y == 30) %>% 
  summarize(mean(pi), sd(pi))

summarize_beta_binomial(alpha = 1, beta = 10, y = 26, n = 40)

plot_beta_binomial(alpha = 1, beta = 10, y = 26, n = 40)


# 2.3 ------------------------------------

# hand roll the binomial model
fun_binom_model <- function(arg_y, arg_pie, arg_n) {
  term1 <- factorial(arg_n) / 
    (factorial(arg_y) * factorial(arg_n - arg_y))
  term2 <- arg_pie^arg_y
  term3 <- (1 - arg_pie)^(arg_n - arg_y)
  return_me <- term1 * term2 * term3
  return(return_me)
}
fun_binom_model(6, 0.8, 6)
lapply(c(0:6), fun_binom_model, 
       arg_n = 6, arg_pie = 0.8)
lapply(c(0:6), fun_binom_model, 
       arg_n = 6, arg_pie = 0.5)
lapply(c(0:6), fun_binom_model, 
       arg_n = 6, arg_pie = 0.2)

lapply(c(0.2, 0.5, 0.8), fun_binom_model, 
       arg_n = 6, arg_y = 1)

# doing the chess simulation exercise
# Define possible win probabilities
chess <- data.frame(pi = c(0.2, 0.5, 0.8))
# Define the prior model
prior <- c(0.10, 0.25, 0.65)
chess_sim <- sample_n(chess, size = 10000, 
                      weight = prior, replace = TRUE)
chess_sim <- chess_sim %>% 
  mutate(y = rbinom(10000, size = 6, prob = pi))
chess_sim
chess_sim %>% 
  tabyl(pi) %>% 
  adorn_totals("row")
ggplot(chess_sim, aes(x = y)) + 
  stat_count(aes(y = ..prop..)) + 
  facet_wrap(~ pi)


# 2.1 ------------------------------------

data("fake_news")
dim(fake_news)
head(fake_news)
fake_news %>% tabyl(title_has_excl, type) %>% adorn_totals('row')

article <- data.frame(type = c('real', 'fake'))
prior <- c(0.6, 0.4)
article_sim <- sample_n(article, size = 10000, 
                        weight = prior, replace = TRUE)

article_sim <- article_sim %>% 
  mutate(data_model = case_when(type == "fake" ~ 0.2667,
                                type == "real" ~ 0.0222))

data <- c("no", "yes")

# Simulate exclamation point usage 
set.seed(3)
article_sim <- article_sim %>%
  group_by(1:n()) %>% 
  mutate(usage = sample(data, size = 1, 
                        prob = c(1 - data_model, data_model)))

article_sim %>% 
  tabyl(usage, type) %>% 
  adorn_totals(c("col","row"))

ggplot(article_sim, aes(x = type, fill = usage)) + 
  geom_bar(position = "fill")
ggplot(article_sim, aes(x = type)) + 
  geom_bar()
