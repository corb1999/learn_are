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

# 8 ----------------------------------------

# 0.025th & 0.975th quantiles of the Beta(18,92) posterior
stats::qbeta(c(0.025, 0.975), 18, 92)

# Posterior probability that pi < 0.20
stats::pbeta(0.20, 18, 92)


# 7 ----------------------------------------------------

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

# in this example binomial liklihoods are Y = 1, n = 2, 
#   Beta(2, 3) is the prior pdf
bb_iteration <- function(arg_beta_a, arg_beta_b, arg_cur_pi_val) {
  # propose next chain location
  prop <- rbeta(1, arg_beta_a, arg_beta_b)
  
  # calc vals to evaluate plausibility of next location
  prop_plaus <- dbeta(prop, 2, 3) * dbinom(1, 2, prop)
  prop_q <- dbeta(prop, arg_beta_a, arg_beta_b)
  
  curr_plaus <- dbeta(arg_cur_pi_val, 2, 3) * dbinom(1, 2, arg_cur_pi_val)
  curr_q <- dbeta(arg_cur_pi_val, arg_beta_a, arg_beta_b)
  
  # decide on whether to move to next location
  algo_alpha <- min(1, 
                    prop_plaus / curr_plaus * 
                      curr_q / prop_q)
  
  next_iteration <- sample(c(prop, arg_cur_pi_val), 
                           size = 1, 
                           prob = c(algo_alpha, 1 - algo_alpha))
  
  return(data.frame(arg_cur_pi_val, 
                    prop, 
                    algo_alpha, 
                    next_iteration))
}
bb_iteration(1, 1, 0.5)
rbeta(5000, 3, 4) %>% hist()

bb_tour <- function(arg_n, arg_a, arg_b, chain_start = 0.5) {
  # initialize sim creating a home for data produced by loop
  chain_position <- chain_start
  pi_val <- rep(0, arg_n)
  
  # simulate the chain iterations
  for (i in 1:arg_n) {
    # sim a single iteration
    single_sim <- bb_iteration(arg_a, arg_b, chain_position)
    
    # record the single sim result
    pi_val[i] <- single_sim$next_iteration
    
    # set new chain value to begin loop again
    chain_position <- single_sim$next_iteration
    
  }
  
  return(data.frame(iter_num = c(1:arg_n), 
                    sim_results = pi_val))
}

bb_tour(5000, 1, 1) %>% ggplot() + 
  geom_histogram(aes(x = sim_results, y = ..density..), 
                 color = 'white', bins = 30) + 
  stat_function(fun = dbeta, args = list(3, 4), color = "blue")

bb_tour(5000, 1, 1) %>% ggplot() + 
  geom_line(aes(x = iter_num, y = sim_results))


# 6 ----------------------------------------------------

data.frame(pi_grid = seq(from = 0, 
                         to = 1, length = 100)) %>% 
  mutate(prior = dbeta(pi_grid, 2, 2),
         likelihood = dbinom(9, 10, pi_grid)) %>% 
  mutate(unnormalized = likelihood * prior,
         posterior = unnormalized / sum(unnormalized)) %>% 
  ggplot(aes(x = pi_grid, y = posterior)) + 
  geom_point() + 
  geom_segment(aes(x = pi_grid, xend = pi_grid, 
                   y = 0, yend = posterior))

dbeta(seq(from = 0, 
          to = 1, length = 100), 5, 5) %>% plot()

dbinom(1, 2, seq(from = 0, 
                 to = 1, length = 100)) %>% plot()

dgamma(seq(1, 20, length = 100), 
       10, 2) %>% plot()

dpois(10, seq(1, 20, length = 100)) %>% plot()

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
