# practicing plotting some data

library(tidyverse)
library(scales)
library(patchwork)

# dataset -----

dff <- data.frame(feature_1 = sample(LETTERS, 50000, replace = TRUE, 
                                     prob = c(0.35, 0.25, 0.15, 0.1, 0.05, 
                                              rep(0.01, 21))), 
                  revenue = rpois(50000, 1500), 
                  model1_rank = rnorm(50000, mean = 5, sd = 23), 
                  model2_rank = runif(50000, min = 1, max = 50)) |>
  filter(model1_rank >= 1, model1_rank <= 50, 
         feature_1 %in% c('A', 'B', 'C', 'D', 'E')) |> 
  mutate(model1_rank = floor(model1_rank), 
         model2_rank = floor(model2_rank), 
         rank1_bin_10 = floor((model1_rank - 1) / 10) * 10 + 1, 
         rank2_bin_10 = floor((model2_rank - 1) / 10) * 10 + 1, 
         rank1_bin_05 = floor((model1_rank - 1) / 5) * 5 + 1, 
         rank2_bin_05 = floor((model2_rank - 1) / 5) * 5 + 1) |>
  mutate(moved_05plus = ifelse(abs(model2_rank - model1_rank) >= 5, 
                               TRUE, FALSE), 
         moved_10plus = ifelse(abs(model2_rank - model1_rank) >= 10, 
                               TRUE, FALSE))
  as_tibble()

dim(dff)
dff |> group_by(rank2_bin_10) |> 
  summarise(min(model2_rank), max(model2_rank))
dff |> group_by(rank1_bin_05) |> 
  summarise(min(model1_rank), max(model1_rank))

# agg versions of the dataframe
dff_a <- dff |> 
  group_by(model1_rank, model2_rank) |>
  summarise(recs = n(), 
            revenue = sum(revenue)) |>  
  mutate(recs_mix = recs / sum(recs), 
         rev_mix = revenue / sum(revenue))

dff_a2 <- dff |> 
  group_by(rank1_bin_10, rank2_bin_10) |>
  summarise(recs = n(), 
            revenue = sum(revenue), 
            m05plus = sum(moved_05plus), 
            m10plus = sum(moved_10plus)) |>  
  mutate(recs_mix = recs / sum(recs), 
         rev_mix = revenue / sum(revenue))

dff_a3 <- dff |>
  mutate(gkey = paste0(rank1_bin_05, '::', rank2_bin_05)) |>
  group_by(gkey) |>
  summarise(recs = n(), 
            revenue = sum(revenue)) |>  
  mutate(recs_mix = recs / sum(recs), 
         rev_mix = revenue / sum(revenue)) |>
  mutate(rank1_bin_05 = stringr::str_extract(gkey, '^.*?(?=::)'), 
         rank2_bin_05 = stringr::str_extract(gkey, '(?<=::)\\d+'), 
         rank1_bin_05 = as.double(rank1_bin_05), 
         rank2_bin_05 = as.double(rank2_bin_05))

# ^ -----

# plotting ----------------------------------------

fun_plt_delta_matrix <- function(arg_df, arg_dfa,  
                                 arg_xaxis, 
                                 arg_yaxis) {
  p1 <- arg_dfa %>% 
    rename(xvar = !!arg_xaxis, yvar = !!arg_yaxis) %>% 
    ggplot(aes(x = as.factor(xvar), 
               y = fct_rev(as.factor(yvar)))) + 
    geom_tile(aes(fill = recs), color = 'white') + 
    geom_label(aes(label = scales::percent(recs_mix, accuracy = 0.1)), 
               size = 3) + 
    scale_fill_distiller(palette = 'Spectral') + 
    scale_x_discrete(guide = guide_axis(position = 'top')) + 
    theme_minimal() + 
    theme(legend.position = 'none') + 
    labs(x = '', y = '')
  p2 <- arg_df %>% 
    ggplot() + 
    geom_histogram(aes(x = model1_rank), alpha = 0.33, 
                   binwidth = 1, 
                   color = 'white', fill = 'blue') + 
    theme_minimal() + 
    labs(y = '')
  p3 <- arg_df %>% 
    ggplot() + 
    geom_histogram(aes(x = model2_rank), alpha = 0.33, 
                   binwidth = 1, 
                   color = 'white', fill = 'red') + 
    theme_minimal() + 
    labs(y = '')
  p_out <- p1 | (p2 / p3)
  return(p_out)
}
# fun_plt_delta_matrix(dff_a, 
#                      arg_xaxis = 'model1_rank', arg_yaxis = 'model2_rank')
# fun_plt_delta_matrix(dff_a2, 
#                      arg_xaxis = 'rank1_bin_10', arg_yaxis = 'rank2_bin_10')
fun_plt_delta_matrix(arg_df = dff, arg_dfa = dff_a3, 
                     arg_xaxis = 'rank1_bin_05', arg_yaxis = 'rank2_bin_05')
