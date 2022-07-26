# learning by practicing with gganimate

library(tidyverse)
library(gganimate)

# dataset -----

dff <- data.frame(feature_1 = sample(LETTERS, 50000, replace = TRUE, 
                              prob = c(0.35, 0.25, 0.15, 0.1, 0.05, 
                                       rep(0.01, 21))), 
                  revenue = rpois(50000, 1500), 
                  model1_rank = rnorm(50000, mean = 8, sd = 23), 
                  model2_rank = runif(50000, min = 1, max = 50)) |>
  filter(model1_rank >= 1, model1_rank <= 50, 
         feature_1 %in% c('A', 'B', 'C', 'D', 'E')) |> 
  mutate(model1_rank = floor(model1_rank), 
         model2_rank = floor(model2_rank), 
         rank1_bin_10 = floor((model1_rank - 1) / 10) * 10 + 1, 
         rank2_bin_10 = floor((model2_rank - 1) / 10) * 10 + 1) |>
  as_tibble()
dim(dff)
dff |> group_by(rank2_bin_10) |> 
  summarise(min(model2_rank), max(model2_rank))


dff_a <- dff |> 
  group_by(model1_rank, model2_rank) |>
  summarise(recs = n(), 
            revenue = sum(revenue))

# practice plotting -----

dff |>
  ggplot(aes(x = model1_rank, 
             fill = feature_1)) + 
  geom_histogram(binwidth = 1, color = 'white') + 
  theme_bw() + 
  theme(legend.position = 'none')

dff |>
  ggplot(aes(x = model2_rank, 
             fill = feature_1)) + 
  geom_histogram(binwidth = 1, color = 'white') + 
  theme_bw() + 
  theme(legend.position = 'none')

# gganimate -----

# this is not working !!!!!!!!!!!!!!!!!!!!!!!

# pp <- 
  dff_a |> 
  ggplot(aes(x = model1_rank, y = model2_rank)) + 
  geom_tile(aes(fill = recs)) + 
  theme_bw() #+ 
  transition_states(states = model2_rank, 
                    transition_length = 1, state_length = 1, 
                    wrap = TRUE)

pp


