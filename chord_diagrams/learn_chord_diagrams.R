# learning by practicing with circularize

library(tidyverse)
library(circlize)

# make a dataframe -------------------------

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
                               TRUE, FALSE), 
         model_delta = model2_rank - model1_rank) |>
  as_tibble()

# tests ?????????????????????????????????????
dim(dff)
dff |> group_by(rank2_bin_10) |> 
  summarise(min(model2_rank), max(model2_rank))
dff |> group_by(rank1_bin_05) |> 
  summarise(min(model1_rank), max(model1_rank))

# agg versions of the dataframe :::::::::::::::::::::::
# dff_a <- dff |> 
#   group_by(model1_rank, model2_rank) |>
#   summarise(recs = n(), 
#             revenue = sum(revenue)) |>  
#   mutate(recs_mix = recs / sum(recs), 
#          rev_mix = revenue / sum(revenue))

dff_a2 <- dff |>
  mutate(gkey = paste0(rank1_bin_10, '::', rank2_bin_10)) |>
  group_by(gkey) |>
  summarise(recs = n(), 
            revenue = sum(revenue), 
            m05plus = sum(moved_05plus), 
            m10plus = sum(moved_10plus)) |>  
  mutate(recs_mix = recs / sum(recs), 
         rev_mix = revenue / sum(revenue)) |>
  mutate(rank1_bin_10 = stringr::str_extract(gkey, '^.*?(?=::)'), 
         rank2_bin_10 = stringr::str_extract(gkey, '(?<=::)\\d+'), 
         rank1_bin_10 = as.double(rank1_bin_10), 
         rank2_bin_10 = as.double(rank2_bin_10))

dff_a3 <- dff |>
  mutate(gkey = paste0(rank1_bin_05, '::', rank2_bin_05)) |>
  group_by(gkey) |>
  summarise(recs = n(), 
            revenue = sum(revenue), 
            m05plus = sum(moved_05plus), 
            m10plus = sum(moved_10plus)) |>  
  mutate(recs_mix = recs / sum(recs), 
         rev_mix = revenue / sum(revenue)) |>
  mutate(rank1_bin_05 = stringr::str_extract(gkey, '^.*?(?=::)'), 
         rank2_bin_05 = stringr::str_extract(gkey, '(?<=::)\\d+'), 
         rank1_bin_05 = as.double(rank1_bin_05), 
         rank2_bin_05 = as.double(rank2_bin_05))

# ^ -----

# chord functions --------------------------

dff_a2 |> 
  select(rank1_bin_10, rank2_bin_10, recs) |> 
  chordDiagramFromDataFrame()
circos.clear()


dff_a2 |> 
  select(rank1_bin_10, rank2_bin_10, recs) |> 
  chordDiagramFromDataFrame(order = c(41, 31, 21, 11, 1))
circos.clear()


circos.par(start.degree = 90, clock.wise = TRUE)
dff_a2 |> 
  select(rank1_bin_10, rank2_bin_10, recs) |> 
  chordDiagramFromDataFrame()
circos.clear()


grid.col1 <- c('green', 'blue', 'purple', 
               'orange', 'red')
circos.par(start.degree = 90, clock.wise = TRUE)
dff_a2 |> 
  select(rank1_bin_10, rank2_bin_10, recs) |> 
  chordDiagramFromDataFrame(grid.col = grid.col1)
circos.clear()


# ^ -----


