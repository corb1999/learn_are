# learning how to use rpolars dataframe library

# not available on cran: 
# install.packages("rpolars", repos = "https://rpolars.r-universe.dev")

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


library(tidyverse)
library(rpolars)

# lists polars datatypes
pl$dtypes

# example creating a dataframe
pl$DataFrame(
  pl$Series((1:5) * 5, "a"),
  pl$Series(letters[1:5], "b"),
  newname = pl$Series(c(1,2,3,4,5), "oldname"), 
  c(5,4,3,2,1), 
  named_vector = c(15,14,13,12,11) , 
  c(5,4,3,2,0)
)

size_of_df <- 5000000
aa <- sample(1:100, size_of_df, replace = TRUE)
bb <- runif(size_of_df)
cc <- sample(letters, size_of_df, replace = TRUE)
dd <- sample(c(TRUE, FALSE), size_of_df, replace = TRUE)

df_pl <- pl$DataFrame(pl$Series(aa, 'col_a'), 
                      pl$Series(bb, 'col_b'), 
                      pl$Series(cc, 'col_c'), 
                      pl$Series(dd, 'col_d'))

df_pl_lazy <- df_pl$lazy()

df_tidy <- tibble('col_a' = aa, 'col_b' = bb, 
                  'col_c' = cc, 'col_d' = dd)

rm(aa, bb, cc, dd)
gc()

df_pl
df_pl_lazy
df_tidy

clockin()
df_pl$filter(
  pl$col('col_a') > 50
)$filter(
  pl$col('col_d') == TRUE
)$groupby(
  'col_c'
)$agg(
  pl$col('col_a')$sum()$sort()$alias('sum_col_a')
)
clockout()

# slightly different so cant get the cache benefit
clockin()
df_pl_lazy$filter(
  pl$col('col_a') < 51
)$filter(
  pl$col('col_d') == FALSE
)$groupby(
  'col_c'
)$agg(
  pl$col('col_a')$sum()$sort()$alias('sum_col_a')
)$collect()
clockout()

clockin()
df_tidy |> filter(col_a > 50, 
                  col_d == TRUE) |> 
  group_by(col_c) |> 
  summarise(sum_col_a = sum(col_a)) |> 
  arrange(sum_col_a)
clockout()




