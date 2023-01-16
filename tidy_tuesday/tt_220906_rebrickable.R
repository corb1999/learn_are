
# intro -------------------------------------------------------

library(tidyverse)
library(tidylog)

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

# ^ -----

# read data in --------------------------------------------

inventory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

# ^ -----

# clean data --------------------------------------------------

inventory
sets
inventory_sets

# tidylog::left_join(inventory, inventory_sets, by = "set_num") |> 
#   arrange(id) |> print(n = 50)

inventory_sets <- inventory_sets |> 
  group_by(set_num) |> 
  summarise(quantity_total = sum(quantity))

df1 <- tidylog::left_join(sets, inventory_sets, by = "set_num")

dfa <- df1 |> 
  select(-img_url) |> 
  mutate(year_decade = floor(year / 10) * 10) |> 
  mutate(quantity_total = ifelse(is.na(quantity_total), 
                                 0, 
                                 quantity_total), 
         quantity_zero = ifelse(quantity_total == 0, 
                                TRUE, 
                                FALSE), 
         parts_zero = ifelse(num_parts == 0, 
                             TRUE, 
                             FALSE)) |> 
  mutate(random_bag_ind = stringr::str_detect(name, 
                                              'Random Bag'), 
         guidebook_ind = stringr::str_detect(set_num, 
                                             '^FLL')) |> 
  mutate(parts500_ind = ifelse(num_parts >= 500, 
                               TRUE, 
                               FALSE), 
         parts1000_ind = ifelse(num_parts >= 1000, 
                                TRUE, 
                                FALSE))

dfa |> filter(parts_zero == TRUE, quantity_zero == FALSE) |> viewer()

dfa |> filter(parts1000_ind == TRUE) |> View()

ls()
gc()

# ^ -----

# simple explore -------------------------------------------

dfa |> 
  filter(quantity_total > 0) |> 
  mutate(quantity_total_capped = ifelse(quantity_total > 20, 
                                        20, 
                                        quantity_total)) |> 
  ggplot(aes(x = as.factor(quantity_total_capped), 
             y = quantity_total, 
             fill = parts_zero)) + 
  geom_col()

dfa |> 
  filter(quantity_total >= 0, 
         parts_zero == FALSE) |> 
  ggplot(aes(x = as.factor(year_decade), 
             # y = quantity_total, 
             fill = as.factor(year_decade))) + 
  # geom_col() + 
  geom_bar() + 
  theme(legend.position = 'none', 
        axis.text.x = element_text(angle = 90))

dfa |> 
  filter(parts_zero == FALSE) |> 
  mutate(num_parts_capped = ifelse(num_parts > 1000, 
                                   1000, 
                                   num_parts)) |> 
  ggplot(aes(x = num_parts_capped)) + 
  geom_histogram(bins = 30, 
                 color = 'white', 
                 fill = 'blue', 
                 alpha = 0.5)

# ^ -----
