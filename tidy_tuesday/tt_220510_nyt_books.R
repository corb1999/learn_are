
library(tidyverse)

# read data ---------------------------------------
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# ^ -----

nyt_titles |> 
  filter(best_rank <= 5) |> 
  ggplot(aes(x = year, y = total_weeks, 
             color = as.factor(best_rank))) + 
  geom_jitter(alpha = 0.25) + 
  theme_bw() + 
  theme(legend.position = 'top') 

nyt_titles |> 
  mutate(best_rank1_ind = ifelse(best_rank == 1, TRUE, FALSE), 
         total_weeks_cap50 = ifelse(total_weeks > 50, 
                                    50, total_weeks)) |>
  ggplot(aes(x = total_weeks_cap50, fill = best_rank1_ind)) + 
  geom_histogram(alpha = 0.5, color = 'black', 
                 bins = 30) + 
  theme_bw()

nyt_titles |> 
  arrange(desc(total_weeks)) |> 
  head(25) |> 
  # View()
  ggplot(aes(x = reorder(title, total_weeks), 
             y = total_weeks, 
             color = year)) + 
  geom_segment(aes(y = 0, yend = total_weeks, 
                   xend = title)) + 
  geom_point(size = 3) + 
  scale_color_distiller(palette = 7) + 
  coord_flip() + 
  theme_bw() + 
  theme(legend.position = 'top') + 
  labs(x = '')
