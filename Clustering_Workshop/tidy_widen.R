library('tidyverse')

data <- read.csv("power_grid_characters.txt")

data %>% group_by(character_name, power_category) |> tally()|>
  filter(n != 1) |> group_by(n) |> tally(name='n_count')

data %>% pivot_wider(id_cols="character_name", 
                             names_from=power_category, 
                             values_from = numeric_level,
                             values_fn = function(x) max(x)) %>%
                             filter(complete.cases(.)) %>%
                             write_csv("derived_data/clean_wide_characters.csv") 
