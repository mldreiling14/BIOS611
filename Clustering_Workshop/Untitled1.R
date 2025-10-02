library(tidyverse)
library(cluster)
data <- read.csv("power_grid_characters.txt")
 
data %>% group_by(character_name, power_category) |> tally()|>
  filter(n != 1) |> group_by(n) |> tally(name='n_count')

wide <- data |> pivot_wider( id_cols="character_name", 
                             names_from=power_category, 
                             values_from = numeric_level,
                             values_fn = function(x) max(x))
wide %>% filter(!complete.cases(.))
#Only 19 rows observes with missing values so we can throw it away

cc_wide <- wide %>% filter(complete.cases(.))

#PCA###########################################################################
wide <- read_csv("derived_data/clean_wide_characters.csv")

prcomp_res <- prcomp(wide %>% select(-character_name))
prcomp_res
