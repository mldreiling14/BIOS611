library(tidyverse)
library(cluster)
chr_clean <- read_csv("derived_data/chr_clean_full.csv")

pred_vars <- c(
  "percentage_of_households_with_high_housing_costs",
  "severe_housing_cost_burden_raw_value",
  "median_household_income_raw_value",
  "children_in_poverty_raw_value",
  "uninsured_raw_value",
  "ratio_of_population_to_primary_care_physicians",
  "preventable_hospital_stays_raw_value"
)

stopifnot(all(pred_vars %in% names(chr_clean)))
stopifnot("life_expectancy_raw_value" %in% names(chr_clean))

pca_dat <- chr_clean |> 
  select(life_expectancy_raw_value, all_of(pred_vars)) |> 
  drop_na()

X <- pca_dat |>
  select(all_of(pred_vars))

pc_res <- prcomp(X, center = TRUE, scale. = TRUE)

pc_res

pcs <- pc_res$x |>
  as_tibble() |>
  mutate(
    life_expectancy_raw_value = pca_dat$life_expectancy_raw_value
  )

pcs <- as_tibble(pc_res$x)

pcs$life_expectancy_raw_value <- pca_dat$life_expectancy_raw_value


pca_plot <- ggplot(pcs, aes(PC1, PC2, color = life_expectancy_raw_value)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    title = "PCA Colored by Life Expectancy",
    color = "Life Expectancy"
  )

pca_plot

ggsave("figures/pca_life_expectancy.png", pca_plot, width = 7, height = 5, dpi = 300)