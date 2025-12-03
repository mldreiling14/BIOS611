# Strong Predictors for Life Expectancy
library('tidyverse')
library('stringr')

le_data <- read_csv("derived_data/chr_clean_full.csv", show_col_types = F)

outcome <- "life_expectancy_raw_value"

le_data <- le_data |>
  select(
    -state_fips_code,
    -county_fips_code,
    -x5_digit_fips_code,
    -state_abbreviation,
    -name
  ) |>
  # remove mortality variables EXCEPT outcome
  select(
    -matches("death|mortality|life_expectancy(?!_raw_value)", perl = TRUE)
  )

glimpse(le_data)

correlation <- le_data |>
  select(-all_of(outcome)) |>
  summarise(across(
    everything(),
    ~ cor(.x, le_data[[outcome]], use = "pairwise.complete.obs")
  )) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "correlation"
  ) |>
  arrange(desc(abs(correlation)))

correlation

# Look at the top 20
head(correlation, 20)

correlated_bar <- correlation |>
  slice(1:20) |>
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Correlations with Life Expectancy",
    x = "Predictor",
    y = "Correlation with Life Expectancy"
  )

ggsave("figures/correlated_bar.png", correlated_bar)


top_vars <- correlation |> slice(1:15) |> pull(variable)
predictors_small <- le_data |> select(all_of(top_vars))
corr_mat_small <- cor(predictors_small, use = "pairwise.complete.obs")


corr_long_small <- corr_mat_small |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlation"
  ) |>
  mutate(
    var1 = factor(var1, levels = unique(var1)),
    var2 = factor(var2, levels = unique(var2)),
    var1_label = str_wrap(gsub("_raw_value", "", var1), width = 10),
    var2_label = str_wrap(gsub("_raw_value", "", var2), width = 10)
  )

x_labs <- corr_long_small |>
  distinct(var1, var1_label) |>
  arrange(var1)

y_labs <- corr_long_small |>
  distinct(var2, var2_label) |>
  arrange(var2)

# make wrapped display labels (no _raw_value)
labs_tbl <- tibble(
  var = levels(corr_long_small$var1)
) |>
  mutate(
    label = stringr::str_wrap(gsub("_raw_value", "", var), width = 12)
  )

corr_heatmap_small <- ggplot(corr_long_small, aes(var1, var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-1, 1)
  ) +
  coord_fixed() +
  scale_x_discrete(
    breaks = labs_tbl$var,
    labels = labs_tbl$label
  ) +
  scale_y_discrete(
    breaks = labs_tbl$var,
    labels = labs_tbl$label
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9)
  ) +
  labs(x = "", y = "",
       title = "Correlation Matrix of Top 15 Predictors of Life Expectancy")

corr_heatmap_small


ggsave("figures/corr_heatmap_small.png", corr_heatmap_small, width = 9, height = 8)
