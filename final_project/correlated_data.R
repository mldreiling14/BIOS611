# Strong Predictors for Life Expectancy

le_data <- read_csv("derived_data/chr_clean_full.csv")

outcome <- "life_expectancy_raw_value"

le_data <- le_data |>
  select(-c(state_fips_code, 
            county_fips_code, 
            x5_digit_fips_code, 
            state_abbreviation, 
            name)) |>
  select(
    -contains("death"),
    -contains("mortality"),
    -contains("life_expectancy"),   
    all_of(outcome)                 
  )

glimpse(le_data)

correlation <- le_data |>
  select(-all_of(outcome)) |>
  summarise(across(
    everything(),
    ~ cor(.x, le_data[[outcome]], use = "pairwise.complete.obs")
  )) |>
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "correlation") |>
  arrange(desc(abs(correlation)))



# Look at the top 20
head(correlation, 20)

correlated_bar <- correlation |> 
  slice(1:20) |>
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Correlations with Life Expectancy",
    x = "Predictor",
    y = "Correlation with Life Expectancy"
  )

ggsave("figures/correlated_bar.png", correlated_bar)


predictors <- le_data |> select(-all_of(outcome))

corr_mat <- cor(predictors, use = "pairwise.complete.obs")

# Convert to long format for ggplot
corr_long <- corr_mat |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlation"
  )

corr_long |> 
  filter(correlation < 1) |>
  arrange(desc(correlation)) |> 
  slice(1:10)

# ---- Heatmap ----
## Too many variables. Need to size it down.

top_vars <- correlation |>
  slice_max(order_by = abs(correlation), n = 15) |>
  pull(variable)

predictors <- le_data |>
  select(-all_of(outcome))

predictors_small <- predictors |>
  select(all_of(top_vars))

corr_mat_small <- cor(predictors_small, use = "pairwise.complete.obs")

corr_long_small <- corr_mat_small |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlation"
  )

corr_heatmap_small <- corr_long_small |>
  ggplot(aes(var1, var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title  = element_blank()
  ) +
  labs(
    title = "Correlation Matrix of Top 25 Predictors of Life Expectancy"
  )
corr_heatmap_small
