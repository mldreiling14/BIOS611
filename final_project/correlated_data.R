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

correlated_data <- correlation |> 
  slice(1:20) |>
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Correlations with Life Expectancy",
    x = "Predictor",
    y = "Correlation with Life Expectancy"
  )

ggsave("figures/correlated_data.png", correlated_data)

