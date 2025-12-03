library('missForest')
library('readxl')
library('janitor')

chr <- read_csv("source_data/analytic_data2025_v3.csv", show_col_types = FALSE) |> slice(-1)
names(chr)

chr <- chr |> clean_names()

names(chr)
chr <- chr |> 
  mutate(across(contains("raw_value"), as.numeric))

health_outcome_vars <- c(
  "premature_death_raw_value",
  "poor_or_fair_health_raw_value",
  "poor_physical_health_days_raw_value",
  "poor_mental_health_days_raw_value",
  "life_expectancy_raw_value",
  "injury_deaths_raw_value",
  "suicides_raw_value",
  "diabetes_prevalence_raw_value",
  "hiv_prevalence_raw_value",
  "drug_overdose_deaths_raw_value"
)

chr |>
  select(all_of(health_outcome_vars)) |>
  pivot_longer(everything(), names_to = "measure", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ measure, scales = "free") +
  theme_minimal() +
  labs(
    title = "Health Outcome Distributions Across Counties",
    x = "Value",
    y = "Count"
  )


vars_for_pca <- c(
  # outcomes
  "life_expectancy_raw_value",
  "poor_physical_health_days_raw_value",
  "poor_mental_health_days_raw_value",
  
  # housing burden (your predictor)
  "severe_housing_cost_burden_raw_value",
  "percentage_of_households_with_high_housing_costs",
  
  # clinical care
  "ratio_of_population_to_primary_care_physicians",
  "preventable_hospital_stays_raw_value",
  "uninsured_raw_value",
  "primary_care_physicians_raw_value"
)

df |> select(all_of(vars_for_pca)) |> summarise(across(everything(), class))

df <- chr |>
  select(name, state_abbreviation, all_of(vars_for_pca)) |>
  mutate(across(all_of(vars_for_pca), as.numeric))

imp <- missForest(df |> select(-name, -state_abbreviation))$ximp |> 
  as_tibble()

imputed <- imp |> 
  mutate(name = df$name,
         state = df$state_abbreviation)

pc_res <- prcomp(imp, center = TRUE, scale. = TRUE)

pca_data <- chr |>
  select(all_of(vars_for_pca)) |>
  mutate(across(everything(), as.numeric)) |>
  drop_na() 

summary(pca_data)

