chr <- read_csv("derived_data/chr_clean_full.csv", show_col_types = FALSE)

vars_for_pca <- c(
  "life_expectancy_raw_value",
  "poor_physical_health_days_raw_value",
  "poor_mental_health_days_raw_value",
  "severe_housing_cost_burden_raw_value",
  "ratio_of_population_to_primary_care_physicians",
  "preventable_hospital_stays_raw_value",
  "uninsured_raw_value",
  "primary_care_physicians_raw_value",
  "percentage_of_households_with_high_housing_costs"
)

df_pca <- chr_clean |>
  select(name, state_abbreviation, all_of(vars_for_pca))

df_pca_imputed <- df_pca |>
  mutate(across(all_of(vars_for_pca),
                ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

pc_res <- prcomp(df_pca_imputed |> select(all_of(vars_for_pca)),
                 scale. = TRUE,
                 center = TRUE)

pcs <- pc_res$x |> 
  as_tibble() |>
  bind_cols(df_pca_imputed |> select(name, state_abbreviation))       

pc_for_cluster <- pcs |>
  select(starts_with("PC")) |>        
  select(PC1:PC4)

k <- 4

km_res <- kmeans(pc_for_cluster, centers = k, nstart = 25)

pcs_clustered <- pcs |>
  mutate(cluster = factor(km_res$cluster))

pca_plot <- ggplot(pcs_clustered, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "PCA: Health Outcomes & Clinical Care Clusters",
    subtitle = "Clusters based on first 4 principal components",
    color = "Cluster"
  )

ggsave("figures/pca_cluster.png", pca_plot)

pcs_clustered |>
  bind_cols(
    chr_clean |>
      select(life_expectancy_raw_value,
             poor_physical_health_days_raw_value,
             poor_mental_health_days_raw_value,
             ratio_of_population_to_primary_care_physicians,
             preventable_hospital_stays_raw_value,
             uninsured_raw_value,
             severe_housing_cost_burden_raw_value,
             percentage_of_households_with_high_housing_costs)
  ) |>
  group_by(cluster) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

