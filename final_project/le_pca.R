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

pca_dat <- chr_clean |>
  select(
    life_expectancy_raw_value,
    all_of(pred_vars)
  ) |>
  drop_na()

X <- pca_dat |>
  select(all_of(pred_vars))

pc_res <- prcomp(X, center = TRUE, scale. = TRUE)

pcs <- pc_res$x |>
  as_tibble() |>
  mutate(
    life_expectancy_raw_value = pca_dat$life_expectancy_raw_value
  )

pcs

pc_for_cluster <- pcs |>
  select(PC1:PC3)

km <- kmeans(pc_for_cluster, centers = 4, nstart = 25)

pcs <- pcs |>
  mutate(cluster = factor(km$cluster))

le_pca <- ggplot(pcs, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Clusters of Counties by Housing, SDOH, and Clinical Care (PC Space)",
    color = "Cluster"
  )

ggsave("figures/le_pca.png", le_pca, width = 7, height = 5, dpi = 300)


pcs |>
  group_by(cluster) |>
  summarise(
    n = n(),
    mean_le = mean(life_expectancy_raw_value, na.rm = TRUE),
    sd_le   = sd(life_expectancy_raw_value, na.rm = TRUE)
  )

le_boxplot <- ggplot(pcs, aes(cluster, life_expectancy_raw_value, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Life Expectancy by Cluster",
    x = "Cluster",
    y = "Life expectancy (years)"
  )

ggsave("figures/le_pca_boxplot.png", le_boxplot, width = 7, height = 5, dpi = 300)


