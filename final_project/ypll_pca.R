library('tidyverse')
library('tidyr')


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
    premature_death_raw_value,    # outcome (keep for later)
    all_of(pred_vars)
  ) |>
  tidyr::drop_na()

X <- pca_dat |>
  select(all_of(pred_vars))

pc_res <- prcomp(X, center = TRUE, scale. = TRUE)

pcs <- pc_res$x |>
  as_tibble() |>
  mutate(
    premature_death_raw_value = pca_dat$premature_death_raw_value
  )

pc_for_cluster <- pcs |>
  select(PC1:PC3)

km <- kmeans(pc_for_cluster, centers = 4, nstart = 25)

pcs <- pcs |>
  mutate(cluster = factor(km$cluster))


ypll_pca <- ggplot(pcs, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Clusters of Counties by Housing + SDOH + Clinical Care (PC Space)",
    color = "Cluster"
  )
ggsave("figures/ypll_pca.png", ypll_pca)


pcs |>
  group_by(cluster) |>
  summarise(
    n = n(),
    mean_ypll = mean(premature_death_raw_value, na.rm = TRUE),
    sd_ypll   = sd(premature_death_raw_value, na.rm = TRUE)
  )

boxplot <- ggplot(pcs, aes(cluster, premature_death_raw_value, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Premature Death (YPLL) by Cluster",
    x = "Cluster",
    y = "Premature death (YPLL rate)"
  )
ggsave("figures/ypll_pca_boxplot.png", boxplot)

cluster_profile <- pca_dat |>
  mutate(cluster = pcs$cluster) |>
  group_by(cluster) |>
  summarise(
    n = n(),
    across(all_of(pred_vars), ~ mean(.x, na.rm = TRUE))
  )

cluster_profile

