library(tidyverse)
library(cluster)

chr_clean <- read_csv("derived_data/chr_clean_full.csv", show_col_types = F)

# ---- Predictor variables ----
pred_vars <- c(
  "percentage_of_households_with_high_housing_costs",
  "severe_housing_cost_burden_raw_value",
  "median_household_income_raw_value",
  "children_in_poverty_raw_value",
  "uninsured_raw_value",
  "ratio_of_population_to_primary_care_physicians",
  "preventable_hospital_stays_raw_value"
)

# ---- Build PCA dataset ----
pca_dat <- chr_clean |>
  select(life_expectancy_raw_value, all_of(pred_vars)) |>
  drop_na()

# Matrix for PCA
X <- pca_dat |> select(all_of(pred_vars))

# ---- Run PCA ----
pc_res <- prcomp(X, center = TRUE, scale. = TRUE)


plot(pc_res, type = "l", main = "Scree Plot")

# Add life expectancy back to PCA scores
pcs <- as_tibble(pc_res$x) |>
  mutate(life_expectancy_raw_value = pca_dat$life_expectancy_raw_value)

# ---- Prepare data for clustering ----
pc_for_cluster <- pcs |> select(PC1:PC3)

# ---- Gap Statistic ----
gap <- clusGap(pc_for_cluster, FUN = kmeans, K.max = 10, B = 50)
png("figures/clusGAP.png", width = 1200, height = 900, res = 150)
plot(gap) 
dev.off()

optimal_k <- maxSE(gap$Tab[,"gap"], gap$Tab[,"SE.sim"], method = "Tibs2001SEmax")
print(paste("Gap statistic suggested k =", optimal_k))

# ---- K-means clustering ----
km <- kmeans(pc_for_cluster, centers = 4, nstart = 25)

pcs <- pcs |>
  mutate(cluster = factor(km$cluster))

# ---- PCA scatterplot ----
le_pca <- ggplot(pcs, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Clusters of Counties by Housing, SDOH, and Clinical Care (PC Space)",
    color = "Cluster"
  )

ggsave("figures/le_pca.png", le_pca, width = 7, height = 5, dpi = 300)

# ---- Cluster summary by life expectancy ----
cluster_summary <- pcs |>
  group_by(cluster) |>
  summarise(
    n = n(),
    mean_life_exp = mean(life_expectancy_raw_value, na.rm = TRUE),
    sd_life_exp   = sd(life_expectancy_raw_value, na.rm = TRUE)
  )

write_csv(cluster_summary, "derived_data/life_expectancy_cluster_summary.csv")

# ---- Boxplot ----
le_boxplot <- ggplot(pcs, aes(cluster, life_expectancy_raw_value, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Life Expectancy by Cluster",
    x = "Cluster",
    y = "Life expectancy (years)"
  )

ggsave("figures/le_pca_boxplot.png", le_boxplot, width = 7, height = 5, dpi = 300)
