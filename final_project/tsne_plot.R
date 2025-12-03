library(Rtsne)
library(tidyverse)
library(cluster)
# ---- Load data ----
chr_clean <- read_csv("derived_data/chr_clean_full.csv", show_col_types = F)

# ---- Add a unique identifier BEFORE filtering ----
# Prefer using FIPS because it uniquely identifies counties
chr_clean <- chr_clean %>%
  mutate(id = x5_digit_fips_code)

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

# ---- Keep only rows with complete predictor data ----
pca_dat <- chr_clean %>%
  select(id, all_of(pred_vars)) %>%
  drop_na()

X <- pca_dat %>%
  select(all_of(pred_vars))

# ---- Run PCA ----
pc_res <- prcomp(X, center = TRUE, scale. = TRUE)

# ---- PCA scores, keep IDs to merge back later ----
pcs <- as_tibble(pc_res$x) %>%
  mutate(id = pca_dat$id)

# ---- Select components for clustering ----
pc_for_cluster <- pcs %>% select(PC1:PC3)

# ---- K-means clustering (k = 4 from GAP statistic decision) ----
km <- kmeans(pc_for_cluster, centers = 4, nstart = 25)

pcs <- pcs %>%
  mutate(cluster = factor(km$cluster))

# ---- t-SNE on first 3 PCs ----
perp <- max(5, min(30, floor((nrow(pc_for_cluster) - 1) / 3)))

tsne_res <- Rtsne(
  as.matrix(pc_for_cluster),
  dims = 2,
  perplexity = perp,
  verbose = TRUE,
  check_duplicates = FALSE
)

tsne_df <- as_tibble(tsne_res$Y) %>%
  setNames(c("TSNE1", "TSNE2")) %>%
  mutate(
    id = pcs$id,        # attach ID
    cluster = pcs$cluster
  )

# ---- Plot t-SNE ----
tsne_plot <- ggplot(tsne_df, aes(TSNE1, TSNE2, color = cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "t-SNE Visualization of County Clusters",
    subtitle = "t-SNE on PC1–PC3, colored by k-means clusters",
    color = "Cluster"
  )

ggsave("figures/le_tsne.png", tsne_plot, width = 7, height = 5, dpi = 300)


# ============================
# Exporting County Cluster Info
# ============================

# ---- Merge cluster assignments back to full CHR dataset ----
clustered_df <- chr_clean %>%
  left_join(
    pcs %>% select(id, cluster),
    by = "id"
  )

cluster_labels <- c(
  "1" = "Advantaged socioeconomic profile",
  "2" = "Moderate resources / transitional",
  "3" = "Clinical & socioeconomic disadvantage",
  "4" = "High burden, high risk"
)


# ---- Export only counties used in PCA/Clustering (no NAs) ----
cluster_export <- clustered_df %>%
  filter(!is.na(cluster)) %>%
  select(
    id,
    name,
    state_abbreviation,
    population_raw_value,
    cluster
  ) %>%
  mutate(
    cluster_label = cluster_labels[as.character(cluster)]
  ) %>%
  arrange(cluster, state_abbreviation, name)

# Save combined file
write_csv(cluster_export, "derived_data/county_clusters_all.csv")
