library(tidyverse)
library(stringr)

cluster_export <- read_csv('derived_data/county_clusters_all.csv', show_col_types = F)


cluster_label_counts <- cluster_export |>
  count(cluster_label) |>
  arrange(desc(n))

state_cluster_label_counts <- cluster_export |>
  count(state_abbreviation, cluster_label) |>
  arrange(state_abbreviation, cluster_label)

state_cluster_label_pct <- cluster_export |>
  count(state_abbreviation, cluster_label) |>
  group_by(state_abbreviation) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

state_cluster_label_pct <- state_cluster_label_pct |>
  mutate(state_abbreviation = factor(state_abbreviation,
                                     levels = sort(unique(state_abbreviation),
                                                   decreasing = TRUE)))

plot_state_pct_label <- ggplot(state_cluster_label_pct,
                               aes(x = state_abbreviation,
                                   y = pct,
                                   fill = cluster_label)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of County Cluster Types Within Each State",
    x = "State",
    y = "Percent of Counties",
    fill = "Cluster Type"
  ) +
  coord_flip() + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    axis.text.y = element_text(size = 9)
  )

ggsave("figures/state_cluster_label_distribution_pct.png",
       plot_state_pct_label,
       width = 10, height = 6, dpi = 300)

