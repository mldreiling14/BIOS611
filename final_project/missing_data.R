#data distribution

data <- read_csv("source_data/county_health_rankings_2025.csv") |> clean_names()

data |> head()

names(data)

dim(data)
colSums(is.na(data)) |> sort(decreasing = TRUE)
sum(duplicated(data))
n_distinct(data)
sapply(data, class)

missing_df <- data.frame(
  variable = names(data),
  missing = colSums(is.na(data))
)

missing_data <- missing_df |> 
  filter(missing > 0) |>
  slice(1:15) |>
  arrange(desc(missing)) |>
ggplot(aes(x=reorder(variable, missing), y=missing)) + 
  geom_col() + 
  coord_flip()

ggsave("figures/missing_data.png", missing_data)

