library(tidyverse)
library(matlab)
library(cluster)

generate_hypercube_clusters <- function(n, k =100, side_length, noise_sd = 1.0){
  centers <- diag(side_length, nrow = n, ncol=n)
  clusters <- lapply(seq_len(n), function(i){
    center <- centers[i,]
    matrix(rnorm(k * n, mean=0, sd = noise_sd), ncol=n) + 
      matrix(rep(center, each=k), ncol=n)
  })
  do.call(rbind, clusters) %>%
    as_tibble() %>%
    mutate(label = rep(seq_len(n), each=k))
}

# Parameters
dims <- c(6,5,4,3,2)
side_lengths <- 10:1
results <- tibble()

for (n in dims) {
  for (L in side_lengths){
    cat("Running", n, "D side length", L, "\n")
    data <- generate_hypercube_clusters(
      n=n, 
      side_length = L, 
      k=100, 
      noise_sd =1.0)
    
    r <- clusGap(
      data,
      FUNcluster = kmeans,
      K.max = n + 3,
      nstart = 20,
      iter.max = 50
    )
    
    best_k <- maxSE(r$Tab[,"gap"], r$Tab[,"SE.sim"], method = "firstSEmax")
    results <- results |> bind_rows(tibble(n=n, side_length=L, best_k = best_k))
  }
}

p1 <- ggplot(results, aes(x = side_length, y = best_k)) +
  geom_line() + geom_point() +
  geom_hline(aes(yintercept = n), linetype = "dashed", color = "red") +
  facet_wrap(~ n, labeller = label_both) +
  scale_x_reverse() +
  labs(
    title = "Estimated Clusters (Gap Statistic) vs Side Length",
    x = "Side Length (distance of centers from origin)",
    y = "Estimated number of clusters"
  ) +
  theme_minimal()

dir.create("figures", showWarnings = FALSE)
ggsave("figures/task1_gap_results.png", p1, width = 10, height = 6, dpi = 200)


