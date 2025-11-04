install.packages('rdist')
install.packages('RSpectra')
library(rdist)
library(RSpectra)


shannon <- function(sequence){
  p <- (table(sequence)/length(sequence)) |> as.numeric()
  -sum(p*log2(p))
}
mutinf <- function(a,b){
  sa <- shannon(a); sb <- shannon(b)
  sab <- shannon(sprintf("%d:%d", a, b))
  sa + sb - sab
}
normalized_mutinf <- function(a,b){
  2*mutinf(a,b)/(shannon(a)+shannon(b))
}

generate_shell_clusters <- function(radii = c(1, 3, 5), k_per = 200, noise_sd = 0.2){
  pts <- map_dfr(seq_along(radii), function(i){
    r <- radii[i]
    theta <- runif(k_per, 0, 2*pi)
    phi   <- acos(runif(k_per, -1, 1))  
    x <- r*sin(phi)*cos(theta) + rnorm(k_per, 0, noise_sd)
    y <- r*sin(phi)*sin(theta) + rnorm(k_per, 0, noise_sd)
    z <- r*cos(phi)            + rnorm(k_per, 0, noise_sd)
    tibble(x, y, z, label = i)
  })
  pts
}

build_adj_epsilon <- function(X, eps = 1.0){
  D <- as.matrix(dist(X))
  A <- (D <= eps) * 1
  diag(A) <- 0
  A <- (A + t(A)) > 0  
  A * 1
}

spectral_cluster <- function(X, k_clusters, eps = 1.0, nstart = 20, iter.max = 50){
  A <- build_adj_epsilon(X, eps = eps)
  deg <- rowSums(A)
  
  if(any(deg == 0)){
    iso_idx <- which(deg == 0)
    Dmat <- as.matrix(dist(X))
    for (i in iso_idx){
      nn <- setdiff(order(Dmat[i, ], decreasing = FALSE)[1:2], i)[1]
      A[i, nn] <- 1; A[nn, i] <- 1
    }
    deg <- rowSums(A)
  }
  Dm12 <- diag(1 / sqrt(deg))
  Lsym <- diag(nrow(A)) - Dm12 %*% A %*% Dm12
  
  ev <- eigs_sym(Lsym, k = k_clusters, which = "SM")
  U  <- ev$vectors
  U <- sweep(U, 1, sqrt(rowSums(U^2)) + 1e-12, "/")
  cl <- kmeans(U, centers = k_clusters, nstart = nstart, iter.max = iter.max)$cluster
  cl
}


R_values   <- seq(3.0, 0.5, by = -0.25) 
k_per      <- 180
noise_sd   <- 0.20
k_clusters <- 3
eps_thresh <- 1.0

sweep_results <- tibble()

for (R in R_values){
  radii <- c(R, 2*R, 3*R)
  cat("R =", R, "radii =", paste(radii, collapse=", "), "\n")
  
  shell <- generate_shell_clusters(radii = radii, k_per = k_per, noise_sd = noise_sd)
  X <- shell %>% select(x, y, z) %>% as.matrix()
  
  pred <- spectral_cluster(X, k_clusters = k_clusters, eps = eps_thresh, nstart = 20, iter.max = 50)
  
  nmi <- normalized_mutinf(pred, shell$label)
  
  sweep_results <- bind_rows(sweep_results,
                             tibble(R = R,
                                    min_separation = diff(radii)[1],  # equals R
                                    eps = eps_thresh,
                                    NMI = nmi))
}

p <- ggplot(sweep_results, aes(R, NMI)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  theme_minimal() +
  labs(
    title = "Spectral Clustering on Concentric Shells (3D)",
    subtitle = "ε-neighborhood graph (eps = 1.0); k = 3; noise_sd = 0.20",
    x = "Base radius R (radii = R, 2R, 3R) — smaller → shells closer",
    y = "Normalized Mutual Information (NMI vs true labels)"
  )

p

dir.create("figures", showWarnings = FALSE)
 ggsave("figures/task2_spectral_shells.png", p, width = 8, height = 4, dpi = 200)
