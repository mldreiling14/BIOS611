## TASK 1 ###################################################################

# Load raw NUFORC file
data <- read.csv('nuforc_sightings.csv')

# Filter to U.S. rows; normalize key text fields
US_filter <- function(x){
  x |>
    mutate(
      country = str_to_upper(country),
      state   = str_to_upper(state),
      shape   = str_to_lower(str_trim(shape)),
      shape   = na_if(shape, "")           # blank -> NA
    ) |>
    # Keep United States rows with 2-letter state codes
    filter(country == "USA", !is.na(state), nchar(state) == 2)
}

US_data <- US_filter(data)                  # row-level U.S.-only data (keep for Tasks 3–4)
valid_states <- state.abb                   # 50 official states

# Build state × shape wide table (for Task 2). This is the ONLY place we need aggregation.
# NOTE: US_filtered is a wide table; don't use it in Tasks 3–4 where we need row-level text.
US_filtered <- US_data %>%
  filter(state %in% valid_states) %>%       # only the 50 states (drop DC/PR/etc.)
  drop_na(shape) %>%                        # no missing shape
  filter(!(shape %in% c("unknown", "other"))) %>%  # remove generic shapes
  count(state, shape) %>%                   # tall frequency table
  pivot_wider(                              # widen to state × shape
    names_from  = shape,
    values_from = n,
    values_fill = 0
  )

# Save the shape table for reuse
write_csv(US_filtered, "derived_data/US_shape_table.csv")

# Task 1 answers: count of distinct known shapes, and top circle state
US_data |>
  filter(!is.na(shape)) |>
  filter(!(shape %in% c("unknown","other","na","?","unspecified","not sure"))) |>
  distinct(shape) |>
  nrow()

US_data |>
  filter(shape == "circle") |>
  count(state) |>
  slice_max(order_by = n, n = 1)

## TASK 2 ###################################################################
# PCA on state × shape proportions

# Use the wide table built above (CORRECT object here is US_filtered)
pca_data <- US_filtered |>
  column_to_rownames("state") |>
  as.matrix()

# Normalize each state's row to proportions so PCA reflects MIX, not volume
normalize_rows <- function(x) {
  rs <- rowSums(x, na.rm = TRUE)
  rs[rs == 0] <- 1e-9
  sweep(x, 1, rs, "/")
}

props_mat <- normalize_rows(pca_data)
pca_shapes <- prcomp(props_mat)  # (You can add center=TRUE, scale.=TRUE if desired)

# Scree plot
var <- pca_shapes$sdev^2 / sum(pca_shapes$sdev^2)
scree <- tibble(PC = seq_along(var), var = var)
g <- ggplot(scree, aes(PC, var)) + geom_point()
ggsave("images/pca_shapes_scree.png", g)
g

# PC1–PC2 scatter of states
scores <- as_tibble(pca_shapes$x, rownames = "state")
g2 <- ggplot(scores, aes(PC1, PC2, label = state)) + geom_point()
ggsave("images/pca_shapes_pc12.png", g2)
g2

# Loadings (which shapes drive PCs)
loadings <- as_tibble(pca_shapes$rotation, rownames = "shape")
write_csv(loadings, "derived_data/pca_shapes_loadings.csv")

# Top contributors by absolute loading
top_pc1 <- loadings |> arrange(desc(abs(PC1))) |> slice_head(n=3)
top_pc2 <- loadings |> arrange(desc(abs(PC2))) |> slice_head(n=3)

## TASK 3 ###################################################################
# Clean & tokenize U.S. summaries (row-level), then histograms and wordclouds

# IMPORTANT: We must use row-level data with text
summaries_clean <- US_data %>%
  filter(state %in% valid_states) %>%       # same 50-state restriction
  mutate(
    summary = str_to_lower(summary),        # lowercase
    summary = str_replace_all(summary, "[^ -~]", " "),  # strip non-ASCII
    summary = str_squish(summary)           # trim + collapse whitespace
  ) %>%
  select(state, summary) %>%
  unnest_tokens(word, summary) %>%          # tokenize words
  filter(str_detect(word, "^[a-z]+$"))      # keep alphabetic tokens only

# Top words BEFORE stopword removal (mostly generic English)
word_counts <- summaries_clean %>% count(word, sort = TRUE)

g3 <- word_counts %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() + coord_flip() +
  labs(title = "Top 20 Words (Before Stopword Removal)", x = "Word", y = "Count")
ggsave("images/top_words_before.png", g3)
g3

# Wordcloud (limit top 100). Draw to a device and CLOSE it.
png("images/wordcloud_before.png", width = 800, height = 600)
set.seed(123)
wordcloud(words = word_counts$word, freq = word_counts$n,
          max.words = 100, colors = brewer.pal(8, "Dark2"))
dev.off()  # NEW: close device

# Remove stopwords, then redo histogram
word_counts_nostop <- summaries_clean %>%
  filter(!(word %in% stopwords("en"))) %>%
  count(word, sort = TRUE)

g4 <- word_counts_nostop %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() + coord_flip() +
  labs(title = "Top 20 Words (After Stopword Removal)", x = "Word", y = "Count")
ggsave("images/top_words_after.png", g4)
g4

# Wordcloud after stopword removal
png("images/wordcloud_after.png", width = 800, height = 600)
set.seed(123)
wordcloud(words = word_counts_nostop$word, freq = word_counts_nostop$n,
          max.words = 100, colors = brewer.pal(8, "Dark2"))
dev.off() 

# Save top-100 cleaned vocabulary for Task 4
dictionary <- word_counts_nostop %>%
  slice_max(n, n = 100) %>%
  pull(word)
write_csv(tibble(word = dictionary), "derived_data/dictionary_top100.csv")

## TASK 4 ###################################################################
# Build 50-state × top-100-word table and run PCA (repeat Task 2 logic on text)

# 1) Tokens AFTER stopword removal (row-level again -> stick with US_data)
tokens_nostop <- US_data %>%
  filter(state %in% valid_states) %>%
  mutate(
    summary = str_to_lower(summary),
    summary = str_replace_all(summary, "[^ -~]", " "),
    summary = str_squish(summary)
  ) %>%
  select(state, summary) %>%
  unnest_tokens(word, summary) %>%
  filter(str_detect(word, "^[a-z]+$"), nchar(word) >= 3) %>%
  filter(!(word %in% stopwords("en")))

# 2) Global top-100 vocabulary for columns
vocab <- tokens_nostop %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 100) %>%
  pull(word)
write_csv(tibble(word = vocab), "derived_data/dictionary_top100.csv")

# 3) State × word tall counts restricted to vocab, then widen
state_word_counts <- tokens_nostop %>%
  semi_join(tibble(word = vocab), by = "word") %>%
  count(state, word, name = "n")

state_word_wide <- state_word_counts %>%
  pivot_wider(names_from = word, values_from = n, values_fill = 0)
write_csv(state_word_wide, "derived_data/state_word_counts.csv")

# 4) Normalize rows (proportions) and PCA
M_words <- state_word_wide %>%
  column_to_rownames("state") %>%
  as.matrix()
P_words <- normalize_rows(M_words)
p_words <- prcomp(P_words, center = TRUE, scale. = TRUE)

# 5) Scree + PC1–PC2 scatter (words PCA)
ve_w <- p_words$sdev^2 / sum(p_words$sdev^2)

g_sw <- tibble(PC = seq_along(ve_w), var = ve_w) %>%
  ggplot(aes(PC, var)) +
  geom_point() + geom_line()
ggsave("images/pca_words_scree.png", g_sw, width = 7, height = 5, dpi = 150)

g_pc <- as_tibble(p_words$x, rownames = "state") %>%
  ggplot(aes(PC1, PC2, label = state)) +
  geom_point() +
  ggrepel::geom_text_repel(size = 3) +
  theme_minimal()
ggsave("images/pca_words_pc12.png", g_pc, width = 7, height = 5, dpi = 150)

# 6) Word loadings for interpretation
word_loadings <- as_tibble(p_words$rotation, rownames = "word")
write_csv(word_loadings, "derived_data/pca_words_loadings.csv")

# 7) Top drivers (absolute loadings) for reporting
top_pc1_words <- word_loadings %>%
  arrange(desc(abs(PC1))) %>% slice_head(n = 8)
top_pc2_words <- word_loadings %>%
  arrange(desc(abs(PC2))) %>% slice_head(n = 8)
cat("\nTop PC1 words (abs loadings):\n"); print(top_pc1_words)
cat("\nTop PC2 words (abs loadings):\n"); print(top_pc2_words)
