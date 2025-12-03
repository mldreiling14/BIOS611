library('janitor')
library('tidyverse')

chr <- read_csv("source_data/county_health_rankings_2025.csv", 
                show_col_types = FALSE) |> 
  slice(-1) |>
  clean_names()

clean_numeric_column <- function(x) {
  x |>
    str_replace_all("[^0-9\\.\\-]", "") |>
    na_if("") |>                          
    as.numeric()
}

id_cols <- c(
  "state_fips_code", "county_fips_code", "x5_digit_fips_code",
  "state_abbreviation", "name", "release_year"
)

drop_patterns <- c(
  "ci_high",
  "ci_low",
  "_denominator",
  "_numerator",
  "_white",
  "_black",
  "_hispanic",
  "_asian",
  "_aian",
  "_nhopi"
)

chr_clean <- chr |>
  select(-matches(str_c(drop_patterns, collapse = "|"))) |>
  mutate(
    across(
      .cols = !all_of(id_cols),
      .fns = clean_numeric_column
    )
  )

chr_clean |> head()

write_csv(chr_clean, "derived_data/chr_clean_full.csv")

