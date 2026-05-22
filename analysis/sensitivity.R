#' Sensitivity analysis. For each parameter grid point, run the full
#' simulation with a smaller N_SIMS and record the mean tax_change,
#' share better-off and share worse-off. Output a tidy summary parquet.

library(dplyr)
library(furrr)
library(future)
library(purrr)
library(tibble)
library(tidyr)

devtools::load_all()
ensure_dir(OUTPUT_DIR)

SENSITIVITY_N_SIMS <- 2000L

sensitivity_grid <- expand_grid(
  disposition_ratio = c(1.0, 1.5, 2.0, 2.5),
  weibull_scale_years = c(3, 5, 7),
  portfolio_median = c(10000, 15000, 25000)
)

plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

run_one_spec <- function(spec) {
  res <- run_simulation(
    entry_years = c(2009, 2015, 2020),
    params = list(
      n_sims = SENSITIVITY_N_SIMS,
      disposition_ratio = spec$disposition_ratio,
      weibull_scale_years = spec$weibull_scale_years,
      portfolio_median = spec$portfolio_median
    ),
    use_furrr = FALSE
  )
  sims <- res$sim_results
  tibble(
    mean_tax_change = mean(sims$tax_change),
    median_tax_change = median(sims$tax_change),
    share_better_off = mean(sims$tax_change < 0),
    share_worse_off = mean(sims$tax_change > 0)
  )
}

sensitivity_results <- sensitivity_grid |>
  mutate(idx = row_number()) |>
  group_split(idx) |>
  future_map_dfr(
    function(row) bind_cols(row, run_one_spec(row)),
    .options = furrr_options(seed = 20260521L)
  ) |>
  select(-idx)

write_parquet_safely(
  sensitivity_results,
  file.path(OUTPUT_DIR, "sensitivity_results.parquet")
)

plan(sequential)
