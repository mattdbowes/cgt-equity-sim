#' Headline run: 10,000 simulations per entry year, default parameters,
#' parallelised across cohorts via furrr. Writes sim_results and
#' holding_results to data/.

library(arrow)
library(furrr)
library(future)

devtools::load_all()
ensure_dir(OUTPUT_DIR)

plan(multisession, workers = max(1L, parallel::detectCores() - 1L))

results <- run_simulation(
  entry_years = c(2009, 2015, 2020),
  params = list(n_sims = 10000),
  use_furrr = FALSE
)

write_parquet_safely(results$sim_results,
                     file.path(OUTPUT_DIR, "sim_results.parquet"))
write_parquet_safely(results$holding_results,
                     file.path(OUTPUT_DIR, "holding_results.parquet"))
saveRDS(results$params, file.path(OUTPUT_DIR, "params_headline.rds"))

plan(sequential)
