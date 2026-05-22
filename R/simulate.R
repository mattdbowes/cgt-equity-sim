#' Simulation orchestrator.
#'
#' `run_year_cohort()` runs N_SIMS Monte Carlo iterations for a single
#' entry year and returns two tibbles: per-simulation aggregates and
#' per-holding diagnostics. `run_simulation()` fans out across entry
#' years using furrr so multiple cohorts run in parallel.

DEFAULT_PARAMS <- list(
  n_sims = 10000,
  n_holdings = 10,
  portfolio_median = 15000,
  portfolio_sdlog = 1.1,
  weibull_shape = 1.2,
  weibull_scale_years = 5,
  disposition_window_months = 12,
  disposition_ratio = 2.0,
  june_loss_multiplier = 3.0,
  marginal_rate = 0.30,
  seed = 20260521L
)

merge_params <- function(overrides) {
  modifyList(DEFAULT_PARAMS, overrides %||% list())
}

`%||%` <- function(a, b) if (is.null(a)) b else a

build_price_lookup <- function(price_panel) {
  price_panel <- price_panel[!is.na(price_panel$adj_close) &
                               price_panel$adj_close > 0, ]
  split(price_panel, price_panel$ticker)
}

simulate_one_portfolio <- function(entry_year,
                                   sim_id,
                                   constituents_by_year,
                                   price_lookup,
                                   cpi_monthly,
                                   params) {
  portfolio_value <- draw_portfolio_value(
    1,
    median_value = params$portfolio_median,
    sdlog = params$portfolio_sdlog
  )

  picks <- build_portfolio(
    entry_year = entry_year,
    sim_id = sim_id,
    constituents_by_year = constituents_by_year,
    portfolio_value = portfolio_value,
    n_holdings = params$n_holdings
  )

  entry_date <- as.Date(sprintf("%d-01-01", entry_year))

  holdings <- purrr::map_dfr(seq_len(nrow(picks)), function(h) {
    ticker <- picks$ticker[h]
    full_path <- price_lookup[[ticker]]
    if (is.null(full_path)) {
      stop(sprintf("No price path for ticker %s in entry year %d.",
                   ticker, entry_year), call. = FALSE)
    }
    path <- full_path[full_path$date >= entry_date, ]
    if (nrow(path) < 2) {
      stop(sprintf("Insufficient price data for %s from %s.",
                   ticker, entry_date), call. = FALSE)
    }
    result <- simulate_holding(
      price_path = path,
      allocation = picks$allocation[h],
      weibull_shape = params$weibull_shape,
      weibull_scale_years = params$weibull_scale_years,
      disposition_window_months = params$disposition_window_months,
      disposition_ratio = params$disposition_ratio,
      june_loss_multiplier = params$june_loss_multiplier
    )
    tibble::as_tibble(c(
      list(entry_year = entry_year, sim_id = sim_id, holding_id = h,
           ticker = ticker),
      result
    ))
  })

  if (nrow(holdings) == 0) {
    return(list(
      summary = tibble::tibble(
        entry_year = entry_year, sim_id = sim_id,
        portfolio_value = portfolio_value,
        total_nominal_gain = 0, total_real_gain = 0,
        tax_current = 0, tax_reformed = 0,
        tax_change = 0
      ),
      holdings = tibble::tibble()
    ))
  }

  tax_result <- compute_tax(holdings, cpi_monthly,
                            marginal_rate = params$marginal_rate)

  list(
    summary = tibble::tibble(
      entry_year = entry_year,
      sim_id = sim_id,
      portfolio_value = portfolio_value,
      total_nominal_gain = tax_result$total_nominal_gain,
      total_real_gain = tax_result$total_real_gain,
      tax_current = tax_result$tax_current,
      tax_reformed = tax_result$tax_reformed,
      tax_change = tax_result$tax_reformed - tax_result$tax_current
    ),
    holdings = tax_result$holdings
  )
}

run_year_cohort <- function(entry_year,
                            constituents_by_year,
                            price_lookup,
                            cpi_monthly,
                            params) {
  results <- purrr::map(seq_len(params$n_sims), function(sim_id) {
    simulate_one_portfolio(
      entry_year = entry_year,
      sim_id = sim_id,
      constituents_by_year = constituents_by_year,
      price_lookup = price_lookup,
      cpi_monthly = cpi_monthly,
      params = params
    )
  })

  list(
    summary = purrr::map_dfr(results, "summary"),
    holdings = purrr::map_dfr(results, "holdings")
  )
}

run_simulation <- function(entry_years = c(2009, 2015, 2020),
                           params = list(),
                           price_panel_path = file.path(DATA_DIR, "price_panel.parquet"),
                           constituents_path = file.path(DATA_DIR, "asx200_constituents.parquet"),
                           cpi_path = file.path(DATA_DIR, "cpi_monthly.parquet"),
                           use_furrr = TRUE) {

  params <- merge_params(params)

  price_panel <- read_parquet_safely(price_panel_path)
  price_lookup <- build_price_lookup(price_panel)

  constituents_raw <- read_parquet_safely(constituents_path)
  before <- nrow(constituents_raw)
  # Per-row check: keep only constituents whose price path has at
  # least 2 monthly observations at or after the cohort's entry date.
  ticker_last_date <- vapply(price_lookup, function(df) {
    if (nrow(df) == 0) as.Date(NA) else max(df$date)
  }, FUN.VALUE = as.Date(NA))
  constituents_raw$entry_date <- as.Date(sprintf(
    "%d-01-01", lubridate::year(constituents_raw$as_of_date)
  ))
  constituents_raw$last_price_date <-
    ticker_last_date[constituents_raw$ticker]
  constituents_raw <- constituents_raw[
    !is.na(constituents_raw$last_price_date) &
      constituents_raw$last_price_date >= constituents_raw$entry_date +
        lubridate::days(60),
  ]
  constituents_raw$entry_date <- NULL
  constituents_raw$last_price_date <- NULL
  dropped <- before - nrow(constituents_raw)
  if (dropped > 0) {
    message(sprintf(
      "Dropped %d of %d constituent rows with no usable post-entry price path.",
      dropped, before
    ))
  }
  constituents_by_year <- split(
    constituents_raw,
    lubridate::year(constituents_raw$as_of_date)
  )

  cpi_monthly <- read_parquet_safely(cpi_path)

  mapper <- if (use_furrr) furrr::future_map else purrr::map
  furrr_opts <- if (use_furrr) {
    list(.options = furrr::furrr_options(seed = params$seed))
  } else {
    list()
  }

  per_year <- do.call(mapper, c(
    list(.x = entry_years, .f = function(yr) {
      with_seed(params$seed + yr, {
        run_year_cohort(
          entry_year = yr,
          constituents_by_year = constituents_by_year,
          price_lookup = price_lookup,
          cpi_monthly = cpi_monthly,
          params = params
        )
      })
    }),
    furrr_opts
  ))

  list(
    sim_results = purrr::map_dfr(per_year, "summary"),
    holding_results = purrr::map_dfr(per_year, "holdings"),
    params = params
  )
}
