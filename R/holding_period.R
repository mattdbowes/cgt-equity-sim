#' Holding-period model (monthly stepping).
#'
#' Each holding has a baseline monthly hazard derived from a Weibull
#' survival curve with shape and scale-in-years parameters. The hazard
#' is modulated by:
#'   * a disposition effect for the first `disposition_window_months`
#'     month-steps: hazard * `disposition_ratio` when the position is
#'     in unrealised gain, hazard / `disposition_ratio` when in loss;
#'   * a tax-loss-selling spike in June for positions currently in
#'     unrealised loss, scaled by `june_loss_multiplier`.
#' We thin the monthly Bernoulli sequence to determine the actual sale
#' month. If the price series ends before the hazard fires (e.g. the
#' ticker delisted), the holding is sold at the last available month's
#' price.

weibull_monthly_hazard <- function(t_months, shape, scale_years) {
  scale_months <- scale_years * 12
  cum_now <- (t_months / scale_months)^shape
  cum_prev <- pmax(0, (t_months - 1) / scale_months)^shape
  1 - exp(cum_prev - cum_now)
}

simulate_holding <- function(price_path,
                             allocation,
                             weibull_shape = 1.2,
                             weibull_scale_years = 5,
                             disposition_window_months = 12,
                             disposition_ratio = 2.0,
                             june_loss_multiplier = 3.0) {
  stopifnot(nrow(price_path) >= 2)
  entry_date <- price_path$date[1]
  entry_price <- price_path$adj_close[1]
  n_steps <- nrow(price_path)

  for (m in seq.int(2, n_steps)) {
    months_held <- m - 1L
    base_haz <- weibull_monthly_hazard(months_held, weibull_shape,
                                       weibull_scale_years)
    current_price <- price_path$adj_close[m]
    in_gain <- current_price > entry_price

    modifier <- 1
    if (months_held <= disposition_window_months) {
      modifier <- if (in_gain) disposition_ratio else 1 / disposition_ratio
    }
    if (lubridate::month(price_path$date[m]) == 6L && !in_gain) {
      modifier <- modifier * june_loss_multiplier
    }

    haz <- min(1, base_haz * modifier)
    if (stats::runif(1) < haz) {
      return(finalise_holding(price_path, m, current_price, allocation,
                              entry_price, entry_date))
    }
  }

  finalise_holding(price_path, n_steps, price_path$adj_close[n_steps],
                   allocation, entry_price, entry_date)
}

finalise_holding <- function(price_path, sale_idx, sale_price, allocation,
                             entry_price, entry_date) {
  sale_date <- price_path$date[sale_idx]
  units <- allocation / entry_price
  proceeds <- units * sale_price
  cost_base <- allocation
  list(
    entry_date = entry_date,
    sale_date = sale_date,
    entry_price = entry_price,
    sale_price = sale_price,
    units = units,
    cost_base = cost_base,
    proceeds = proceeds,
    nominal_gain = proceeds - cost_base,
    holding_days = as.integer(sale_date - entry_date)
  )
}
