#' Portfolio construction.
#'
#' For a given entry year, draw a portfolio value from a log-normal
#' distribution, then draw `n_holdings` tickers from the ASX 200
#' constituent list as at January of that year, with selection
#' probability proportional to free-float market cap. Equal-weight the
#' portfolio across holdings.

draw_portfolio_value <- function(n,
                                 median_value = 15000,
                                 sdlog = 1.1) {
  meanlog <- log(median_value)
  stats::rlnorm(n, meanlog = meanlog, sdlog = sdlog)
}

draw_holdings <- function(constituents,
                          n_holdings = 10) {
  if (nrow(constituents) < n_holdings) {
    stop("Constituent list smaller than requested portfolio size.", call. = FALSE)
  }
  weights <- constituents$mkt_cap / sum(constituents$mkt_cap)
  picked <- sample(seq_len(nrow(constituents)),
                   size = n_holdings,
                   replace = FALSE,
                   prob = weights)
  constituents[picked, c("ticker", "mkt_cap")]
}

build_portfolio <- function(entry_year,
                            sim_id,
                            constituents_by_year,
                            portfolio_value,
                            n_holdings = 10) {
  pool <- constituents_by_year[[as.character(entry_year)]]
  if (is.null(pool)) {
    stop(sprintf("No constituent list for entry year %s.", entry_year), call. = FALSE)
  }
  picks <- draw_holdings(pool, n_holdings = n_holdings)
  per_holding <- portfolio_value / n_holdings
  tibble::tibble(
    entry_year = entry_year,
    sim_id = sim_id,
    holding_id = seq_len(n_holdings),
    ticker = picks$ticker,
    allocation = per_holding
  )
}
