#' Pull close price series (split-adjusted only, NOT dividend-adjusted)
#' for every ticker that has ever appeared in the ASX 200 between 2005
#' and 2025. We use Yahoo's raw `close` rather than `adjusted` because
#' for CGT we model the price an investor actually pays / receives, and
#' dividends are taxed separately — adjusted close would inflate
#' capital gains by ~the dividend yield compounded over the holding
#' period, and for stocks with multiple demergers (e.g. TAH) produces
#' near-zero historical prices that explode the simulation. Splits are
#' rare in the ASX 200 window so accepting the residual split-step
#' artefacts is the lesser evil for v1.
#'
#' Daily Yahoo data is downsampled to the last trading day of each
#' month, since the simulation steps monthly. Output: a single parquet
#' at data/price_panel.parquet with schema: ticker, date, adj_close
#' (column kept named `adj_close` for downstream compatibility — it now
#' holds raw close).
#'
#' If a ticker's series ends before the simulation window does, the
#' holding is sold at the last available month's price by the engine
#' in R/holding_period.R.
#'
#' Run order:
#'   1. data-raw/02_asx200_constituents.R  (so we know which tickers
#'      need data)
#'   2. this script.

library(dplyr)
library(lubridate)
library(purrr)
library(tibble)
library(tidyquant)

devtools::load_all()

START_DATE <- as.Date("2005-01-01")
END_DATE <- as.Date("2025-12-31")

constituents <- read_parquet_safely(
  file.path(DATA_DIR, "asx200_constituents.parquet")
)

ticker_universe <- constituents |>
  distinct(ticker) |>
  pull(ticker)

ticker_yahoo <- paste0(ticker_universe, ".AX")

fetch_one <- function(symbol) {
  message("Fetching ", symbol)
  out <- tryCatch(
    withCallingHandlers(
      tidyquant::tq_get(symbol, from = START_DATE, to = END_DATE,
                        get = "stock.prices"),
      warning = function(w) {
        message("  warning: ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      message("  failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0 ||
      !all(c("date", "close") %in% names(out))) {
    return(NULL)
  }
  out |>
    mutate(ticker = sub("\\.AX$", "", symbol)) |>
    select(ticker, date, adj_close = close)
}

prices_daily <- map_dfr(ticker_yahoo, fetch_one)

price_panel <- prices_daily |>
  mutate(month = floor_date(date, "month")) |>
  group_by(ticker, month) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(ticker, date, adj_close) |>
  arrange(ticker, date)

write_parquet_safely(
  price_panel,
  file.path(DATA_DIR, "price_panel.parquet")
)
