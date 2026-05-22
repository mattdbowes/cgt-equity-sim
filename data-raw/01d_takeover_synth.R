#' Synthetic price series for 2009 ASX 200 constituents that were
#' taken over / delisted with no Yahoo successor. For each name we know
#' the entry price (Feb 2009, from the spreadsheet) and a deal exit
#' price + date (manually curated in takeover_map.csv). The simulator
#' steps monthly, so we generate a monthly series by linear price
#' interpolation between entry and exit.
#'
#' Caveats:
#'   * No intra-period volatility, so the disposition effect cannot
#'     trigger on month-to-month price moves — most holdings will sell
#'     via the baseline Weibull hazard. Realised holding periods for
#'     these tickers will be biased upward relative to volatile names.
#'   * For names with non-cash takeovers (scrip+cash) the exit price is
#'     the implied AUD value at deal close; subsequent moves in the
#'     acquirer aren't captured.
#'   * Exit prices flagged "memory" in takeover_map.csv are from
#'     general knowledge and should be verified against ASX
#'     announcements before any external use.
#'
#' Run AFTER 01_price_panel.R, 01b_recover_yahoo.R, 01c_manual_recovery.R.

suppressMessages({
  library(arrow)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(tibble)
})
devtools::load_all()

PANEL_PATH <- file.path(DATA_DIR, "price_panel.parquet")
ENTRY_DATE <- as.Date("2009-02-27")

map <- readr::read_csv(file.path(RAW_DIR, "takeover_map.csv"),
                       show_col_types = FALSE)

# Map last-trading-day of each month between entry and exit
month_ends_between <- function(from, to) {
  starts <- seq(floor_date(from, "month"), floor_date(to, "month"),
                by = "month")
  ends <- ceiling_date(starts, "month") - 1
  # Adjust to actual trading-style end (just use month-end date)
  ends
}

build_series <- function(row) {
  ends <- month_ends_between(ENTRY_DATE, as.Date(row$exit_date))
  n <- length(ends)
  if (n < 2) return(tibble())
  # Linear interp from entry_price (at start) to exit_price (at end)
  prices <- seq(row$entry_price, row$exit_price, length.out = n)
  # For zero-recovery cases, ensure the last month is exactly 0
  if (row$exit_price == 0) prices[n] <- 0
  tibble(ticker = row$orig_ticker, date = ends, adj_close = prices) |>
    filter(adj_close > 0 | row$exit_price == 0)  # keep zero only at terminal
}

panel <- arrow::read_parquet(PANEL_PATH)
already_have <- unique(panel$ticker)

results <- purrr::map_dfr(seq_len(nrow(map)), function(i) {
  if (map$orig_ticker[i] %in% already_have) {
    message(sprintf("%s already in panel — skipping.", map$orig_ticker[i]))
    return(tibble())
  }
  message(sprintf("Synth series for %s: %s @ %.2f -> %s @ %.2f (%s)",
                  map$orig_ticker[i], ENTRY_DATE, map$entry_price[i],
                  map$exit_date[i], map$exit_price[i], map$confidence[i]))
  build_series(map[i, ])
})

# Treat the absolute-zero last row specially — we want it kept
# (filter above keeps adj_close > 0 OR exit_price == 0).
# For OST (zero exit), keep the zero-price terminal row; the simulator's
# build_price_lookup currently drops adj_close <= 0 entries, so set a
# tiny positive epsilon instead to keep the sale row visible.
results <- results |>
  mutate(adj_close = pmax(adj_close, 1e-4))

if (nrow(results) > 0) {
  panel_new <- bind_rows(panel, results) |> arrange(ticker, date)
  write_parquet_safely(panel_new, PANEL_PATH)
  message(sprintf(
    "Appended %d synthetic rows for %d tickers; panel now %d rows / %d tickers.",
    nrow(results), length(unique(results$ticker)),
    nrow(panel_new), length(unique(panel_new$ticker))
  ))
} else {
  message("Nothing to append.")
}
