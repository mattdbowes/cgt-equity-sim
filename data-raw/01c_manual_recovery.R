#' Manual successor-ticker recovery. For ASX 200 constituents whose
#' original ticker was removed from Yahoo after a rename (without a
#' takeover purging the price history), Yahoo typically migrates the
#' full historical series under the new ticker. We hand-curate the
#' mapping in successor_map.csv and append the migrated series back
#' under the ORIGINAL ticker so the constituent join still works.
#'
#' Only continuous-history renames qualify here — takeovers that
#' delisted the entity (NCM, FGL, AXA etc.) genuinely have no Yahoo
#' series and would need a paid source.
#'
#' Run AFTER 01_price_panel.R and 01b_recover_yahoo.R.

suppressMessages({
  library(arrow)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(tidyquant)
})
devtools::load_all()

PANEL_PATH <- file.path(DATA_DIR, "price_panel.parquet")
START_DATE <- as.Date("2005-01-01")
END_DATE   <- as.Date("2025-12-31")

map <- readr::read_csv(file.path(RAW_DIR, "successor_map.csv"),
                       show_col_types = FALSE)

panel <- arrow::read_parquet(PANEL_PATH)
already_have <- unique(panel$ticker)

results <- purrr::map_dfr(seq_len(nrow(map)), function(i) {
  orig <- map$orig_ticker[i]
  succ <- map$successor[i]
  if (orig %in% already_have) {
    message(sprintf("%s already in panel — skipping.", orig))
    return(tibble::tibble())
  }
  message(sprintf("Fetching %s as %s", orig, succ))
  out <- tryCatch(
    suppressWarnings(tidyquant::tq_get(succ, from = START_DATE, to = END_DATE,
                                       get = "stock.prices")),
    error = function(e) { message("  failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) {
    message("  no data returned for ", succ)
    return(tibble::tibble())
  }
  out |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month) |>
    slice_max(date, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(ticker = orig, date, adj_close = close) |>
    filter(!is.na(adj_close), adj_close > 0)
})

if (nrow(results) > 0) {
  panel_new <- bind_rows(panel, results) |> arrange(ticker, date)
  write_parquet_safely(panel_new, PANEL_PATH)
  message(sprintf(
    "Appended %d rows for %d recovered tickers; panel now %d rows / %d tickers.",
    nrow(results), length(unique(results$ticker)),
    nrow(panel_new), length(unique(panel_new$ticker))
  ))
} else {
  message("Nothing to append.")
}
