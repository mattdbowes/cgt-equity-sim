#' Recovery pass for tickers that failed direct ASX-suffix fetch in
#' 01_price_panel.R. Uses Yahoo's free-text search endpoint to look up
#' candidate replacement symbols by company name, fetches any that
#' resolve to an ASX listing, and appends the series to the existing
#' price panel under the ORIGINAL constituent ticker so downstream
#' joins still match.
#'
#' Run AFTER 01_price_panel.R and 02_asx200_constituents.R.

suppressMessages({
  library(arrow)
  library(dplyr)
  library(httr2)
  library(jsonlite)
  library(purrr)
  library(readxl)
  library(tibble)
  library(tidyquant)
  library(lubridate)
})
devtools::load_all()

PANEL_PATH <- file.path(DATA_DIR, "price_panel.parquet")
START_DATE <- as.Date("2005-01-01")
END_DATE   <- as.Date("2025-12-31")
SLEEP_SECS <- 0.3

build_master <- function() {
  sheets <- list(
    c(s = "27-02-2009", tcol = "symbol", ncol = "company_name"),
    c(s = "8-09-2014",  tcol = "symbol", ncol = "company_name"),
    c(s = "25_01_2020", tcol = "symbol", ncol = "company_name")
  )
  purrr::map_dfr(sheets, function(x) {
    df <- read_excel("asx200_wiki.xlsx", sheet = x[["s"]])
    tibble(ticker = df[[x[["tcol"]]]], company_name = df[[x[["ncol"]]]])
  }) |> filter(!is.na(ticker)) |> distinct(ticker, .keep_all = TRUE)
}

yahoo_search <- function(query) {
  url <- "https://query2.finance.yahoo.com/v1/finance/search"
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_url_query(q = query, quotesCount = 6, newsCount = 0) |>
      httr2::req_user_agent("Mozilla/5.0") |>
      httr2::req_timeout(15) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) != 200) return(NULL)
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                             simplifyVector = FALSE)
  body$quotes
}

pick_asx_symbol <- function(quotes) {
  for (q in quotes) {
    sym <- q$symbol %||% NA_character_
    exch <- q$exchange %||% NA_character_
    if (!is.na(sym) && grepl("\\.AX$", sym) && identical(exch, "ASX")) {
      return(sym)
    }
  }
  NULL
}

fetch_prices <- function(symbol) {
  out <- tryCatch(
    withCallingHandlers(
      tidyquant::tq_get(symbol, from = START_DATE, to = END_DATE,
                        get = "stock.prices"),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0 ||
      !all(c("date", "close") %in% names(out))) return(NULL)
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

main <- function() {
  master <- build_master()
  panel <- arrow::read_parquet(PANEL_PATH)
  have <- unique(panel$ticker)
  todo <- master |> filter(!ticker %in% have)
  message(sprintf("Recovery pass: %d unfetched tickers to search.", nrow(todo)))

  recovery_rows <- list()
  log_rows <- list()

  for (i in seq_len(nrow(todo))) {
    orig_ticker <- todo$ticker[i]
    name <- todo$company_name[i]
    message(sprintf("[%d/%d] %s  -- %s", i, nrow(todo), orig_ticker, name))
    quotes <- yahoo_search(name)
    cand <- if (is.null(quotes)) NULL else pick_asx_symbol(quotes)
    Sys.sleep(SLEEP_SECS)
    if (is.null(cand)) {
      log_rows[[length(log_rows) + 1]] <-
        tibble(orig_ticker = orig_ticker, company_name = name,
               candidate = NA_character_, status = "no_asx_match", n_rows = 0L)
      next
    }
    prices <- fetch_prices(cand)
    Sys.sleep(SLEEP_SECS)
    if (is.null(prices)) {
      log_rows[[length(log_rows) + 1]] <-
        tibble(orig_ticker = orig_ticker, company_name = name,
               candidate = cand, status = "fetch_failed", n_rows = 0L)
      next
    }
    monthly <- prices |>
      mutate(month = floor_date(date, "month")) |>
      group_by(month) |>
      slice_max(date, n = 1, with_ties = FALSE) |>
      ungroup() |>
      transmute(ticker = orig_ticker, date, adj_close = close) |>
      filter(!is.na(adj_close), adj_close > 0)
    if (nrow(monthly) < 2) {
      log_rows[[length(log_rows) + 1]] <-
        tibble(orig_ticker = orig_ticker, company_name = name,
               candidate = cand, status = "too_few_rows", n_rows = nrow(monthly))
      next
    }
    recovery_rows[[length(recovery_rows) + 1]] <- monthly
    log_rows[[length(log_rows) + 1]] <-
      tibble(orig_ticker = orig_ticker, company_name = name,
             candidate = cand, status = "ok", n_rows = nrow(monthly))
  }

  log_df <- bind_rows(log_rows)
  write.csv(log_df, file.path(RAW_DIR, "yahoo_recovery_log.csv"),
            row.names = FALSE)
  message("Recovery log -> ", file.path(RAW_DIR, "yahoo_recovery_log.csv"))
  print(log_df |> count(status))

  if (length(recovery_rows) > 0) {
    add <- bind_rows(recovery_rows)
    panel_new <- bind_rows(panel, add) |>
      arrange(ticker, date)
    write_parquet_safely(panel_new, PANEL_PATH)
    message(sprintf("Appended %d rows for %d recovered tickers; panel now %d rows / %d tickers.",
                    nrow(add), length(unique(add$ticker)),
                    nrow(panel_new), length(unique(panel_new$ticker))))
  } else {
    message("No recoveries to append.")
  }
}

main()
