#' Assemble ASX 200 historical constituents for each entry-year cohort.
#' There is no free machine-readable source for the full S&P/ASX 200
#' history; in practice we have three snapshots stitched together from
#' Wikipedia and contemporary index reports (2009-02-27, 2014-09-08,
#' 2020-01-25). Each snapshot is mapped to its closest January and
#' used as the constituent list for that entry year:
#'
#'   2009-02-27  ->  entry year 2009  (mkt_cap from source)
#'   2014-09-08  ->  entry year 2015  (mkt_cap = price x shares-on-issue)
#'   2020-01-25  ->  entry year 2020  (mkt_cap from Wikipedia)
#'
#' Rows with NA tickers are dropped (the 2009 sheet has four). All
#' mkt_caps are raw AUD.
#'
#' This script reads the per-cohort CSVs at
#' data-raw/asx200_constituents/asx200_YYYY-01.csv (columns:
#' ticker, mkt_cap), stacks them, and writes
#' data/asx200_constituents.parquet with columns:
#'   as_of_date, ticker, mkt_cap

library(dplyr)
library(fs)
library(lubridate)
library(purrr)
library(readr)
library(tibble)

devtools::load_all()

INPUT_DIR <- file.path(RAW_DIR, "asx200_constituents")
ensure_dir(INPUT_DIR)

files <- fs::dir_ls(INPUT_DIR, glob = "*.csv")
if (length(files) == 0) {
  stop(sprintf(
    "No constituent CSVs found under %s/. Drop one CSV per entry year (e.g. asx200_2005-01.csv) before running this script.",
    INPUT_DIR
  ), call. = FALSE)
}

extract_date <- function(path) {
  stub <- fs::path_ext_remove(fs::path_file(path))
  stub <- sub("^asx200_", "", stub)
  as.Date(paste0(stub, "-01"))
}

constituents <- map_dfr(files, function(f) {
  read_csv(f, show_col_types = FALSE) |>
    mutate(as_of_date = extract_date(f)) |>
    select(as_of_date, ticker, mkt_cap)
})

stopifnot(
  all(c("as_of_date", "ticker", "mkt_cap") %in% names(constituents)),
  all(!is.na(constituents$mkt_cap)),
  all(constituents$mkt_cap > 0)
)

write_parquet_safely(
  constituents,
  file.path(DATA_DIR, "asx200_constituents.parquet")
)
