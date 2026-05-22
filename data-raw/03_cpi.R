#' Build a monthly CPI series for cost-base indexation.
#'
#' ABS series A2325846C is the quarterly All Groups CPI for the
#' weighted average of eight capital cities. We pull it via the
#' `readabs` package, then linearly interpolate to month-end. The
#' resulting parquet has columns: month (first-of-month Date), cpi.

library(dplyr)
library(lubridate)
library(readabs)
library(tibble)
library(tidyr)

devtools::load_all()

CPI_SERIES_ID <- "A2325846C"
START_DATE <- as.Date("2004-01-01")
END_DATE <- as.Date("2026-06-30")

cpi_q <- read_abs_series(CPI_SERIES_ID) |>
  filter(date >= START_DATE, date <= END_DATE) |>
  transmute(quarter_end = date, cpi = value)

months <- tibble(
  month = seq(floor_date(min(cpi_q$quarter_end), "month"),
              floor_date(max(cpi_q$quarter_end), "month"),
              by = "month")
)

cpi_monthly <- months |>
  left_join(cpi_q, by = c("month" = "quarter_end")) |>
  mutate(
    cpi = stats::approx(
      x = as.numeric(month),
      y = cpi,
      xout = as.numeric(month),
      rule = 2
    )$y
  )

stopifnot(all(!is.na(cpi_monthly$cpi)))

write_parquet_safely(
  cpi_monthly,
  file.path(DATA_DIR, "cpi_monthly.parquet")
)
