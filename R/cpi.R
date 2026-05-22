#' CPI indexation helpers.
#'
#' The cost-base indexation factor under the pre-1999 CGT regime is
#' CPI(sale) / CPI(purchase). The ABS publishes quarterly CPI; we
#' linearly interpolate to monthly so each calendar date can be matched
#' to an indexation reference.

load_cpi <- function(path = file.path(DATA_DIR, "cpi_monthly.parquet")) {
  read_parquet_safely(path)
}

cpi_at <- function(date, cpi_monthly) {
  target <- lubridate::floor_date(as.Date(date), "month")
  idx <- match(target, cpi_monthly$month)
  if (any(is.na(idx))) {
    stop("CPI missing for one or more requested dates.", call. = FALSE)
  }
  cpi_monthly$cpi[idx]
}

indexation_factor <- function(purchase_date, sale_date, cpi_monthly) {
  cpi_sale <- cpi_at(sale_date, cpi_monthly)
  cpi_purchase <- cpi_at(purchase_date, cpi_monthly)
  cpi_sale / cpi_purchase
}
