#' Project-wide constants and small helpers.

DATA_DIR <- "data"
RAW_DIR <- "data-raw"
OUTPUT_DIR <- "outputs"
FIGURE_DIR <- "figures"

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

write_parquet_safely <- function(df, path) {
  ensure_dir(dirname(path))
  arrow::write_parquet(df, path)
  invisible(path)
}

read_parquet_safely <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf(
      "Expected %s to exist. Run the relevant data-raw/ script first.",
      path
    ), call. = FALSE)
  }
  arrow::read_parquet(path)
}

with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  set.seed(seed)
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = .GlobalEnv)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old, envir = .GlobalEnv)
    }
  })
  force(expr)
}

trading_days_between <- function(from, to, calendar) {
  calendar[calendar >= from & calendar <= to]
}
