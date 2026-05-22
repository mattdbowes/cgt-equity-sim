# cgt-equity-sim

A Monte Carlo simulation of the CGT reform proposed in the 2026 Australian
Federal Budget — replacing the 50% capital-gains discount with CPI
indexation of the cost base, subject to a 30% minimum effective tax rate —
applied to representative retail share portfolios drawn from the
S&P/ASX 200 in 2009, 2015, and 2020.

The full methodology, including parameter sources, the disposition-effect
model, and the treatment of taken-over or delisted constituents, is
documented in [`methodology.qmd`](methodology.qmd).

## Disclaimer

This project was built in a personal capacity. It does not represent the views
of my employer.

## Repository layout

```
R/                Simulation engine (load with devtools::load_all)
data-raw/         Scripts that build the input panels from external sources
data/             Pre-computed inputs (parquet, gitignored)
analysis/         Headline run, sensitivity sweep, and figure generation
outputs/          Simulation results (parquet + RDS, gitignored)
figures/          Generated PNGs (gitignored)
methodology.qmd   Documented methodology
```

## First-time setup

```r
renv::init()         # builds the library from DESCRIPTION
renv::snapshot()     # writes renv.lock
```

## Building the inputs

The `data-raw/` scripts run in order. `02_asx200_constituents.R` reads
manually curated constituent CSVs from
`data-raw/asx200_constituents/`; see the header of that script for the
expected schema.

```r
source("data-raw/02_asx200_constituents.R")  # constituent panel
source("data-raw/03_cpi.R")                   # ABS CPI series
source("data-raw/01_price_panel.R")           # Yahoo monthly prices
source("data-raw/01b_recover_yahoo.R")        # name-based ticker recovery
source("data-raw/01c_manual_recovery.R")      # hand-curated renames (successor_map.csv)
source("data-raw/01d_takeover_synth.R")       # synthetic series for delisted names (takeover_map.csv)
```

Steps `01b`–`01d` exist because Yahoo's free history is missing most
ASX names that were renamed, taken over, or delisted between 2009 and
today. `successor_map.csv` records continuous-history renames; for
genuine delistings, `takeover_map.csv` records an entry and terminal
price and the synth script interpolates a monthly series between them.

## Running the simulation

```r
source("analysis/run_simulation.R")   # writes outputs/sim_results.parquet
source("analysis/charts.R")           # writes figures/*.png
source("analysis/sensitivity.R")      # writes outputs/sensitivity_results.parquet
```

`run_simulation.R` runs 10,000 portfolios per entry year (2009, 2015,
2020) by default, parallelised across cohorts via `furrr`.
