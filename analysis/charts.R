#' Produce the five headline charts. Reads sim_results and
#' holding_results from outputs/ and writes PNGs to figures/.

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

devtools::load_all()
ensure_dir(FIGURE_DIR)

sim_results <- read_parquet_safely(file.path(OUTPUT_DIR, "sim_results.parquet"))
holding_results <- read_parquet_safely(file.path(OUTPUT_DIR, "holding_results.parquet"))

theme_grattan <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey30"),
      strip.text = element_text(face = "bold")
    )
}

save_chart <- function(plot, name, width = 7, height = 4.5) {
  ggsave(file.path(FIGURE_DIR, paste0(name, ".png")),
         plot, width = width, height = height, dpi = 200)
}

# ---- Chart 1: distribution of % tax change per simulated investor ----
# Excludes sims where tax_current == 0 (no current-system liability, so
# the % change is undefined). Those are the no-gain / net-loss portfolios.
chart1_data <- sim_results |>
  filter(tax_current > 0) |>
  mutate(pct_tax_change = (tax_reformed - tax_current) / tax_current)

share_better <- mean(chart1_data$pct_tax_change < 0)
share_worse  <- mean(chart1_data$pct_tax_change > 0)
share_excluded <- 1 - nrow(chart1_data) / nrow(sim_results)

p1 <- ggplot(chart1_data, aes(x = pct_tax_change)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text", x = -Inf, y = Inf,
           label = sprintf("%.1f%% better off", 100 * share_better),
           hjust = -0.1, vjust = 2, colour = "darkgreen") +
  annotate("text", x = Inf, y = Inf,
           label = sprintf("%.1f%% worse off", 100 * share_worse),
           hjust = 1.1, vjust = 2, colour = "firebrick") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(xlim = quantile(chart1_data$pct_tax_change,
                                  c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "Distribution of % CGT change per simulated investor",
    subtitle = sprintf(
      "Reformed (CPI indexation) vs current (50%% discount), 30%% MTR. %.1f%% of sims excluded (no current-system liability).",
      100 * share_excluded
    ),
    x = "% change in tax (reformed / current − 1)", y = "Density"
  ) +
  theme_grattan()
save_chart(p1, "01_tax_change_density")

# ---- Chart 2: % tax change by entry-year cohort ----
chart2_data <- sim_results |>
  filter(tax_current > 0) |>
  mutate(pct_tax_change = (tax_reformed - tax_current) / tax_current)

chart2_ann <- chart2_data |>
  group_by(entry_year) |>
  summarise(
    share_better = mean(pct_tax_change < 0),
    share_worse  = mean(pct_tax_change > 0),
    .groups = "drop"
  )

p2 <- ggplot(chart2_data, aes(x = pct_tax_change)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data = chart2_ann, aes(x = -Inf, y = Inf,
            label = sprintf("%.1f%% better off", 100 * share_better)),
            hjust = -0.05, vjust = 2, colour = "darkgreen",
            inherit.aes = FALSE) +
  geom_text(data = chart2_ann, aes(x = Inf, y = Inf,
            label = sprintf("%.1f%% worse off", 100 * share_worse)),
            hjust = 1.05, vjust = 2, colour = "firebrick",
            inherit.aes = FALSE) +
  facet_wrap(~ entry_year, scales = "free_y") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(xlim = quantile(chart2_data$pct_tax_change,
                                  c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "% tax change by entry-year cohort",
    subtitle = "Excludes sims with no current-system liability",
    x = "% change in tax (reformed / current − 1)", y = "Density"
  ) +
  theme_grattan()
save_chart(p2, "02_tax_change_by_cohort", width = 9, height = 6)

# ---- Chart 3: tax change vs portfolio nominal return ----
binned <- sim_results |>
  mutate(nominal_return = total_nominal_gain / portfolio_value,
         return_bin = cut(nominal_return,
                          breaks = quantile(nominal_return,
                                            probs = seq(0, 1, 0.05),
                                            na.rm = TRUE),
                          include.lowest = TRUE)) |>
  group_by(return_bin) |>
  summarise(
    mean_return = mean(nominal_return, na.rm = TRUE),
    mean_tax_change = mean(tax_change, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(binned, aes(x = mean_return, y = mean_tax_change)) +
  geom_point(colour = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, colour = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Tax change vs portfolio nominal return",
    subtitle = "Each point: mean within 5% return quantile bin",
    x = "Portfolio nominal return", y = "Mean tax change ($)"
  ) +
  theme_grattan()
save_chart(p3, "03_tax_change_vs_return")

# ---- Chart 4: median % tax change by realised holding period (gains only) ----
# Treats each holding as a standalone tax event (ignores intra-portfolio
# netting), restricted to holdings that produced a nominal gain. Short-
# term gains are taxed at the marginal rate under both regimes, so their
# % tax change is identically zero and they're excluded from the chart
# to focus on where indexation actually bites.
chart4_data <- holding_results |>
  filter(nominal_gain > 0, is_long_term) |>
  mutate(
    holding_years = holding_days / 365.25,
    tax_current_holding = 0.5 * MARGINAL_RATE * nominal_gain,
    tax_reformed_holding = MARGINAL_RATE * pmax(real_gain, 0),
    pct_tax_change = (tax_reformed_holding - tax_current_holding) /
      tax_current_holding,
    holding_bin = cut(holding_years,
                      breaks = c(1, 2, 3, 5, 7, 10, 20),
                      include.lowest = TRUE)
  ) |>
  filter(!is.na(holding_bin))

chart4_summary <- chart4_data |>
  group_by(entry_year, holding_bin) |>
  summarise(
    median_pct_change = median(pct_tax_change),
    n = n(),
    .groups = "drop"
  )

p4 <- ggplot(chart4_summary,
             aes(x = holding_bin, y = median_pct_change)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = percent(median_pct_change, accuracy = 1)),
            vjust = -0.4, size = 3) +
  facet_wrap(~ entry_year) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Median % tax increase by realised holding period",
    subtitle = "Long-term gain holdings only, faceted by entry year. Reformed (CPI indexation) vs current (50% discount).",
    x = "Holding period (years)", y = "Median % change in tax on the holding"
  ) +
  theme_grattan() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_chart(p4, "04_tax_change_by_holding_period", width = 10, height = 5)

message("Wrote charts to ", FIGURE_DIR, "/")
