#' Tax calculation under the current and reformed CGT regimes.
#'
#' Current regime: 50% discount on gains from assets held more than 12
#' months, applied to the net of long-term gains and losses. Short-term
#' losses are netted against short-term gains first; remaining losses
#' offset long-term gains pre-discount.
#'
#' Reformed regime: cost base is indexed by CPI for assets held more
#' than 12 months. Indexation cannot create or increase a capital loss,
#' so the indexed cost base is capped at the sale proceeds for any
#' position with a positive nominal gain. The 30% minimum-tax floor is
#' non-binding by construction because the assumed marginal rate is 30%.

LONG_TERM_THRESHOLD_DAYS <- 365L
MARGINAL_RATE <- 0.30
DISCOUNT_RATE <- 0.50

annotate_holdings_with_indexation <- function(holdings, cpi_monthly) {
  factors <- mapply(
    indexation_factor,
    purchase_date = holdings$entry_date,
    sale_date = holdings$sale_date,
    MoreArgs = list(cpi_monthly = cpi_monthly)
  )
  holdings$indexation_factor <- factors
  holdings$indexed_cost_base <- holdings$cost_base * holdings$indexation_factor
  holdings$is_long_term <- holdings$holding_days >= LONG_TERM_THRESHOLD_DAYS
  holdings$real_gain <- with(holdings, {
    raw <- proceeds - indexed_cost_base
    nominal_pos <- nominal_gain > 0
    ifelse(is_long_term & nominal_pos, pmax(raw, 0),
           ifelse(is_long_term & !nominal_pos, nominal_gain, nominal_gain))
  })
  holdings
}

net_gains_with_discount <- function(holdings, discount = DISCOUNT_RATE) {
  lt <- sum(holdings$nominal_gain[holdings$is_long_term])
  st <- sum(holdings$nominal_gain[!holdings$is_long_term])

  if (st < 0 && lt > 0) {
    lt <- lt + st
    st <- 0
  } else if (lt < 0 && st > 0) {
    st <- st + lt
    lt <- 0
  }

  taxable_st <- max(0, st)
  taxable_lt <- max(0, lt) * (1 - discount)
  taxable_st + taxable_lt
}

net_gains_with_indexation <- function(holdings) {
  lt <- sum(holdings$real_gain[holdings$is_long_term])
  st <- sum(holdings$nominal_gain[!holdings$is_long_term])

  if (st < 0 && lt > 0) {
    lt <- lt + st
    st <- 0
  } else if (lt < 0 && st > 0) {
    st <- st + lt
    lt <- 0
  }

  max(0, st) + max(0, lt)
}

compute_tax <- function(holdings, cpi_monthly,
                        marginal_rate = MARGINAL_RATE) {
  holdings <- annotate_holdings_with_indexation(holdings, cpi_monthly)

  taxable_current <- net_gains_with_discount(holdings, DISCOUNT_RATE)
  taxable_reformed <- net_gains_with_indexation(holdings)

  list(
    holdings = holdings,
    tax_current = taxable_current * marginal_rate,
    tax_reformed = taxable_reformed * marginal_rate,
    total_nominal_gain = sum(holdings$nominal_gain),
    total_real_gain = sum(holdings$real_gain)
  )
}
