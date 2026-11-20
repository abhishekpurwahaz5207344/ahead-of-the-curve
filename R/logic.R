# Ahead of The Curve — DATA5002 (2026 T3)
# All observations are supplied NSW Department of Communities and Justice (DCJ)
# quarterly reports. No modelled geographic coordinates or invented returns.

read_dcj_markets <- function(filename) {
  if (!file.exists(filename)) stop("Missing DCJ data: ", filename)
  d <- utils::read.csv(filename, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                       colClasses = c(area = "character"))
  required <- c("area", "period", "median_sale_price", "median_weekly_rent", "sales_count", "rent_count")
  missing <- setdiff(required, names(d))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))
  d$area <- trimws(as.character(d$area))
  d$period <- as.Date(d$period, format = "%Y-%m-%d")
  for (column in c("median_sale_price", "median_weekly_rent", "sales_count", "rent_count")) {
    d[[column]] <- suppressWarnings(as.numeric(d[[column]]))
    d[[column]][!is.finite(d[[column]])] <- NA_real_
  }
  if (anyNA(d$period) || any(!nzchar(d$area))) stop("Invalid market area or quarter-start date")
  if (anyDuplicated(d[c("area", "period")])) stop("Duplicate market + quarter keys")
  if (any(!format(d$period, "%m-%d") %in% c("01-01", "04-01", "07-01", "10-01"))) {
    stop("Market periods must be quarter-start dates")
  }
  if (any(d$median_sale_price <= 0, na.rm = TRUE) ||
      any(d$median_weekly_rent <= 0, na.rm = TRUE)) stop("Invalid non-positive median")
  d[order(d$area, d$period), , drop = FALSE]
}

read_dcj_detail <- function(path) {
  if (!file.exists(path)) return(data.frame())
  if (grepl("\\.gz$", path)) utils::read.csv(gzfile(path), stringsAsFactors = FALSE, na.strings = c("", "NA"))
  else utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

money <- function(x, short = FALSE) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) return("Not available")
  if (short && abs(x) >= 1000000) return(sprintf("$%.2fm", x / 1000000))
  if (short && abs(x) >= 1000) return(sprintf("$%.0fk", x / 1000))
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

pct <- function(x, digits = 1L) {
  if (length(x) == 0L || is.na(x) || !is.finite(x)) return("Not available")
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE, scientific = FALSE), "%")
}

qtr_label <- function(date) {
  if (length(date) == 0L || is.na(date)) return("Unknown quarter")
  paste0(format(date, "%Y"), " Q", (as.integer(format(date, "%m")) - 1L) %/% 3L + 1L)
}

shift_years <- function(date, years) {
  as.Date(sprintf("%d-%s", as.integer(format(date, "%Y")) - as.integer(years),
                  format(date, "%m-%d")))
}

market_snapshot <- function(all_quarters, chosen_period, years = 5L) {
  chosen_period <- as.Date(chosen_period)
  current <- all_quarters[all_quarters$period == chosen_period, , drop = FALSE]
  if (!nrow(current)) return(current)
  baseline <- all_quarters[all_quarters$period == shift_years(chosen_period, years),
                           c("area", "median_sale_price"), drop = FALSE]
  previous <- baseline$median_sale_price[match(current$area, baseline$area)]
  valid_growth <- is.finite(current$median_sale_price) & current$median_sale_price > 0 &
    is.finite(previous) & previous > 0
  current$growth_pa <- rep(NA_real_, nrow(current))
  current$growth_pa[valid_growth] <- 100 *
    ((current$median_sale_price[valid_growth] / previous[valid_growth]) ^ (1 / years) - 1)
  valid_yield <- is.finite(current$median_sale_price) & current$median_sale_price > 0 &
    is.finite(current$median_weekly_rent) & current$median_weekly_rent > 0
  current$gross_yield <- rep(NA_real_, nrow(current))
  current$gross_yield[valid_yield] <- 100 * 52 * current$median_weekly_rent[valid_yield] /
    current$median_sale_price[valid_yield]
  current$baseline_period <- shift_years(chosen_period, years)
  current
}

# Percentile components are calculated BEFORE the viewer filters results.
# Scores therefore remain stable when budget/yield filters change.
score_snapshot <- function(d, weights = c(growth = 5, yield = 3, affordability = 2)) {
  d$score <- rep(NA_real_, nrow(d))
  for (name in c("growth", "yield", "affordability")) d[[paste0("component_", name)]] <- NA_real_
  metrics <- c(growth = "growth_pa", yield = "gross_yield", affordability = "median_sale_price")
  active <- names(metrics)[weights[names(metrics)] > 0]
  if (length(active) == 0L || nrow(d) == 0L) return(d)
  ok <- rep(TRUE, nrow(d))
  for (name in active) ok <- ok & is.finite(d[[metrics[[name]]]])
  indices <- which(ok)
  if (length(indices) == 0L) return(d)
  score <- rep(0, length(indices))
  for (name in active) {
    vals <- d[[metrics[[name]]]][indices]
    percentile <- if (length(vals) > 1L) 100 * (rank(vals, ties.method = "average") - 1) /
      (length(vals) - 1) else 50
    if (name == "affordability") percentile <- 100 - percentile
    d[[paste0("component_", name)]][indices] <- percentile
    score <- score + weights[[name]] * percentile
  }
  d$score[indices] <- score / sum(weights[active])
  d
}

filter_snapshot <- function(d, price_range, min_yield = 0, min_sales = 0) {
  if (!nrow(d)) return(d)
  ok <- is.finite(d$median_sale_price) & d$median_sale_price >= price_range[1] &
    d$median_sale_price <= price_range[2]
  if (min_yield > 0) ok <- ok & is.finite(d$gross_yield) & d$gross_yield >= min_yield
  if (min_sales > 0) ok <- ok & is.finite(d$sales_count) & d$sales_count >= min_sales
  x <- d[ok, , drop = FALSE]
  x[order(is.na(x$score), -ifelse(is.na(x$score), -Inf, x$score), x$area), , drop = FALSE]
}

# Historical records are intentionally not attached to modern LGA names.
read_historical <- function(filename) {
  d <- read_dcj_detail(filename)
  if (!nrow(d)) return(d)
  d$period <- as.Date(d$period)
  d$value <- suppressWarnings(as.numeric(d$value))
  d
}

available_paired_periods <- function(d) {
  paired <- is.finite(d$median_sale_price) & is.finite(d$median_weekly_rent)
  sort(unique(d$period[paired]))
}

source_latest <- function(d, metric_column) {
  valid <- is.finite(d[[metric_column]])
  if (!any(valid)) return(as.Date(NA))
  max(d$period[valid])
}
