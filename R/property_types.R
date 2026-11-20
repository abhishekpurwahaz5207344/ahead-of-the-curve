# Property-type exploration uses existing NSW DCJ *published aggregated* tables only.
# Categories are proxies. Do not imply exact one-to-one title/property matching.
# Established/new status is not captured in the DCJ release.

property_dcj_labels <- function(selected) {
  switch(selected,
    "Established home" = list(sale = "Non Strata", rent = "House"),
    "Apartment/unit" = list(sale = "Strata", rent = "Flat/Unit"),
    "Townhouse" = list(sale = NA_character_, rent = "Townhouse"),
    list(sale = "Total", rent = "Total"))
}

# Exact quarter/area joins. There is no imputation for unmatched, suppressed or
# unpublished categories. When selected = Townhouse, no DCJ townhouse-specific
# *sale* category exists; sale price, growth, yield and research score are NA.
typed_snapshot <- function(all_quarters, chosen_period, years, selected,
                           sales_detail, rents_detail) {
  d <- market_snapshot(all_quarters, chosen_period, years)
  if (is.null(selected) || identical(selected, "All dwellings") || !nrow(d)) return(d)
  labels <- property_dcj_labels(selected)
  as_date <- as.Date(chosen_period)
  ds <- if (!is.na(labels$sale) && nrow(sales_detail)) sales_detail[
    sales_detail$period == as_date &
    sales_detail$dwelling_type == labels$sale,
    , drop = FALSE] else data.frame()
  dr <- if (nrow(rents_detail)) rents_detail[
    rents_detail$period == as_date &
    rents_detail$dwelling_type == labels$rent &
    rents_detail$bedrooms == "Total",
    , drop = FALSE] else data.frame()
  # For detailed input files the canonical key is area/type/quarter.
  if (nrow(ds) && anyDuplicated(ds$area)) stop("Duplicate sales type-quarter LGA")
  if (nrow(dr) && anyDuplicated(dr$area)) stop("Duplicate rent type-quarter LGA")
  sale_match <- if (nrow(ds)) match(d$area, ds$area) else rep(NA_integer_, nrow(d))
  rent_match <- if (nrow(dr)) match(d$area, dr$area) else rep(NA_integer_, nrow(d))
  d$median_sale_price <- if (nrow(ds)) ds$sale_median_aud[sale_match] else rep(NA_real_, nrow(d))
  d$sales_count <- if (nrow(ds)) ds$sales_count[sale_match] else rep(NA_real_, nrow(d))
  d$median_weekly_rent <- if (nrow(dr)) dr$rent_median_weekly_aud[rent_match] else rep(NA_real_, nrow(d))
  d$rent_count <- if (nrow(dr)) dr$new_bonds[rent_match] else rep(NA_real_, nrow(d))
  d$growth_pa <- rep(NA_real_, nrow(d))
  d$gross_yield <- rep(NA_real_, nrow(d))
  if (nrow(ds)) {
    old <- sales_detail[
      sales_detail$period == shift_years(as_date, years) &
      sales_detail$dwelling_type == labels$sale,
      , drop = FALSE]
    if (nrow(old) && anyDuplicated(old$area)) stop("Duplicate baseline sales type-quarter LGA")
    base <- if (nrow(old)) old$sale_median_aud[match(d$area, old$area)] else rep(NA_real_, nrow(d))
    valid <- is.finite(base) & base > 0 & is.finite(d$median_sale_price) & d$median_sale_price > 0
    d$growth_pa[valid] <- ((d$median_sale_price[valid] / base[valid])^(1 / years) - 1) * 100
  }
  valid <- is.finite(d$median_sale_price) & d$median_sale_price > 0 &
    is.finite(d$median_weekly_rent) & d$median_weekly_rent > 0
  d$gross_yield[valid] <- 100 * 52 * d$median_weekly_rent[valid] / d$median_sale_price[valid]
  d$source_note <- paste0(d$source_note, "; DCJ category-proxy sale:",
                          ifelse(is.na(labels$sale), "none", labels$sale),
                          " rent:", labels$rent, "; not matched properties")
  d
}

# Historical charts retain their observed time stamps and plot only reported
# specific-category medians. They never interpolate or splice incomplete series.
typed_history <- function(all_quarters, metric, area_name, selected,
                          sales_detail, rents_detail) {
  if (is.null(selected) || selected == "All dwellings") {
    return(all_quarters[all_quarters$area == area_name, , drop = FALSE])
  }
  labels <- property_dcj_labels(selected)
  if (identical(metric, "median_sale_price")) {
    d <- sales_detail
    if (!nrow(d) || is.na(labels$sale)) return(data.frame(
      period = as.Date(character()), median_sale_price = numeric()))
    d <- d[d$area == area_name & d$dwelling_type == labels$sale, , drop = FALSE]
    if (!nrow(d)) return(data.frame(period = as.Date(character()), median_sale_price = numeric()))
    return(data.frame(period = as.Date(d$period), median_sale_price = d$sale_median_aud))
  }
  d <- rents_detail
  if (!nrow(d)) return(data.frame(period = as.Date(character()), median_weekly_rent = numeric()))
  d <- d[d$area == area_name & d$dwelling_type == labels$rent &
         d$bedrooms == "Total", , drop = FALSE]
  if (!nrow(d)) return(data.frame(period = as.Date(character()), median_weekly_rent = numeric()))
  data.frame(period = as.Date(d$period), median_weekly_rent = d$rent_median_weekly_aud)
}
