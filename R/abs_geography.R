# Official ABS 2021 Census General Community Profile: Local Government Areas.
# Source: https://services1.arcgis.com/vHnIGBHHqDR6y0CR/ArcGIS/rest/services/
#   2021_ABS_General_Community_Profile/FeatureServer/3
# This source carries ABS ASGS Edition 3 (2021) geometries and Census attributes.
# Do not relabel Census 2021 observations as current population estimates.

ABS_LAYER <- paste0(
  "https://services1.arcgis.com/vHnIGBHHqDR6y0CR/ArcGIS/rest/services/",
  "2021_ABS_General_Community_Profile/FeatureServer/3"
)
ABS_CACHE <- "data/abs_lga_2021_nsw.geojson"
ABS_SOURCE_URL <- "https://www.abs.gov.au/census/find-census-data/datapacks"
ABS_GEOGRAPHY_URL <- paste0(
  "https://www.abs.gov.au/statistics/standards/",
  "australian-statistical-geography-standard-asgs/edition-3-july-2021-june-2026/",
  "access-and-downloads/digital-boundary-files"
)

abs_fields <- c(
  "LGA_CODE_2021", "LGA_NAME_2021", "Tot_P_P",
  "Age_0_4_yr_P", "Age_5_14_yr_P", "Age_15_19_yr_P",
  "Age_20_24_yr_P", "Age_25_34_yr_P", "Age_35_44_yr_P",
  "Age_45_54_yr_P", "Age_55_64_yr_P", "Age_65_74_yr_P",
  "Age_75_84_yr_P", "Age_85ov_P",
  "Median_age_persons", "Median_tot_hhd_inc_weekly",
  "Median_tot_prsnl_inc_weekly", "Average_household_size",
  "Total_DS_Sep_house", "Total_DS_SemiD_ro_or_tce_h_th",
  "Total_DS_Flat_apart", "Total_Total_G37",
  "O_OR_Total", "O_MTG_Total", "R_Tot_Total",
  "Birthplace_Elsewhere_P", "Percent_Unem_loyment_P"
)

abs_query_url <- function() {
  # Restrict to the NSW LGA code prefix. All other states are excluded.
  paste0(ABS_LAYER, "/query?where=",
    utils::URLencode("LGA_CODE_2021 LIKE '1%'", reserved = TRUE),
    "&outFields=", paste(abs_fields, collapse = ","),
    "&returnGeometry=true&outSR=4326&f=geojson",
    "&geometryPrecision=5&maxAllowableOffset=0.0001")
}

fetch_abs_geojson <- function(cache = ABS_CACHE, force = FALSE) {
  if (file.exists(cache) && !isTRUE(force)) return(cache)
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(pattern = "abs_lga_", fileext = ".geojson")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  tryCatch(
    utils::download.file(abs_query_url(), destfile = tmp,
                         mode = "wb", quiet = TRUE, method = "libcurl"),
    error = function(e) stop("ABS download failed. See README for manual import: ", conditionMessage(e))
  )
  if (!file.exists(tmp) || file.info(tmp)$size < 1000) stop("ABS server returned no geometry data")
  valid <- tryCatch(sf::st_read(tmp, quiet = TRUE), error = function(e) {
    stop("ABS response is not readable GeoJSON: ", conditionMessage(e))
  })
  if (!("LGA_CODE_2021" %in% names(valid))) stop("ABS response missing LGA code field")
  if (nrow(valid) < 100L || nrow(valid) > 150L) {
    stop("Unexpected number of NSW LGAs in ABS response: ", nrow(valid))
  }
  if (any(!grepl("^1", as.character(valid$LGA_CODE_2021)))) {
    stop("The ABS response included records outside NSW; stopping to avoid incorrect joins")
  }
  if (anyDuplicated(valid$LGA_CODE_2021)) stop("Duplicate ABS LGA codes in API response")
  if (!file.copy(tmp, cache, overwrite = TRUE)) stop("Could not save ABS GeoJSON cache")
  cache
}

safe_ratio_pct <- function(numer, denom) {
  n <- suppressWarnings(as.numeric(numer))
  d <- suppressWarnings(as.numeric(denom))
  ans <- rep(NA_real_, length(n))
  ok <- is.finite(n) & is.finite(d) & d > 0
  ans[ok] <- 100 * n[ok] / d[ok]
  ans
}

# Use a conservative normalised-name key only. No fuzzy nearest-neighbour joins.
# Retain unmatched LGAs rather than assigning their statistics to another council.
abs_lga_key <- function(names) {
  x <- as.character(names)
  x <- gsub("\\s*\\([^)]*\\)$", "", x)  # ABS council type suffix, e.g. (C)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  tolower(gsub("[^a-zA-Z0-9]", "", x))
}

read_abs_geography <- function(path = ABS_CACHE, allow_download = TRUE) {
  if (!file.exists(path)) {
    if (!allow_download) stop("ABS boundary cache missing: ", path)
    fetch_abs_geojson(cache = path)
  }
  g <- sf::st_read(path, quiet = TRUE)
  # Normalise field case across GDAL releases and GeoJSON field handling.
  names(g) <- tolower(names(g))
  required <- tolower(c("LGA_CODE_2021", "LGA_NAME_2021", "Tot_P_P"))
  if (!all(required %in% names(g))) {
    stop("ABS cache does not contain expected Census and geography fields")
  }
  g <- g[grepl("^1", as.character(g$lga_code_2021)), , drop = FALSE]
  if (nrow(g) < 100L || anyDuplicated(g$lga_code_2021)) stop("ABS LGA cache failed key validation")
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)
  g$abs_code <- as.character(g$lga_code_2021)
  g$abs_name <- as.character(g$lga_name_2021)
  g$lga_key <- abs_lga_key(g$abs_name)
  for (nm in setdiff(tolower(abs_fields), names(g))) g[[nm]] <- NA_real_
  n <- function(col) suppressWarnings(as.numeric(g[[tolower(col)]]))
  g$population_2021 <- n("Tot_P_P")
  g$median_age_2021 <- n("Median_age_persons")
  g$median_household_income_2021 <- n("Median_tot_hhd_inc_weekly")
  g$median_personal_income_2021 <- n("Median_tot_prsnl_inc_weekly")
  g$average_household_size_2021 <- n("Average_household_size")
  g$renter_share_2021 <- safe_ratio_pct(n("R_Tot_Total"), n("Total_Total_G37"))
  g$apartment_share_2021 <- safe_ratio_pct(n("Total_DS_Flat_apart"), n("Total_Total_G37"))
  g$young_adult_share_2021 <- safe_ratio_pct(n("Age_25_34_yr_P") + n("Age_35_44_yr_P"), n("Tot_P_P"))
  g$overseas_born_share_2021 <- safe_ratio_pct(n("Birthplace_Elsewhere_P"), n("Tot_P_P"))
  g$unemployment_rate_2021 <- n("Percent_Unem_loyment_P")
  if (anyDuplicated(g$lga_key)) stop("ABS geography names are ambiguous after normalisation")
  g
}

# Merge exactly one DCJ observation per area and quarter with each ABS LGA.
# All 2021 ABS LGAs remain visible even when the selected DCJ quarter has no data.
join_abs_dcj <- function(abs_geo, dcj_snapshot) {
  stopifnot(inherits(abs_geo, "sf"), "area" %in% names(dcj_snapshot))
  d <- as.data.frame(dcj_snapshot)
  d$key <- abs_lga_key(d$area)
  if (anyDuplicated(d$key)) stop("Ambiguous DCJ names in this quarter; refusing spatial join")
  ii <- match(abs_geo$lga_key, d$key)
  result <- abs_geo
  fields <- c("area", "period", "median_sale_price", "median_weekly_rent",
              "sales_count", "rent_count", "gross_yield", "growth_pa", "score")
  for (nm in fields) if (nm %in% names(d)) result[[nm]] <- d[[nm]][ii]
  result$dcj_matched <- !is.na(ii)
  result$dcj_name <- ifelse(is.na(ii), NA_character_, d$area[ii])
  result
}

abs_metric_labels <- c(
  "Median sale price · DCJ quarter (AUD)" = "median_sale_price",
  "Median weekly new-bond rent · DCJ (AUD)" = "median_weekly_rent",
  "Indicative gross rental yield · DCJ (%)" = "gross_yield",
  "Historical median-price growth · DCJ (% p.a.)" = "growth_pa",
  "Research score · DCJ (/100)" = "score",
  "Population · ABS 2021 Census" = "population_2021",
  "Median age · ABS 2021 (years)" = "median_age_2021",
  "Median weekly household income · ABS 2021 (AUD)" = "median_household_income_2021",
  "Share of occupied dwellings rented · ABS 2021 (%)" = "renter_share_2021",
  "Share of occupied dwellings apartments · ABS 2021 (%)" = "apartment_share_2021",
  "Residents aged 25–44 · ABS 2021 (%)" = "young_adult_share_2021",
  "Residents born overseas · ABS 2021 (%)" = "overseas_born_share_2021",
  "Unemployment rate · ABS 2021 (%)" = "unemployment_rate_2021"
)

abs_metric_fmt <- function(value, name) {
  if (length(value) == 0L || is.na(value) || !is.finite(value)) return("Unavailable")
  if (name %in% c("median_sale_price", "median_weekly_rent", "median_household_income_2021"))
    return(paste0("$", format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE)))
  if (name %in% c("gross_yield", "growth_pa", "renter_share_2021",
                  "apartment_share_2021", "young_adult_share_2021", "overseas_born_share_2021",
                  "unemployment_rate_2021")) return(sprintf("%.1f%%", value))
  if (name == "score") return(sprintf("%.0f / 100", value))
  if (name == "median_age_2021") return(sprintf("%.0f years", value))
  format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE)
}
