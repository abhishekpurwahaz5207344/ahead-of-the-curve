# Ahead of The Curve — NSW DCJ interactive property research
# DATA5002, UNSW | R Shiny | DCJ market data + ABS 2021 Census geography.
# To run: source('install_packages.R'); shiny::runApp('.')

library(shiny)
library(plotly)
library(DT)
library(leaflet)
library(sf)

source("R/logic.R", local = TRUE)
source("R/abs_geography.R", local = TRUE)
source("R/property_types.R", local = TRUE)

# The ABS 2021 Census layer is cached locally when it has been downloaded once.
# If missing, the app fetches official ABS API polygons and Census variables.
abs_problem <- NULL
abs_geo <- tryCatch(read_abs_geography(), error = function(e) {
  abs_problem <<- conditionMessage(e)
  warning("ABS layer unavailable: ", abs_problem)
  NULL
})

lga_data <- read_dcj_markets("data/quarterly_markets.csv")
postcode_data <- read_dcj_markets("data/postcode_markets.csv")
legacy_sales <- read_historical("data/historical_sales_1991_2017.csv.gz")
legacy_rents <- read_historical("data/historical_rents_1990_2017.csv.gz")
# Detailed *LGA* medians and quartiles; postcode detailed files intentionally not loaded.
sales_detail <- read_dcj_detail("data/sales_lga_detailed.csv.gz")
rents_detail <- read_dcj_detail("data/rents_lga_detailed.csv.gz")
if (nrow(sales_detail)) sales_detail$period <- as.Date(sales_detail$period)
if (nrow(rents_detail)) rents_detail$period <- as.Date(rents_detail$period)

paired <- available_paired_periods(lga_data)
if (!length(paired)) stop("No common DCJ sales/rental quarters available")
latest_common <- max(paired)
latest_sales <- source_latest(lga_data, "median_sale_price")
latest_rents <- source_latest(lga_data, "median_weekly_rent")
# Use actual published LGA median prices, not the hand-drawn budget example.
startup_market <- market_snapshot(lga_data, latest_common, 5L)
startup_prices <- startup_market$median_sale_price[
  is.finite(startup_market$median_sale_price) & startup_market$median_sale_price > 0]
if (!length(startup_prices)) stop("No published sale medians in the latest common quarter")
startup_budget <- range(startup_prices)
quarter_choices <- stats::setNames(as.character(rev(paired)),
  vapply(as.character(rev(paired)), function(x) qtr_label(as.Date(x)), character(1)))

col_blue <- "#3269D5"
col_teal <- "#15A79C"
col_amber <- "#E6A04B"
col_ink <- "#132742"
col_grid <- "#E9EFF7"

chart_base <- function(p, y_label = NULL, x_label = NULL, hovermode = "closest") {
  plotly::layout(p,
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Arial, sans-serif", color = "#50637D", size = 12),
    margin = list(l = 58, r = 20, t = 16, b = 57),
    hovermode = hovermode,
    xaxis = list(title = x_label, showgrid = TRUE, gridcolor = col_grid,
                 zeroline = FALSE, linecolor = "#CDD9E8", automargin = TRUE),
    yaxis = list(title = y_label, showgrid = TRUE, gridcolor = col_grid,
                 zeroline = FALSE, linecolor = "#CDD9E8", automargin = TRUE),
    legend = list(orientation = "h", x = 0, y = -0.23))
}

empty_chart <- function(message) {
  p <- plotly::plot_ly()
  p <- plotly::layout(p, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
     annotations = list(list(x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        text = message, showarrow = FALSE, font = list(color = "#6A7890", size = 13))))
  plotly::config(p, displaylogo = FALSE, responsive = TRUE)
}

pill <- function(label, cls = "") tags$span(class = paste("badge-pill", cls), label)
panel <- function(title, subtitle = NULL, ..., class = "") {
  div(class = paste("panel", class),
      div(class = "panel-top", h3(title), if (!is.null(subtitle)) p(subtitle)), ...)
}
metric_card <- function(title, value, desc, emphasis = FALSE) {
  div(class = paste("metric-card", if (emphasis) "metric-feature" else ""),
      div(class = "metric-title", title),
      div(class = "metric-number", value),
      div(class = "metric-desc", desc))
}

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Ahead of The Curve | NSW Property Research"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(class = "site-header",
    div(class = "brand-icon", icon("building")),
    div(class = "brand-name", "AHEAD OF", tags$span("THE CURVE")),
    div(class = "brand-divider"),
    div(class = "brand-subtitle", "A virtual buyer's agency for NSW"),
    div(class = "header-right", pill("NSW DCJ + ABS 2021 CENSUS", "pill-data"))
  ),
  div(class = "page-shell",
    tags$aside(class = "side-panel",
      div(class = "sidebar-head", "INVESTOR SETTINGS",
          p("Explore NSW using DCJ property trends and ABS demographics.")),
      div(class = "sidebar-section",
        div(class = "sidebar-label", "Market geography"),
        radioButtons("geography", label = NULL,
          choices = c("LGA" = "LGA", "Postcode" = "Postcode"), selected = "LGA", inline = TRUE),
        selectInput("quarter", "Analysis quarter", choices = quarter_choices,
                    selected = as.character(latest_common)),
        div(class = "minor-note", "The latest quarter with both rent and sales data is selected by default."),
        sliderInput("budget", "Median purchase price (AUD)",
                    min = startup_budget[1], max = startup_budget[2],
                    value = startup_budget, step = 1000, pre = "$", sep = ","),
        textOutput("budget_data_note"),
        selectInput("property_type", "Property type (DCJ proxy)",
          choices = c("All dwellings" = "All dwellings",
            "Established home · non-strata/house proxy" = "Established home",
            "Apartment/unit · strata/flat proxy" = "Apartment/unit",
            "Townhouse · rent only (no sales score)" = "Townhouse"),
          selected = "All dwellings"),
        p(class = "minor-note", "DCJ cannot distinguish established from new homes. Strata ≠ apartments and non-strata ≠ houses; type-specific yields are indicative comparisons of different properties. Townhouses have no separate DCJ sale-price series."),
        sliderInput("min_yield", "Minimum indicative gross yield", min = 0,
                    max = 10, value = 0, step = 0.25, post = "%"),
        numericInput("min_sales", "Minimum reported quarterly sales", value = 30,
                     min = 0, max = 10000, step = 10)
      ),
      div(class = "sidebar-section",
        div(class = "sidebar-label", "Research priorities"),
        selectInput("growth_years", "Historical growth window", choices = c(
          "3 years" = 3, "5 years" = 5, "7 years" = 7), selected = 5),
        sliderInput("weight_growth", "Capital growth", min = 0, max = 5, value = 5, step = 1),
        sliderInput("weight_yield", "Rental yield (indicative)", min = 0, max = 5, value = 5, step = 1),
        sliderInput("weight_price", "Affordability (optional)", min = 0, max = 5, value = 0, step = 1),
        p(class = "minor-note", "Set a weight to zero to switch that priority off. Scores are relative descriptions, not return forecasts.")
      ),
      div(class = "sidebar-foot", "Sources: DCJ quarterly reports · ABS 2021 Census",
          tags$br(), "Education and research only · Nominal AUD")
    ),
    tags$main(class = "work-area",
      div(class = "page-intro",
        div(class = "intro-eyebrow", "NSW PROPERTY MARKET INTELLIGENCE"),
        h1("Find your next market", tags$span(" on the map.")),
        p("Explore every NSW local government area with ABS Census demographics, official council boundaries and NSW DCJ property statistics."),
        div(class = "intro-tags", pill(paste("Common quarter:", qtr_label(latest_common))),
            pill(paste("Sales through:", qtr_label(latest_sales))),
            pill(paste("Rents through:", qtr_label(latest_rents))))
      ),
      tabsetPanel(id = "section", type = "pills",
        tabPanel("NSW investment map", value = "map",
          div(class = "map-heading",
            div(h2("NSW by local government area"),
                p("Select a council on the map or search by name. Zoom in for smaller Sydney LGAs; use the full-screen button for maximum space.")),
            pill("2021 ABS LGA boundaries · 2021 Census + DCJ quarterly metrics")),
          div(class = "map-toolbar",
            div(class = "map-metric-control",
              selectInput("map_metric", "Colour the map by", choices = abs_metric_labels,
                          selected = "score")),
            div(class = "map-lga-control",
              selectizeInput("map_lga", "Search or select an LGA", choices = NULL,
                options = list(placeholder = "Search NSW councils..."))),
            div(class = "map-actions",
              actionButton("map_zoom_nsw", "All NSW", icon = icon("globe")),
              actionButton("map_zoom_sydney", "Sydney metro", icon = icon("search-plus")),
              actionButton("map_zoom_newcastle", "Hunter", icon = icon("map-marker")),
              actionButton("map_zoom_selected", "Zoom to LGA", icon = icon("crosshairs"))
            )
          ),
          if (is.null(abs_geo))
            div(class = "map-alert", icon("exclamation-circle"),
              strong("ABS map data not cached."),
              " Run ", tags$code("Rscript scripts/fetch_abs.R"),
              " with internet access to download the authoritative ABS 2021 LGA polygons. ",
              if (!is.null(abs_problem)) tags$span(abs_problem)),
          div(class = "map-frame", leafletOutput("nsw_map", height = "760px")),
          div(class = "map-source-line",
            "For complete sale-and-rent markets, LGAs outside the budget or minimum activity/yield thresholds are greyed out. ",
            "Townhouse mode displays published new-bond rents only because no matching sales category exists. ",
            "Grey can also mean suppressed/unavailable data. Use search, zoom or full-screen for tiny Sydney LGAs."),
          div(class = "wireframe-selected-grid",
            panel("Selected LGA · market trends", "Separate price and rent axes prevent misleading visual comparison.",
              plotlyOutput("map_price_chart", height = "260px"),
              plotlyOutput("map_rent_chart", height = "220px")),
            panel("Key metrics · selected LGA", "The selected property-type proxy and data quarter are shown alongside the council profile.",
              uiOutput("wireframe_metrics"))),
          panel("Evaluation · top 10 matching LGAs",
            "Relative DCJ research scores (/100). Select a listed LGA to zoom and inspect it. Scores are not return forecasts.",
            uiOutput("wireframe_evaluation")),
          uiOutput("map_facts"),
          div(class = "content-grid two-even",
            panel("Demographic profile · 2021 Census",
              "Historical area characteristics. ABS 2021 is not an estimate of current-year population.",
              plotlyOutput("map_age_chart", height = "290px")),
            panel("Selected LGA · DCJ market context",
              "ABS Census characteristics are 2021 snapshots, while property data refer to the chosen DCJ quarter.",
              uiOutput("wireframe_context"))
          ),
          panel("Data matching and download",
            "ABS boundaries are joined to DCJ observations via carefully normalised LGA names. Ambiguous and unmatched records are never guessed.",
            uiOutput("map_coverage"),
            downloadButton("download_abs_join", "Download LGA join audit / map data"))
        ),
        tabPanel("Explore markets", value = "explore",
          uiOutput("summary_cards"),
          div(class = "content-grid",
            panel("Opportunity landscape",
              "Hover to inspect each area. Click a point to open its market details. Only markets with a score appear on this chart.",
              plotlyOutput("opportunity_plot", height = "360px"), class = "panel-wide"),
            panel("Research shortlist", "Top-scoring areas that meet your filters. Scores are relative to the statewide observed market.",
                  uiOutput("shortlist"), class = "panel-narrow")
          ),
          panel("Market explorer",
            "Search, sort and select any row to inspect its published median price, rent and long-term trends.",
            div(class = "table-buttons", downloadButton("download_filtered", "Export filtered market table")),
            DTOutput("market_table")),
          panel("Selected market", "Area-level statistics; descriptive evidence, not a property valuation.",
            selectInput("focus_area", "Market to inspect", choices = NULL),
            uiOutput("focus_cards"),
            plotlyOutput("focus_plot", height = "280px"))
        ),
        tabPanel("Compare markets", value = "compare",
          panel("Compare up to four markets", "Trends use the actual available dates for each series. Rental and sale prices have separate charts and units.",
            selectizeInput("compare_areas", "Choose LGAs or postcodes", choices = NULL,
                            multiple = TRUE, options = list(maxItems = 4, placeholder = "Choose up to four areas")),
            div(class = "chart-block", h4("Median sale price (AUD)"), plotlyOutput("compare_sales", height = "335px")),
            div(class = "chart-block", h4("Median weekly rent (AUD/week)"), plotlyOutput("compare_rents", height = "335px")),
            div(class = "subsection-head", "Comparison at the selected quarter"),
            DTOutput("compare_table"))
        ),
        tabPanel("Market trends", value = "trends",
          panel("Understand one market over time", "Explore original quarterly sale and rental statistics, including transaction/bond activity.",
            selectInput("trend_metric", "Choose a statistic", choices = c(
              "Median sale price (AUD)" = "median_sale_price",
              "Median weekly rent (AUD/week)" = "median_weekly_rent",
              "Reported sale transactions" = "sales_count",
              "New rental bonds" = "rent_count")),
            plotlyOutput("trend_plot", height = "390px"),
            p(class = "explain-note", "The shaded quartile interval (where available) is the published 25th–75th percentile band, not a prediction interval. Low-count/suppressed data are left missing.")),
          div(class = "content-grid two-even",
            panel("Price and rent at the selected quarter", NULL, uiOutput("trend_summary")),
            panel("How to interpret the series", NULL, tags$ul(class = "note-list",
              tags$li("Changes in medians can reflect a change in the mix of properties sold or rented."),
              tags$li("The source rent data describes newly lodged bonds, not the entire rental stock."),
              tags$li("Long-term nominal prices are not inflation-adjusted.")))
          ),
          div(class = "content-grid two-even",
            panel("Sale prices by title category", "Strata and non-strata are legal title types, not strict property shapes.",
                  plotlyOutput("sales_type_plot", height = "285px")),
            panel("Rents by dwelling category", "New-bond weekly medians for all bedroom counts within each dwelling type.",
                  plotlyOutput("rent_type_plot", height = "285px"))
          )
        ),
        tabPanel("Historical explorer", value = "historical",
          panel("A longer view: 1990 / 1991 to 2017",
             "Dedicated pre-2017 metropolitan series. Original historical LGA names and geographic boundaries are preserved and are NOT joined to modern rankings.",
             div(class = "history-controls",
               selectInput("legacy_metric", "Historical series", choices = c(
                   "Median weekly rent (1990–2017)" = "rent",
                   "Median sale price (1991–2017)" = "sale"), selected = "rent"),
               selectInput("legacy_area", "Historical area", choices = NULL),
               uiOutput("legacy_category_ui"),
               uiOutput("legacy_bedroom_ui")
             ),
             plotlyOutput("legacy_plot", height = "410px"),
             uiOutput("legacy_stat"),
             div(class = "history-warning", icon("exclamation-triangle"),
               "Historical LGA amalgamations and changes to rental processing mean these are not directly comparable with current council boundaries. The historical graph intentionally stops in 2017.")),
          panel("Keep, but don't merge", "Source selection for historical data",
            p("The legacy rent workbook contains 110 quarters from March 1990 to June 2017. The legacy sale workbook contains 105 quarters from March 1991 to March 2017. These are published observations from historical metropolitan areas, not reconstructed property transactions."),
            p("Pre-2017 PDF reports have been retained as an index and methodological reference, rather than bulk-merged into modern postcodes. See docs/HISTORICAL_DECISION.md."))
        ),
        tabPanel("Data & methods", value = "methods",
          panel("What the dashboard measures", "Read the methodology before interpreting a ranking as an investment opportunity.",
            div(class = "method-grid",
              div(h4("1. Indicative gross rental yield"),
                p(class = "formula", "52 × median weekly new-bond rent ÷ median sale price × 100"),
                p("This compares different groups of properties. It excludes vacancy, tax, management fees, insurance, financing and other ownership costs. It is NOT a property-level rental yield.")),
              div(h4("2. Historical price change (annualised)"),
                p(class = "formula", "[(Current quarterly median ÷ same-quarter median N years ago)^(1/N) − 1] × 100"),
                p("Requires both exact quarters. Missing prior data remain missing; the result is a change in reported median, not a constant-quality house-price index.")),
              div(h4("3. Weighted research score"),
                p(class = "formula", "Σ(weight × statewide percentile rank) ÷ Σ(weights)"),
                p("Capital growth, gross yield and lower price receive separate 0–100 percentile components. We calculate percentiles BEFORE applying budget filters and only score markets with every positively weighted input.")),
              div(h4("4. Suppression and comparability"),
                p(class = "formula", "Missing ≠ zero. Small samples require caution."),
                p("DCJ suppresses cells with 10 or fewer observations. Type-specific medians are only proxies: non-strata ≠ houses, strata ≠ flats/units, and established status cannot be identified. Townhouses lack a separate sale category. Small samples require caution."))
            ),
            tags$hr(),
            h4("Data provenance and publication coverage"),
            uiOutput("provenance"),
            tags$p(tags$a(href = "https://dcj.nsw.gov.au/about-us/families-and-communities-statistics/housing-rent-and-sales.html",
                          target = "_blank", rel = "noopener", "NSW DCJ official Rent and Sales reports")),
            h4("Valuer General transaction-data decision"),
            p("The available Valuer General extract contains just 81 transactions, heavily concentrated in a single Sydney district. It includes non-residential uses, incomplete postcodes, and extreme transactions. It is not representative NSW LGA coverage and is deliberately EXCLUDED from statewide prices, yields and scores. No additional external dataset was added beyond the existing verified ABS layer."),
            h4("ABS Census demographics and geographic boundaries"),
            p("The map uses ABS 2021 Census General Community Profile attributes and ABS ASGS 2021 LGA boundaries. These are 2021 snapshots, not 2026 demographic estimates. The DCJ sale/rental figures refer to the selected quarter; some councils may have unmatched names or suppressed market observations. Map colouring never extrapolates missing values."),
            tags$p(tags$a(href = ABS_SOURCE_URL, target = "_blank", rel = "noopener", "ABS 2021 Census DataPacks"),
              " · ", tags$a(href = ABS_GEOGRAPHY_URL, target = "_blank", rel = "noopener", "ABS ASGS LGA digital boundaries")),
            h4("Reproducibility and acknowledgements"),
            p("The repository contains runnable R source, cleaned DCJ CSV inputs, an ABS data acquisition script, data dictionary, a validation script, the historical-series decision, and a README. The data were converted from user-supplied DCJ archives with AI-assisted scripts; preserve the source archives and disclose AI chat logs in your submission."),
            downloadButton("download_all", "Download complete LGA quarterly series")
          )
        )
      ),
      div(class = "footer", "AHEAD OF THE CURVE", " · ", "UNSW DATA5002 2026 T3",
          tags$span("NSW DCJ property statistics + ABS 2021 Census · No predictive financial advice"))
    )
  )
)

server <- function(input, output, session) {
  # ----- ABS-powered map. The original DCJ exploration tabs remain intact. -----
  if (!is.null(abs_geo)) {
    council_choices <- stats::setNames(as.character(abs_geo$abs_code), abs_geo$abs_name)
    default_idx <- match("sydney", abs_geo$lga_key)
    default_code <- if (is.finite(default_idx)) abs_geo$abs_code[default_idx] else abs_geo$abs_code[1]
    observeEvent(TRUE, {
      updateSelectizeInput(session, "map_lga", choices = council_choices,
                           selected = default_code, server = TRUE)
    }, once = TRUE)
  }

  map_snapshot <- reactive({
    req(!is.null(abs_geo), input$quarter, input$growth_years,
        !is.null(input$weight_growth), !is.null(input$weight_yield),
        !is.null(input$weight_price))
    # Deliberately map LGAs, even when the independent explorer is in postcode mode.
    d <- typed_snapshot(lga_data, chosen_period(), as.integer(input$growth_years),
                        input$property_type, sales_detail, rents_detail)
    d <- score_snapshot(d, c(growth = input$weight_growth,
                            yield = input$weight_yield,
                            affordability = input$weight_price))
    join_abs_dcj(abs_geo, d)
  })

  # Filter the LGA choropleth using the same market criteria as the ranked results.
  # The score itself is computed on the whole observed universe before filtering.
  map_eligible <- reactive({
    d <- map_snapshot()
    req(input$budget, input$min_yield, input$min_sales)
    if (identical(input$property_type, "Townhouse")) {
      # No townhouse-only sale medians exist, so budget/growth/sale-count
      # thresholds cannot legitimately be applied to this rent-only layer.
      d$passes_filters <- is.finite(d$median_weekly_rent)
    } else {
      eligible <- filter_snapshot(sf::st_drop_geometry(d), input$budget,
                                  input$min_yield, input$min_sales)
      d$passes_filters <- d$abs_code %in% eligible$abs_code
    }
    d
  })
  observeEvent(input$property_type, {
    if (identical(input$property_type, "Townhouse"))
      updateSelectInput(session, "map_metric", selected = "median_weekly_rent")
    else if (identical(isolate(input$map_metric), "median_weekly_rent"))
      updateSelectInput(session, "map_metric", selected = "score")
  }, ignoreInit = TRUE)

  output$nsw_map <- leaflet::renderLeaflet({
    map <- leaflet::leaflet(options = leaflet::leafletOptions(
      preferCanvas = TRUE, minZoom = 4, maxZoom = 18, zoomControl = TRUE)) |>
      leaflet::addProviderTiles("CartoDB.Positron", group = "Streets") |>
      leaflet::addScaleBar(position = "bottomleft") |>
      leaflet::fitBounds(140.9, -37.7, 154.1, -28.0)
    # Full-screen mode avoids tiny hit targets in dense metropolitan areas.
    leaflet.extras::addFullscreenControl(map, position = "topleft", pseudoFullscreen = TRUE)
  })

  observeEvent(list(map_eligible(), input$map_metric), {
    d <- map_eligible()
    metric <- input$map_metric
    req(metric %in% names(d))
    values <- suppressWarnings(as.numeric(d[[metric]]))
    display_values <- values
    display_values[!d$passes_filters] <- NA_real_
    finite <- display_values[is.finite(display_values)]
    pal <- leaflet::colorNumeric(
      palette = "YlGnBu", domain = if (length(finite)) finite else c(0, 1),
      na.color = "#D9DEE7")
    popup_text <- vapply(seq_len(nrow(d)), function(i) {
      name <- htmltools::htmlEscape(d$abs_name[i])
      measure <- abs_metric_fmt(values[i], metric)
      src <- if (grepl("_2021$", metric)) "ABS Census 2021" else qtr_label(chosen_period())
      dcj_note <- if (!isTRUE(d$dcj_matched[i])) "No matching DCJ LGA for this quarter" else
        paste0("DCJ name: ", htmltools::htmlEscape(d$dcj_name[i]))
      if (!isTRUE(d$passes_filters[i])) dcj_note <- paste(dcj_note, "· Outside filters or no eligible data")
      sprintf("<strong>%s</strong><br>%s: <strong>%s</strong><br><small>%s · %s</small>",
        name, htmltools::htmlEscape(names(abs_metric_labels)[match(metric, abs_metric_labels)]),
        htmltools::htmlEscape(measure), src, dcj_note)
    }, character(1))
    proxy <- leaflet::leafletProxy("nsw_map", session = session) |>
      leaflet::clearGroup("LGA polygons") |>
      leaflet::clearGroup("Selected LGA") |>
      leaflet::removeControl("lga_value_legend") |>
      leaflet::addPolygons(data = d, group = "LGA polygons",
        layerId = as.character(d$abs_code),
        color = "#FFFFFF", weight = 0.8, opacity = 1,
        fillColor = pal(display_values), fillOpacity = 0.86,
        popup = lapply(popup_text, htmltools::HTML),
        label = lapply(paste(d$abs_name, "—", vapply(values, abs_metric_fmt,
                    character(1), name = metric)), htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(
          color = "#172C49", weight = 3, bringToFront = TRUE, fillOpacity = 0.94))
    if (length(finite)) {
      proxy <- proxy |>
        leaflet::addLegend(position = "bottomright", pal = pal, values = finite,
          title = htmltools::htmlEscape(names(abs_metric_labels)[match(metric, abs_metric_labels)]),
          opacity = 0.95, layerId = "lga_value_legend")
    }
    if (!is.null(input$map_lga) && input$map_lga %in% d$abs_code) {
      chosen <- d[d$abs_code == input$map_lga, , drop = FALSE]
      proxy <- proxy |>
        leaflet::addPolygons(data = chosen, group = "Selected LGA",
          color = "#F7AA43", weight = 4, fill = FALSE, opacity = 1,
          options = leaflet::pathOptions(interactive = FALSE))
    }
  }, ignoreInit = FALSE)

  # Shared selection supports map clicks, typed searches, and quick zoom controls.
  observeEvent(input$nsw_map_shape_click, {
    if (is.null(abs_geo)) return()
    clicked <- input$nsw_map_shape_click$id
    if (length(clicked) && clicked %in% abs_geo$abs_code)
      updateSelectizeInput(session, "map_lga", selected = clicked)
  })

  map_focus_row <- reactive({
    req(!is.null(abs_geo), input$map_lga)
    row <- map_snapshot()
    row[row$abs_code == input$map_lga, , drop = FALSE]
  })

  zoom_map_bbox <- function(bounds, min_degrees = 0.025) {
    b <- as.numeric(bounds)
    # st_bbox is xmin,ymin,xmax,ymax. Padding and a minimum extent keep
    # postage-stamp Sydney LGAs comfortably selectable when zoomed into.
    xx <- (b[1] + b[3]) / 2
    yy <- (b[2] + b[4]) / 2
    dx <- max(min_degrees, abs(b[3] - b[1]) * 1.3)
    dy <- max(min_degrees, abs(b[4] - b[2]) * 1.3)
    leaflet::fitBounds(leaflet::leafletProxy("nsw_map", session),
      xx - dx / 2, yy - dy / 2, xx + dx / 2, yy + dy / 2)
  }
  observeEvent(input$map_lga, {
    d <- map_focus_row()
    if (!nrow(d)) return()
    # Outline selected council while leaving the whole choropleth intact.
    leaflet::leafletProxy("nsw_map", session = session) |>
      leaflet::clearGroup("Selected LGA") |>
      leaflet::addPolygons(data = d, group = "Selected LGA", color = "#F7AA43",
                           weight = 4, fill = FALSE, opacity = 1,
                           options = leaflet::pathOptions(interactive = FALSE))
    zoom_map_bbox(sf::st_bbox(d))
  }, ignoreInit = TRUE)
  observeEvent(input$map_zoom_selected, {
    d <- map_focus_row()
    if (nrow(d)) zoom_map_bbox(sf::st_bbox(d))
  })
  observeEvent(input$map_zoom_nsw, {
    leaflet::fitBounds(leaflet::leafletProxy("nsw_map", session), 140.9, -37.7, 154.1, -28.0)
  })
  observeEvent(input$map_zoom_sydney, {
    leaflet::fitBounds(leaflet::leafletProxy("nsw_map", session), 150.7, -34.15, 151.42, -33.47)
  })
  observeEvent(input$map_zoom_newcastle, {
    leaflet::fitBounds(leaflet::leafletProxy("nsw_map", session), 151.0, -33.35, 152.15, -32.55)
  })

  output$map_facts <- renderUI({
    if (is.null(abs_geo)) return(div(class = "map-alert", "Map data unavailable: see the setup instructions."))
    x <- map_focus_row(); req(nrow(x))
    div(class = "map-facts",
      div(class = "map-selected-title", tags$span("SELECTED LGA"),
          h3(x$abs_name[1]),
          p("2021 ABS council geography · ", if (isTRUE(x$dcj_matched[1]))
              paste("Joined to", x$dcj_name[1], "in DCJ") else "No exact DCJ match")),
      div(class = "map-fact-grid",
        metric_card("2021 Census population", abs_metric_fmt(x$population_2021[1], "population_2021"), "Usual residents"),
        metric_card("Median age · 2021", abs_metric_fmt(x$median_age_2021[1], "median_age_2021"), "Age in years"),
        metric_card("Household income · 2021", abs_metric_fmt(x$median_household_income_2021[1], "median_household_income_2021"), "Median AUD per week"),
        metric_card("Rented dwellings · 2021", abs_metric_fmt(x$renter_share_2021[1], "renter_share_2021"), "Share of occupied private dwellings"),
        metric_card("DCJ median sale", abs_metric_fmt(x$median_sale_price[1], "median_sale_price"), qtr_label(chosen_period())),
        metric_card("DCJ indicative gross yield", abs_metric_fmt(x$gross_yield[1], "gross_yield"), "Area-level proxy · not a forecast")
      ))
  })

  output$wireframe_metrics <- renderUI({
    if (is.null(abs_geo)) return(p("Map download required before viewing a council."))
    x <- map_focus_row(); req(nrow(x))
    label <- if (identical(input$property_type, "All dwellings")) "All dwellings" else
      paste0(input$property_type, " · indicative DCJ proxy")
    div(class = "wireframe-metric-list",
      metric_card("Median sale price", money(x$median_sale_price[1], TRUE), paste(qtr_label(chosen_period()), label)),
      metric_card("Median weekly rent", money(x$median_weekly_rent[1]), "New bond rent · AUD/week"),
      metric_card("Indicative gross rental yield", pct(x$gross_yield[1]), "Not a property-specific rental yield"),
      metric_card(paste0(input$growth_years, "-year median-price CAGR"), pct(x$growth_pa[1]), "Observed change in published medians"),
      metric_card("Relative research score", if (is.finite(x$score[1])) sprintf("%.0f / 100", x$score[1]) else "N/A",
                  "Weighted growth, yield and affordability"))
  })

  output$wireframe_context <- renderUI({
    x <- map_focus_row(); req(nrow(x))
    tagList(
      p(strong("Council: "), x$abs_name[1], " | ", strong("ABS 2021 population: "),
        abs_metric_fmt(x$population_2021[1], "population_2021"),
        " | ", strong("Renter share: "), abs_metric_fmt(x$renter_share_2021[1], "renter_share_2021")),
      p(class = "explain-note", "ABS demographics are historical context only; they are not included in the investment score. Property-type categories are proxies, not linked sales and rental observations.")
    )
  })

  map_rankings <- reactive({
    d <- sf::st_drop_geometry(map_eligible())
    d <- d[d$passes_filters & is.finite(d$score), , drop = FALSE]
    d <- d[order(-d$score, d$abs_name), , drop = FALSE]
    utils::head(d, 10)
  })

  output$wireframe_evaluation <- renderUI({
    d <- map_rankings()
    if (!nrow(d)) return(div(class = "empty-state",
      "No LGAs can be ranked under these filters and weightings. Try a wider budget, lower thresholds or select all dwellings."))
    div(class = "wireframe-rank-list", lapply(seq_len(nrow(d)), function(i) {
      div(class = "wireframe-rank-row",
        span(class = "wireframe-rank-n", i),
        div(class = "wireframe-rank-name",
          shiny::actionLink(paste0("rank_pick_", i), d$abs_name[i]),
          small(paste("Price:", money(d$median_sale_price[i], TRUE), " · Yield:",
                      pct(d$gross_yield[i]), " · Growth:", pct(d$growth_pa[i])))),
        span(class = "wireframe-rank-score", sprintf("%.0f", d$score[i]),
             small(" / 100")))
    }))
  })
  for (rank_idx in seq_len(7)) local({
    idx <- rank_idx
    observeEvent(input[[paste0("rank_pick_", idx)]], {
      d <- map_rankings()
      if (nrow(d) >= idx) updateSelectizeInput(session, "map_lga", selected = d$abs_code[idx])
    })
  })

  output$map_rent_chart <- plotly::renderPlotly({
    x <- map_focus_row(); req(nrow(x))
    if (!isTRUE(x$dcj_matched[1])) return(empty_chart("No matched DCJ rent series for this LGA."))
    d <- typed_history(lga_data, "median_weekly_rent", x$dcj_name[1],
                       input$property_type, sales_detail, rents_detail)
    d <- d[is.finite(d$median_weekly_rent), , drop = FALSE]
    if (!nrow(d)) return(empty_chart("No published rent data for this category."))
    d <- d[order(d$period), , drop = FALSE]
    p <- plotly::plot_ly(d, x = ~period, y = ~median_weekly_rent,
      type = "scatter", mode = "lines", line = list(color = col_teal, width = 3),
      text = paste0(qtr_label_vec(d$period), " · ",
        vapply(d$median_weekly_rent, money, character(1)), " / week"), hoverinfo = "text")
    p <- chart_base(p, y_label = "Median weekly new-bond rent (AUD)")
    p <- plotly::layout(p, yaxis = list(tickprefix = "$", tickformat = "~s"),
      showlegend = FALSE)
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  })

  output$map_age_chart <- plotly::renderPlotly({
    x <- map_focus_row(); req(nrow(x))
    fields <- c("age_0_4_yr_p", "age_5_14_yr_p", "age_15_19_yr_p",
                "age_20_24_yr_p", "age_25_34_yr_p", "age_35_44_yr_p",
                "age_45_54_yr_p", "age_55_64_yr_p", "age_65_74_yr_p",
                "age_75_84_yr_p", "age_85ov_p")
    ages <- c("0–4", "5–14", "15–19", "20–24", "25–34", "35–44",
              "45–54", "55–64", "65–74", "75–84", "85+")
    vals <- as.numeric(unlist(sf::st_drop_geometry(x[1, fields, drop = FALSE]), use.names = FALSE))
    if (!any(is.finite(vals))) return(empty_chart("No ABS age profile is available for this LGA."))
    p <- plotly::plot_ly(x = ages, y = vals, type = "bar",
                         marker = list(color = col_blue), text = vals, hovertemplate = "%{x}: %{y:,} people<extra></extra>")
    p <- chart_base(p, y_label = "People (ABS 2021 Census)")
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  })

  output$map_price_chart <- plotly::renderPlotly({
    x <- map_focus_row(); req(nrow(x))
    if (!isTRUE(x$dcj_matched[1])) return(empty_chart("This ABS LGA is unmatched to DCJ quarterly data."))
    history <- typed_history(lga_data, "median_sale_price", x$dcj_name[1],
                             input$property_type, sales_detail, rents_detail)
    history <- history[is.finite(history$median_sale_price), , drop = FALSE]
    if (!nrow(history)) return(empty_chart("No published DCJ sale series."))
    history <- history[order(history$period), , drop = FALSE]
    p <- plotly::plot_ly(history, x = ~period, y = ~median_sale_price,
                         type = "scatter", mode = "lines+markers",
                         line = list(color = col_teal, width = 3), marker = list(size = 4),
                         text = paste(qtr_label_vec(history$period),
                           vapply(history$median_sale_price, money, character(1)), sep = " · "),
                         hoverinfo = "text")
    p <- chart_base(p, y_label = "Median sale price (AUD)")
    p <- plotly::layout(p, yaxis = list(tickprefix = "$", tickformat = "~s"))
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  })

  output$map_coverage <- renderUI({
    if (is.null(abs_geo)) return(p("ABS API download is required before join coverage can be assessed."))
    d <- map_snapshot()
    matched <- sum(d$dcj_matched)
    un <- d$abs_name[!d$dcj_matched]
    div(class = "map-coverage-note",
      paste(nrow(d), "2021 ABS NSW LGA polygons ·", matched, "linked to the selected DCJ quarter ·",
            length(un), "unmatched or missing."),
      if (length(un)) p("Unmatched ABS LGAs: ", paste(un, collapse = ", ")))
  })
  output$download_abs_join <- downloadHandler(
    filename = function() paste0("ABS_DCJ_LGA_2021_join_", input$quarter, ".csv"),
    content = function(file) {
      d <- sf::st_drop_geometry(map_snapshot())
      cols <- c("abs_code", "abs_name", "dcj_name", "dcj_matched", "population_2021",
                "median_age_2021", "median_household_income_2021", "renter_share_2021",
                "apartment_share_2021", "young_adult_share_2021", "median_sale_price",
                "median_weekly_rent", "gross_yield", "growth_pa", "sales_count", "score")
      utils::write.csv(d[, intersect(cols, names(d)), drop = FALSE], file,
                       row.names = FALSE, na = "")
    })

  observeEvent(input$property_type, {
    if (!identical(input$property_type, "All dwellings") &&
        identical(input$geography, "Postcode"))
      updateRadioButtons(session, "geography", selected = "LGA")
  })
  observeEvent(input$geography, {
    if (identical(input$geography, "Postcode") &&
        !identical(input$property_type, "All dwellings"))
      updateSelectInput(session, "property_type", selected = "All dwellings")
  })
  markets <- reactive(if (identical(input$geography, "Postcode")) postcode_data else lga_data)
  chosen_period <- reactive(as.Date(input$quarter))

  # Refresh exact slider endpoints whenever the chosen quarter, geography or
  # property category changes. These are market medians, NOT individual sales.
  observed_budget_data <- reactive({
    req(input$quarter, input$geography, input$property_type)
    if (identical(input$property_type, "Townhouse")) return(data.frame())
    if (identical(input$geography, "Postcode")) {
      return(market_snapshot(postcode_data, chosen_period(), 5L))
    }
    typed_snapshot(lga_data, chosen_period(), 5L, input$property_type,
                   sales_detail, rents_detail)
  })
  observeEvent(observed_budget_data(), {
    d <- observed_budget_data()
    if (!nrow(d)) return()
    v <- d$median_sale_price[is.finite(d$median_sale_price) & d$median_sale_price > 0]
    if (!length(v)) return()
    bounds <- range(v)
    updateSliderInput(session, "budget", min = bounds[1], max = bounds[2],
                      value = bounds, step = 1000)
  }, ignoreInit = FALSE)
  output$budget_data_note <- renderText({
    if (identical(input$property_type, "Townhouse"))
      return("Townhouse sale medians are unavailable; the budget filter does not apply to this rent-only category.")
    d <- observed_budget_data()
    if (!nrow(d)) return("No published sale medians for this quarter/category.")
    idx <- which(is.finite(d$median_sale_price) & d$median_sale_price > 0)
    if (!length(idx)) return("No published sale medians for this quarter/category.")
    low <- idx[which.min(d$median_sale_price[idx])]
    high <- idx[which.max(d$median_sale_price[idx])]
    paste0("DCJ ", qtr_label(chosen_period()), " · ", length(idx), " areas. ",
           "Lowest: ", d$area[low], " (", money(d$median_sale_price[low]), "); ",
           "highest: ", d$area[high], " (", money(d$median_sale_price[high]), ").")
  })
  all_areas <- reactive(sort(unique(markets()$area)))

  # Keep available quarter choices in sync with geographic resolution.
  observeEvent(input$geography, {
    periods <- available_paired_periods(markets())
    choices <- stats::setNames(as.character(rev(periods)),
      vapply(as.character(rev(periods)), function(x) qtr_label(as.Date(x)), character(1)))
    old <- isolate(input$quarter)
    value <- if (!is.null(old) && old %in% choices) old else as.character(max(periods))
    updateSelectInput(session, "quarter", choices = choices, selected = value)
    areas <- sort(unique(markets()$area))
    defaults <- if (identical(input$geography, "LGA"))
      intersect(c("Newcastle", "Lake Macquarie", "Central Coast"), areas) else head(areas, 3)
    updateSelectizeInput(session, "compare_areas", choices = areas, selected = defaults, server = TRUE)
  }, ignoreInit = FALSE)

  snapshot <- reactive({
    req(input$quarter, input$growth_years,
        !is.null(input$weight_growth), !is.null(input$weight_yield),
        !is.null(input$weight_price))
    d <- typed_snapshot(markets(), chosen_period(), as.integer(input$growth_years),
                        if (identical(input$geography, "Postcode")) "All dwellings" else input$property_type,
                        sales_detail, rents_detail)
    score_snapshot(d, c(growth = input$weight_growth,
                        yield = input$weight_yield,
                        affordability = input$weight_price))
  })

  candidates <- reactive({
    req(input$budget, input$min_yield, input$min_sales)
    filter_snapshot(snapshot(), input$budget, input$min_yield, input$min_sales)
  })

  observe({
    d <- candidates()
    choices <- d$area
    selected <- isolate(input$focus_area)
    if (!length(choices)) selected <- character(0)
    else if (is.null(selected) || !selected %in% choices) selected <- choices[1]
    updateSelectInput(session, "focus_area", choices = choices, selected = selected)
  })

  focus <- reactive({
    req(input$focus_area)
    x <- snapshot()
    x[x$area == input$focus_area, , drop = FALSE]
  })

  output$summary_cards <- renderUI({
    d <- candidates()
    paired_rows <- sum(is.finite(d$median_sale_price) & is.finite(d$median_weekly_rent))
    median_price <- if (any(is.finite(d$median_sale_price)))
      stats::median(d$median_sale_price[is.finite(d$median_sale_price)]) else NA_real_
    median_yield <- if (any(is.finite(d$gross_yield)))
      stats::median(d$gross_yield[is.finite(d$gross_yield)]) else NA_real_
    div(class = "metric-grid",
      metric_card("Markets matching filters", format(nrow(d), big.mark = ","),
        paste("Published", if (identical(input$geography, "LGA")) "LGAs" else "postcodes"), TRUE),
      metric_card("Typical median sale price", money(median_price, TRUE), "Median across the matching areas"),
      metric_card("Median indicative gross yield", pct(median_yield), "Median of available area-level proxies"),
      metric_card("Markets with both measures", as.character(paired_rows),
        paste("At", qtr_label(chosen_period())))
    )
  })

  output$opportunity_plot <- renderPlotly({
    d <- candidates()
    d <- d[is.finite(d$score) & is.finite(d$gross_yield), , drop = FALSE]
    if (!nrow(d)) return(empty_chart("No complete, scored markets match your filters. Try reducing filters or changing weights."))
    hover <- sprintf("<b>%s</b><br>Median sale: %s<br>Indicative yield: %s<br>%s-year price change: %s<br>Research score: %.1f/100",
      d$area, vapply(d$median_sale_price, money, character(1), short = TRUE),
      vapply(d$gross_yield, pct, character(1)), input$growth_years,
      vapply(d$growth_pa, pct, character(1)), d$score)
    sizes <- ifelse(is.finite(d$sales_count), pmin(24, 9 + sqrt(pmax(0, d$sales_count)) / 2), 10)
    p <- plotly::plot_ly(d, x = ~median_sale_price, y = ~gross_yield, type = "scatter",
      mode = "markers", text = hover, hoverinfo = "text", customdata = ~area,
      source = "opportunity", marker = list(size = sizes, opacity = 0.81,
        color = d$score, colorscale = list(c(0, "#DCE8FC"), c(0.5, "#5795DF"), c(1, "#0A4F99")),
        cmin = 0, cmax = 100, showscale = TRUE,
        colorbar = list(title = "Score", thickness = 12, len = 0.7),
        line = list(color = "#FFFFFF", width = 1)))
    p <- chart_base(p, y_label = "Indicative gross yield (%)", x_label = "Median sale price (AUD)")
    p <- plotly::layout(p, xaxis = list(tickprefix = "$", tickformat = "~s", gridcolor = col_grid),
      yaxis = list(ticksuffix = "%", gridcolor = col_grid))
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  })

  observeEvent(plotly::event_data("plotly_click", source = "opportunity", priority = "event"), {
    ev <- plotly::event_data("plotly_click", source = "opportunity")
    if (!is.null(ev) && "customdata" %in% names(ev) && length(ev$customdata)) {
      value <- as.character(ev$customdata[[1]])
      if (value %in% candidates()$area) updateSelectInput(session, "focus_area", selected = value)
    }
  }, ignoreInit = TRUE)

  output$shortlist <- renderUI({
    d <- candidates()
    d <- d[is.finite(d$score), , drop = FALSE]
    if (!nrow(d)) return(div(class = "empty-state", "No markets have all the active scoring inputs. Reduce the growth horizon or weights."))
    d <- head(d[order(-d$score), , drop = FALSE], 6)
    div(class = "shortlist-items", lapply(seq_len(nrow(d)), function(i) {
      div(class = "shortlist-item", div(class = "shortlist-number", sprintf("%02d", i)),
          div(class = "shortlist-info", div(class = "shortlist-name", d$area[i]),
              div(class = "shortlist-stats", money(d$median_sale_price[i], TRUE), " median · ",
                  pct(d$gross_yield[i]), " yield")),
          div(class = "shortlist-score", sprintf("%.0f", d$score[i]), tags$small("/100")))
    }))
  })

  market_table_display <- reactive({
    d <- candidates()
    data.frame(
      "Market" = d$area,
      "Median sale (AUD)" = d$median_sale_price,
      "Median rent / week" = d$median_weekly_rent,
      "Gross yield (%)" = round(d$gross_yield, 2),
      "Growth p.a. (%)" = round(d$growth_pa, 2),
      "Sales" = d$sales_count,
      "New rental bonds" = d$rent_count,
      "Research score" = round(d$score, 1),
      check.names = FALSE
    )
  })

  output$market_table <- DT::renderDT({
    d <- market_table_display()
    table <- DT::datatable(d, filter = "top", rownames = FALSE, selection = "single",
        options = list(pageLength = 10, scrollX = TRUE,
          dom = "ftip", autoWidth = TRUE,
          language = list(emptyTable = "No markets match the selected filters")))
    DT::formatCurrency(table, c("Median sale (AUD)", "Median rent / week"), currency = "$", digits = 0)
  }, server = TRUE)

  observeEvent(input$market_table_rows_selected, {
    idx <- input$market_table_rows_selected
    d <- market_table_display()
    if (length(idx) == 1L && idx >= 1L && idx <= nrow(d))
      updateSelectInput(session, "focus_area", selected = d$Market[idx])
  })

  output$focus_cards <- renderUI({
    d <- focus(); req(nrow(d) > 0)
    div(class = "focus-metrics",
        div(tags$small("Median sale price"), tags$b(money(d$median_sale_price[1]))),
        div(tags$small("Median weekly new-bond rent"), tags$b(money(d$median_weekly_rent[1]))),
        div(tags$small("Indicative gross yield"), tags$b(pct(d$gross_yield[1]))),
        div(tags$small(paste0(input$growth_years, "-year price change p.a.")), tags$b(pct(d$growth_pa[1]))),
        div(tags$small("Reported sales"), tags$b(if (is.na(d$sales_count[1])) "Not available" else format(d$sales_count[1], big.mark = ",")))
    )
  })

  output$focus_plot <- renderPlotly({
    req(input$focus_area)
    d <- markets()
    d <- d[d$area == input$focus_area & is.finite(d$median_sale_price), , drop = FALSE]
    if (!nrow(d)) return(empty_chart("No published quarterly sale prices for this market."))
    p <- plotly::plot_ly(d, x = ~period, y = ~median_sale_price, type = "scatter",
      mode = "lines+markers", line = list(color = col_blue, width = 2.6),
      marker = list(size = 5), name = "Median sale price",
      text = paste(qtr_label_vec(d$period), vapply(d$median_sale_price, money, character(1)), sep = " · "),
      hoverinfo = "text")
    p <- chart_base(p, y_label = "Median sale price (AUD)", x_label = NULL)
    p <- plotly::layout(p, xaxis = list(type = "date", gridcolor = col_grid),
      yaxis = list(tickprefix = "$", tickformat = "~s", gridcolor = col_grid))
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  })

  compare_series_plot <- function(column, y_title, color_palette) {
    req(input$compare_areas)
    d <- markets()
    chosen <- intersect(input$compare_areas, unique(d$area))
    if (!length(chosen)) return(empty_chart("Choose one or more areas to compare."))
    p <- plotly::plot_ly()
    anything <- FALSE
    for (i in seq_along(chosen)) {
      rows <- d[d$area == chosen[i] & is.finite(d[[column]]), , drop = FALSE]
      rows <- rows[order(rows$period), , drop = FALSE]
      if (!nrow(rows)) next
      anything <- TRUE
      values <- rows[[column]]
      tooltip <- paste0(qtr_label_vec(rows$period), " · ", chosen[i], "<br>",
                        vapply(values, money, character(1)))
      p <- plotly::add_trace(p, x = rows$period, y = values, type = "scatter", mode = "lines+markers",
        name = chosen[i], text = tooltip, hoverinfo = "text",
        line = list(color = color_palette[(i - 1L) %% length(color_palette) + 1L], width = 2.4),
        marker = list(size = 4))
    }
    if (!anything) return(empty_chart("No published observations in these areas."))
    p <- chart_base(p, y_label = y_title, x_label = NULL, hovermode = "x unified")
    p <- plotly::layout(p, xaxis = list(type = "date", gridcolor = col_grid),
      yaxis = list(tickprefix = "$", tickformat = "~s", gridcolor = col_grid))
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  }

  output$compare_sales <- renderPlotly(compare_series_plot("median_sale_price", "Median sale price (AUD)",
    c(col_blue, col_teal, "#8063C9", col_amber)))
  output$compare_rents <- renderPlotly(compare_series_plot("median_weekly_rent", "Median weekly rent (AUD)",
    c(col_teal, col_blue, "#8063C9", col_amber)))

  output$compare_table <- DT::renderDT({
    req(input$compare_areas)
    d <- snapshot()
    d <- d[d$area %in% input$compare_areas, , drop = FALSE]
    tab <- data.frame(Market = d$area, "Median price" = d$median_sale_price,
      "Weekly rent" = d$median_weekly_rent, "Gross yield %" = round(d$gross_yield, 2),
      "Price CAGR %" = round(d$growth_pa, 2), check.names = FALSE)
    DT::formatCurrency(DT::datatable(tab, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)),
      c("Median price", "Weekly rent"), currency = "$", digits = 0)
  })

  output$trend_plot <- renderPlotly({
    req(input$focus_area, input$trend_metric)
    source <- markets()
    rows <- source[source$area == input$focus_area &
      is.finite(source[[input$trend_metric]]), , drop = FALSE]
    rows <- rows[order(rows$period), , drop = FALSE]
    if (!nrow(rows)) return(empty_chart("No reported data available for this combination."))
    metric <- input$trend_metric
    values <- rows[[metric]]
    is_money <- metric %in% c("median_sale_price", "median_weekly_rent")
    p <- plotly::plot_ly()
    # Published quartile bands are available for the LGA category totals.
    if (identical(input$geography, "LGA") && is_money) {
      detail <- if (identical(metric, "median_sale_price")) sales_detail else rents_detail
      q1 <- if (identical(metric, "median_sale_price")) "sale_q1_aud" else "rent_q1_weekly_aud"
      q3 <- if (identical(metric, "median_sale_price")) "sale_q3_aud" else "rent_q3_weekly_aud"
      if (nrow(detail)) {
        if (identical(metric, "median_sale_price")) {
          detail <- detail[detail$area == input$focus_area & detail$dwelling_type == "Total", , drop = FALSE]
        } else {
          detail <- detail[detail$area == input$focus_area & detail$dwelling_type == "Total" &
            detail$bedrooms == "Total", , drop = FALSE]
        }
        detail <- detail[order(detail$period), , drop = FALSE]
        detail <- detail[is.finite(detail[[q1]]) & is.finite(detail[[q3]]), , drop = FALSE]
        if (nrow(detail)) {
          p <- plotly::add_trace(p, x = detail$period, y = detail[[q3]],
            type = "scatter", mode = "lines", line = list(width = 0),
            name = "75th percentile", showlegend = FALSE, hoverinfo = "skip")
          p <- plotly::add_trace(p, x = detail$period, y = detail[[q1]],
            type = "scatter", mode = "lines", line = list(width = 0),
            fill = "tonexty", fillcolor = "rgba(50,105,213,0.13)",
            name = "25th–75th percentiles", hoverinfo = "skip")
        }
      }
    }
    display <- if (is_money) vapply(values, money, character(1)) else format(values, big.mark = ",")
    p <- plotly::add_trace(p, x = rows$period, y = values,
      type = "scatter", mode = "lines+markers", name = "Reported quarterly value",
      line = list(color = col_blue, width = 3), marker = list(size = 4),
      text = paste0(qtr_label_vec(rows$period), " · ", display), hoverinfo = "text")
    p <- chart_base(p, y_label = if (is_money) "Australian dollars" else "Reported count", x_label = NULL)
    p <- plotly::layout(p, xaxis = list(type = "date", gridcolor = col_grid),
      yaxis = list(tickprefix = if (is_money) "$" else "", tickformat = "~s", gridcolor = col_grid))
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  })

  output$trend_summary <- renderUI({
    d <- focus()
    if (!nrow(d)) return(p("Select a matching market."))
    tags$dl(class = "summary-dl",
      tags$dt("Quarter"), tags$dd(qtr_label(chosen_period())),
      tags$dt("Median sale price"), tags$dd(money(d$median_sale_price[1])),
      tags$dt("Median new-bond rent"), tags$dd(paste(money(d$median_weekly_rent[1]), "per week")),
      tags$dt("Sale transactions"), tags$dd(if (is.finite(d$sales_count[1])) as.character(d$sales_count[1]) else "Suppressed/unavailable"),
      tags$dt("New rental bonds"), tags$dd(if (is.finite(d$rent_count[1])) as.character(d$rent_count[1]) else "Suppressed/unavailable"))
  })

  # Descriptive category comparisons: do not pair rent house with non-strata
  # or rent flat/unit with sale strata as if they are identical populations.
  title_type_plot <- function(kind) {
    if (!identical(input$geography, "LGA"))
      return(empty_chart("Select LGA geography to view detailed dwelling categories."))
    req(input$focus_area, input$quarter)
    if (identical(kind, "sale")) {
      d <- sales_detail
      if (!nrow(d)) return(empty_chart("Detailed sales statistics not available."))
      d <- d[d$area == input$focus_area & d$period == chosen_period() &
        d$dwelling_type %in% c("Non Strata", "Strata") & is.finite(d$sale_median_aud), , drop = FALSE]
      values <- d$sale_median_aud; label <- "Median sale price (AUD)"
      color <- col_blue
    } else {
      d <- rents_detail
      if (!nrow(d)) return(empty_chart("Detailed rental statistics not available."))
      d <- d[d$area == input$focus_area & d$period == chosen_period() &
        d$dwelling_type %in% c("House", "Flat/Unit", "Townhouse") &
        d$bedrooms == "Total" & is.finite(d$rent_median_weekly_aud), , drop = FALSE]
      values <- d$rent_median_weekly_aud; label <- "Median weekly rent (AUD)"
      color <- col_teal
    }
    if (!nrow(d)) return(empty_chart("No published type-specific medians for this market and quarter."))
    txt <- paste0(d$dwelling_type, "<br>", vapply(values, money, character(1)),
      ifelse(d$median_status == "small_sample_11_to_30", "<br>Small sample: 11–30", ""))
    p <- plotly::plot_ly(x = values, y = d$dwelling_type, type = "bar", orientation = "h",
      marker = list(color = color, opacity = 0.84), text = txt, hoverinfo = "text")
    p <- chart_base(p, y_label = NULL, x_label = label)
    p <- plotly::layout(p, margin = list(l = 110, r = 16, t = 16, b = 54),
      xaxis = list(tickprefix = "$", tickformat = "~s", gridcolor = col_grid),
      yaxis = list(categoryorder = "array", categoryarray = rev(d$dwelling_type), showgrid = FALSE))
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  }

  output$sales_type_plot <- renderPlotly(title_type_plot("sale"))
  output$rent_type_plot <- renderPlotly(title_type_plot("rent"))

  legacy_data <- reactive(if (identical(input$legacy_metric, "sale")) legacy_sales else legacy_rents)
  observeEvent(input$legacy_metric, {
    d <- legacy_data()
    choices <- sort(unique(d$area))
    desired <- if (identical(input$legacy_metric, "sale")) "Greater Sydney" else "GREATER SYDNEY"
    if (!desired %in% choices) desired <- choices[1]
    updateSelectInput(session, "legacy_area", choices = choices, selected = desired)
  }, ignoreInit = FALSE)

  output$legacy_category_ui <- renderUI({
    d <- legacy_data(); req(input$legacy_area)
    d <- d[d$area == input$legacy_area, , drop = FALSE]
    types <- sort(unique(d$dwelling_type))
    selectInput("legacy_type", "Dwelling category", choices = types,
                selected = if ("Total" %in% types) "Total" else types[1])
  })

  output$legacy_bedroom_ui <- renderUI({
    if (!identical(input$legacy_metric, "rent")) return(NULL)
    req(input$legacy_area, input$legacy_type)
    d <- legacy_data()
    d <- d[d$area == input$legacy_area & d$dwelling_type == input$legacy_type, , drop = FALSE]
    vals <- sort(unique(d$bedrooms))
    if (!length(vals)) return(NULL)
    selectInput("legacy_bedroom", "Bedroom category", choices = vals,
                selected = if ("Total" %in% vals) "Total" else vals[1])
  })

  historical_selection <- reactive({
    req(input$legacy_area, input$legacy_type)
    d <- legacy_data()
    metric <- if (identical(input$legacy_metric, "sale")) "median_sale_price" else "median_weekly_rent"
    d <- d[d$area == input$legacy_area & d$dwelling_type == input$legacy_type & d$metric == metric, , drop = FALSE]
    if (identical(input$legacy_metric, "rent")) {
      req(input$legacy_bedroom)
      d <- d[d$bedrooms == input$legacy_bedroom, , drop = FALSE]
    }
    d[order(d$period), , drop = FALSE]
  })

  output$legacy_plot <- renderPlotly({
    d <- historical_selection()
    d <- d[is.finite(d$value), , drop = FALSE]
    if (!nrow(d)) return(empty_chart("No observations for this historical category."))
    p <- plotly::plot_ly(d, x = ~period, y = ~value, type = "scatter", mode = "lines+markers",
      line = list(color = "#8063C9", width = 2.5), marker = list(size = 3),
      name = "Original historical series", text = paste0(qtr_label_vec(d$period), " · ",
        vapply(d$value, money, character(1))), hoverinfo = "text")
    p <- chart_base(p, y_label = if (identical(input$legacy_metric, "rent")) "Weekly rent (AUD)" else "Sale price (AUD)", x_label = NULL)
    p <- plotly::layout(p, xaxis = list(type = "date", gridcolor = col_grid),
      yaxis = list(tickprefix = "$", tickformat = "~s", gridcolor = col_grid))
    plotly::config(p, displaylogo = FALSE, responsive = TRUE)
  })

  output$legacy_stat <- renderUI({
    d <- historical_selection()
    d <- d[is.finite(d$value), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    div(class = "legacy-stats",
      metric_card("First published quarter", qtr_label(min(d$period)), money(d$value[1])),
      metric_card("Last published quarter", qtr_label(max(d$period)), money(d$value[nrow(d)])),
      metric_card("Published quarterly observations", as.character(nrow(d)), "Legacy metropolitan series"))
  })

  output$provenance <- renderUI({
    d <- snapshot()
    div(class = "provenance-row",
      pill(paste(nrow(d), if (identical(input$geography, "LGA")) "LGA records" else "postcode records")),
      pill(paste(sum(is.finite(d$median_sale_price)), "published sale medians")),
      pill(paste(sum(is.finite(d$median_weekly_rent)), "published rent medians")),
      pill(paste(sum(is.finite(d$growth_pa)), "growth comparisons")),
      pill(paste(sum(is.finite(d$score)), "scoreable markets")))
  })

  output$download_filtered <- downloadHandler(
    filename = function() paste0("DCJ_market_shortlist_", input$quarter, "_", tolower(input$geography), ".csv"),
    content = function(file) utils::write.csv(candidates()[,
      c("area", "period", "median_sale_price", "median_weekly_rent", "sales_count", "rent_count",
        "gross_yield", "growth_pa", "score", "source_note"), drop = FALSE], file, row.names = FALSE, na = ""))
  output$download_all <- downloadHandler(
    filename = function() paste0("DCJ_quarterly_", tolower(input$geography), ".csv"),
    content = function(file) utils::write.csv(markets(), file, row.names = FALSE, na = ""))
}

# Vectorised quarter labels are used only for chart hover text.
qtr_label_vec <- function(dates) {
  paste0(format(dates, "%Y"), " Q", (as.integer(format(dates, "%m")) - 1L) %/% 3L + 1L)
}

shinyApp(ui = ui, server = server)
