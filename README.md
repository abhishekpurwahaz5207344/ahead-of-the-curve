# Ahead of The Curve

**Interactive NSW property-market research dashboard · UNSW DATA5002**

This repository contains the published **HTML dashboard** and the **R Shiny source code and processed data** used for reproducibility. The main dashboard examines NSW local government areas (LGAs) using NSW DCJ quarterly rent and sales data and ABS 2021 geographic/demographic information.

## Open the dashboard

- **Published site:** Replace this line with your own GitHub Pages URL after deployment.
- **Local HTML preview:** Open `index.html` in a browser. The DCJ dashboard data is already embedded in this file.
- **Official ABS map:** The HTML tries to load the 2021 ABS LGA polygons from the official ABS ArcGIS service. An internet connection is required; if that service is unavailable, the charts, filters and tables remain usable. You can also import the relevant ABS GeoJSON through the map interface.

## Publish on GitHub Pages

1. Create a GitHub repository called `ahead-of-the-curve` (or any name you prefer).
2. Upload the **contents of this folder**, keeping `index.html` at the repository root. Do not upload the ZIP as the website.
3. In **Settings → Pages**, choose **Deploy from a branch**, then select `main` and `/(root)`.
4. Open the URL displayed in the Pages settings and verify the LGA map, filters, charts, ranking and download button.
5. Paste that live URL into the first bullet of this README and submit **the live URL**, not a local HTML or ZIP file, as the dashboard link.

## Reproduce the dashboard in R

These instructions are for the companion **R Shiny implementation**. It is a separate implementation and may not have identical styling to the HTML website.

1. Install R and RStudio (or another R environment).
2. In R, set the working directory to the repository root.
3. Run:

```r
source("install_packages.R")
source("scripts/fetch_abs.R")  # downloads/caches official ABS 2021 LGA geography
shiny::runApp(".")
```

The `data/` folder contains the processed DCJ inputs used by `app.R`; `R/` contains the loading, scoring, property-type and geographic functions. `www/styles.css` styles the R application. Internet access is needed for the initial ABS geography download. An ABS cache is not included to avoid adding a large duplicate asset.

## Files in this repository

| Location | Purpose |
| --- | --- |
| `index.html` | GitHub Pages website; includes its own HTML/CSS/JS and embedded DCJ data |
| `README.md` | Setup, source and methodological instructions |
| `.nojekyll` | Serves the site on GitHub Pages without Jekyll processing |
| `app.R` | Runnable R Shiny dashboard |
| `install_packages.R` | Installs required R packages |
| `R/` | Required R functions for data, scoring, property types and ABS joins |
| `scripts/fetch_abs.R` | Retrieves and caches official ABS geography |
| `data/` | Six processed DCJ data tables required by the R application |
| `www/styles.css` | Styling for the R application |

No screenshots, draft reports, original XLSX files, duplicate JSON files, obsolete data audits or development scripts are included.

## Data sources and interpretation

- **NSW Department of Communities and Justice (DCJ):** Quarterly *Rent and Sales Reports*. The main comparable sales/rental snapshot is **January–March 2026 (2026 Q1)**; the available **June 2026 rent-only** figures are reserved for rent trends, not paired-quarter yields. [DCJ Rent and Sales Reports](https://dcj.nsw.gov.au/about-us/families-and-communities-statistics/housing-rent-and-sales.html).
- **Australian Bureau of Statistics (ABS):** 2021 Census LGA boundaries and demographic measures. These are **2021** measurements, not current 2026 population estimates. [ABS Census](https://www.abs.gov.au/census).
- **Historical series:** The historical rent and sales files are supplied separately to avoid obscuring changes in geographic definitions or statistical methods.

**Property-type limitations:** DCJ non-strata sales are used as an approximate established-home category and strata sales as an approximate apartment/unit category. Rental house/flat categories are **not identical** to the sales categories. Townhouse rentals do not have a matching dedicated townhouse-sale-price measure.

**Gross rental yield proxy:** `median weekly rent × 52 / median sale price × 100`. It compares area-level new-bond rents and published sale-price medians, *not* the rental yield on a specific property. Matched-quarter figures are used for the comparison.

**Five-year CAGR:** `((median sale price in 2026 Q1 / median sale price in 2021 Q1)^(1/5) - 1) × 100`. Historical changes in area medians are not individual-home investment returns or forecasts; the mix of properties sold may change.

**Research score:** A descriptive **relative** 0–100 ranking based on percentile-normalised historical growth, yield and affordability. The user changes weights from 0 to 5; active weights are divided by their sum. LGAs with missing values for a selected factor are not ranked, rather than having missing values treated as zero. It is not an estimate of profit probability or a financial recommendation.

**Data reliability:** DCJ uses `-` for suppressed small samples and `s` to indicate 11–30 observations. Such figures warrant caution, particularly the **Walgett $100,000 published median** in 2026 Q1, which is based on 11–30 transactions. Figures are descriptive and subject to reporting lag.

## Assessment deliverables

The repository supplies the website, R source, processed inputs and reproduction instructions. **Submit the final proposal, ≤500-word reflection and relevant generative-AI conversation logs separately as required by the assignment brief.** AI was used to help develop, edit and debug the dashboard, data preparation and documentation. Disclose that assistance fully in the assessed reflection and provide the actual interaction logs. Do not assume this README substitutes for them.

## Verification and important hosting distinction

**HTML website:** The local Chromium checks verified the current 2026 Q1 headline values, filters, top-10 ranking, CSV download and viewport widths; JavaScript produced no page errors during those tests. **The live ABS map request was not validated in the deployment environment.** Test it on the published website.

**R source:** The companion R Shiny implementation is included for reproducibility, but could not be executed in the available environment. It is separate from the HTML interface, so its appearance is not necessarily identical. Check it in R before describing it as runtime-verified.

## Final checks before submission

Open the live URL in a private browser window; confirm that the ABS map loads, price filters and 0–5 weights respond, the Top 10 list changes appropriately, CSV download works and the mobile layout is readable. The R Shiny app must also be tested in an R environment if it is being submitted as runnable code.
