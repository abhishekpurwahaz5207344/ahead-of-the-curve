packages <- c("shiny", "plotly", "DT", "sf", "leaflet", "leaflet.extras")
missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("Required packages installed. Run shiny::runApp('.')")
