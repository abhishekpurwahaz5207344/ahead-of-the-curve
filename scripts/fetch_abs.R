# Run from the repository root: Rscript scripts/fetch_abs.R
# Fetches the ABS 2021 Census General Community Profile with matching LGA polygons.
source("R/abs_geography.R")
if (!requireNamespace("sf", quietly = TRUE)) stop("Please install.packages('sf') first")
path <- fetch_abs_geojson(force = TRUE)
g <- read_abs_geography(path, allow_download = FALSE)
message("Saved ", nrow(g), " NSW ABS 2021 LGA polygons to ", path)
message("Census coverage example: ", g$abs_name[1], ": ", g$population_2021[1], " people")
