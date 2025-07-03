---
editor_options: 
  markdown: 
    wrap: 72
---

# Cluster Sampling Scripts

This repository contains R scripts for selecting and analyzing
settlements within FEM radio bounds, calculating population coverage,
and drawing a random sample of settlements for fieldwork.

## Contents

### `1_preselection.R`

> Select settlements (polygons) and points that are within the FEM radio
> coverage area.

-   Reads:
    -   Settlement polygons from GeoJSON.
    -   Candidate points (villages, towns, etc.) from Shapefile.
    -   FEM radio coverage polygons from GPKG files.
-   Dissolves all radio coverage polygons into a single area of
    coverage.
-   Selects only settlement polygons that intersect with the coverage
    area.
-   Joins points to selected polygons to add metadata.
-   Outputs:
    -   GeoJSON of selected settlement polygons within coverage.
    -   Visualization of settlements and radio coverage area.

Output file:\
`input/settlement_areas/<country>/<country>_preselected.geojson`

------------------------------------------------------------------------

### `2_population_in_polygon.R`

> Calculate population coverage within a specified radius around each
> point.

-   Reads:
    -   Population raster (GeoTIFF).
    -   Point locations (CSV with `lon` and `lat` columns).
-   Converts points to buffers of configurable radius (`km`) in
    equal-area projection.
-   Computes the total population within each buffered area using the
    raster.
-   Outputs:
    -   CSV summary table of point IDs, population coverage, and radius.
    -   Validation GeoJSON for last processed point.
    -   Map visualizations for quality check.

Output file:\
`output/<country>_summary_reach_<km>km.csv`

Validation output:\
`validation/<country>/<country>_point_validation.geojson`

------------------------------------------------------------------------

### `3_random_sample.R`

> Randomly select a specified number of settlements weighted by
> population.

-   Reads:
    -   Preselected settlement polygons (`1_preselection.R` output).
-   Filters out settlements with population `< 500` (conservative WRA
    estimate).
-   Computes selection probabilities proportional to population.
-   Randomly samples `n` settlements (with replacement) based on
    probabilities.
-   Outputs:
    -   GeoJSON of sampled settlements.
    -   Visualization of selected points.

Output file:\
`output/sample/<country>/<country>_sample_<n>.geojson`

------------------------------------------------------------------------

## Requirements

All scripts are written in R and depend on the following packages: -
`sf` - `dplyr` - `here` - `purrr` - `mapview` - `digest` - `jsonlite` -
`terra` - `raster` - `openxlsx` - `stringr` - `exactextractr`

Install them with: \`\`\`r install.packages(c( "sf", "dplyr", "here",
"purrr", "mapview", "digest", "jsonlite", "terra", "raster", "openxlsx",
"stringr", "exactextractr" ))
