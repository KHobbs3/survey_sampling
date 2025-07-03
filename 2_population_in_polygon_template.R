library(sf)
library(terra)
library(dplyr)
library(here)
library(mapview)
library(openxlsx)
library(stringr)
library(raster)


# 1. Set-up ----
options(warn = -1)  # Suppress warnings

# TODO: Set the number of km for the point buffer
km <- 7

# Set country name
country <- "senegal"

# Set-up function for calculating population coverage
pop_coverage <- function (population_raster, polygon){
  exactextractr::exact_extract(population_raster, polygon,
                               fun = function(values, coverage_fractions) {
                                 sum(values * coverage_fractions, na.rm = TRUE)
                               })
}

# TODO: Set projection to equal area projection for country
# Source: https://epsg.io/2548-area
# Other options: https://epsg.io/?q=senegal%20%20kind%3AAREA
area_proj <- "epsg:2548" # west africa


# 2. Read data ----

# Read population raster and reproject it
print("Reading population raster...")
population_raster <- raster(list.files(here("input", "population_rasters", country),
                                       full.names = T,
                                       pattern = "*.tif$")[1]
                            )

# Get path to csv with point coordinates
file_path <- list.files(here("input", "points", country),
                   pattern = '*csv$',
                   full.names = T)[1]

print(sprintf("Reading point layer data from: %s...", file_path))

# Read as table
# Note that the csv must contain columns "lat" and "lon"
points_csv <- read.csv(file_path) %>% tidyr::drop_na() # Drop null records

# Convert to spatial object
points <- st_as_sf(points_csv,
                  coords = c("lon", "lat"), # TODO: update column names. Note that lon precedes lat
                  crs = 4326)

points <- points %>% tidyr::drop_na()

# 3. Reproject and add buffer to points ----
# Reproject from degrees to area projection
points_proj <- st_transform(points, area_proj)

# Add x-KM buffer in an areal projection
print(sprintf("Adding %s-KM buffer", km))
points_buffer <- points_proj %>% 
  st_buffer(dist = km * 1000)

# Reproject buffered points to the popgrid degrees proj
points_buffer_deg <- st_transform(points_buffer, crs(population_raster))

# 4. Set-up table for export ----
population_data <- tibble(
                          interation_number = integer(), # optional, generated in loop below
                          point_id = character(),
                          population_coverage = numeric(),
                          kilometre = integer()
                          )

# 5. Calculate population living within x-km of points ----
print("Calculating X-km population coverage..")


# Iterator ----
# Initialize list to track errors
errors = list()

# Loop each buffered point
for (i in nrow(points_buffer_deg)) {
      point <- points_buffer_deg[i,]
      
      # get name of point
      name = point$name # TODO: update with column name

      # Verbosity: count progress
      print(sprintf("Point %s of %s: %s", i, length(points_buffer_deg), name))

      # Calculation population coverage
      population_coverage <- pop_coverage(population_raster, point)
        
      # Ensure polygon covers a population
      if (length(population_coverage) == 0){
        len <- length(errors)
        
        # append value to end of list
        errors[[len+1]] <- i
        
      } else {
        
        # Add data to table
        population_data <- population_data %>%
          add_row(
            interation_number = i,
            point_id = name,
            population_coverage = population_coverage,
            kilometre = km
          )
      }
}


# 6. Export summary output ----
print("Exporting summary output...")
write.csv(population_data, here("output", sprintf("%s_summary_reach_%skm.csv", country, km)), row.names = F)

print("Errors:")
errors
print("Fin.")

# Visual Checks
mapview(population_raster, col.regions = "green",
          na.color = NA) +
    mapview(points_buffer_deg, col.regions = "red") +
  mapview(point, col.regions = "blue") 
  

# Export for validation if desired
dir.create(file.path(here("validation", country)))
st_write(point,  here("validation", sprintf("%s/%s_point_validation.geojson", country, country)), row.names = F)
