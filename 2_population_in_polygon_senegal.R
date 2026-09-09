# Calculate population coverage within a specified radius around each point.
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

# Set country name
country <- "senegal"

# Set-up function for calculating population coverage
pop_coverage <- function (population_raster, polygon){
  exactextractr::exact_extract(population_raster, polygon,
                               fun = function(values, coverage_fractions) {
                                 sum(values * coverage_fractions, na.rm = TRUE)
                               })
}

# 2. Read data ----
# Read population raster and reproject it
print("Reading population raster...")
population_raster <- raster(list.files(here("input", "population_rasters", country),
                                       full.names = T,
                                       pattern = "*.tif$")[1]
                            )

# read polygon data as shapefile
poly_sf <- st_read(here("output", "preselection", country, sprintf("0_senegal_surveybounds_%s.geojson", suffix)))

# read point data as shapefile
point_sf <- st_read(here("input", "points", country, "sen_plpp_gov_ocha_09082017.shp"))

# join points to cluster
poly_point <- st_join(poly_sf, point_sf)

poly_summary <- poly_point %>%
  st_drop_geometry() %>%
  group_by(geom_id) %>% 
  summarise(
    Admin1Name = first(na.omit(Admin2Name)),                # top answer only
    Admin2Name = first(na.omit(Admin2Name)),                
    Admin4Name = paste(Admin4Name, collapse = ", "),         # all answers
    FeatureNam = paste(FeatureNam, collapse = ", "),
    FID_sen_se = paste(FID_sen_se, collapse = ", "),
    popPlace_1 = paste(popPlace_1, collapse = ", ")
  )

# rejoin to polygon geometries (use left_join to keep polygons with 0 matches)
poly_final <- poly_sf %>%
  left_join(poly_summary, by = "geom_id")

# 3. Reproject buffered points to the popgrid degrees proj
poly_crs <- st_transform(poly_final, crs(population_raster))

# 4. Set-up table for export
population_data <- tibble(
                          interation_number = integer(), # optional, generated in loop below
                          poly_id = character(),
                          population_coverage = numeric()
                          )

# 5. Calculate population living within x-km of points ----
print("Calculating population coverage..")

# Iterator ----
# Initialize list to track errors
errors = list()

# Loop each buffered point
for (i in seq(nrow(poly_crs))) {
      poly <- poly_crs[i,]
      
      # get name of point
      name = poly$geom_id

      # Verbosity: count progress
      print(sprintf("Polygon %s of %s: %s", i, nrow(poly_crs), name))

      # Calculation population coverage
      population_coverage <- pop_coverage(population_raster, poly)
        
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
            poly_id = name,
            population_coverage = population_coverage
          )
      }
}


# 6. Export summary output ----
print("Exporting summary output...")
write.csv(population_data,
          here("output", "preselection", sprintf("%s/2_%s_settlement_populations_%s.csv", country, country, suffix)),
          row.names = F)

# 7. Export shapefile for sampling
poly_pops <- bind_cols(poly_crs, population_data)
st_write(poly_pops,
         here("output", "preselection", sprintf("%s/2_%s_settlement_populations_%s.geojson", country, country, suffix)),
         row.names = F,
         append=F)


# Visual Checks
mapview(population_raster, col.regions = "green",
        na.color = NA) +
  mapview(poly_sf, col.regions = "red") +
  mapview(poly_crs, col.regions = "blue") 

# Export for validation if desired
dir.create(file.path(here("validation", country)))
st_write(poly,  here("validation", sprintf("%s/%s_settlement_validation_%s.geojson", country, country, suffix)), row.names = F)
