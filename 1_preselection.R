# select points only within FEM radio bounds
# UPDATE 09-2025: select points within 100 km of major cities
library(sf)
library(dplyr)
library(here)
library(purrr)
library(mapview)
library(digest)
library(jsonlite)

# 0. SET UP ----
# User input
country <- "niger"
subfolder <- 'marion_partners'
major_cities <- c("Badaguichiri", "Maradi", "Zinder")
city_radius <- 100 # in kilometres

# Read polygons
poly <- st_read(sprintf("input/settlement_areas/%s/osm_settlements_unconstrained_population_2020_150m_buffer.geojson",country))

# Read points
points = st_read(list.files(sprintf("input/points/%s/",country),
                            pattern = ".geojson$",
                            full.names = T)[1])

points <- points %>%
  select(
         "name",
         "place",
         "osm_id",
         "source",
         "geometry")


# Set-up file path
filepath <- sprintf('/Users/kt/Documents/work/AIM Charities/FEM/Family Planning/2. Everything Radio/Radio Reach/cloudrf/output/gpkg/%s/%s/', country, subfolder)

# Get all gpkg files
gpkg_files <- list.files(filepath, pattern = "\\.gpkg$", full.names = TRUE)


# Read all .gpkg files into a list of sf objects
station_list <- lapply(gpkg_files, function(file) {
  print(file)
  
  # read raster, replace 0s with NA, dissolve/unionize/aggregate
  radio_polygon <- st_read(file)
  
  # add column
  radio_polygon$source_file <- basename(file)
  
  # reproject
  radio_polygon <- st_transform(radio_polygon, "epsg:4326")
  
  # return
  return(radio_polygon)
})


# 1. Only select major cities ----
selected_points <- points %>% filter(name %in% major_cities & place != "hamlet")

# Buffer cities 
selected_points_transformed <- st_transform(selected_points, "epsg:32632")
selected_buffer_transformed <- st_buffer(selected_points_transformed, dist=city_radius*1000)
selected_buffer_4326 <- st_transform(selected_buffer_transformed, "epsg:4326")

# 2. Dissolve all radio stations ---
sf::sf_use_s2(FALSE)

station_union <- reduce(lapply(station_list, st_geometry), st_union)
station_union <- st_sf(geometry = station_union)

# Dissolve stations to x-KM radius of major selected cities
survey_bounds <- st_intersection(station_union, selected_buffer_4326)

# Visual check
mapview(survey_bounds, col.regions='yellow') +
  mapview(selected_buffer_4326) +
  mapview(station_union)


# 3. Select points within survey bounds ----
within_index <- st_within(poly, survey_bounds)
poly_in_stations <- poly[lengths(within_index) > 0,]

# Set Unique id for poly using geometry
poly_in_stations$geom_id <- sapply(st_as_text(st_geometry(poly_in_stations)), digest, algo = "sha1")

# Spatial join points to polygon get ADM3_NAME, NAME, MILIEU, TYPELOCALI columns per polygon
joined <- st_join(points, poly_in_stations, suffix = c("", "_polygon"))

result <- joined %>%
  st_drop_geometry() %>%
  group_by(geom_id) %>%
  summarise(
    name = paste(unique(name), collapse = "; "),
    place = paste(unique(place), collapse = "; "),
    osm_ids = paste(unique(osm_id), collapse = "; "),
    .groups = "drop"
  )

# Get geometry of polygons
final <- poly_in_stations %>%
  left_join(result, by = "geom_id", suffix = c("polygon", "")) 

final_simple <- final %>%
  select(geom_id,X_sum, name, place, osm_id, geometry)

# 4. optional: add boundaries ----
boundary_file <- list.files(sprintf('../../General Data/HDX Boundaries/%s/', country), pattern = '\\.geojson$', full.names=T)[1]
print(paste0("Reading boundaries for assignment: ", boundary_file))
boundaries <- st_read(boundary_file) %>% 
  select(
    name,
    geometry
  )

# spatial join final to boundaries
final_simple <- final_simple %>%
  st_join(boundaries) 


# 5. Export ----
st_write(final_simple, sprintf("input/settlement_areas/%s/%s_preselected.geojson",country, country),
         append=F,
         delete_dsn = TRUE)

# Visualize
mapview(final_simple[!is.na(final_simple$name.x),], col.regions = 'purple') +
  mapview(final_simple[is.na(final_simple$name.x),], col.regions = "red") + 
    mapview(survey_bounds, col.regions = "green") 


any(sapply(final_simple$shapeName, is_null))


       
