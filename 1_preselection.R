# select points only within FEM radio bounds
library(sf)
library(dplyr)
library(here)
library(purrr)
library(mapview)
library(digest)
library(jsonlite)

# User input
country <- "niger"
subfolder <- 'marion_partners'

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
filepath <- sprintf('/Users/kt/Documents/work/FEM/Family Planning/Radio Reach/cloudrf/output/gpkg/%s/%s', country, subfolder)

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


# Dissolve all radio stations
sf::sf_use_s2(FALSE)

union_result <- reduce(lapply(station_list, st_geometry), st_union)
union_result <- st_sf(geometry = union_result)

# Select points within
within_index <- st_within(poly, union_result)
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

# get geometry of polygons
final <- poly_in_stations %>%
  left_join(result, by = "geom_id", suffix = c("polygon", "")) 

final_simple <- final %>%
  select(geom_id,X_sum, name, place, osm_id, geometry)

# optional: add boundaries
boundary_file <- list.files(sprintf('../../General Data/HDX Boundaries/%s/', country), full.names=T)[1]
print(paste0("Reading boundaries for assignment: ", boundary_file))
boundaries <- st_read(boundary_file) %>% 
  select(
    shapeName,
    geometry
  )

# spatial join final to boundarys
final_simple <- final_simple %>%
  st_join(boundaries) 


# Export
st_write(final_simple, sprintf("input/settlement_areas/%s/%s_preselected.geojson",country, country),
         append=F,
         delete_dsn = TRUE)

# Visualize
mapview(final_simple[!is.na(final_simple$shapeName),], col.regions = 'purple') +
  mapview(final_simple[is.na(final_simple$shapeName),], col.regions = "red") + 
    mapview(union_result, col.regions = "green") 


any(sapply(final_simple$shapeName, is_null))

       
