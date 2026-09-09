# select points only within FEM radio bounds
# UPDATE 03-2026: option to not constrain survey sample to radio bounds
# UPDATE 07-2026: fixing broken geometries in boundary file

library(sf)
library(dplyr)
library(here)
library(purrr)
library(mapview)
library(digest)
library(jsonlite)

# 0. SET UP ----
source("config-senegal.R")

# Read polygons
poly <- st_read(sprintf("input/settlement_areas/%s/%s", country, settlement_polys)) %>%
  st_make_valid()

# Read boundary file
boundaries <- st_read(sprintf("input/boundaries/%s/%s", country, boundary_file))


if (constrain_distance == T) {
  # Get all gpkg files
  gpkg_files <- list.files(sprintf("%s/%s", radio_filepath,country), pattern = "\\.gpkg$", full.names = TRUE)

  # Read all .gpkg files into a list of sf objects
  print("Reading radio configuration...")
  print("===============================")
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

  sprintf("Applying constraints: %s km from any of : %s", city_radius, major_cities)
  print("===============================")
    # Read points
    points = st_read(sprintf("input/points/%s/%s", country, settlement_points)) %>%
      dplyr::select(
        point_col_select
      ) %>%
      dplyr::rename(all_of(point_rename_col))
    
  
    # 2. Only select major cities ----
    selected_points <- points %>% filter(Admin4Name %in% major_cities)# & place != "hamlet")
    
    # # Benin - keep only one dogbo and albomey-calavi
    # selected_points <- selected_points %>% filter(!osm_id %in% c('3862741927', '10890183156', '7615581115'))
    
    # Buffer cities 
    selected_points_transformed <- st_transform(selected_points, aerial_crs)
    selected_buffer_transformed <- st_buffer(selected_points_transformed, dist=city_radius*1000)
    selected_buffer_4326 <- st_transform(selected_buffer_transformed, "epsg:4326")
    
    
    # Dissolve stations to x-KM radius of major selected cities
    survey_bounds <- st_intersection(boundaries, selected_buffer_4326) %>%
      st_union() %>%

      st_make_valid()
    
    # Check:
    mapview(survey_bounds, col.regions='yellow') +
      mapview(boundaries) +
      mapview(selected_buffer_4326, col.regions='red')
    
} else {
  print("Not applying radio constraints...")
  print("===============================")

  # Read points
  points = st_read(sprintf("input/points/%s/%s", country, settlement_points)) %>%
    dplyr::select(
      point_col_select
    )
  
  
  # 2. Only select major cities ----
  selected_points <-  points %>% filter(Admin4Name %in% major_cities) #filter(name %in% major_cities & place != "hamlet")
  
  # # Benin - keep only one dogbo and albomey-calavi
  # selected_points <- selected_points %>% filter(!osm_id %in% c('3862741927', '10890183156', '7615581115'))
  
  # Buffer cities 
  selected_points_transformed <- st_transform(selected_points, aerial_crs)
  selected_buffer_transformed <- st_buffer(selected_points_transformed, dist=city_radius*1000)
  selected_buffer_4326 <- st_transform(selected_buffer_transformed, "epsg:4326")
  
  
  # Dissolve administrative boundaries to x-KM radius of major selected cities
  survey_bounds <- st_intersection(boundaries, selected_buffer_4326) %>%
    st_union() %>%
    st_make_valid()
  
  # Check:
  mapview(survey_bounds, col.regions='yellow')
}


# 3. Select points within survey bounds ----
within_index <- st_intersects(poly, survey_bounds) # st_intersects to keep Dakar in Senegal
poly_in_stations <- poly[lengths(within_index) > 0,]


# Set Unique id for poly using geometry
poly_in_stations$geom_id <- sapply(st_as_text(st_geometry(poly_in_stations)), digest, algo = "sha1")

# Spatial join points to polygon get name, and type columns per polygon
# joined <- st_join(points, poly_in_stations, suffix = c("", "_polygon"))
joined <- st_join(points, poly_in_stations, join = st_within, left = FALSE, suffix = c("", "_polygon"))
mapview(joined)


result <- joined %>%
  st_drop_geometry() %>%
  group_by(geom_id) %>%
  summarise(
    name = paste(unique(Admin4Name), collapse = "; "),
    # place = paste(unique(popPlace_1), collapse = "; "),
    # osm_ids = paste(unique(osm_id), collapse = "; "),
    .groups = "drop"
  )

# Get geometry of polygons
poly_in_stations$geom_id <- as.character(poly_in_stations$geom_id)
result$geom_id <- as.character(result$geom_id)

final <- poly_in_stations %>%
  left_join(result, by = "geom_id", suffix = c("polygon", "")) 

# 4. Add boundaries ----
# spatial join final to boundaries
final <- final %>%
  st_join(boundaries) 


# 5. Export ----
st_write(final, sprintf("output/preselection/%s/0_%s_surveybounds%s.geojson",country, country, suffix),
         append=F,
         delete_dsn = TRUE)
st_write(final, sprintf("output/preselection/%s/0_%s_surveybounds%s.kml",country, country, suffix),
         append=F,
         delete_dsn = TRUE)

# preselection points
st_write(selected_points_transformed, sprintf("output/preselection/%s/0_%s_points%s.kml",country, country, suffix),
         append=F,
         delete_dsn = TRUE)

