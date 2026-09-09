# randomly select towns/villages..etc. ----
library(sf)
library(dplyr)
library(tibble)
library(here)
library(purrr)
library(mapview)

# 0. SET UP ----
source("config-senegal.R")

# Read polygons (run after 1_preselection.R to get suffix, or manually define)
regions <- st_read(here("output", "preselection", sprintf("%s/2_%s_settlement_populations_%s.geojson", country, country, suffix)))
mapview(regions)
# deduplicate (optional)
regions_dedupe <- regions %>% distinct(geom_id, .keep_all = TRUE)

# Determine minimum population size per cluster ----
n_per_cluster <- n_desired_total / n_cluster

min_pop <- n_per_cluster / (prop_qual*p_available)

sprintf("Total sample size: %s
        Total clusters:%s
        Surveys per cluster:%s
        Min population per cluster:%s",
        n_desired_total,
        n_cluster,
        n_per_cluster,
        min_pop
        )

## Filter minimum cluster population and drop clusters outside of Niger boundaries ----
# - we need at least 840 people in a town for it to be suitable
# - this is a conservative assumption because WRA comprise around 25% of populations or less. 
# I round down to 20% to be conservative. I expect 25% will be unavailable on day of interview.
# npop = (n WRA to interview) / probability of interviewing WRA (20% x 25%) = 20/10% = 840
regions_filt <- regions_dedupe %>%
  filter(
    population_coverage >= min_pop
  )


# Clean result and add selection probabilities based off population proportion ----
regions_sf <- regions_filt %>%
  dplyr::mutate(
    population = population_coverage,
    proportion = round(population/sum(population),10)
  ) %>%
  dplyr::select(
    geom_id,
    Admin1Name,
    popPlace_1,
    # name,
    Admin4Name,
    population,
    proportion
  ) 

# dataframe object for sampling
regions_df <- regions_sf %>%
  st_drop_geometry() %>%
  as_tibble()

## Random selection ----
set.seed(2)

# Add strata to your sampling frame
regions_df2 <- regions_df %>%
  # left_join(dept_to_strata, join_by(ADM1_FR == "admin_name")) %>%
  left_join(alloc_df, by = c("Admin1Name" = "strata_description"), keep=T)

# --- Stratified sampling: n per strata from chart, weight within strata by `proportion` ---
sample_df <- regions_df2 %>%
  split(.$strata_description) %>%
  lapply(\(x) slice_sample(x, n = unique(x$n_clusters_target)[1],
                           replace = F, weight_by = proportion)) %>%
  bind_rows()

# Check that the correct allocation happened
sample_df %>%
  group_by(.$strata_description) %>% count()
  
# Create spatial file of sample
sample_sf <- regions_sf %>% 
  # dplyr::select(
  #   geom_id,
  #   geometry
  # ) %>%
  right_join(sample_df, by="geom_id", suffix = c("", "_sample"))

nrow(sample_sf) == nrow(sample_df)

## Summary of sample population ----
summary(sample_sf$population_sample)

# Get centroids, lat and lon
sample_sf <- sample_sf %>%
  st_transform(32632) %>%  # UTM zone 32N
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    centroid_lonlat = st_transform(centroid, 4326),
    longitude = st_coordinates(centroid_lonlat)[, 1],
    latitude  = st_coordinates(centroid_lonlat)[, 2]
  ) %>%
  dplyr::select(-centroid, -centroid_lonlat)

## EXPORT ----
# Export as spatial file

dir.create(file.path(here("output", "sample", country)))
st_write(sample_sf, here("output", "sample", country, sprintf("%s_sample_%s_%s.geojson", country, n_cluster, suffix)), 
         append=F, delete_dsn = T)


# export as KML
st_write(sample_sf, here("output", "sample", country, sprintf("%s_sample_%s_%s.kml", country, n_cluster, suffix)), 
         append=F, delete_dsn = T)

# Convert to DF
sample_df <- sample_sf %>% 
  st_drop_geometry() %>%
  as_tibble()

# Export as csv
write.csv(sample_df,
          here("output", "sample", country, sprintf("%s_sample_%s_%s.csv", country, n_cluster, suffix)),
          fileEncoding = 'utf-8'
          )

## VISUALISE ----
mapview(sample_sf, col.regions = 'purple')
