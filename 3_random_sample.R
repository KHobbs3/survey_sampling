# randomly select towns/villages..etc. ----
library(sf)
library(dplyr)
library(here)
library(purrr)
library(mapview)

## User input ----
country <- "niger"
n_desired_total <- 990
n_cluster <- 27  # e.g. 18 clusters + 21 replacement clusters, for Niger
prop_wra <- 0.20 # typical proportion of population, rounded down
p_wra_available <- 0.25 # i assume x% of WRA will be available on day of interview

# Read polygons
regions <- st_read(sprintf("input/settlement_areas/%s/%s_preselected.geojson",country, country))


## Determine minimum population size per cluster ----
n_per_cluster <- n_desired_total / n_cluster

min_pop <- n_per_cluster / (prop_wra*p_wra_available)

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
regions_filt <- regions %>%
  filter(
    X_sum >= min_pop,
    !is.na(name.y) # filter out-of-bounds
  )


## Clean result and add selection probabilities based off population proportion ----
regions_sf <- regions_filt %>%
  mutate(
    population = X_sum,
    proportion = round(population/sum(population),10),
    state = name.y
  ) %>%
  select(
    geom_id,
    place,
    name.x,
    state,
    population,
    proportion
  ) 

# dataframe object for sampling
regions_df <- regions_sf %>%
  st_drop_geometry() %>%
  as_tibble()

## Random selection ----
set.seed(2)

# Set the number of clusters per state (or proportionally adjust this logic)
n_states <- 3
n_cluster_per_state <- n_cluster/n_states  # for example, 5 samples per state

# Stratified sampling
sample_df <- regions_df %>%
  group_by(state) %>%
  slice_sample(
    n = n_cluster_per_state,
    replace = TRUE,
    weight_by = proportion
  ) %>%
  ungroup()

sample_sf <- regions_sf %>% filter(geom_id %in% sample_df$geom_id)

## Summary of sample population ----
summary(sample_sf$population)

# Get centroids, lat and lon
sample_sf <- sample_sf %>%
  st_transform(32632) %>%  # 32632 is UTM zone 32N
  mutate(
    centroid = st_centroid(geometry)
  ) %>%
  # Get lon/lat by transforming centroid back to EPSG:4326
  mutate(
    centroid_lonlat = st_transform(centroid, 4326),
    longitude = st_coordinates(centroid_lonlat)[,1],
    latitude = st_coordinates(centroid_lonlat)[,2],
    name = name.x
  ) %>%
  # Optionally drop the temporary projected centroid
  select(-c(centroid,centroid_lonlat,name.x))

## EXPORT ----
# Export as spatial file
st_write(sample_sf, here("output", "sample", country, sprintf("%s_sample_%s.geojson", country, n_cluster)), 
         append=F, delete_dsn = T)


# export as KML
st_write(sample_sf, here("output", "sample", country, sprintf("%s_sample_%s.kml", country, n_cluster)), 
         append=F, delete_dsn = T)

# Convert to DF
sample_df <- sample_sf %>% 
  st_drop_geometry() %>%
  as_tibble()

# Export as csv
write.csv(sample_df,
          here("output", "sample", country, sprintf("%s_sample_%s.csv", country, n_cluster)),
          fileEncoding = 'cp1252'
          )

## VISUALISE ----
mapview(sample_sf, col.regions = 'purple')


# Compare representativeness of sample to original population ----
library(ggplot2)

## set-up samples ----
# Filter out-of-bounds clusters
regions_in_bounds <- regions %>%
  filter(
    !is.na(geometry)
  )


# ORIGINAL
ggplot(regions_in_bounds, aes(x = X_sum)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Full Population",
       x = "Population",
       y = "Frequency") +
  theme_minimal()


# ORIGINAL FILTTERED
ggplot(regions_filt, aes(x = X_sum)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = sprintf("Population > %s",min_pop),
       x = "Population",
       y = "Frequency") +
  theme_minimal()

# SAMPLE
# with replacement
ggplot(sample_sf, aes(x = population)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Sampling with replacement",
       x = "Population",
       y = "Frequency") +
  theme_minimal()


# withOUT replacement
idx_sample_wo <- sample(
  seq_len(nrow(regions_df)), # to sample rows
  size = n,
  replace = F,
  prob = regions_df$proportion
)

sample_sf_wo <- regions_sf[idx_sample_wo, ]


## PLOTS ----
ggplot() +
  geom_boxplot(data = sample_sf_wo,
               aes(x = "Without replacement", y = population), fill = "blue") +
  geom_boxplot(data = sample_sf,
               aes(x ="With replacement", y = population), fill = "red") +
  geom_boxplot(data = regions_filt,
               aes(x = ">Min Pop Target Population", y = X_sum), fill = "grey") +
  geom_boxplot(data = regions_in_bounds,
               aes(x = "Full Target Population", y = X_sum), fill = "green")
