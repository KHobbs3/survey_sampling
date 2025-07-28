# randomly select towns/villages..etc. ----
library(sf)
library(dplyr)
library(here)
library(purrr)
library(mapview)

## User input ----
country <- "niger"
n_desired_total <- 1050
n_cluster <- 25
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
    !is.na(shapeName) # filter out-of-bonds
  )


## Clean result and add selection probabilities based off population proportion ----
regions_sf <- regions_filt %>%
  mutate(
    population = X_sum,
    proportion = population/sum(population),
    state = shapeName
  ) %>%
  select(
    geom_id,
    place,
    name,
    state,
    population,
    proportion
  ) 

# dataframe object for sampling
regions_df <- regions_sf %>%
  st_drop_geometry() %>%
  as_tibble()

## Random selection ----
set.seed(1)

idx_sample <- sample(
  seq_len(nrow(regions_df)), # to sample rows
  size = n,
  replace = T,
  prob = regions_df$proportion
)

sample_sf <- regions_sf[idx_sample, ]

## Summary of sample population ----
summary(sample_sf$population)

# Get centroids, lat and lon
sample_sf <- sample_sf %>% mutate(
  centroid = st_centroid(geometry),
  longitude = st_coordinates(centroid)[,1],
  latitude = st_coordinates(centroid)[,2]
)

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
