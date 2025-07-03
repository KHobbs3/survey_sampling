# randomly select towns/villages..etc. 
library(sf)
library(dplyr)
library(here)
library(purrr)
library(mapview)

# User input
country <- "niger"
n <- 25 # sample size

# Read polygons
regions <- st_read(sprintf("input/settlement_areas/%s/%s_preselected.geojson",country, country))

# Filter minimum cluster population
# - we need at least 500 people in a town for it to be suitable
# - this is a conservative assumption because WRA make up around 25% of populations or less. 
# I round down to 20% to be conservative. I expect 25% will be unavailable on day of interview.
# npop = (n households x guess of average HH size) / probability of WRA (20% x 25%) = 100/10% = 500
regions_filt <- regions %>%
  filter(
    X_sum >= 500
  )

# Clean result and add selection probabilities based off population proportion
regions_sf <- regions_filt %>%
  mutate(
    population = X_sum,
    proportion = population/sum(population)
  ) %>%
  select(
    geom_id,
    ADM3_NAME,
    NAME,
    population,
    proportion
  ) 

# dataframe object for sampling
regions_df <- regions_sf %>%
  st_drop_geometry() %>%
  as_tibble()

# Random selection
set.seed(1)

idx_sample <- sample(
  seq_len(nrow(regions_df)), # to sample rows
  size = n,
  replace = T,
  prob = regions_df$proportion
)

# Summary of sample population
summary(sample_sf$population)

# Export
st_write(sample_sf, here("output", "sample", country, sprintf("%s_sample_%s.geojson", country, n)), 
         append=F)

# See result
sample_sf <- regions_sf[idx_sample,]
mapview(sample_sf, col.regions = 'purple')

