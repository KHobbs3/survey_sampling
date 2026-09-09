# User input for scripts

###############################
# 1_preselection.R ----
###############################
country <- "togo"

point_col_select <- c("name",
                "place",
                "osm_id",
                "source",
                "geom")

aerial_crs <- "epsg:32632" # specific to country

# File paths
# radio_filepath <- "input/radio_configs/"
# subfolder <- ""

# File names
settlement_polys <- "ZONAL_GRID3_TGO_settlement_extents_v3_0.gpkg"
settlement_points <- "hotosm_tgo_populated_places_points_gpkg.gpkg"
boundary_file <- "tgo_admbnda_adm1_inseed_itos_20210107.shp"

# Column names
## Boundary file
boundary_cols <- c("ADM1_FR", "geometry")

# Boolean: conduct constraints on settlements within radio config boundaries
constrain_distance <- F

# Adjust inputs for applying constraints

major_cities <- c(
  
  # Kara / near-Savanes
 "Kara",
 "Kandé",
 
 # Maritime
 "Lomé",
 "Tsévié",
 
 # Centrale
 "Sokodé",
 
 # Plateaux
 "Kpalimé",
 "Atakpamé"
 
)
city_radius <-75 # in kilometres



###############################
# 3_random_sample.R ----
###############################
n_desired_total <- 750
prop_qual <- 0.45 # typical proportion of population with Men and Women of reproductive age
p_available <- 0.20 # i assume x% of MWRA will be available on day of interview

# --- Cluster allocation from your chart (total should match n_cluster = 30) ---
alloc_df <- tibble(
  strata_description = c(
  "Kara",
  "Centrale",
  "Plateaux",
  "Maritime"),
  n_clusters_target  = c(4, 3, 7, 15)*2 # multiply by 2 for replacements
)

# total clusters selected
n_cluster <- sum(alloc_df$n_clusters_target)

# --- Map each department/state into the chart strata (edit if your labels differ) ---
dept_to_strata <- tibble(
  admin_name = c("Kara", "Centrale", "Plateaux", "Maritime"),
  strata_description = c("Kara", "Centrale", "Plateaux", "Maritime")
)
