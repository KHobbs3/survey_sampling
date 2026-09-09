# User input for scripts

###############################
# 1_preselection.R ----
###############################
country <- "benin"

point_col_select <- c("name",
                "place",
                "osm_id",
                "source",
                "geom")

aerial_crs <- "epsg:32632" # specific to country

# File paths
radio_filepath <- "input/radio_configs/"
subfolder <- ""

# File names
settlement_polys <- "ZONALPOP_GRID3_BEN_Settlement_Extents_v3_0_8527819486404085423.gpkg"
settlement_points <- "hotosm_ben_populated_places_points_gpkg.gpkg"
boundary_file <- "ben_admbnda_adm1_1m_salb_20190816.shp"

# Boolean: conduct constraints on settlements within radio config boundaries
constrain_distance <- F

# Adjust inputs for applying constraints

major_cities <- c(
  # Zou
  # "Abomey",        # administrative capital
  "Bohicon",       # major economic center
  # "Djidja",       # where partner can hire enumerator
  
  # Couffo
  # "Aplahoué",      # administrative capital
  "Dogbo",         # major economic center
  "Klouékanmè",      # where partner can hire enumerator

  # # Ouémé
  # "Porto-Novo",    # administrative capital
  # "Adjarra",       # important urban area
  
  # Littoral
  "Cotonou",       # economic capital (only department)
  
  # Atlantique
  # "Allada",        # administrative capital
  # "Abomey-Calavi", # major economic/urban center
  # "Ouidah",        # important economic/tourism center
  
  # Plateau
  "Pobè",          # administrative capital
  # "Kétou",         # major economic center
  "Sakété",        # important urban center
  # "Infangni",      # where partner can hire enumerator
  
  # Collines
  # "Dassa-Zoumé",   # administrative capital
  # "Savè",          # major economic center
  
  # Donga
  "Djougou",       # administrative & largest city
  # "Bassila",       # secondary urban center
  
  # Borgou
  "Parakou"       # major economic center
  # "N’Dali",      # where partner can hire enumerator
  # "Ina"      # where partner can hire enumerator
  # "Nikki"          # administrative capital
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
  strata_description = c("North-East", "North-West", "South-South", "Mid-South"),
  n_clusters_target  = c(5, 2, 16, 7)*2 # multiply by 2 for replacements
)

# total clusters selected
n_cluster <- sum(alloc_df$n_clusters_target)

# --- Map each department/state into the chart strata (edit if your labels differ) ---
dept_to_strata <- tibble(
  admin_name = c("Borgou", "Donga", "Littoral", "Oueme", "Atlantique", "Couffo", "Zou", "Plateau"),
  strata_description = c("North-East", "North-West", "South-South", "South-South", "South-South",
                         "Mid-South", "Mid-South", "Mid-South")
)
