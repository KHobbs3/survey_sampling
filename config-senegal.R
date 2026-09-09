# User input for scripts

###############################
# 1_preselection.R ----
###############################
country <- "senegal"

aerial_crs <- "epsg:2548" # specific to country

# File paths
radio_filepath <- "input/radio_configs/senegal"
# subfolder <- ""

# File names
settlement_polys <- "GRID3_SEN_settlement_extents_v3_0.gpkg"
settlement_points <- "sen_plpp_gov_ocha_09082017.shp"
boundary_file <- "geoBoundaries-SEN-ADM1_simplified.geojson"

# Columns
# boundary_cols <- c(
#                     "shapeName",
#                     "geometry"
#                   )


point_col_select <- c(
                    "Admin2Name",
                     "Admin4Name",
                     "FeatureNam",
                     "FID_sen_se",
                     "popPlace_1",
                     "geometry"
    )

point_rename_col <-  c(
  name     = "Admin2Name",
  place    = "popPlace_1",
  geom_id  = "FID_sen_se"
)

# Boolean: conduct constraints on settlements within radio config boundaries
constrain_distance <- T
if (constrain_distance == T){
  
  # Suffix for file export
  suffix <- "radio_constr"
} else {
  suffix <- "dist_constr_only"
}


# Adjust inputs for cities where enumerators will be hired.
major_cities <- c(
  # Dakar
  "Dakar",# "Pikine", "Guédiawaye", "Rufisque",
  
  # Thiès
  "Thies", "Mbour",# "Tivaouane", "Mékhé",
  
  # Diourbel
  "Diourbel",# "Touba", "Mbacké", "Diourbel", "Bambey",
  
  # Kaolack
  "Kaolack", #"Guinguinéo", "Nioro du Rip",
  
  # Fatick
  "Fatick" #,"Foundiougne", "Gossas", "Sokone"
)

# Adjust driving distnce radius from enumerator hiring city
city_radius <-75 # in kilometres


###############################
# 3_random_sample.R ----
###############################
n_desired_total <- 750
prop_qual <- 0.45 # typical proportion of population with Men and Women of reproductive age
p_available <- 0.20 # i assume x% of MWRA will be available on day of interview

# --- Cluster allocation from your chart (total should match n_cluster = 30) ---
alloc_df <- tibble(
  strata_description = c("Dakar", "Thies", "Kaolack", "Fatick", "Diourbel"),
  n_clusters_target  = c(11, 7, 4, 2, 6)*2 # multiply by 2 for replacements
)

# total clusters selected
n_cluster <- sum(alloc_df$n_clusters_target)

# # --- Map each department/state into the chart strata (for countries grouped into regions) ---
# dept_to_strata <- tibble(
#   admin_name = c("Dakar", "Thiès", "Kaolack", "Fatick", "Diourbel"),
#   strata_description = c("Dakar", "Thiès", "Kaolack", "Fatick", "Diourbel")
# )
