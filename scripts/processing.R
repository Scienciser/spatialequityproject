# Script used to generate sample data on "Spatial Equity Aotearoa" website.

###############
# ENVIRONMENT #
###############

library(spatialequityproject)
source("./scripts/data_cleaning.R")

library(arcgisbinding)
arc.check_product()
# Strange workaround for strange arcgisbinding bug breaking PATH
Sys.setenv(PATH = Sys.getenv("PATH"))




######################
#  GLOBAL VARIABLES  #
######################

global_osm_raw_input <- "data/osm_pbf/new-zealand-latest.osm.pbf"
global_sa2_path <- "data/gis/statsnzstatistical-area-2-2018-clipped-generalised-FGDB/statistical-area-2-2018-clipped-generalised.gdb"
global_addresses_path <- "data/gis/lds-nz-street-address-FGDB/nz-street-address.gdb"
global_schools_path <- "data/moe/schooldirectory-24-05-2021-083017.csv"
global_tmp_dir <- "tmp"
global_output_gdb <- "project_output.gdb"
global_osmconf <- file.path(global_tmp_dir, "osmconf.ini")
global_sf_osmconf <- paste0("CONFIG_FILE=", global_osmconf)

global_wkid <- 2193
global_pt_cell_size = 100
global_area_cell_size = 100
# Derived from boundaries of SA2 shapefile with NZGD 2000 Transverse Mercator (WKID:2193)
global_bounds = raster::extent(c(1089000, 2471000, 4747000, 6234000))



###############
# GLOBAL DATA #
###############

global_sa2 <- get_sa2(global_sa2_path, id_col = SA22018_V1_00, name_col = SA22018_V1_00_NAME)
global_addresses <- get_addresses_with_sa2(global_addresses_path,
                                           global_sa2,
                                           id_col = address_id,
                                           full_address_col = full_address_ascii,
                                           wkid = global_wkid)

global_schools <- get_schools(global_schools_path, wkid = global_wkid)

global_rast_pt_data <- generate_rast_data(global_addresses, global_pt_cell_size, global_bounds)
global_rast_area_data <- generate_rast_data(global_addresses, global_area_cell_size, global_bounds)



##############
# PROCESSING #
##############

libraries_list <- process_nearest_pts(
  osm_command = "nw/amenity=library",
  osm_fields = "name",
  feature_name = "libraries",
  osmconf = global_osmconf,
  addresses = global_addresses,
  rast_data = global_rast_pt_data,
  tmp_dir = global_tmp_dir,
  osm_raw_input = global_osm_raw_input,
  wkid = global_wkid)

fast_food_list <- process_nearest_pts(
  osm_command = "nw/amenity=fast_food",
  osm_fields = c("name", "brand"),
  feature_name = "fast_food",
  filter_func = fast_food_filter,
  osmconf = global_osmconf,
  addresses = global_addresses,
  rast_data = global_rast_pt_data,
  tmp_dir = global_tmp_dir,
  osm_raw_input = global_osm_raw_input,
  wkid = global_wkid)

supermarkets_list <- process_nearest_pts(
  osm_command = "nw/shop=supermarket",
  osm_fields = c("name", "brand"),
  feature_name = "supermarkets",
  filter_func = supermarkets_filter,
  osmconf = global_osmconf,
  addresses = global_addresses,
  rast_data = global_rast_pt_data,
  tmp_dir = global_tmp_dir,
  osm_raw_input = global_osm_raw_input,
  wkid = global_wkid)

schools_list <- process_nearest_pts(
  sf_to_process = global_schools,
  feature_name = "schools",
  filter_func = schools_filter,
  osm_extract = FALSE,
  osmconf = global_osmconf,
  addresses = global_addresses,
  rast_data = global_rast_pt_data,
  tmp_dir = global_tmp_dir,
  osm_raw_input = global_osm_raw_input,
  wkid = global_wkid)

parks_list <- process_adjacent_area(
  osm_command = "wr/leisure=park,nature_reserve wr/boundary=protected_area,national_park",
  feature_name = "parks",
  detection_distance = 1000,
  addresses = global_addresses,
  rast_data = global_rast_area_data,
  tmp_dir = global_tmp_dir,
  osm_raw_input = global_osm_raw_input,
  wkid = global_wkid,
  osmconf = global_osmconf,
  cell_size = global_area_cell_size,
  sa2 = global_sa2)



###############
# AGGREGATION #
###############

addresses <- bind_cols(global_addresses,
                       libraries_list$address_dists,
                       fast_food_list$address_dists,
                       supermarkets_list$address_dists,
                       schools_list$address_dists,
                       parks_list$address_areas)

sa2_tbl <- summarise_addresses_by_sa2(addresses) %>%
  round_take_quantiles(cols_to_excl = "sa2_id")

addresses_out <- addresses %>%
  round_take_quantiles(cols_to_excl = c("address_id", "full_address_ascii", "sa2_id"))

sa2_out <- global_sa2 %>%
  dplyr::left_join(sa2_tbl, by = "sa2_id")



##########
# EXPORT #
##########

gdb_write(addresses_out, "spatial_equity_by_address", gdb = global_output_gdb, out_dir = global_tmp_dir)
gdb_write(sa2_out, "spatial_equity_by_sa2", gdb = global_output_gdb, out_dir = global_tmp_dir)
