# Wrapper functions for running the whole analysis process.



#' Process nearest points
#'
#' Wrapper function for point processing.
#' Generates a tibble of addresses giving distances to sets of points.
#'
#' @param osm_command Passed to `osmium_extract` to generate points feature class if `osm_extract` is TRUE.
#' @param osm_fields  Fields to read in from Osmium output if `osm_extract` is TRUE.
#' @param feature_name Name of feature
#' @param filter_func Optional function that takes in an sf of points of interest and returns a named list of filtered sfs to analyse individually.
#' @param osm_extract TRUE if Osmium is to be called, FALSE otherwise.
#' @param sf_to_process Points to calculate distance to if `osm_extract` is FALSE.
#' @param osmconf Path to temporary GDAL OSM driver conf file.
#' @param addresses sf of addresses to calculate nearest feature class point to.
#' @param rast_data Output of `generate_rast_data`
#' @param tmp_dir Location to store Osmium OSM output files.
#' @param osm_raw_input OpenStreetMap file for Osmium to filter
#' @param wkid WKID for projection
#'
#' @return
#' A list of two elements:
#'
#' `pts_list`: A single sf if `filter_func` is not defined, giving the points from OSM, or multiple sfs from `filter_func` if provided.
#'
#' `address_dists` : A tibble with order of `addresses`, with each col corresponding to a pts list and each val being the approx dist from that address to the nearest pt.
#' @export
process_nearest_pts <- function(osm_command = NULL,
                        osm_fields = "name",
                        feature_name,
                        filter_func = NULL,
                        osm_extract = TRUE,
                        sf_to_process = NULL,
                        osmconf,
                        addresses,
                        rast_data,
                        tmp_dir,
                        osm_raw_input,
                        wkid) {

  cat(paste0("Nearest points processing started for feature ", feature_name, ":"), sep = "\n\n")

  if (osm_extract) {
    assertthat::assert_that(!is.null(osm_command), msg = "A command must be supplied for OSM extraction")

    # Extract relevant points with Osmium
    output_path <- file.path(tmp_dir, paste0(feature_name, ".osm"))
    osmium_extract(osm_command, osm_raw_input, output_path)

    all_pts <- combine_pts_mpoly_project_centroids(output_path, wkid, osmconf, osm_fields)
  } else {
    assertthat::assert_that(!is.null(sf_to_process), msg = "An sf to process must be supplied if no OSM processing occurs.")
    assertthat::assert_that(sf::st_crs(sf_to_process) == sf::st_crs(wkid), msg = "The WKID of the sf to process must be the global WKID")
    all_pts <- sf_to_process
  }

  # Filter points into sets
  if (!is.null(filter_func)) {
    assertthat::assert_that(methods::is(filter_func, "function"))
    cat("Filtering points...", sep = "\n")
    pts_list <- filter_func(all_pts)
    assertthat::assert_that(methods::is(pts_list, "list"), msg = "Filter function did not return a list.")
    assertthat::assert_that(!any(is.null(names(pts_list))) && !any(names(pts_list) == ""), msg = "Filter function must name all elements.")
  } else {
    pts_list <- list(all_pts)
    names(pts_list) = feature_name
  }

  # For each set of points, calculate approximate distance to addresses.
  address_dists <- purrr::map2(pts_list, names(pts_list), function(pts, set_name) {
    cat(paste0('Calculating distances for set "', set_name, '"...'), sep = "\n")
    pts %>%
      dist_raster(rast_data) %>%
      join_raster_to_addresses(addresses)
  }) %>%
    tibble::as_tibble()

  cat(paste("Processing of feature", feature_name, "is complete."), sep = "\n\n")

  return(list("pts_list" = pts_list,
              "address_dists" = address_dists))
}



#' Wrapper function for area processing
#'
#' Generates a tibble of addresses, each with the area of the intersection of a feature class extracted from OSM and a circle of radius `detection_distance` around the address.
#'
#' @param osm_command Passed to `osmium_extract` to generate points feature class if `osm_extract` is TRUE.
#' @param osm_fields  Fields to read in from Osmium output if `osm_extract` is TRUE.
#' @param feature_name Name of feature
#' @param detection_distance Radius of overlap area to calculate intersection.
#' @param addresses sf of addresses to calculate nearest feature class point to.
#' @param rast_data Output of `generate_rast_data`
#' @param tmp_dir Location to store Osmium OSM output files.
#' @param osm_raw_input OpenStreetMap file for Osmium to filter
#' @param wkid WKID for projection
#' @param osmconf Path to temporary GDAL OSM driver conf file.
#' @param cell_size Size of raster cells.
#' @param sa2 SA2 sf file.
#'
#' @return
#' A list of two elements:
#'
#' `combined_ways`: A single sf containing all areas extracted.
#'
#' `address_areas` : A single-column tibble with order of `addresses` of approximate areas of overlap with feature class around each address.
#'
#' @export
process_adjacent_area <- function(osm_command,
                                  osm_fields = "name",
                                  feature_name,
                                  detection_distance,
                                  addresses,
                                  rast_data,
                                  tmp_dir,
                                  osm_raw_input,
                                  wkid,
                                  osmconf,
                                  cell_size,
                                  sa2) {

  cat(paste0("Ways area processing started for feature ", feature_name, ":"), sep = "\n\n")

  # Extract relevant points with Osmium
  output_path <- file.path(tmp_dir, paste0(feature_name, ".osm"))
  osmium_extract(osm_command, osm_raw_input, output_path)

  combined_ways <- combine_project_ways(output_path, wkid, osmconf, osm_fields, sa2)

  address_areas <- get_relevant_cell_area_estimate(combined_ways, rast_data, cell_size, detection_distance) %>%
    join_raster_to_addresses(addresses) %>%
    tibble::as_tibble_col(column_name = feature_name)

  cat(paste("Processing of feature", feature_name, "is complete."), sep = "\n\n")

  return(list("combined_ways" = combined_ways,
              "address_areas" = address_areas))
}


#' GDB write
#' Write output to an Esri Geodatabase
#' @export
gdb_write <- function(x, name, gdb, out_dir) {
  arcgisbinding::arc.write(file.path(out_dir, gdb, name), data = x, validate = T)
}
