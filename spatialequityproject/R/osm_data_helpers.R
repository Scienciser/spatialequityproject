# Functions for working with Osmium and cleaning OpenStreetMap data.

#' Osmium extraction
#'
#' Uses the osmium tool to extract relevant data from an OSM dump. Osmium isn't properly supported on Windows, so this function calls Osmium through WSL 2.
#' Ensure that you have a WSL distro with osmium installed as your default distro.
#'
#' @param osm_command Inserted after `tag-filter` in the Osmium command.
#' @param input_path Path to OSM input file
#' @param output_path Path to Osmium output file
#' @param print_output If TRUE, print the Osmium output to console
#'
#' @export
osmium_extract <- function(osm_command, input_path, output_path, print_output = F) {
  cat("Running Osmium extraction...", sep = "\n")
  assertthat::assert_that(length(osm_command) == 1)

  # Passing "" to the `stdout` or `stderr` params of `system2` prints the output to console.
  sys2_output <- ifelse(print_output, "", FALSE)

  if (Sys.info()['sysname'] == "Windows") {
    osmium_check <- system2("wsl", c("osmium", "--version"), stdout = T, stderr = T)
    assertthat::assert_that(stringr::str_detect(osmium_check[1], "^osmium version 1"), msg = "Osmium is not configured correctly in WSL")
    input_path <- generate_wsl2_path(input_path)
    output_path <- generate_wsl2_path(output_path)
  }

  args <- c("tags-filter", input_path, "--remove-tags", "--verbose", "--overwrite", osm_command, "-o", output_path)

  if (Sys.info()['sysname'] == "Windows") {
    output <- system2("wsl", c("osmium", args), stdout = sys2_output, stderr = sys2_output)
  } else {
    output <- system2("osmium", args, stdout = sys2_output, stderr = sys2_output)
  }
  assertthat::assert_that(output == 0, msg = paste("Osmium has failed with error code", output))

  return(invisible((NULL)))
}


#' Generate a WSL2 path
#'
#' For Windows only, generate a WSL-friendly path (`/mnt/<drive>/...`)
#'
#' @export
generate_wsl2_path <- function(rel_path) {
  abs_path <- normalizePath(rel_path, winslash = "/", mustWork = F)
  assertthat::assert_that(stringr::str_detect(abs_path, "^[A-Z]:/\\w"), msg = "Failed to parse path.")
  file.path("/mnt", tolower(stringr::str_sub(abs_path, 1,1)), stringr::str_sub(abs_path, 4))
}


#' Modify OSM driver
#'
#' Update GDAL OSM INI file to determine which tags to use as columns
#' @param input_tags One of a character vector of tags to be applied to all sections, a named list of character vectors, with each name corresponding to a section, or `NULL`, to reset the INI file to default.
#'
#' @export
gdal_osm_mod_tmp_ini <- function(conf_path, input_tags = NULL) {
  # Create temporary osmconf.ini file in the extracts folder
  if (!file.exists(conf_path)) {
    osmconf_ini_path <- file.path(find.package("sf"), "gdal/osmconf.ini")
    file.copy(osmconf_ini_path, conf_path)
  }

  sections <- c("points", "lines", "multipolygons", "multilinestrings", "other_relations")
  if (typeof(input_tags) == "list") {
    assertthat::assert_that(
      all(sections %in% names(input_tags)),
      msg = "Sections missing from tag list")
    tags <- lapply(input_tags, paste, collapse = ",")
  }
  else if (typeof(input_tags) == "character") {
    tags <- list()
    tags[sections] <- list(paste(input_tags, collapse = ","))
  }
  # Reset INI to default
  else if (typeof(input_tags) == "NULL") {
    tags <- list(
      "points" = "name,barrier,highway,ref,address,is_in,place,man_made",
      "lines" = "name,highway,waterway,aerialway,barrier,man_made",
      "multipolygons" = "name,type,aeroway,amenity,admin_level,barrier,boundary,building,craft,geological,historic,land_area,landuse,leisure,man_made,military,natural,office,place,shop,sport,tourism",
      "multilinestrings" = "name,type",
      "other_relations" = "name,type"
    )
  }
  else {
    stop("Invalid input")
  }

  osm_conf_ini <- readLines(conf_path)
  section_titles <- paste0("[", sections, "]")
  curr_section <- NULL
  for (i in seq_along(osm_conf_ini)) {
    if (osm_conf_ini[i] %in% section_titles) {
      curr_section <- substr(osm_conf_ini[i], 2, nchar(osm_conf_ini[i]) - 1)
    }
    if (stringr::str_detect(osm_conf_ini[i], "^attributes=*")) {
      osm_conf_ini[i] <- paste0("attributes=", tags[curr_section])
    }
  }
  writeLines(osm_conf_ini, conf_path)

  return(invisible(NULL))
}


#' Get OSM summary
#'
#' Parses each layer of an OSM file and prints the number of rows and tag availability statistics.
#'
#' @param input_path Path to the OSM file
#' @param tmp_dir Path to store temporary OSM GDAL driver config.
#'
#' @export
get_osm_summary <- function(input_path, tmp_dir = "tmp") {
  osm_layers <- c("points", "lines", "multilinestrings", "multipolygons", "other_relations")
  layers <- sf::st_layers(input_path)
  assertthat::assert_that(all(layers[[1]] == osm_layers))

  tmp_osmconf <- file.path(tmp_dir, "osmconf_func.ini")
  if (!file.exists(tmp_osmconf)) {
    osmconf_ini_path <- file.path(find.package("sf"), "gdal/osmconf.ini")
    file.copy(osmconf_ini_path, tmp_osmconf)
  }
  tmp_sf_osmconf <- paste0("CONFIG_FILE=", file.path(tmp_dir, "osmconf_func.ini"))
  # Force all tags to other_tags column
  gdal_osm_mod_tmp_ini(tmp_osmconf, character())

  for (layer in osm_layers) {
    layer_sf <- sf::st_read(input_path, layer= layer, options = tmp_sf_osmconf, quiet = T)


    if (nrow(layer_sf) == 0) {
      message(paste0("Layer \"", layer, "\" is empty.\n"))
      next
    }

    # Extract all tags with "=>"
    tags <- layer_sf %>%
      dplyr::pull(.data$other_tags) %>%
      stringr::str_extract_all('"\\w+"=>') %>%
      purrr::flatten_chr() %>%
      unique()
    nice_tags <- stringr::str_extract(tags, "\\w+")
    names(tags) <- nice_tags

    detected <- tags %>%
      purrr::map_dfc(~ str_detect(layer_sf$other_tags, .x))

    if (length(tags) > 10)
      message(paste0("Layer \"", layer, "\" includes ", nrow(layer_sf), " objects and ", length(tags), " tags. Top 10 tags:"))
    else
      message(paste0("Layer \"", layer, "\" includes ", nrow(layer_sf), " objects and ", length(tags), " tags:"))

    tibble::tibble(tag = nice_tags, count = colSums(detected)) %>%
      dplyr::arrange(dplyr::desc(.data$count)) %>%
      utils::head(10) %>%
      dplyr::mutate(availability = scales::percent(.data$count / nrow(layer_sf), accuracy = 0.1)) %>%
      print()

    cat("\n")
  }
  file.remove(tmp_osmconf)
  return(invisible(NULL))

}



#' Combine points and multipolygons
#'
#' Combine the `multipolygon` and `points` layers of an OSM layer by taking the multipolygon centroids.
#' @param osm_path Path to Osmium output file
#' @param wkid WKID to project OSM data with
#' @param osmconf Path to temporary OSM GDAL configuration file.
#' @param osm_fields Fields to extract from OSM data.
#'
#' @export
combine_pts_mpoly_project_centroids <- function(osm_path, wkid, osmconf, osm_fields) {
  cat("Generating centroids from ways...", sep = "\n")
  gdal_osm_mod_tmp_ini(osmconf, osm_fields)
  pts <- sf::st_read(osm_path, layer= "points", options = paste0("CONFIG_FILE=", osmconf), quiet = T) %>%
    sf::st_transform(wkid)
  mpoly <- sf::st_read(osm_path, layer= "multipolygons", options = paste0("CONFIG_FILE=", osmconf), quiet = T) %>%
    sf::st_transform(wkid) %>%
    dplyr::select(-.data$osm_id) %>%
    sf::st_centroid(mpoly) %>%
    dplyr::bind_rows(pts) %>%
    dplyr::select(-.data$other_tags)
}



#' Project and combine all ways into a single multipolygon.
#'
#' @param osm_path Path to Osmium output file
#' @param wkid WKID to project OSM data with
#' @param osmconf Path to temporary OSM GDAL configuration file.
#' @param osm_fields Fields to extract from OSM data.
#'
#' @export
combine_project_ways <- function(osm_path, wkid, osmconf, osm_fields, sa2) {
  cat("Combining and projecting ways...", sep = "\n")
  gdal_osm_mod_tmp_ini(osmconf, osm_fields)
  ways <- sf::st_read(osm_path, layer = "multipolygons", options = paste0("CONFIG_FILE=", osmconf), quiet = T) %>%
    sf::st_transform(wkid) %>%
    # Handle invalid polygons due to self-intersection
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>% # Get rid of any stray lines that were created by validation
    sf::st_union()

  # Remove any bodies of water.
  sa2 %>%
    sf::st_union() %>%
    sf::st_intersection(ways)
}
