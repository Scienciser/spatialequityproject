# Functions for spatial analysis.



#' Generate a distance raster
#'
#' Calculates the distance to the nearest neighbour input point of each relevant cell.
#'
#' @param input_pts Candidates to be the nearest neighbour
#' @param rast_data: A list of three elements. `rast`, the empty global raster; `cell_nums` numeric[n], cell numbers of relevant cells (containing addresses) and `centroids_mat`, their centroids as x-y coords.
#'
#' @return The global raster, with the value of each cell being the distance to the nearest neighbour.
#' @importFrom rlang .data
#' @export
dist_raster <- function(input_pts, rast_data) {
  force(input_pts) # Prevent lazy eval calling cat() early
  cat("Generating distance raster...", sep = "\n")

  input_mat <- sf::st_coordinates(input_pts)
  rast <- rast_data$rast # Make copy of list element
  dists <- tibble::tibble(
    cell = rast_data$cell_nums,
    dist = nabor::knn(input_mat, rast_data$centroids_mat, k = 1)$nn.dists[,1]
  )

  complete_dists <- tibble::tibble(cell = 1:raster::ncell(rast)) %>%
    dplyr::left_join(dists, by = "cell") %>%
    tidyr::replace_na(list(dist = 0))

  raster::values(rast) <- complete_dists$dist

  return(rast)
}


#' Get area estimates for each cell
#'
#' Rasterise the feature class, and extract the approximate intersection area in a circle of radius `detection_distance` around
#' each address.
#'
#' @export
get_relevant_cell_area_estimate <- function(input_poly, rast_data, cell_size, detection_distance) {
  force(input_poly)
  cat("Generating area raster...", sep = "\n")
  rast <- rast_data$rast
  area_rast <- fasterize::fasterize(sf::st_sf(input_poly), rast)

  # Since `area_rast` cells are 1 for intersections and NA otherwise, their sum with a buffer of `detection_distance` is the number of
  # cells of area aproximately `cell_size^2` within `detection_distance`.
  out <- raster::extract(area_rast, rast_data$centroids_mat, buffer = detection_distance, fun = sum, na.rm = T)
  out <- dplyr::if_else(is.na(out), 0, out) # Where no parks are found, set to zero
  addresses_rast <- rast_data$rast
  raster::values(addresses_rast)[rast_data$cell_nums] <- out * cell_size^2

  return(addresses_rast)
}



#' Extract the raster value at each address.
#'
#' @export
join_raster_to_addresses <- function(rast, addresses) {
  force(rast) # Prevent lazy eval calling cat() early
  cat("Joining raster to addresses...", sep = "\n")
  raster::extract(rast, addresses, method = 'bilinear')
}



#' Aggregate statistics for each address by SA2
#'
#' @importFrom rlang .data
#' @export
summarise_addresses_by_sa2 <- function(addresses) {
  assertthat::assert_that("sa2_id" %in% colnames(addresses))
  addresses %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data$sa2_id) %>%
    dplyr::summarise(dplyr::across(!c(.data$address_id, .data$full_address_ascii), ~median(.x, na.rm = T)), .groups = "drop_last") %>%
    tidyr::drop_na() # Some SA2s don't contain addresses - lakes etc.
}



#' Round all data columns, and create quantile columns.
#'
#' Note that for points data, lower numbers corresponds to higher quantiles, since being closer is "better", but for area data, higher numbers correspond to higher quantiles, since more nearby area is "better".
#' @export
round_take_quantiles <- function(pts, cols_to_excl, join_cols, n = 100) {
  assertthat::assert_that(all(c(cols_to_excl, "parks") %in% colnames(pts)), msg = "Invalid column names")
  all_excl = c(cols_to_excl, "SHAPE", "geometry")
  quants <- pts %>%
    dplyr::mutate(dplyr::across(!dplyr::any_of(c(all_excl, "parks")), ~ 1 / n * dplyr::ntile(desc(.x), n))) %>%
    dplyr::mutate(parks = 1 / n * dplyr::ntile(.data$parks, n)) %>% # More parks = better
    dplyr::rename_with(~paste0(.x, "_quant"), .cols = !dplyr::any_of(all_excl))
  pts <- pts %>%
    dplyr::mutate(dplyr::across(!dplyr::any_of(all_excl), ~round(.x, digits = 0)))
  if(methods::is(pts, "sf")) {
    sf:::merge.sf(pts, sf::st_drop_geometry(quants), by = cols_to_excl)
  } else {
    dplyr::left_join(pts, quants, by = cols_to_excl)
  }
}
