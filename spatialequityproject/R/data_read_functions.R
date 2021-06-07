# Functions for reading and cleaning non-OSM data.

#' Generate raster data
#'
#' Establish which cells in the raster actually contain addresses, and determine their centroids.
#'
#' @param addresses Projected coordinates of all addresses
#' @param cell_size Size of cells to use in raster
#' @param bounds Boundaries of raster in projected coordinate system; must be a multiple of cell_size
#'
#' @return A list of three elements: `rast`, the empty global raster; `cell_nums`, cell numbers of relevant cells (containing addresses) and `centroids_mat`, their centroids as x-y coords.
#'
#' @export
generate_rast_data <- function(addresses, cell_size, bounds) {
  cat("Generating blank raster and relevant cell data...", sep = "\n")
  assertthat::assert_that(identical(unique(purrr::map_dbl(methods::slotNames(bounds), ~methods::slot(bounds, .x)) %% cell_size), 0),
              msg = "Raster bounds are not an exact multiple of cell size")

  rast_rows <- (bounds@ymax - bounds@ymin) / cell_size
  rast_cols <- (bounds@xmax - bounds@xmin) / cell_size
  rast <- raster::raster(ext = bounds, crs = sf::st_crs(addresses)$proj4string, nrows = rast_rows, ncols = rast_cols)

  rast_centroids <- rast # Make a copy of `rast` for centroid calculations
  raster::values(rast_centroids) = 1 # Must set a value for `extract` to work

  address_cells <- raster::extract(rast_centroids, addresses, cellnumbers = T)[, "cells"] %>%
    unique() %>%
    sort() %>%
    as.integer()

  raster::values(rast_centroids) = dplyr::if_else(1:raster::ncell(rast_centroids) %in% address_cells, 1L, NA_integer_)

  list(
    "rast" = rast,
    "cell_nums" = address_cells,
    "centroids_mat" = raster::rasterToPoints(rast_centroids)[,c("x", "y")]
  )
}
