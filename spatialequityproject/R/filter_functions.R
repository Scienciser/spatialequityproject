# Functions for filtering OSM data into groups for separate analysis and aggregation.



# Helper function for filtering by name or brand.
#' @importFrom rlang .data
#' @export
name_brand_ext <- function(pts, search_term) {
  pts %>%
    dplyr::filter(stringr::str_detect(tolower(.data$name), search_term) | stringr::str_detect(tolower(.data$brand), search_term))
}



# Filter function for use with fast food data in `process_nearest_pts`
# Data quality notes - not entirely complete
# OSM is missing:
# - St Lukes foodcourt
# - Avondale Ash Street (manually added)
# - Hamilton Central Mall is missing, but food court as a whole is "amenity.fast_food"
# - Pizza data is a bit flaky.
#' @export
fast_food_filter <- function(pts) {
  mcdonalds <- name_brand_ext(pts, "mcdonald")
  burger_king <- name_brand_ext(pts, "burger king")
  kfc <- name_brand_ext(pts, "kfc")
  wendys <- name_brand_ext(pts, "wendys|wendy's")
  pizza_hut <- name_brand_ext(pts, "pizza hut")
  dominos <- name_brand_ext(pts, "dominos|domino's")

  major_chains <- dplyr::bind_rows(mcdonalds, burger_king, kfc, wendys, pizza_hut, dominos)

  list("ff_all" = pts,
       "ff_major_chains" = major_chains,
       "ff_mcdonalds" = mcdonalds)
}



# Filter function for use with schools data in `process_nearest_pts`
#' @importFrom rlang .data
#' @export
schools_filter <- function(pts) {
  list("sc_all" = pts,
       "sc_primary" = dplyr::filter(pts, .data$primary),
       "sc_intermediate" = dplyr::filter(pts, .data$intermediate),
       "sc_secondary" = dplyr::filter(pts, .data$secondary))
}



# Filter function for use with supermarkets data in `process_nearest_pts`
#' @export
supermarkets_filter <- function(pts) {
  countdown <- name_brand_ext(pts, "countdown")
  new_world <- name_brand_ext(pts, "new world")
  paknsave <- name_brand_ext(pts, "pak.*save") # This name gets really mangled
  supervalue <- name_brand_ext(pts, "supervalue|super value")
  fresh_choice <- name_brand_ext(pts, "fresh choice|freshchoice")
  four_square <- name_brand_ext(pts, "four square|foursquare")
  list("sm_all" = pts,
       "sm_large_major_chains" = dplyr::bind_rows(countdown, new_world, paknsave),
       "sm_major_chains" = dplyr::bind_rows(countdown, new_world, paknsave, supervalue, fresh_choice, four_square))
}
