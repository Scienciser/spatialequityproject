library(tidyverse)
library(sf)

# Read in SA2 data from file.

get_sa2 <- function(sa2_path, id_col, name_col) {
  cat("Reading in SA2 GDB...", sep = "\n")
  st_read(sa2_path, quiet = T) %>%
    transmute(sa2_id = {{id_col}}, sa2_name = {{name_col}})
}



# Read in address data from file and join on SA2 data

get_addresses_with_sa2 <- function(addresses_path, sa2, id_col, full_address_col, wkid) {
  cat("Reading in addresses GDB...", sep = "\n")
  addresses <- st_read(addresses_path, quiet = T) %>%
    select({{id_col}}, {{full_address_col}})

  cat("Joining SA2 to addresses...", sep = "\n")
  addresses <- addresses %>%
    st_transform(global_wkid, quiet = T) %>%
    st_join(select(sa2, sa2_id))

  # Some addresses (particularly coastal) are outside any SA2, just ignore these.
  if (anyNA(addresses$sa2_id)) {
    message(paste("Warning:", sum(is.na(addresses$sa2_id)), "addresses do not match an SA2, dropping..."))
    addresses <- filter(addresses, !is.na(sa2_id))
  }
}




# Primary = Year 1-6
# Intermediate = Year 7-8
# Secondary = Year 9-13

# MOE defs:
# Full Primary = Year 1-8
# Contributing = Year 1-6
# Composite = Year 1-13

# For simplicity, I'm treating `Secondary (Year 7-10)` as "Intermediate" and `Composite (Year 1-10)` as "Primary"/"Intermediate"

get_schools <- function(schools_path, wkid = global_wkid) {
  spec <- cols(
    School_Id = col_double(),
    Latitude = col_double(),
    Longitude = col_double(),
    Decile = col_double(),
    Total = col_double(),
    .default = col_character()
  )

  schools <- read_csv(schools_path, col_types = spec) %>%
    transmute(id = School_Id,
              name = Org_Name,
              city = Add1_City,
              type = Org_Type,
              lat = Latitude,
              lon = Longitude,
              roll = Total,
              decile = Decile) %>%
    filter(roll > 0, !(type %in% c("Correspondence School", "Special School"))) %>%
    mutate(primary = type %in% c("Full Primary",
                                 "Contributing",
                                 "Composite",
                                 "Composite (Year 1-10)"),
           intermediate = type %in% c("Full Primary",
                                      "Composite",
                                      "Intermediate",
                                      "Secondary (Year 7-15)",
                                      "Restricted Composite (Year 7-10)",
                                      "Secondary (Year 7-10)",
                                      "Composite (Year 1-10)"),
           secondary = type %in% c("Secondary (Year 9-15)",
                                   "Composite",
                                   "Secondary (Year 7-15)",
                                   "Secondary (Year 11-15)"))

  # Fix errors in Chatham Islands school data
  schools[schools$city == "Chatham Islands", "lon"] = abs(schools[schools$city == "Chatham Islands", "lon"])

  schools %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # WGS 1984
    st_transform(wkid)
}


