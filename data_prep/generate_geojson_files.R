library(tidyverse)
library(tigris)
library(geojsonsf)

options(tigris_use_cache = TRUE)

# Generate state GeoJSON
GEO_STATE <- states() %>%
  filter(STATEFP == "17") %>%
  select(-everything())

GEO_LAKEMICH <- tracts(state = "17", county = c("031", "097")) %>%
  filter(TRACTCE == "990000") %>%
  group_by(TRACTCE) %>%
  summarize()

GEO_STATE <- st_difference(GEO_STATE, GEO_LAKEMICH)  # Erase Lake Michigan

IL_GEOJSON <- sf_geojson(GEO_STATE, digits=4)
writeLines(IL_GEOJSON, paste0(getwd(), "/input/illinois.geojson"))

# Generate muni GeoJSON
GEO_PLACE <- places(state = "17") %>%
  filter(str_ends(NAMELSAD, " CDP", negate = TRUE)) %>%
  select(GEOID, NAME)

MUNI_GEOJSON <- sf_geojson(GEO_PLACE, digits=4)
writeLines(MUNI_GEOJSON, paste0(getwd(), "/input/municipalities.geojson"))
