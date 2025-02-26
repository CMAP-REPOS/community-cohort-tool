
library(sf)
library(tidyverse)

new_edas <- read_sf("T:\\ab\\community-cohort-tool\\update_DAs\\new_edas.gpkg") %>% select(GEOID) %>% mutate(DA = 1)
tracts <- read_sf("V:\\Demographic_and_Forecast\\Census\\2020\\dhc_tract_2020.shp") %>% select(GEOID, POP2020)

old_edas <- read_sf(dsn = "C:\\Users\\abahls\\Documents\\ArcGIS\\Projects\\gis_sandbox\\gis_sandbox.gdb",
                    layer = "old_das")

old_das_2 <- old_edas %>% mutate(old_da = 1, GEOID = as.character(GEOID10)) %>% select(old_da, GEOID)

total_pop_2020 <- tracts %>%
  left_join(new_edas %>% as_tibble() %>% select(!geom)) %>%
  filter(DA == 1)

sum(total_pop_2020$POP2020)

old_tracts <- read_sf("V:\\Demographic_and_Forecast\\Census\\2010\\Tracts_CensusSF1_2010.shp") %>% select(GEOID10) %>%
  st_transform(cmapgeo::cmap_crs)

blocks <- read_sf("V:\\Demographic_and_Forecast\\Census\\2020\\dhc_blocks_2020.shp") %>% select(GEOID, POP2020) %>%
  st_transform(cmapgeo::cmap_crs)

interp <- st_interpolate_aw(
  x = blocks,
  to = old_tracts,
  extensive = T
)

interp_new <- old_tracts %>%
  as_tibble() %>%
  left_join(interp %>% as_tibble(), by = "geometry")

interp_new_das <- interp_new %>%
  left_join(old_das_2 %>% as_tibble(), by = c("GEOID10" = "GEOID"))

just_old_das <- interp_new_das %>%
  filter(old_da == 1)


sum(total_pop_2020$POP2020)
sum(just_old_das$POP2020)

