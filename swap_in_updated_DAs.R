library(tidyverse)
library(sf)
library(tidycensus)
library(readxl)

# load_data ---------------------------------------------------------------

blocks <- read_sf("V:\\Demographic_and_Forecast\\Census\\2020\\dhc_blocks_2020.shp") %>% select(GEOID, POP2020)
tracts <- read_sf("V:\\Demographic_and_Forecast\\Census\\2020\\dhc_tract_2020.shp") %>% select(GEOID)
munis <- read_sf("V:\\Administrative_and_Political_Boundaries\\Municipalities\\Muni_CMAP_Current.gdb",
                  layer = "MunisDissolved_2024") %>%
  select(muni, starts_with("geo"))
ccas <- read_sf("V:\\Administrative_and_Political_Boundaries\\Miscellaneous\\ChicagoCommunityAreas_Chicago_202309.shp") %>%
  select(area_number = area_numbe, community)

###UPDATE path
new_edas <- read_sf("T:\\ab\\community-cohort-tool\\update_DAs\\new_edas.gpkg") %>% select(GEOID) %>% mutate(DA = 1)



tracts_u <- tracts %>%
  left_join(new_edas %>% as_tibble() %>% select(!geom)) %>%
  mutate(DA = ifelse(is.na(DA),0,DA)) %>%
  distinct()


muni_eda_merge <- interpolate_pw(
  from = tracts_u,
  to = munis,
  to_id = "geoid_7",
  extensive = F,
  weights = blocks,
  weight_column = "POP2020",
  crs = cmapgeo::cmap_crs
)

new_input_muni <- muni_eda_merge %>%
  as_tibble() %>%
  select(geoid_7, pct_eda_new = DA) %>%
  mutate(pct_eda_new = ifelse(geoid_7 == "1707640",0,pct_eda_new))

cca_eda_merge <- interpolate_pw(
  from = tracts_u,
  to = ccas,
  to_id = "area_number",
  extensive = F,
  weights = blocks,
  weight_column = "POP2020",
  crs = cmapgeo::cmap_crs
)

new_input_cca <- cca_eda_merge %>%
  as_tibble() %>%
  select(area_number, pct_eda_new = DA) %>%


# FACTORS_MUNI
# FACTORS_CCA



update_input <- function(year){

  input_path <- paste0("input\\community_cohort_inputs_",year,"_old.xlsx")

  df_muni <- read_excel(input_path, sheet = "FACTORS_MUNI") %>%
    mutate(GEOID = as.character(GEOID)) %>%
    left_join(new_input_muni, by = c("GEOID" = "geoid_7")) %>%
    mutate(PCT_DA_POP = pct_eda_new) %>%
    select(!pct_eda_new)

  df_cca <- read_excel(input_path, sheet = "FACTORS_CCA") %>%
    mutate(CCA_ID = as.character(CCA_ID)) %>%
    left_join(new_input_cca, by = c("CCA_ID" = "area_number")) %>%
    mutate(PCT_DA_POP = pct_eda_new) %>%
    select(!pct_eda_new)

  output_path <- paste0("input\\community_cohort_inputs_",year,".xlsx")

  weights <-  read_excel(input_path, sheet = "WEIGHTS")
  cohorts <-  read_excel(input_path, sheet = "COHORTS")


  writexl::write_xlsx(list(FACTORS_CCA = df_cca, FACTORS_MUNI = df_muni, WEIGHTS = weights, COHORTS = cohorts), output_path)

}

update_input(2023)
update_input(2024)
update_input(2025)
