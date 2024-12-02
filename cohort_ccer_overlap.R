library(sf)
library(tidyverse)
library(janitor)
library(readxl)

eda_muni <- read_csv("output\\1yr\\cohort_assignments_muni_1yr_2024.csv") |> select(MUNI, COHORT, GEOID) |> mutate(GEOID = as.character(GEOID))
eda_cca <- read_csv("output\\1yr\\cohort_assignments_cca_1yr_2024.csv")|> select(CCA_NAME, COHORT, CCA_ID) |> mutate(CCA_ID = as.character(CCA_ID))

munis_shp <- read_sf(dsn = "V:Administrative_and_Political_Boundaries\\Municipalities\\Muni_CMAP_Current.gdb",
                     layer = "MunisDissolved_2024")

cca_shp <- read_sf("V:Administrative_and_Political_Boundaries\\Miscellaneous\\ChicagoCommunityAreas_Chicago_202309.shp")


eda_muni_shape <- munis_shp |>
  left_join(eda_muni, by = c("geoid_7" = "GEOID"))

eda_cca_shape <- cca_shp |>
  left_join(eda_cca, by = c("area_numbe" = "CCA_ID"))

#ccer / cohort overlap

clusters <- read_sf(dsn = "S:\\Projects\\CCER\\kmeans_10clust_No_EffRate_New_ClusterID_V2_040924.gpkg") |>
  select(geoid_tr, CLUSTER_ID)


muni_intersection <- eda_muni_shape %>%
  st_intersection(clusters) %>%
  mutate(overlap_area = as.numeric(st_area(.))) %>%
  group_by(MUNI) %>%
  mutate(muni_area = sum(overlap_area)) %>%
  group_by(MUNI, CLUSTER_ID)  %>%
  mutate(muni_cluster_area = sum(overlap_area)) %>%
  ungroup() %>%
  mutate(percent_area_in_cluster = muni_cluster_area / muni_area) %>%
  as_tibble()  %>%
  distinct(MUNI, CLUSTER_ID, percent_area_in_cluster, COHORT) |>
  mutate(percent_area_in_cluster = case_when(percent_area_in_cluster < 0.01 ~ 0,
                                             percent_area_in_cluster > 0.99 ~ 100,
                                             T ~ round(percent_area_in_cluster,3)*100)) |>
  filter(!(percent_area_in_cluster == 0 & CLUSTER_ID != 1))

cohort_intersection_muni <- muni_cohorts %>%
  st_intersection(clusters) %>%
  mutate(overlap_area = as.numeric(st_area(.))) %>%
  group_by(COHORT) %>%
  mutate(cohort_area = sum(overlap_area)) %>%
  group_by(COHORT, CLUSTER_ID)  %>%
  mutate(cohort_cluster_area = sum(overlap_area)) %>%
  ungroup() %>%
  mutate(percent_area_in_cohort = cohort_cluster_area / cohort_area) %>%
  as_tibble() %>%
  distinct(COHORT, CLUSTER_ID, percent_area_in_cohort) |>
  mutate(percent_area_in_cohort = case_when(percent_area_in_cohort < 0.01 ~ 0,
                                             percent_area_in_cohort > 0.99 ~ 100,
                                             T ~ round(percent_area_in_cohort,3)*100)) |>
  filter(!(percent_area_in_cohort == 0 & CLUSTER_ID != 1)) |>
  pivot_wider(id_cols = CLUSTER_ID, names_from = COHORT, values_from = percent_area_in_cohort) |>
  clean_names() |>
  select(cluster_id, cohort_1 = x1, cohort_2 = x2, cohort_3 = x3, cohort_4 = x4) |>
  mutate(across(cohort_1:cohort_4, \(x) coalesce(x, 0)))


cca_intersection <- cca_cohorts %>%
  st_intersection(clusters) %>%
  mutate(overlap_area = as.numeric(st_area(.))) %>%
  group_by(community) %>%
  mutate(cca_area = sum(overlap_area)) %>%
  group_by(community, CLUSTER_ID)  %>%
  mutate(cca_cluster_area = sum(overlap_area)) %>%
  ungroup() %>%
  mutate(percent_area_in_cluster = cca_cluster_area / cca_area) %>%
  as_tibble()  %>%
  distinct(community, CLUSTER_ID, percent_area_in_cluster, COHORT) |>
  mutate(percent_area_in_cluster = case_when(percent_area_in_cluster < 0.01 ~ 0,
                                             percent_area_in_cluster > 0.99 ~ 100,
                                             T ~ round(percent_area_in_cluster,3)*100)) |>
  filter(!(percent_area_in_cluster == 0 & CLUSTER_ID != 1))

cohort_intersection_cca <- cca_cohorts %>%
  st_intersection(clusters) %>%
  mutate(overlap_area = as.numeric(st_area(.))) %>%
  group_by(COHORT) %>%
  mutate(cohort_area = sum(overlap_area)) %>%
  group_by(COHORT, CLUSTER_ID) %>%
  mutate(cohort_cluster_area = sum(overlap_area)) %>%
  ungroup() %>%
  mutate(percent_area_in_cohort = cohort_cluster_area / cohort_area) %>%
  as_tibble()  %>%
  distinct(COHORT, CLUSTER_ID, percent_area_in_cohort) |>
  mutate(percent_area_in_cohort = case_when(percent_area_in_cohort < 0.01 ~ 0,
                                            percent_area_in_cohort > 0.99 ~ 100,
                                            T ~ round(percent_area_in_cohort,3)*100)) |>
  filter(!(percent_area_in_cohort == 0 & CLUSTER_ID != 1)) |>
  pivot_wider(id_cols = CLUSTER_ID, names_from = COHORT, values_from = percent_area_in_cohort) |>
  clean_names() |>
  select(cluster_id, cohort_1 = x1, cohort_2 = x2, cohort_3 = x3, cohort_4 = x4) |>
  mutate(across(cohort_1:cohort_4, \(x) coalesce(x, 0)))


ccer_muni <- read_excel("muni_ccer.xlsx") |> mutate(COHORT = as.numeric(COHORT))
ccer_cca <- read_excel("cca_ccer.xlsx") |> mutate(COHORT = as.numeric(COHORT))

# ccer_muni_6_7 <- read_excel("muni_ccer_6_7.xlsx") |> mutate(COHORT = as.numeric(COHORT))
# ccer_cca_6_7 <- read_excel("cca_ccer_6_7.xlsx") |> mutate(COHORT = as.numeric(COHORT))

eda_muni <- read_csv("output\\1yr\\cohort_assignments_muni_1yr_2024.csv") |> mutate(GEOID = as.character(GEOID), COHORT = as.numeric(COHORT))
eda_cca <- read_csv("output\\1yr\\cohort_assignments_cca_1yr_2024.csv") |> mutate(CCA_ID = as.character(CCA_ID))

# table(ccer_cca_6_7$COHORT)
table(ccer_cca$COHORT)
table(eda_cca$COHORT)

# table(ccer_muni_6_7$COHORT)
table(ccer_muni$COHORT)
table(eda_muni$COHORT)

muni_join <- ccer_muni |>
  select(GEOID, cohort_ccer = COHORT) |>
  left_join(eda_muni |> select(GEOID, MUNI, cohort_eda = COHORT)) |>
  # left_join(ccer_muni_6_7 |>  select(GEOID, cohort_6_7 = COHORT))
  mutate(abs_diff = abs(cohort_eda - cohort_ccer),
         diff = cohort_eda - cohort_ccer,
         category = case_when(
           diff == 0 ~ "Same",
           diff < 0 ~ "lower need in eda",
           diff > 0 ~ "higher need in eda"
         )) |>
  select(MUNI, cohort_ccer_3678 = cohort_ccer, cohort_eda, diff, category)

# big_diff_muni <- muni_join |>
#   filter(abs_diff > 1)

cca_join <- ccer_cca |>
  select(CCA_ID, CCA_NAME, ccer_cohort = COHORT) |>
  left_join(eda_cca |> select(CCA_ID, eda_cohort = COHORT)) |>
  # left_join(ccer_cca_6_7 |>  select(CCA_ID, cohort_6_7 = COHORT)) |>
  mutate(abs_diff = abs(eda_cohort - ccer_cohort),
         diff = eda_cohort - ccer_cohort,
         category = case_when(
           diff == 0 ~ "Same",
           diff < 0 ~ "lower need in eda",
           diff > 0 ~ "higher need in eda"
         )) |>
  select(CCA_NAME, cohort_ccer_3678 = ccer_cohort, eda_cohort, diff, category)

# big_diff_cca <- cca_join |>
#   filter(abs_diff > 1)


prop.table(table(cca_join$diff))  %>% `*`(100) %>% round(2)
prop.table(table(muni_join$diff))  %>% `*`(100) %>% round(2)


list_of_datasets <- list("Munis % in each cluster" = muni_intersection, "% of cohort in cluster -- muni" = cohort_intersection_muni,
                         "CCAs % in each cluster" = cca_intersection, "% of cohort in cluster -- cca" = cohort_intersection_cca,
                         "Cohort Difference -- Muni" = muni_join, "Cohort Difference -- cca" = cca_join)

openxlsx::write.xlsx(list_of_datasets, file = "output/cohort_ccer_relationship.xlsx")

