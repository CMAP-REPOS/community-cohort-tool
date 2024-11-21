library(tidyverse)
library(tidycensus)
library(sf)
library(rio)
library(readxl)

options(scipen = 999)

#https://eig.org/distressed-communities/?geo=places&lat=38.55&lon=-96.42&z=3.67&showAbout=true

query_geo <- "tract"


# load data that doesnt change --------------------------------------------


#this doesnt change per loop run and takes a while
blocks_cmap <- get_decennial(geography = "block",
                             variables = c('POP2020' = 'P8_001N'),
                             year = 2020,
                             state = 'il',
                             sumfile = "dhc",
                             county = str_remove(cmapgeo::county_fips_codes$cmap,"17"),
                             output = 'wide',
                             geometry = T,
                             cb = F #bigger files, more accurate shapes
) |>
  st_transform(crs = 3435)

munis <- read_sf("V:\\Demographic_and_Forecast\\Census\\2020\\dhc_place_2020.shp") |>
  filter(GEOID != "1770460") |>
  select(GEOID, NAME, POP2020) |>
  st_transform(crs = 3435)

ccas <- read_sf("V:\\Administrative_and_Political_Boundaries\\Miscellaneous\\ChicagoCommunityAreas_Chicago_202309.shp") |>
  st_transform(crs = 3435)

zc_shape <- read_sf("V:\\Administrative_and_Political_Boundaries\\Miscellaneous\\ZipCodesIL_ESRI_2022.shp") |>
  st_transform(crs = 3435)


for (acs_year in c(2020, 2021, 2022)) {


dict <- load_variables(year = acs_year,
                       dataset = "acs5") |>
  filter(!str_detect(concept, "PUERTO RICO|ALONE"))


# median income -- B06011_001



# educational attainment --------------------------------------------------
edu_helper <- dict |>
  filter(str_sub(name,1,6)== "B15002")


edu_table <- get_acs(geography = query_geo,
                     table = "B15002",
                     year = acs_year,
                     state = "il",
                     county = str_remove(cmapgeo::county_fips_codes$cmap,"17"),
                     survey = "acs5")

edu_proc <- edu_table |>
  mutate(number = as.numeric(str_sub(variable,-3,-1)),
         category = case_when(
           number == 1 ~ "total",
           number >= 3 & number <= 10 ~ "no diploma",
           number >= 11 & number <= 18 ~ "diploma",
           number >= 20 & number <= 27 ~ "no diploma",
           number >= 28 & number <= 35 ~ "diploma",
           T ~ NA
         )) |>
  filter(!is.na(category)) |>
  group_by(category, GEOID) |>
  mutate(total = sum(estimate)) |>
  distinct(total, category, GEOID) |>
  pivot_wider(values_from = total,
              names_from = category) |>
  mutate(percent_diploma = diploma / total) |>
  distinct(GEOID, percent_diploma) |>
  ungroup()


# poverty -----------------------------------------------------------------

pov_helper <- dict |>
  filter(str_sub(name,1,6)== "B17001")

pov_pull <- get_acs(geography = query_geo,
                     variables = c("B17001_001","B17001_002"),
                     year = acs_year,
                     state = "il",
                     output = "wide",
                     county = str_remove(cmapgeo::county_fips_codes$cmap,"17"),
                     survey = "acs5")

pov_proc <- pov_pull |>
  mutate(percent_pov = B17001_002E / B17001_001E) |>
  distinct(GEOID, percent_pov)


# employment --------------------------------------------------------------
emp_helper <- dict |>
  filter(str_sub(name,1,6)== "B23001")

emp_pull <- get_acs(geography = query_geo,
                    table = "B23001",
                    year = acs_year,
                    state = "il",
                    county = str_remove(cmapgeo::county_fips_codes$cmap,"17"),
                    survey = "acs5")

emp_proc <- emp_pull |>
  left_join(emp_helper, by = c("variable" = "name")) |>
  filter() |>
  mutate(category = case_when(
    str_detect(label,"Armed|Emp") ~ "employed",
    str_detect(label,"Unem|Not") ~ "not_employed",
    T ~ NA
  )) |>
  filter(!is.na(category),
         parse_number(label) >= 25 & parse_number(label) <= 45) |> #specific age groups
  group_by(GEOID, category) |>
  mutate(total = sum(estimate)) |>
  distinct(GEOID, category, total) |>
  pivot_wider(names_from = category,
              values_from = total) |>
  mutate(percent_working = employed / (employed+not_employed)) |>
  distinct(GEOID, percent_working)


# vacancy -----------------------------------------------------------------

vac_helper <- dict |>
  filter(str_sub(name,1,6)== "B25004")

vac_table <- get_acs(geography = query_geo,
                     variables = c(total = "B25002_001",vacant = "B25002_003"),
                     year = acs_year,
                     state = "il",
                     output = "wide",
                     county = str_remove(cmapgeo::county_fips_codes$cmap,"17"),
                     survey = "acs5")

vac_proc <- vac_table |>
  mutate(vac_pct = vacantE / totalE) |>
  distinct(vac_pct, GEOID)


# med income --------------------------------------------------------------
inc_helper <- dict |>
  filter(str_sub(name,1,6)== "B06011")

inc_table <- get_acs(geography = query_geo,
                     variables = c(med_inc = "B06011_001"),
                     year = acs_year,
                     state = "il",
                     output = "wide",
                     county = str_remove(cmapgeo::county_fips_codes$cmap,"17"),
                     survey = "acs5")

inc_proc <- inc_table |>
  select(GEOID, med_inc = med_incE) |>
  distinct()


# combine Census ----------------------------------------------------------

tracts_cmap <- get_decennial(geography = "tract",
                             variables = c('POP2020' = 'P8_001N'),
                             year = 2020,
                             state = 'il',
                             sumfile = "dhc",
                             county = str_remove(cmapgeo::county_fips_codes$cmap,"17"),
                             output = 'wide',
                             geometry = T,
                             cb = F #bigger files, more accurate shapes
)


census_data_combined <- tracts_cmap |>
  select(GEOID, POP2020) |>
  left_join(edu_proc) |>
  left_join(pov_proc) |>
  left_join(emp_proc) |>
  left_join(vac_proc) |>
  left_join(inc_proc) |>
  st_transform(crs = 3435)

# Business ---------------------------------------------------------------------


zip_code_data_new <- import(paste0("https://www2.census.gov/programs-surveys/cbp/datasets/",acs_year,"/zbp",acs_year - 2000,"totals.zip"))

zc_proc_new <- zip_code_data_new |>
  filter(stabbr =="IL",
         cty_name %in% str_to_upper(names(cmapgeo::county_fips_codes$cmap))) |>
  select(zip, employment_new = emp, establishments_new = est)


zip_code_data_old <- import(paste0("https://www2.census.gov/programs-surveys/cbp/datasets/",acs_year-5,"/zbp",acs_year - 2005,"totals.zip"))

zc_proc_old <- zip_code_data_old |>
  filter(stabbr =="IL",
         cty_name %in% str_to_upper(names(cmapgeo::county_fips_codes$cmap))) |>
  select(zip, employment_old = emp, establishments_old = est)

zc_join <- zc_proc_new |>
  left_join(zc_proc_old) |>
  mutate(change_emp  = employment_new - employment_old,
         change_est = establishments_new - establishments_old,
         zip = as.character(zip)) |>
  distinct(zip, change_emp, change_est)


zc_join <- zc_shape |>
  select(zip = ZIP_CODE) |>
  left_join(zc_join)


# interpolate ------------------------------------------------------------------

tract_level_emp <- interpolate_pw(
  from = zc_join,
  to = census_data_combined,
  to_id = "GEOID",
  extensive = T,
  weights = blocks_cmap,
  weight_column = "POP2020",
  crs = 3435
) |>
  as_tibble()


# total -------------------------------------------------------------------

all_data <- census_data_combined |>
  left_join(tract_level_emp) |>
  mutate(any_na_flag = case_when(
    is.na(percent_diploma + percent_pov + percent_working + vac_pct + med_inc + change_emp + change_est) ~ T,
    T ~ F
  ),
  no_pop_tract = case_when(
    POP2020 == 0 ~ T,
    T ~ F
  )) |>
  filter(no_pop_tract == F)

all_data_no_na <- all_data |>
  mutate(
    percent_diploma = ifelse(is.na(percent_diploma) | is.nan(percent_diploma), mean(percent_diploma, na.rm = T), percent_diploma),
    percent_pov = ifelse(is.na(percent_pov) | is.nan(percent_pov), mean(percent_pov, na.rm = T), percent_pov),
    percent_working = ifelse(is.na(percent_working) | is.nan(percent_working), mean(percent_working, na.rm = T), percent_working),
    vac_pct = ifelse(is.na(vac_pct) | is.nan(vac_pct), mean(vac_pct, na.rm = T), vac_pct),
    med_inc = ifelse(is.na(med_inc) | is.nan(med_inc), mean(med_inc, na.rm = T), med_inc),
    change_emp = ifelse(is.na(change_emp) | is.nan(change_emp), mean(change_emp, na.rm = T), change_emp),
    change_est = ifelse(is.na(change_est) | is.nan(change_est), mean(change_est, na.rm = T), change_est)
  )

#17097863005 -- naval station
#17031071000 -- depaul
#17031842900 -- Cabrini/RR


# scoring -----------------------------------------------------------------

all_data_scores <- all_data_no_na |>
  mutate(diploma_score = ntile(percent_diploma, 10), #higher = good
         pov_score = abs(10-ntile(percent_pov, 10)), #higher = bad
         work_score = ntile(percent_working, 10), #higher = good
         vacancy_score = abs(10-ntile(vac_pct, 10)), #higher = bad
         income_score = ntile(med_inc,10), #higher = good
         emp_socre = ntile(change_emp,10), #higher = good
         est_score = ntile(change_est,10), #higher = good
         total = diploma_score + pov_score + work_score + vacancy_score + income_score + emp_socre + est_score,
         total_ntile = ntile(total,10))  #higher = good

grouped_scores <- all_data_scores |>
  group_by(total_ntile) |>
  summarize(average_score = mean(total,na.rm = T))

eda_equiv <- all_data_scores |>
  filter(total <= 32) |>
  distinct(GEOID, total)


# munis  ------------------------------------------------------------------




block_mod <- blocks_cmap |>
  select(!NAME) |>
  mutate(tract = str_sub(GEOID, 1,11)) |>
  left_join(eda_equiv, by = c("tract" = "GEOID")) |>
  mutate(eda_equiv = case_when(
    is.na(total) ~ 0,
    T ~ 1
  )) |>
  st_transform(crs = 3435) |>
  rename(pop_block = POP2020) |>
  mutate(eda_pop = pop_block*eda_equiv)

block_mod <- block_mod %>%
  mutate(block_area = as.numeric(st_area(.)))

block_muni_intersect <- munis %>%
  st_intersection(block_mod)

edi_muni <- block_muni_intersect  %>%
  select(GEOID, NAME, muni_pop = POP2020, eda_pop, geometry, block_area)  %>%
  distinct()  %>%
  mutate(intersect_area = as.numeric(st_area(.)),
         pct_of_block = intersect_area/block_area,
         adjusted_edi_pop = pct_of_block*eda_pop) %>%
  group_by(GEOID)  %>%
  mutate(edi_pop_total = sum(adjusted_edi_pop, na.rm = T))  %>%
  ungroup()  %>%
  mutate(percent_edi = round(edi_pop_total / muni_pop, 3))  %>%
  distinct(GEOID, NAME, percent_edi)

# writexl::write_xlsx(block_muni_intersect_proc,"edi_results_muni.xlsx")


# cca ---------------------------------------------------------------------



block_cca_intersect <- ccas %>%
  st_intersection(block_mod)

edi_cca <- block_cca_intersect  %>%
  select(area_numbe, community, eda_pop, geometry, block_area, pop_block)  %>%
  distinct()  %>%
  mutate(intersect_area = as.numeric(st_area(.)),
         pct_of_block = intersect_area/block_area,
         adjusted_edi_pop = pct_of_block*eda_pop) %>%
  group_by(area_numbe)  %>%
  mutate(edi_pop_total = sum(adjusted_edi_pop, na.rm = T),
         cca_pop_total = sum(pop_block, na.rm = T))  %>%
  ungroup()  %>%
  mutate(percent_edi = round(edi_pop_total / cca_pop_total, 3))  %>%
  distinct(area_numbe, community, percent_edi)

# writexl::write_xlsx(block_cca_intersect_proc,"edi_results_cca.xlsx")



# modify_inputs -----------------------------------------------------------


input_file_original <- paste0("input/community_cohort_inputs_original_",acs_year+2,".xlsx")

old_inputs_muni <- read_excel(input_file_original, sheet = "FACTORS_MUNI")
old_inputs_cca <- read_excel(input_file_original, sheet = "FACTORS_CCA")
old_inputs_weights <- read_excel(input_file_original, sheet = "WEIGHTS")
old_inputs_cohorts <- read_excel(input_file_original, sheet = "COHORTS")


new_inputs_muni <- old_inputs_muni |>
  mutate(GEOID = as.character(GEOID)) |>
  left_join(edi_muni) |>
  mutate(PCT_EDA_POP = percent_edi) |>
  select(!c(NAME, percent_edi))

new_inputs_cca <- old_inputs_cca |>
  mutate(CCA_ID = as.character(CCA_ID)) |>
  left_join(edi_cca, by = c("CCA_ID" = "area_numbe")) |>
  mutate(PCT_EDA_POP = percent_edi) |>
  select(!c(community, percent_edi))

openxlsx::write.xlsx(
  list(
    FACTORS_MUNI = new_inputs_muni,
    FACTORS_CCA = new_inputs_cca,
    WEIGHTS = old_inputs_weights,
    COHORTS = old_inputs_cohorts),
  file = paste0("input/community_cohort_inputs_edi_",acs_year + 2,".xlsx"),
  rowNames = FALSE)


}
