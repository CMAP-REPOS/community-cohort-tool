############## PART 1: SETUP AND IMPORT

#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools", "devtools")
#devtools::install_github("CMAP-REPOS/cmapplot", build_vignettes = TRUE)
#devtools::install_github("CMAP-REPOS/cmapgeo", build_vignettes = TRUE)
library(tidyverse)
library(tidycensus)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(cmapplot)
library(cmapgeo)
apply_cmap_default_aes()


year_range <- 2022:2024

max(year_range)

# CCER   ----------------------------------------------------------

## load CCER data  ----------------------------------------------------------

submarkets <- read_sf("S:\\Projects\\CCER\\Complete_Shapefiles\\main_kmeans_10clust_No_EffRate_New_ClusterID_V2_040924_clipped.shp") |> select(CLUSTER_ID)

cohorts <- read_sf("S:\\AdminGroups\\ResearchAnalysis\\DrylaGaca\\Temp_Transfer\\Cohorts") |>
  st_transform(crs = cmapgeo::cmap_crs) |>
  mutate(Community_ = case_when(
    Community_ == "Riverdale" & Area == "Chicago Community Area" ~ "Riverdale -- CCA",
    Community_ == "Riverdale" & Area == "City/Town/Village " ~ "Riverdale -- Muni",
    T ~ Community_
  )) |>
  select(Community_, cohort = F2024_Coho, Area, FIRST_MUNI, area_nr) |>
  mutate(FIRST_MUNI = case_when(
    !is.na(FIRST_MUNI) ~ as.numeric(str_c("17",FIRST_MUNI)),
    T ~ NA
  ))

census_blocks <- read_sf("V:\\Demographic_and_Forecast\\Census\\2020\\dhc_blocks_2020.shp") |> select(GEOID, POP2020)


## set scores  ----------------------------------------------------------

sm_list <- as_tibble(as.character(seq(1:10)))

#weights v1

sm_scores <- sm_list |>
  rename(CLUSTER_ID = 1) |>
  mutate(
    score = case_when(
      CLUSTER_ID %in% c("6","7") ~ 5,
      CLUSTER_ID %in% c("3","8") ~ 3,
      CLUSTER_ID %in% c("9") ~ 1,
      CLUSTER_ID %in% c("1","10","2","4","5") ~ 0
    )
  )

#weights v2

# sm_scores <- sm_list |>
#   rename(CLUSTER_ID = 1) |>
#   mutate(
#     score = case_when(
#       CLUSTER_ID %in% c("6","7","8") ~ 5,
#       CLUSTER_ID %in% c("9") ~ 3,
#       CLUSTER_ID %in% c("10") ~ 2,
#       CLUSTER_ID %in% c("3") ~ 1,
#       CLUSTER_ID %in% c("1","2","4","5") ~ 0
#     )
#   )

## create inputs  ----------------------------------------------------------
scores <- submarkets |>
  left_join(sm_scores)

weights <- interpolate_pw(
  from = scores,
  to = cohorts,
  to_id = "Community_",
  extensive = F,
  weights = census_blocks,
  weight_column = "POP2020",
  crs = 3435
) |>
  mutate(score = ifelse(Community_ == "Braceville",max(score, na.rm = T),score),
         ccer_score = score/max(score)) |>
  as_tibble() |>
  left_join(cohorts |> as_tibble() |> select(Community_, CCA_ID = area_nr, GEOID = FIRST_MUNI))


for (c_year in year_range) {

input_path <- paste0("input\\community_cohort_inputs_original_",c_year,".xlsx")

input_data_munis <- read_excel(input_path, sheet = "FACTORS_MUNI")
input_data_ccas <- read_excel(input_path, sheet = "FACTORS_CCA")

input_data_munis_ccer <- input_data_munis |>
  left_join(weights, by = "GEOID") |>
  mutate(PCT_EDA_POP = ccer_score) |>
  select(!setdiff(c(names(weights)),"GEOID"))

input_data_ccas_ccer <- input_data_ccas |>
  left_join(weights, by = "CCA_ID") |>
  mutate(PCT_EDA_POP = ccer_score) |>
  select(!setdiff(c(names(weights)),"CCA_ID"))

input_data_weights <- readxl::read_excel(input_path, sheet = "WEIGHTS")
input_data_cohorts <- readxl::read_excel(input_path, sheet = "COHORTS")

list_of_datasets <- list("WEIGHTS" = input_data_weights, "COHORTS" = input_data_cohorts,
                         "FACTORS_MUNI" = input_data_munis_ccer, "FACTORS_CCA" = input_data_ccas_ccer)

ouptut_path <- paste0("input/community_cohort_inputs_ccer_",c_year,".xlsx")

openxlsx::write.xlsx(list_of_datasets, file = ouptut_path)


COHORT_YEAR <- c_year
IN_XLSX <- ouptut_path  # Spreadsheet containing latest data


# Load input factors, weights and cohort thresholds -----------------------

FACTORS_MUNI <- read_xlsx(IN_XLSX, sheet="FACTORS_MUNI")
FACTORS_CCA <- read_xlsx(IN_XLSX, sheet="FACTORS_CCA")
WEIGHTS <- read_xlsx(IN_XLSX, sheet="WEIGHTS")

# if (c_year == min(year_range)){
#   WEIGHTS <- WEIGHTS |>
#     mutate(WEIGHT = ifelse(FACTOR_NAME == "PCT_EDA_POP",-0.5,WEIGHT))
# }
#
# if (c_year == (min(year_range)+1)){
#   WEIGHTS <- WEIGHTS |>
#     mutate(WEIGHT = ifelse(FACTOR_NAME == "PCT_EDA_POP",-0.75,WEIGHT))
# }


#Weights 1
COHORTS <- read_xlsx(IN_XLSX, sheet="COHORTS")

#Weights 2
# COHORTS <- read_xlsx(IN_XLSX, sheet="COHORTS") |> mutate(MAX_SCORE = ifelse(COHORT == 3, 52, MAX_SCORE)) #adjusted

COHORTS$COHORT <- as.character(COHORTS$COHORT)


# Calculate factor-specific scoring thresholds ----------------------------

WEIGHTS$MED <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], median)[1,])
WEIGHTS$SD <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], sd)[1,])

WEIGHTS <- WEIGHTS %>%
  mutate(
    CUT0 = -Inf,
    CUT1 = MED - SD * 1.2816,  # ~10th %ile (based on standard normal distribution)
    CUT2 = MED - SD * 0.8416,  # ~20th %ile
    CUT3 = MED - SD * 0.5244,  # ~30th %ile
    CUT4 = MED - SD * 0.2533,  # ~40th %ile
    CUT5 = MED,                # ~50th %ile
    CUT6 = MED + SD * 0.2533,  # ~60th %ile
    CUT7 = MED + SD * 0.5244,  # ~70th %ile
    CUT8 = MED + SD * 0.8416,  # ~80th %ile
    CUT9 = MED + SD * 1.2816,  # ~90th %ile
    CUT10 = Inf
  )

# Force equal intervals and midpoint of 0.5 for PCT_EDA_POP factor
WEIGHTS[WEIGHTS$FACTOR_NAME=="PCT_EDA_POP", paste0("CUT", 1:9)] <- as.list(seq(0.1, 0.9, 0.1))


############## PART 2: CALCULATE SCORES

# Calculate factor-specific scores ----------------------------------------

keep_cols_muni <- append(c("GEOID", "MUNI"), WEIGHTS$FACTOR_NAME)
keep_cols_cca <- append(c("CCA_ID", "CCA_NAME"), WEIGHTS$FACTOR_NAME)
FACTORS_MUNI <- FACTORS_MUNI[, keep_cols_muni]
FACTORS_CCA <- FACTORS_CCA[, keep_cols_cca]

score_cols <- c()
wt_score_cols <- c()

for (factor in unlist(WEIGHTS[WEIGHTS$WEIGHT!=0, "FACTOR_NAME"])) {
  weight <- WEIGHTS[WEIGHTS$FACTOR_NAME==factor, "WEIGHT"][[1]]

  score_col <- paste0("SCORE_", factor)
  score_cols <- append(score_cols, score_col)

  wt_score_col <- paste0("WT_SCORE_", factor)
  wt_score_cols <- append(wt_score_cols, wt_score_col)

  cuts <- WEIGHTS[WEIGHTS$FACTOR_NAME==factor,] %>%
    select(starts_with("CUT"))
  groups <- c(1:10)
  FACTORS_MUNI[, score_col] <- cut(as.matrix(FACTORS_MUNI[, factor]), cuts, groups, labels=FALSE)
  FACTORS_CCA[, score_col] <- cut(as.matrix(FACTORS_CCA[, factor]), cuts, groups, labels=FALSE)
  if (weight < 0) {
    # Reverse score order for factors with negative weights
    FACTORS_MUNI[, score_col] <-  max(groups) + 1 - FACTORS_MUNI[, score_col]
    FACTORS_CCA[, score_col] <-  max(groups) + 1 - FACTORS_CCA[, score_col]
  }
  FACTORS_MUNI[, wt_score_col] <- FACTORS_MUNI[, score_col] * abs(weight)
  FACTORS_CCA[, wt_score_col] <- FACTORS_CCA[, score_col] * abs(weight)



# Calculate 1-yr score & 1-yr cohort ---------------------------------------

FACTORS_MUNI$SCORE_OVERALL <- rowSums(FACTORS_MUNI[, wt_score_cols])
FACTORS_CCA$SCORE_OVERALL <- rowSums(FACTORS_CCA[, wt_score_cols])

# Rescale from 0-100
min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_MUNI$COHORT <- cut(as.vector(FACTORS_MUNI$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(COHORT = fct_relevel(COHORT, sort))

FACTORS_CCA <- FACTORS_CCA %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_CCA$COHORT <- cut(as.vector(FACTORS_CCA$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_CCA <- FACTORS_CCA %>%
  mutate(COHORT = fct_relevel(COHORT, sort))

MUNI_CURRENTYR <- FACTORS_MUNI %>%
  rename(WEIGHTED_SCORE = SCORE_OVERALL_SCALED) %>%
  select(GEOID, MUNI, COHORT, WEIGHTED_SCORE, starts_with("SCORE_")) %>%
  select(-SCORE_OVERALL)

CCA_CURRENTYR <- FACTORS_CCA %>%
  rename(WEIGHTED_SCORE = SCORE_OVERALL_SCALED) %>%
  select(CCA_ID, CCA_NAME, COHORT, WEIGHTED_SCORE, starts_with("SCORE_")) %>%
  select(-SCORE_OVERALL)

output_path_1_yr_muni <- paste0("output/cohort_1yr_muni_",c_year,".csv")
output_path_1_yr_cca <- paste0("output/cohort_1yr_cca_",c_year,".csv")

write.csv(MUNI_CURRENTYR, output_path_1_yr_muni)
write.csv(CCA_CURRENTYR, output_path_1_yr_cca)
}



if (c_year == max(year_range)) {

# Calculate 3-year average scores and reassign cohorts --------------------

# Munis

MUNI_SCORES_YEAR1 <- paste0("output/cohort_1yr_muni_", COHORT_YEAR - 2, ".csv") %>%
  read_csv() %>%
  select(GEOID, SCORE_YEAR1 = WEIGHTED_SCORE)

MUNI_SCORES_YEAR2 <- paste0("output/cohort_1yr_muni_", COHORT_YEAR - 1, ".csv") %>%
  read_csv() %>%
  select(GEOID, SCORE_YEAR2 = WEIGHTED_SCORE)

MUNI_SCORES_3YR_AVG <- MUNI_CURRENTYR %>%
  select(GEOID, MUNI, SCORE_YEAR3 = WEIGHTED_SCORE) %>%
  mutate(GEOID = as.numeric(GEOID)) |>
  left_join(MUNI_SCORES_YEAR2) %>%
  left_join(MUNI_SCORES_YEAR1) %>%
  mutate(WEIGHTED_SCORE_3YR = (SCORE_YEAR1 + SCORE_YEAR2 + SCORE_YEAR3) / 3) %>%
  select(-starts_with("SCORE_YEAR"))

MUNI_SCORES_3YR_AVG$COHORT_3YR <- cut(as.vector(MUNI_SCORES_3YR_AVG$WEIGHTED_SCORE_3YR), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
MUNI_SCORES_3YR_AVG <- MUNI_SCORES_3YR_AVG %>%
  mutate(COHORT_3YR = fct_relevel(COHORT_3YR, sort))

# CCAs

CCA_SCORES_YEAR1 <- paste0("output/cohort_1yr_cca_", COHORT_YEAR - 2, ".csv") %>%
  read_csv() %>%
  select(CCA_ID, SCORE_YEAR1 = WEIGHTED_SCORE)

CCA_SCORES_YEAR2 <- paste0("output/cohort_1yr_cca_", COHORT_YEAR - 1, ".csv") %>%
  read_csv() %>%
  select(CCA_ID, SCORE_YEAR2 = WEIGHTED_SCORE)

CCA_SCORES_3YR_AVG <- CCA_CURRENTYR %>%
  select(CCA_ID, CCA_NAME, SCORE_YEAR3 = WEIGHTED_SCORE) %>%
  mutate(CCA_ID = as.numeric(CCA_ID)) |>
  left_join(CCA_SCORES_YEAR2) %>%
  left_join(CCA_SCORES_YEAR1) %>%
  mutate(WEIGHTED_SCORE_3YR = (SCORE_YEAR1 + SCORE_YEAR2 + SCORE_YEAR3) / 3) %>%
  select(-starts_with("SCORE_YEAR"))

CCA_SCORES_3YR_AVG$COHORT_3YR <- cut(as.vector(CCA_SCORES_3YR_AVG$WEIGHTED_SCORE_3YR), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
CCA_SCORES_3YR_AVG <- CCA_SCORES_3YR_AVG %>%
  mutate(COHORT_3YR = fct_relevel(COHORT_3YR, sort))

openxlsx::write.xlsx(
  list(
    COHORT_EDI_MUNI = MUNI_SCORES_3YR_AVG,
    COHORT_EDI_CCA = CCA_SCORES_3YR_AVG),
  file = paste0("output//cohort_3yr_",c_year,".xlsx"),
  rowNames = FALSE)

}
}

# comparison --------------------------------------------------------------

old_scores_munis <- read_csv("input/cohort_assignments_muni_3yr_2022_2024.csv") |> select(GEOID, old_cohort = COHORT_3YR)
old_scores_ccas <- read_csv("input/cohort_assignments_cca_3yr_2022_2024.csv") |> select(CCA_ID, old_cohort = COHORT_3YR)

comparison_muni <- MUNI_SCORES_3YR_AVG |>
  mutate(COHORT_3YR = as.numeric(COHORT_3YR)) |>
  left_join(old_scores_munis, by = "GEOID") |>
  mutate(abs_diff = abs(old_cohort - COHORT_3YR),
         diff = old_cohort - COHORT_3YR,
         diff_cat = case_when(
           diff == 0 ~ "No Change",
           diff > 0 ~ "Lower need with CCER",
           diff < 0 ~ "Higher need with CCER",
           T ~ NA
         ))

comparison_cca <- CCA_SCORES_3YR_AVG |>
  mutate(COHORT_3YR = as.numeric(COHORT_3YR)) |>
  left_join(old_scores_ccas, by = "CCA_ID") |>
  mutate(abs_diff = abs(old_cohort - COHORT_3YR),
         diff = old_cohort - COHORT_3YR,
         diff_cat = case_when(
           diff == 0 ~ "No Change",
           diff > 0 ~ "Lower need with CCER",
           diff < 0 ~ "Higher need with CCER",
           T ~ NA
         ))

muni_all <- comparison_muni |>
  select(name = MUNI, cohort_with_eda = old_cohort, cohort_with_ccer = COHORT_3YR, diff, diff_cat, WEIGHTED_SCORE_3YR) |>
  mutate(cat = "muni")

cca_all <- comparison_cca |>
  select(name = CCA_NAME, cohort_with_eda = old_cohort, cohort_with_ccer = COHORT_3YR, diff, diff_cat, WEIGHTED_SCORE_3YR) |>
  mutate(cat = "cca")

combined_all <- rbind(muni_all, cca_all)



muni_changes <- comparison_muni |>
  select(name = MUNI, cohort_with_eda = old_cohort, cohort_with_ccer = COHORT_3YR, diff, diff_cat, WEIGHTED_SCORE_3YR) |>
  filter(diff != 0)

cca_changes <- comparison_cca |>
  select(name = CCA_NAME, cohort_with_eda = old_cohort, cohort_with_ccer = COHORT_3YR, diff, diff_cat, WEIGHTED_SCORE_3YR) |>
  filter(diff != 0)

changes <- rbind(muni_changes, cca_changes)

# exam --------------------------------------------------------------------

community_percent_ccer <- cohorts %>%
  mutate(community_area = as.numeric(st_area(.))) %>%
  st_intersection(submarkets) %>%
  mutate(overlap_area = as.numeric(st_area(.))) %>%
  group_by(Community_, Area, CLUSTER_ID) %>%
  mutate(total_overlap_area = sum(overlap_area))  %>%
  distinct(Community_, Area, CLUSTER_ID, community_area, total_overlap_area)  %>%
  mutate(cluster_percentage = total_overlap_area/community_area)  %>%
  distinct(Community_, Area, CLUSTER_ID, cluster_percentage) |>
  filter(cluster_percentage >= 0.05)


# weight_testing ----------------------------------------------------------


#weights 3
weight_testing <- combined_all |>
  mutate(new_cohorts = case_when(
    WEIGHTED_SCORE_3YR < 100 & WEIGHTED_SCORE_3YR >= 64 ~ 1,
    WEIGHTED_SCORE_3YR < 64 & WEIGHTED_SCORE_3YR >= 45 ~ 2,
    WEIGHTED_SCORE_3YR < 45 & WEIGHTED_SCORE_3YR >= 41 ~ 3,
    WEIGHTED_SCORE_3YR < 41 & WEIGHTED_SCORE_3YR >= 0 ~ 4,
    T ~ NA
  ),
  new_diff = new_cohorts - cohort_with_eda,
  new_diff_cat = case_when(
    new_diff < 0 ~ "Lower need with new method",
    new_diff == 0 ~ "Same need",
    new_diff > 0 ~ "Higher need with new method",
    T ~ NA
  )
  )

# weight_testing <- combined_all |>
#   mutate(new_cohorts = case_when(
#     WEIGHTED_SCORE_3YR < 100 & WEIGHTED_SCORE_3YR >= 62 ~ 1,
#     WEIGHTED_SCORE_3YR < 62 & WEIGHTED_SCORE_3YR >= 44 ~ 2,
#     WEIGHTED_SCORE_3YR < 44 & WEIGHTED_SCORE_3YR >= 40 ~ 3,
#     WEIGHTED_SCORE_3YR < 40 & WEIGHTED_SCORE_3YR >= 0 ~ 4,
#     T ~ NA
#   ),
#   new_diff = new_cohorts - cohort_with_eda,
#   new_diff_cat = case_when(
#     new_diff < 0 ~ "Lower need with new method",
#     new_diff == 0 ~ "Same need",
#     new_diff > 0 ~ "Higher need with new method",
#     T ~ NA
#   ),
#   rounded_score = round(WEIGHTED_SCORE_3YR)
#   )


table(weight_testing$new_diff)
prop.table(table(weight_testing$new_diff))

table(weight_testing$new_diff_cat[weight_testing$cat == "muni"])
prop.table(table(weight_testing$new_diff_cat[weight_testing$cat == "muni"]))

table(weight_testing$new_diff_cat[weight_testing$cat == "cca"])
prop.table(table(weight_testing$new_diff_cat[weight_testing$cat == "cca"]))

table(weight_testing$new_diff_cat)
prop.table(table(weight_testing$new_diff_cat))

final_list <- weight_testing |>
  select(name, cohort_with_eda, new_cohorts, new_diff, new_diff_cat) |>
  mutate(abs_diff = abs(new_diff))

writexl::write_xlsx(final_list, "ccer_changes.xlsx")
