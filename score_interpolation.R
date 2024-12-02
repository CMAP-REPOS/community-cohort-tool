
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

input_data_munis <- readxl::read_excel("input/community_cohort_inputs.xlsx", sheet = "FACTORS_MUNI")
input_data_ccas <- readxl::read_excel("input/community_cohort_inputs.xlsx", sheet = "FACTORS_CCA")

input_data_munis_ccer <- input_data_munis |>
  left_join(weights, by = "GEOID") |>
  mutate(PCT_EDA_POP = ccer_score) |>
  select(!setdiff(c(names(weights)),"GEOID"))

input_data_ccas_ccer <- input_data_ccas |>
  left_join(weights, by = "CCA_ID") |>
  mutate(PCT_EDA_POP = ccer_score) |>
  select(!setdiff(c(names(weights)),"CCA_ID"))

input_data_weights <- readxl::read_excel("input/community_cohort_inputs.xlsx", sheet = "WEIGHTS")
input_data_cohorts <- readxl::read_excel("input/community_cohort_inputs.xlsx", sheet = "COHORTS")

list_of_datasets <- list("WEIGHTS" = input_data_weights, "COHORTS" = input_data_cohorts,
                         "FACTORS_MUNI" = input_data_munis_ccer, "FACTORS_CCA" = input_data_ccas_ccer)

openxlsx::write.xlsx(list_of_datasets, file = "input/community_cohort_inputs_CCER_scores.xlsx")
