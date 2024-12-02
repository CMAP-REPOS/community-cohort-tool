############## PART 1: SETUP AND IMPORT

library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(cmapplot)
library(cmapgeo)
apply_cmap_default_aes()

#toggle to get 2022 vs 2023
COHORT_YEAR <- 2023  # Update this each year!
IN_XLSX <- paste0("input/community_cohort_inputs_edi_",COHORT_YEAR,".xlsx")  # Spreadsheet containing latest data

# Load input factors, weights and cohort thresholds -----------------------

FACTORS_MUNI <- read_xlsx(IN_XLSX, sheet="FACTORS_MUNI")
FACTORS_CCA <- read_xlsx(IN_XLSX, sheet="FACTORS_CCA")
WEIGHTS <- read_xlsx(IN_XLSX, sheet="WEIGHTS")
COHORTS <- read_xlsx(IN_XLSX, sheet="COHORTS")
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

}

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

write.csv(FACTORS_MUNI, paste0("output/muni_edi_1yr_",COHORT_YEAR,".csv"))
write.csv(FACTORS_CCA, paste0("output/cca_edi_1yr_",COHORT_YEAR,".csv"))

