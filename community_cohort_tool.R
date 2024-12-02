#################
#
# The purpose of this branch is to test how the 2024 cohort assignments
# may change if the "% Population in EDA or DA" factor is replaced with
# "% Population in DA only" for both munis and ccas
#
#
# November 2024 | amcadams
#
#
#################


############## PART 1: SETUP AND IMPORT

#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools", "devtools")
#devtools::install_github("CMAP-REPOS/cmapplot", build_vignettes = TRUE)
#devtools::install_github("CMAP-REPOS/cmapgeo", build_vignettes = TRUE)
library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(cmapplot)
library(cmapgeo)
apply_cmap_default_aes()

COHORT_YEAR <- 2024  # Update this each year!
IN_XLSX <- "input/community_cohort_inputs_2024.xlsx"  # Spreadsheet containing latest data


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
WEIGHTS[WEIGHTS$FACTOR_NAME=="PCT_DA_POP", paste0("CUT", 1:9)] <- as.list(seq(0.1, 0.9, 0.1))


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

  # Inspect score distribution
  print(
    ggplot(FACTORS_MUNI) +
      geom_histogram(aes(x=get(factor)), color="#222222", fill="#73c9e3", size=0.3, bins=50) +
      geom_vline(xintercept=cuts[[2]], color="#222222", linetype="dotted") +
      geom_vline(xintercept=cuts[[3]], color="#222222", linetype="dotdash") +
      geom_vline(xintercept=cuts[[4]], color="#222222", linetype="dashed") +
      geom_vline(xintercept=cuts[[5]], color="#222222", linetype="longdash") +
      geom_vline(xintercept=cuts[[6]], color="#222222", linetype="solid", size=1) +  # Median
      geom_vline(xintercept=cuts[[7]], color="#222222", linetype="longdash") +
      geom_vline(xintercept=cuts[[8]], color="#222222", linetype="dashed") +
      geom_vline(xintercept=cuts[[9]], color="#222222", linetype="dotdash") +
      geom_vline(xintercept=cuts[[10]], color="#222222", linetype="dotted") +
      labs(title=paste("Distribution of factor values (with group breaks)", factor, sep="\n")) +
      theme_cmap(hline=0, ylab="Number of municipalities")
  )

  print(
    ggplot(FACTORS_MUNI) +
      geom_histogram(aes(x=get(score_col)), color="#222222", fill="#73c9e3", size=0.3, binwidth=1) +
      geom_hline(yintercept=28.4, color="#222222", size=0.5, linetype="dashed") +
      scale_x_continuous(limits=c(min(groups)-0.5, max(groups)+0.5), breaks=groups) +
      labs(title = paste("Distribution of factor scores", score_col, sep="\n"),
           caption="Note: Dashed line represents a perfect decile distribution of 28.4 municipalities per group.") +
      theme_cmap(hline=0, ylab="Number of municipalities")
  )
}

# Calculate 1-yr score & 1-yr cohort for 2024 ---------------------------------

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

# Recalculate scores for 2023 --------------

input_23 <- "input/community_cohort_inputs_2023.xlsx"

FACTORS_MUNI_23 <- read_xlsx(input_23, sheet="FACTORS_MUNI")
FACTORS_CCA_23 <- read_xlsx(input_23, sheet="FACTORS_CCA")

COHORTS <- read_xlsx(input_23, sheet="COHORTS")
COHORTS$COHORT <- as.character(COHORTS$COHORT)

WEIGHTS <- read_xlsx(input_23, sheet="WEIGHTS")

WEIGHTS$MED <- unlist(summarize_all(FACTORS_MUNI_23[, WEIGHTS$FACTOR_NAME], median)[1,])
WEIGHTS$SD <- unlist(summarize_all(FACTORS_MUNI_23[, WEIGHTS$FACTOR_NAME], sd)[1,])

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

keep_cols_muni <- append(c("GEOID", "MUNI"), WEIGHTS$FACTOR_NAME)
keep_cols_cca <- append(c("CCA_ID", "CCA_NAME"), WEIGHTS$FACTOR_NAME)
FACTORS_MUNI_23 <- FACTORS_MUNI_23[, keep_cols_muni]
FACTORS_CCA_23 <- FACTORS_CCA_23[, keep_cols_cca]

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
  FACTORS_MUNI_23[, score_col] <- cut(as.matrix(FACTORS_MUNI_23[, factor]), cuts, groups, labels=FALSE)
  FACTORS_CCA_23[, score_col] <- cut(as.matrix(FACTORS_CCA_23[, factor]), cuts, groups, labels=FALSE)
  if (weight < 0) {
    # Reverse score order for factors with negative weights
    FACTORS_MUNI_23[, score_col] <-  max(groups) + 1 - FACTORS_MUNI_23[, score_col]
    FACTORS_CCA_23[, score_col] <-  max(groups) + 1 - FACTORS_CCA_23[, score_col]
  }
  FACTORS_MUNI_23[, wt_score_col] <- FACTORS_MUNI_23[, score_col] * abs(weight)
  FACTORS_CCA_23[, wt_score_col] <- FACTORS_CCA_23[, score_col] * abs(weight)
}

FACTORS_MUNI_23$SCORE_OVERALL <- rowSums(FACTORS_MUNI_23[, wt_score_cols])
FACTORS_CCA_23$SCORE_OVERALL <- rowSums(FACTORS_CCA_23[, wt_score_cols])

min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI_23 <- FACTORS_MUNI_23 %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_MUNI_23$COHORT <- cut(as.vector(FACTORS_MUNI_23$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_MUNI_23 <- FACTORS_MUNI_23 %>%
  mutate(COHORT = fct_relevel(COHORT, sort))

FACTORS_CCA_23 <- FACTORS_CCA_23 %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_CCA_23$COHORT <- cut(as.vector(FACTORS_CCA_23$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_CCA_23 <- FACTORS_CCA_23 %>%
  mutate(COHORT = fct_relevel(COHORT, sort))


# Repeat for 2022 ---------------------------

input_22 <- "input/community_cohort_inputs_2022.xlsx"

FACTORS_MUNI_22 <- read_xlsx(input_22, sheet="FACTORS_MUNI")
FACTORS_CCA_22 <- read_xlsx(input_22, sheet="FACTORS_CCA")

COHORTS <- read_xlsx(input_22, sheet="COHORTS")
COHORTS$COHORT <- as.character(COHORTS$COHORT)

WEIGHTS <- read_xlsx(input_22, sheet="WEIGHTS")

WEIGHTS$MED <- unlist(summarize_all(FACTORS_MUNI_22[, WEIGHTS$FACTOR_NAME], median)[1,])
WEIGHTS$SD <- unlist(summarize_all(FACTORS_MUNI_22[, WEIGHTS$FACTOR_NAME], sd)[1,])

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

keep_cols_muni <- append(c("GEOID", "MUNI"), WEIGHTS$FACTOR_NAME)
keep_cols_cca <- append(c("CCA_ID", "CCA_NAME"), WEIGHTS$FACTOR_NAME)
FACTORS_MUNI_22 <- FACTORS_MUNI_22[, keep_cols_muni]
FACTORS_CCA_22 <- FACTORS_CCA_22[, keep_cols_cca]

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
  FACTORS_MUNI_22[, score_col] <- cut(as.matrix(FACTORS_MUNI_22[, factor]), cuts, groups, labels=FALSE)
  FACTORS_CCA_22[, score_col] <- cut(as.matrix(FACTORS_CCA_22[, factor]), cuts, groups, labels=FALSE)
  if (weight < 0) {
    # Reverse score order for factors with negative weights
    FACTORS_MUNI_22[, score_col] <-  max(groups) + 1 - FACTORS_MUNI_22[, score_col]
    FACTORS_CCA_22[, score_col] <-  max(groups) + 1 - FACTORS_CCA_22[, score_col]
  }
  FACTORS_MUNI_22[, wt_score_col] <- FACTORS_MUNI_22[, score_col] * abs(weight)
  FACTORS_CCA_22[, wt_score_col] <- FACTORS_CCA_22[, score_col] * abs(weight)
}

FACTORS_MUNI_22$SCORE_OVERALL <- rowSums(FACTORS_MUNI_22[, wt_score_cols])
FACTORS_CCA_22$SCORE_OVERALL <- rowSums(FACTORS_CCA_22[, wt_score_cols])

min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI_22 <- FACTORS_MUNI_22 %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_MUNI_22$COHORT <- cut(as.vector(FACTORS_MUNI_22$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_MUNI_22 <- FACTORS_MUNI_22 %>%
  mutate(COHORT = fct_relevel(COHORT, sort))

FACTORS_CCA_22 <- FACTORS_CCA_22 %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_CCA_22$COHORT <- cut(as.vector(FACTORS_CCA_22$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_CCA_22 <- FACTORS_CCA_22 %>%
  mutate(COHORT = fct_relevel(COHORT, sort))



# Calculate 3-year average scores and reassign cohorts --------------------

# Munis

MUNI_CURRENTYR <- FACTORS_MUNI %>%
  rename(WEIGHTED_SCORE = SCORE_OVERALL_SCALED) %>%
  select(GEOID, MUNI, COHORT, WEIGHTED_SCORE, starts_with("SCORE_")) %>%
  select(-SCORE_OVERALL)

MUNI_SCORES_YEAR1 <- paste0("output/1yr/cohort_assignments_muni_1yr_", COHORT_YEAR - 2, ".csv") %>%
  read_csv() %>%
  select(GEOID, SCORE_YEAR1 = WEIGHTED_SCORE)

MUNI_SCORES_YEAR2 <- paste0("output/1yr/cohort_assignments_muni_1yr_", COHORT_YEAR - 1, ".csv") %>%
  read_csv() %>%
  select(GEOID, SCORE_YEAR2 = WEIGHTED_SCORE)

MUNI_SCORES_3YR_AVG <- MUNI_CURRENTYR %>%
  select(GEOID, MUNI, SCORE_YEAR3 = WEIGHTED_SCORE) %>%
  left_join(MUNI_SCORES_YEAR2) %>%
  left_join(MUNI_SCORES_YEAR1) %>%
  mutate(WEIGHTED_SCORE_3YR = (SCORE_YEAR1 + SCORE_YEAR2 + SCORE_YEAR3) / 3) %>%
  select(-starts_with("SCORE_YEAR"))

MUNI_SCORES_3YR_AVG$COHORT_3YR <- cut(as.vector(MUNI_SCORES_3YR_AVG$WEIGHTED_SCORE_3YR), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
MUNI_SCORES_3YR_AVG <- MUNI_SCORES_3YR_AVG %>%
  mutate(COHORT_3YR = fct_relevel(COHORT_3YR, sort))

# CCAs

CCA_CURRENTYR <- FACTORS_CCA %>%
  rename(WEIGHTED_SCORE = SCORE_OVERALL_SCALED) %>%
  select(CCA_ID, CCA_NAME, COHORT, WEIGHTED_SCORE, starts_with("SCORE_")) %>%
  select(-SCORE_OVERALL)

CCA_SCORES_YEAR1 <- paste0("output/1yr/cohort_assignments_cca_1yr_", COHORT_YEAR - 2, ".csv") %>%
  read_csv() %>%
  select(CCA_ID, SCORE_YEAR1 = WEIGHTED_SCORE)

CCA_SCORES_YEAR2 <- paste0("output/1yr/cohort_assignments_cca_1yr_", COHORT_YEAR - 1, ".csv") %>%
  read_csv() %>%
  select(CCA_ID, SCORE_YEAR2 = WEIGHTED_SCORE)

CCA_SCORES_3YR_AVG <- CCA_CURRENTYR %>%
  select(CCA_ID, CCA_NAME, SCORE_YEAR3 = WEIGHTED_SCORE) %>%
  left_join(CCA_SCORES_YEAR2) %>%
  left_join(CCA_SCORES_YEAR1) %>%
  mutate(WEIGHTED_SCORE_3YR = (SCORE_YEAR1 + SCORE_YEAR2 + SCORE_YEAR3) / 3) %>%
  select(-starts_with("SCORE_YEAR"))

CCA_SCORES_3YR_AVG$COHORT_3YR <- cut(as.vector(CCA_SCORES_3YR_AVG$WEIGHTED_SCORE_3YR), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
CCA_SCORES_3YR_AVG <- CCA_SCORES_3YR_AVG %>%
  mutate(COHORT_3YR = fct_relevel(COHORT_3YR, sort))

############## PART 3: PLOT AND MAP SCORES

# Plot distribution of cohorts (1 yr only) ---------------------------------------

bin_width = 100 / (max_wt_score - min_wt_score)
bin_center = bin_width / 2

ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=SCORE_OVERALL_SCALED, fill=COHORT), color="#222222", size=0.3,
                 binwidth=bin_width, center=bin_center) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title="Distribution of one year scores (municipalities)") +
  theme_cmap(hline=0, ylab="Number of municipalities") +
  scale_fill_manual(values=c(`1`="#70d5ea", `2`="#efa971", `3`="#b6d979", `4`="#c2add6"),
                    breaks=c("1", "2", "3", "4"),
                    labels=c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4"))



chi_overall <- FACTORS_MUNI[FACTORS_MUNI$MUNI=="Chicago", "SCORE_OVERALL_SCALED"][[1]]
ggplot(FACTORS_CCA) +
  geom_histogram(aes(x=SCORE_OVERALL_SCALED, fill=COHORT), color="#222222", size=0.3,
                 binwidth=bin_width, center=bin_center) +
  geom_vline(linetype="dashed", xintercept=chi_overall, size=0.5, color="#222222") +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title="Distribution of one year scores (CCAs)",
       caption="Note: Dashed line represents the overall score for the entire City of Chicago.") +
  theme_cmap(hline=0, ylab="Number of CCAs") +
  scale_fill_manual(values=c(`1`="#70d5ea", `2`="#efa971", `3`="#b6d979", `4`="#c2add6"),
                    breaks=c("1", "2", "3", "4"),
                    labels=c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4"))

# Map distribution of cohorts (1 yr only) ---------------------------------------------------------

tmap_mode("plot")  # "plot" (static) or "view" (interactive)

cnty_geo <- filter(county_sf, cmap)

muni_geo <- municipality_sf %>%
  mutate(GEOID_n = as.integer(geoid_place)) %>%
  left_join(FACTORS_MUNI, by=c("GEOID_n"="GEOID")) %>%
  mutate(COHORT_n = as.integer(COHORT))

# muni_labels <- muni_geo %>%
#   filter(MUNI %in% c("Chicago", "Joliet", "Aurora", "Elgin", "Waukegan"))  # Label select munis

cca_geo <- cca_sf %>%
  left_join(FACTORS_CCA, by=c("cca_num"="CCA_ID")) %>%
  mutate(COHORT_n = as.integer(COHORT))

tm_shape(muni_geo, bbox=bb(cnty_geo, ext=1.2)) +
  tm_polygons("COHORT_n", title="", n=4, border.col="#ffffff", lwd=0.5,
              palette=c("#d2efa7", "#36d8ca", "#0084ac", "#310066"),
              labels=c("1 (low need)", "2 (moderate need)", "3 (high need)", "4 (very high need)")) +
tm_shape(cnty_geo) +
  tm_borders(col="#888888", lwd=2) +
# tm_shape(muni_labels) +
#   tm_text("MUNI", size=0.7, col="#000000") +
tm_legend(legend.position=c("left", "bottom")) +
tm_layout(title="Assigned 1-year cohorts (municipalities)", frame=FALSE,
          fontface=get_cmapplot_global("font$strong$face"),
          fontfamily=get_cmapplot_global("font$strong$family"),
          legend.text.fontface=get_cmapplot_global("font$regular$face"),
          legend.text.fontfamily=get_cmapplot_global("font$regular$family"))

tm_shape(cca_geo, bbox=bb(cca_geo, ext=1.2)) +
  tm_polygons("COHORT_n", title="", n=4, border.col="#ffffff", lwd=0.5,
              palette=c("#d2efa7", "#36d8ca", "#0084ac", "#310066"),
              labels=c("1 (low need)", "2 (moderate need)", "3 (high need)", "4 (very high need)")) +
tm_legend(legend.position=c("left", "bottom")) +
tm_layout(title="Assigned 1-year cohorts (CCAs)", frame=FALSE,
          fontface=get_cmapplot_global("font$strong$face"),
          fontfamily=get_cmapplot_global("font$strong$family"),
          legend.text.fontface=get_cmapplot_global("font$regular$face"),
          legend.text.fontfamily=get_cmapplot_global("font$regular$family"))

# Plot distribution of cohorts (3 yr average) ---------------------------------------

bin_width = 100 / (max_wt_score - min_wt_score)
bin_center = bin_width / 2

ggplot(MUNI_SCORES_3YR_AVG) +
  geom_histogram(aes(x=WEIGHTED_SCORE_3YR, fill=COHORT_3YR), color="#222222", size=0.3,
                 binwidth = bin_width, center=bin_center) +
  #scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10) )+
  labs(title = paste("Distribution of overall scores (municipalities) FY", COHORT_YEAR, sep="")) +
  theme_cmap(hline=0, ylab="Number of municipalities") +
  scale_fill_manual(values=c(`4`="#AA66CD", `3`="#C29ED7", `2`="#89CD66", `1`="#B4D79E"),
                    breaks=c("4","3", "2","1"),
                    labels=c("Cohort 4", "Cohort 3", "Cohort 2", "Cohort 1" ))

chi_overall <- MUNI_SCORES_3YR_AVG[MUNI_SCORES_3YR_AVG$MUNI=="Chicago", "WEIGHTED_SCORE_3YR"][[1]]
ggplot(CCA_SCORES_3YR_AVG) +
  geom_histogram(aes(x=WEIGHTED_SCORE_3YR, fill=COHORT_3YR), color="#222222", size=0.3,
                 binwidth=bin_width, center=bin_center) +
  geom_vline(linetype="dashed", xintercept=chi_overall, size=0.5, color="#222222") +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title=paste("Distribution of overall scores (CCAs) FY", COHORT_YEAR, sep=""),
       caption="Note: Dashed line represents the overall score for the entire City of Chicago.") +
  theme_cmap(hline=0, ylab="Number of CCAs") +
  scale_fill_manual(values=c(`4`="#AA66CD", `3`="#C29ED7", `2`="#89CD66", `1`="#B4D79E"),
                    breaks=c("4","3", "2","1"),
                    labels=c("Cohort 4", "Cohort 3", "Cohort 2", "Cohort 1" ))


# Map distribution of cohorts (3 yr averaged scores) ---------------------------------------------------------

tmap_mode("plot")  # "plot" (static) or "view" (interactive)
cnty_geo <- filter(county_sf, cmap)
chi_geo <- municipality_sf %>% filter(municipality == "Chicago")

# munis

muni_geo <- municipality_sf %>%
  mutate(GEOID_n = as.integer(geoid_place)) %>%
  left_join(MUNI_SCORES_3YR_AVG, by=c("GEOID_n"="GEOID")) %>%
  mutate(COHORT_n = as.integer(COHORT_3YR))

# muni_labels <- muni_geo %>%
#   filter(MUNI %in% c("Chicago", "Joliet", "Aurora", "Elgin", "Waukegan"))  # Label select munis

 tm_shape(muni_geo, bbox=bb(cnty_geo, ext=1.2)) +
   tm_polygons("COHORT_n", title="", n=4, border.col="#ffffff", lwd=0.5,
               palette=c("#B4D79E", "#89CD66", "#C29ED7", "#AA66CD"),
               labels=c("1 (low need)", "2 (moderate need)", "3 (high need)", "4 (very high need)")) +
   tm_shape(cnty_geo) +
   tm_borders(col="#888888", lwd=2) +
   # tm_shape(muni_labels) +
   #   tm_text("MUNI", size=0.7, col="#000000") +
   tm_legend(legend.position=c("left", "bottom")) +
   tm_layout(title= paste("Assigned cohorts (municipalities) FY", COHORT_YEAR, sep=""), frame=FALSE,
             fontface=get_cmapplot_global("font$strong$face"),
             fontfamily=get_cmapplot_global("font$strong$family"),
             legend.text.fontface=get_cmapplot_global("font$regular$face"),
             legend.text.fontfamily=get_cmapplot_global("font$regular$family"))

# ccas

cca_geo <- cca_sf %>%
  left_join(CCA_SCORES_3YR_AVG, by=c("cca_num"="CCA_ID")) %>%
  mutate(COHORT_n = as.integer(COHORT_3YR))

tm_shape(cca_geo, bbox=bb(cca_geo, ext=1.2)) +
  tm_polygons("COHORT_n", title="", n=4, border.col="#ffffff", lwd=0.5,
              palette=c("#B4D79E", "#89CD66", "#C29ED7", "#AA66CD"),
              labels=c("1 (low need)", "2 (moderate need)", "3 (high need)", "4 (very high need)")) +
  tm_legend(legend.position=c("left", "bottom")) +
  tm_layout(title=paste0("Assigned cohorts (CCAs) FY", COHORT_YEAR, sep=""), frame=FALSE,
            fontface=get_cmapplot_global("font$strong$face"),
            fontfamily=get_cmapplot_global("font$strong$family"),
            legend.text.fontface=get_cmapplot_global("font$regular$face"),
            legend.text.fontfamily=get_cmapplot_global("font$regular$family"))


############## PART 4: EXPORT SCORES

# Write output files (1-year version) -------------------------------------

write_csv(MUNI_CURRENTYR, paste0("output/1yr/cohort_assignments_muni_1yr_", COHORT_YEAR, ".csv"))

write_csv(MUNI_SCORES_3YR_AVG, paste0("output/3yr/cohort_assignments_muni_3yr_", COHORT_YEAR - 2, "_", COHORT_YEAR, ".csv"))

write_csv(CCA_CURRENTYR, paste0("output/1yr/cohort_assignments_cca_1yr_", COHORT_YEAR, ".csv"))

write_csv(CCA_SCORES_3YR_AVG, paste0("output/3yr/cohort_assignments_cca_3yr_", COHORT_YEAR - 2, "_", COHORT_YEAR, ".csv"))


############## PART 5: COMPARE CHANGE OF SCORES

# Compare scores/cohorts against previous assignment ----------------------------

 prev_year <- COHORT_YEAR - 1

 prev_muni_csv <- paste0("output/3yr/cohort_assignments_muni_3yr_", prev_year - 2, "_", prev_year, ".csv")
 prev_cca_csv <- paste0("output/3yr/cohort_assignments_cca_3yr_", prev_year - 2, "_", prev_year, ".csv")

 PREV_SCORES_MUNI <- read_csv(prev_muni_csv, col_types=cols(COHORT_3YR=col_character())) %>%
   rename(
     SCORE_PREV = WEIGHTED_SCORE_3YR,
     COHORT_PREV = COHORT_3YR
   ) %>%
   select(MUNI, SCORE_PREV, COHORT_PREV)

 PREV_SCORES_CCA <- read_csv(prev_cca_csv, col_types=cols(COHORT_3YR=col_character())) %>%
   rename(
     SCORE_PREV = WEIGHTED_SCORE_3YR,
     COHORT_PREV = COHORT_3YR
   ) %>%
   select(CCA_NAME, SCORE_PREV, COHORT_PREV)

 COMPARE_MUNI <- MUNI_SCORES_3YR_AVG %>%
   select(GEOID, MUNI, SCORE = WEIGHTED_SCORE_3YR, COHORT = COHORT_3YR) %>%
   left_join(PREV_SCORES_MUNI, by="MUNI") %>%
   mutate(COHORT_CHG = as.numeric(COHORT) - as.numeric(COHORT_PREV))

 CHANGED_MUNIS <- COMPARE_MUNI[COMPARE_MUNI$COHORT != COMPARE_MUNI$COHORT_PREV, c("MUNI", "SCORE", "COHORT", "COHORT_PREV")]
 CHANGED_MUNIS

 COMPARE_CCA <- CCA_SCORES_3YR_AVG %>%
   select(CCA_ID, CCA_NAME, SCORE = WEIGHTED_SCORE_3YR, COHORT = COHORT_3YR) %>%
   left_join(PREV_SCORES_CCA, by="CCA_NAME") %>%
   mutate(COHORT_CHG = as.numeric(COHORT) - as.numeric(COHORT_PREV))

 CHANGED_CCAS <- COMPARE_CCA[COMPARE_CCA$COHORT != COMPARE_CCA$COHORT_PREV, c("CCA_NAME", "SCORE", "COHORT", "COHORT_PREV")]
 CHANGED_CCAS

 # Descriptive statistics for change in cohorts
 library(modelr)

 cat("MUNICIPALITIES")
 lm_muni <- lm(SCORE ~ SCORE_PREV, data=COMPARE_MUNI)
 cat("Correlation (R-squared):", cor(COMPARE_MUNI$SCORE_PREV, COMPARE_MUNI$SCORE))
 cat("Linear trend: SCORE_new = ", lm_muni$coefficients[1], " + ", lm_muni$coefficients[2], "*SCORE_old", sep="")
 cat("Root-mean-square error (RMSE):", rmse(lm_muni, COMPARE_MUNI))

 cat("CHICAGO COMMUNITY AREAS")
 lm_cca <- lm(SCORE ~ SCORE_PREV, data=COMPARE_CCA)
 cat("Correlation (R-squared):", cor(COMPARE_MUNI$SCORE_PREV, COMPARE_MUNI$SCORE))
 cat("Linear trend: SCORE_new = ", lm_cca$coefficients[1], " + ", lm_cca$coefficients[2], "*SCORE_old", sep="")
 cat("Root-mean-square error (RMSE):", rmse(lm_cca, COMPARE_MUNI))

 # Plots
 ggplot(COMPARE_MUNI) +
   geom_point(aes(x=SCORE_PREV, y=SCORE, color=COHORT), alpha=0.6, size=2) +
   geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
   geom_abline(intercept=lm_muni$coefficients[1], slope=lm_muni$coefficients[2]) +
   scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
   scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
   #labs(title="Updated vs. previous scores (municipalities)") +
   theme_cmap(gridlines="hv", xlab="Previous score", ylab="Updated score",
              legend.position="right", legend.direction="vertical",
              legend.title=element_text()) +
   guides(color=guide_legend(title="Updated cohort")) +
   scale_color_manual(values=c(`4`="#AA66CD", `3`="#C29ED7", `2`="#89CD66", `1`="#B4D79E"),
                     breaks=c("1","2","3","4"),
                     labels=c("1","2","3","4"))


 ggplot(COMPARE_CCA) +
   geom_point(aes(x=SCORE_PREV, y=SCORE, color=COHORT), alpha=0.6, size=3) +
   geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
   geom_abline(intercept=lm_cca$coefficients[1], slope=lm_cca$coefficients[2]) +
   scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
   scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
   labs(title="Updated vs. previous scores (CCAs)") +
   theme_cmap(gridlines="hv", xlab="Previous score", ylab="Updated score",
              legend.position="right", legend.direction="vertical",
              legend.title=element_text()) +
   guides(color=guide_legend(title="Updated cohort"))

 ggplot(COMPARE_MUNI) +
   geom_count(aes(x=COHORT_PREV, y=COHORT, color=COHORT)) +
   scale_size_area(max_size = 20) +
   labs(title="Updated vs. previous cohorts (municipalities)") +
   theme_cmap(gridlines="hv", xlab="Previous cohort", ylab="Updated cohort",
              legend.position="right", legend.direction="vertical") +
   guides(color=guide_none())  +
   scale_color_manual(values=c(`4`="#AA66CD", `3`="#C29ED7", `2`="#89CD66", `1`="#B4D79E"),
                      breaks=c("1","2","3","4"),
                      labels=c("1","2","3","4"))

 ggplot(COMPARE_CCA) +
   geom_count(aes(x=COHORT_PREV, y=COHORT, color=COHORT)) +
   scale_size_area(max_size = 20) +
   labs(title="Updated vs. previous cohorts (CCAs)") +
   theme_cmap(gridlines="hv", xlab="Previous cohort", ylab="Updated cohort",
              legend.position="right", legend.direction="vertical") +
   guides(color=guide_none())

 ggplot(COMPARE_MUNI) +
   geom_histogram(aes(x=COHORT_PREV, fill="Previous"), stat="count", width=0.4, position=position_nudge(x=-0.2)) +
   geom_histogram(aes(x=COHORT, fill="Updated"), stat="count", width=0.4, position=position_nudge(x=0.2)) +
   #labs(title="Updated vs. previous cohorts (municipalities)") +
   theme_cmap(xlab="Cohort", ylab="Number of municipalities")

 ggplot(COMPARE_CCA) +
   geom_histogram(aes(x=COHORT_PREV, fill="Previous"), stat="count", width=0.4, position=position_nudge(x=-0.2)) +
   geom_histogram(aes(x=COHORT, fill="Updated"), stat="count", width=0.4, position=position_nudge(x=0.2)) +
   labs(title="Updated vs. previous cohorts (CCAs)") +
   theme_cmap(xlab="Cohort", ylab="Number of CCAs")

# Maps
 muni_geo <- municipality_sf %>%
   mutate(GEOID_n = as.integer(geoid_place)) %>%
   left_join(COMPARE_MUNI, by=c("GEOID_n"="GEOID"))

  muni_labels <- muni_geo %>%
    filter(MUNI %in% CHANGED_MUNIS$MUNI)

# muni_labels <- muni_geo %>%
#   filter(MUNI %in% c("Chicago", "Joliet", "Aurora", "Elgin", "Waukegan"))

 cca_geo <- cca_sf %>%
   left_join(COMPARE_CCA, by=c("cca_num"="CCA_ID"))

 cca_labels <- cca_geo %>%
   filter(cca_name %in% CHANGED_CCAS$CCA_NAME)


 tm_shape(muni_geo, bbox=bb(cnty_geo, ext=1.4)) +
   tm_polygons("COHORT_CHG", title="", palette="-PuOr", contrast=c(0,1), n=7, border.col="#ffffff", lwd=0.5,
               midpoint=NA, style="fixed", breaks=c(-3,-2,-1,0,1,2,3,4),
               labels=c("-3 (low need)", "-2", "-1", "+0 (no change)", "+1", "+2", "+3 (high need)")) +
 tm_shape(cnty_geo) +
   tm_borders(col="#888888", lwd=2) +
 tm_shape(muni_labels) +
    tm_text("MUNI", size=0.7, col="#000000", auto.placement = 1) +
 tm_legend(legend.position=c("left", "bottom")) +
 tm_layout(#title="Change in municipality cohort (previous to updated)",
           frame=FALSE,
           fontface=get_cmapplot_global("font$strong$face"),
           fontfamily=get_cmapplot_global("font$strong$family"),
           legend.text.fontface=get_cmapplot_global("font$regular$face"),
           legend.text.fontfamily=get_cmapplot_global("font$regular$family"))

 tm_shape(cca_geo, bbox=bb(cca_geo, ext=1.2)) +
   tm_polygons("COHORT_CHG", title="", palette="-PuOr", contrast=c(0,1), n=7, border.col="#ffffff", lwd=0.5,
               midpoint=NA, style="fixed", breaks=c(-3,-2,-1,0,1,2,3,4),
               labels=c("-3 (lower need)", "-2", "-1", "+0 (no change)", "+1", "+2", "+3 (higher need)")) +
 tm_shape(chi_geo) +
   tm_borders(col="#888888", lwd=2) +
 tm_shape(cca_labels) +
   tm_text("cca_name", size=0.7, col="#000000") +
 tm_legend(legend.position=c("left", "bottom")) +
 tm_layout(#title="Change in CCA cohort (previous to updated)",
          frame=FALSE,
           fontface=get_cmapplot_global("font$strong$face"),
           fontfamily=get_cmapplot_global("font$strong$family"),
           legend.text.fontface=get_cmapplot_global("font$regular$face"),
           legend.text.fontfamily=get_cmapplot_global("font$regular$family"))

############## PART 6: EXPORT TABLES FOR UPDATE MEMO

# Tables for memo ---------------------------------------------------------

 # Munis (with current 1-year factors joined)
 MEMO_MUNI_1YR <- FACTORS_MUNI %>%
   mutate(
     POP = exp(ln_POP),
     TAX_BASE_PER_CAP = exp(ln_TAX_BASE_PER_CAP),
     MED_HH_INC = exp(ln_MED_HH_INC)
   ) %>%
   select(GEOID, MED_HH_INC, POP, TAX_BASE_PER_CAP, PCT_EDA_POP)

 MEMO_MUNI <- COMPARE_MUNI %>% # includes previous (3yr) cohort assignment
   mutate(
     COHORT_NAME = paste("Cohort", COHORT),
     PREV_COHORT_NAME = paste("Cohort", COHORT_PREV)
   ) %>%
   left_join(MEMO_MUNI_1YR) %>%
   select(MUNI, COHORT_NAME, PREV_COHORT_NAME, COHORT_CHG, SCORE,
          MED_HH_INC, POP, TAX_BASE_PER_CAP, PCT_EDA_POP) %>%
   rename(
     `Community Name` = MUNI,
     `Cohort` = COHORT_NAME,
     `Previous Cohort` = PREV_COHORT_NAME,
     `Change in Cohort` = COHORT_CHG,
     `Overall Score` = SCORE,
     `Household Median Income` = MED_HH_INC,
     `Population` = POP,
     `Tax Base Per Capita` = TAX_BASE_PER_CAP,
     `Percent of Population in EDAs` = PCT_EDA_POP
   )

 for (cohort_name in c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4")) {
   MEMO_MUNI %>%
     filter(`Cohort` == cohort_name) %>%
     select(-`Cohort`, -`Previous Cohort`, -`Change in Cohort`, -`Overall Score`) %>%
     write_csv(paste0("output/memo/Memo - Municipalities - ", cohort_name, ".csv"))
 }

 MEMO_MUNI %>%
   select(`Community Name`, `Cohort`, `Overall Score`) %>%
   write_csv("output/memo/Memo - Municipalities - All Cohorts - Scores.csv")

 MEMO_MUNI %>%
   filter(`Change in Cohort` < 0) %>%
   rename(`Updated Cohort` = `Cohort`) %>%
   select(`Community Name`, `Previous Cohort`, `Updated Cohort`) %>%
   write_csv("output/memo/Memo - Municipalities - Trending Up.csv")

 MEMO_MUNI %>%
   filter(`Change in Cohort` > 0) %>%
   rename(`Updated Cohort` = `Cohort`) %>%
   select(`Community Name`, `Previous Cohort`, `Updated Cohort`) %>%
   write_csv("output/memo/Memo - Municipalities - Trending Down.csv")

write.csv(MEMO_MUNI, paste("output/memo/_Munidata_", COHORT_YEAR, ".csv",sep=""), row.names=FALSE)


 # CCAs (with current 1-year factors joined)

 MEMO_CCA_1YR <- FACTORS_CCA %>%
   mutate(
     TAX_BASE_PER_CAP = exp(ln_TAX_BASE_PER_CAP),
     MED_HH_INC = exp(ln_MED_HH_INC)
   ) %>%
   left_join(read_xlsx(IN_XLSX, sheet="FACTORS_CCA") %>% select(CCA_NAME, CCA_POP), by="CCA_NAME") %>%
   rename(POP = CCA_POP) %>%
   mutate(POP = round(POP,0)) %>%
   select(CCA_ID, MED_HH_INC, POP, TAX_BASE_PER_CAP, PCT_EDA_POP)

 MEMO_CCA <- COMPARE_CCA %>%
   mutate(
     COHORT_NAME = paste("Cohort", COHORT),
     PREV_COHORT_NAME = paste("Cohort", COHORT_PREV)
   ) %>%
   left_join(MEMO_CCA_1YR) %>%
   select(CCA_NAME, COHORT_NAME, PREV_COHORT_NAME, COHORT_CHG, SCORE,
          MED_HH_INC, POP, TAX_BASE_PER_CAP, PCT_EDA_POP) %>%
   rename(
     `Community Name` = CCA_NAME,
     `Cohort` = COHORT_NAME,
     `Previous Cohort` = PREV_COHORT_NAME,
     `Change in Cohort` = COHORT_CHG,
     `Overall Score` = SCORE,
     `Household Median Income` = MED_HH_INC,
     `Population` = POP,  # Using Chicago's population for each CCA to avoid cohort inflation
     `Tax Base Per Capita` = TAX_BASE_PER_CAP,  # Using hybrid of citywide retail sales per cap + local EAV per cap for CCA tax base per cap
     `Percent of Population in EDAs` = PCT_EDA_POP
   )

 for (cohort_name in c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4")) {
   MEMO_CCA %>%
     filter(`Cohort` == cohort_name) %>%
     select(-`Cohort`, -`Previous Cohort`, -`Change in Cohort`, -`Overall Score`, -`Population`) %>%
     write_csv(paste0("output/memo/Memo - CCAs - ", cohort_name, ".csv"))
 }

 MEMO_CCA %>%
   select(`Community Name`, `Cohort`, `Overall Score`) %>%
   write_csv(paste0("output/memo/Memo - CCAs - All Cohorts - Scores.csv"))

 MEMO_CCA %>%
   filter(`Change in Cohort` < 0) %>%
   rename(`Updated Cohort` = `Cohort`) %>%
   select(`Community Name`, `Previous Cohort`, `Updated Cohort`) %>%
   write_csv("output/memo/Memo - CCAs - Trending Up.csv")

 MEMO_CCA %>%
   filter(`Change in Cohort` > 0) %>%
   rename(`Updated Cohort` = `Cohort`) %>%
   select(`Community Name`, `Previous Cohort`, `Updated Cohort`) %>%
   write_csv("output/memo/Memo - CCAs - Trending Down.csv")

write.csv(MEMO_CCA, paste("output/memo/_CCAdata_", COHORT_YEAR, ".csv", sep=""), row.names=FALSE)

