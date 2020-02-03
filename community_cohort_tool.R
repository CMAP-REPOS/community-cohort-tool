#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools")
library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)


# Load input factors, weights and cohort thresholds -----------------------

IN_XLSX <- "input/community_cohort_inputs.xlsx"
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
WEIGHTS[WEIGHTS$FACTOR_NAME=="PCT_EDA_POP", paste0("CUT", 1:9)] <- seq(0.1, 0.9, 0.1)


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
  print(ggplot(FACTORS_MUNI) +
          geom_histogram(aes(x=get(factor)), color="white", fill="skyblue", bins=50) +
          geom_vline(xintercept=cuts[[2]], color="maroon", linetype="dotted") +
          geom_vline(xintercept=cuts[[3]], color="maroon", linetype="dotdash") +
          geom_vline(xintercept=cuts[[4]], color="maroon", linetype="dashed") +
          geom_vline(xintercept=cuts[[5]], color="maroon", linetype="longdash") +
          geom_vline(xintercept=cuts[[6]], color="maroon", linetype="solid", size=1) +  # Median
          geom_vline(xintercept=cuts[[7]], color="maroon", linetype="longdash") +
          geom_vline(xintercept=cuts[[8]], color="maroon", linetype="dashed") +
          geom_vline(xintercept=cuts[[9]], color="maroon", linetype="dotdash") +
          geom_vline(xintercept=cuts[[10]], color="maroon", linetype="dotted") +
          labs(title="Distribution of factor values (with group breaks)",
               subtitle=factor,
               x=factor, y="Number of municipalities") +
          theme_classic())

  print(ggplot(FACTORS_MUNI) +
          geom_histogram(aes(x=get(score_col)), color="white", fill="skyblue", binwidth=1) +
          geom_hline(yintercept=28.4, color="maroon", linetype="dashed") +
          scale_x_continuous(limits=c(min(groups)-1, max(groups)+1), breaks=groups) +
          labs(title="Distribution of factor scores", subtitle=score_col,
               caption="Dashed line represents a perfect decile distribution of 28.4 municipalities per group",
               x=score_col, y="Number of municipalities") +
          theme_classic())
}


# Calculate overall score & cohorts ---------------------------------------

FACTORS_MUNI$SCORE_OVERALL <- rowSums(FACTORS_MUNI[, wt_score_cols])
FACTORS_CCA$SCORE_OVERALL <- rowSums(FACTORS_CCA[, wt_score_cols])

# Rescale from 0-100
min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_MUNI$COHORT <- cut(as.matrix(FACTORS_MUNI$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(COHORT = fct_relevel(COHORT, sort))

bin_width = 100 / (max_wt_score - min_wt_score)
bin_center = bin_width / 2

ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=SCORE_OVERALL_SCALED, fill=COHORT), color="white", binwidth=bin_width, center=bin_center) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_fill_discrete(name="Assigned cohort") +
  labs(title="Distribution of overall scores (municipalities)", x="Overall score", y="Number of municipalities") +
  theme_classic()

FACTORS_CCA <- FACTORS_CCA %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_CCA$COHORT <- cut(as.matrix(FACTORS_CCA$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_CCA <- FACTORS_CCA %>%
  mutate(COHORT = fct_relevel(COHORT, sort))

chi_overall <- FACTORS_MUNI[FACTORS_MUNI$MUNI=="Chicago", "SCORE_OVERALL_SCALED"][[1]]
ggplot(FACTORS_CCA) +
  geom_histogram(aes(x=SCORE_OVERALL_SCALED, fill=COHORT), color="white", binwidth=bin_width, center=bin_center) +
  geom_vline(linetype="dashed", xintercept=chi_overall) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_fill_discrete(name="Assigned cohort") +
  labs(title="Distribution of overall scores (CCAs)",
       caption="Dashed line represents the overall score for the entire City of Chicago",
       x="Overall score", y="Number of CCAs") +
  theme_classic()


# Map the results ---------------------------------------------------------

IL_E_NAD83 <- 26971  # EPSG projection ID: IL State Plane East (NAD83)
tmap_mode("plot")  # "plot" (static) or "view" (interactive)

cnty_geo <- st_read("input/cmap_county_boundaries.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83)

muni_geo <- st_read("input/cmap_munis.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  left_join(FACTORS_MUNI, by=c("GEOID_n"="GEOID"))

muni_labels <- muni_geo %>%
  filter(MUNI.x %in% c("Chicago", "Joliet", "Aurora", "Elgin", "Waukegan"))  # Label select munis

cca_geo <- st_read("input/chicago_ccas.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  left_join(FACTORS_CCA, by=c("CCA_NUM"="CCA_ID"))

tm_shape(muni_geo, bbox=bb(cnty_geo, ext=1.2)) +
  tm_polygons("COHORT", title="", palette="Reds", n=4, border.col="#ffffff",
              labels=c("1 (low need)", "2 (moderate need)", "3 (high need)", "4 (very high need)")) +
tm_shape(cnty_geo) +
  tm_lines(col="#888888", lwd=2) +
tm_shape(muni_labels) +
  tm_text("MUNI.x", size=0.7, col="#000000", fontface="bold") +
tm_legend(legend.position=c("left", "bottom")) +
tm_layout(title="Assigned cohorts (municipalities)", frame=FALSE)

tm_shape(cca_geo, bbox=bb(cca_geo, ext=1.2)) +
  tm_polygons("COHORT", title="", palette="Reds", n=4, border.col="#ffffff",
              labels=c("1 (low need)", "2 (moderate need)", "3 (high need)", "4 (very high need)")) +
tm_legend(legend.position=c("left", "bottom")) +
tm_layout(title="Assigned cohorts (CCAs)", frame=FALSE)


# Write output files ------------------------------------------------------

OUT_DATA_MUNI <- FACTORS_MUNI %>%
  rename(WEIGHTED_SCORE = SCORE_OVERALL_SCALED) %>%
  select(GEOID, MUNI, COHORT, WEIGHTED_SCORE, starts_with("SCORE_")) %>%
  select(-SCORE_OVERALL)

write_csv(OUT_DATA_MUNI, "output/cohort_assignments_muni.csv")

OUT_DATA_CCA <- FACTORS_CCA %>%
  rename(WEIGHTED_SCORE = SCORE_OVERALL_SCALED) %>%
  select(CCA_ID, CCA_NAME, COHORT, WEIGHTED_SCORE, starts_with("SCORE_")) %>%
  select(-SCORE_OVERALL)

write_csv(OUT_DATA_CCA, "output/cohort_assignments_cca.csv")


# Compare scores/cohorts against previous methodology ---------------------

PREV_SCORES_MUNI <- read_csv("input/original_scores_fy20_muni.csv", col_types=cols(COHORT=col_character())) %>%
  rename(
    SCORE_PREV = FINAL_SCORE,
    COHORT_PREV = COHORT
  )

PREV_SCORES_CCA <- read_csv("input/original_scores_fy20_cca.csv", col_types=cols(COHORT=col_character())) %>%
  rename(
    SCORE_PREV = FINAL_SCORE,
    COHORT_PREV = COHORT
  )

COMPARE_MUNI <- FACTORS_MUNI %>%
  left_join(PREV_SCORES_MUNI, by="MUNI") %>%
  mutate(
    COHORT_CHG = as.numeric(COHORT) - as.numeric(COHORT_PREV),
    POP = exp(ln_POP)
  )

COMPARE_CCA <- FACTORS_CCA %>%
  left_join(PREV_SCORES_CCA, by="CCA_NAME") %>%
  mutate(
    COHORT_CHG = as.numeric(COHORT) - as.numeric(COHORT_PREV)
  )

CHANGED_MUNIS <- COMPARE_MUNI[COMPARE_MUNI$COHORT != COMPARE_MUNI$COHORT_PREV, c("MUNI", "SCORE_OVERALL_SCALED", "COHORT", "COHORT_PREV", "POP")]
CHANGED_MUNIS

CHANGED_CCAS <- COMPARE_CCA[COMPARE_CCA$COHORT != COMPARE_CCA$COHORT_PREV, c("CCA_NAME", "SCORE_OVERALL_SCALED", "COHORT", "COHORT_PREV")]
CHANGED_CCAS

# Descriptive statistics
library(modelr)

cat("MUNICIPALITIES")
lm_muni <- lm(SCORE_OVERALL_SCALED ~ SCORE_PREV, data=COMPARE_MUNI)
cat("Correlation (R-squared):", cor(COMPARE_MUNI$SCORE_PREV, COMPARE_MUNI$SCORE_OVERALL_SCALED))
cat("Linear trend: SCORE_new = ", lm_muni$coefficients[1], " + ", lm_muni$coefficients[2], "*SCORE_old", sep="")
cat("Root-mean-square error (RMSE):", rmse(lm_muni, COMPARE_MUNI))

cat("CHICAGO COMMUNITY AREAS")
lm_cca <- lm(SCORE_OVERALL_SCALED ~ SCORE_PREV, data=COMPARE_CCA)
cat("Correlation (R-squared):", cor(COMPARE_MUNI$SCORE_PREV, COMPARE_MUNI$SCORE_OVERALL_SCALED))
cat("Linear trend: SCORE_new = ", lm_cca$coefficients[1], " + ", lm_cca$coefficients[2], "*SCORE_old", sep="")
cat("Root-mean-square error (RMSE):", rmse(lm_cca, COMPARE_MUNI))

# Plots
ggplot(COMPARE_MUNI) +
  geom_point(aes(x=SCORE_PREV, y=SCORE_OVERALL_SCALED, color=COHORT_PREV), alpha=0.6) +
  geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
  geom_abline(intercept=lm_muni$coefficients[1], slope=lm_muni$coefficients[2]) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_color_discrete(name="Previous cohort") +
  labs(title="Comparison of updated scores vs. previous scores (municipalities)", x="Previous score", y="Updated score") +
  theme_classic()

ggplot(COMPARE_CCA) +
  geom_point(aes(x=SCORE_PREV, y=SCORE_OVERALL_SCALED, color=COHORT_PREV), alpha=0.6) +
  geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
  geom_abline(intercept=lm_cca$coefficients[1], slope=lm_cca$coefficients[2]) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_color_discrete(name="Previous cohort") +
  labs(title="Comparison of updated scores vs. previous scores (CCAs)", x="Previous score", y="Updated score") +
  theme_classic()

ggplot(COMPARE_MUNI) +
  geom_count(aes(x=COHORT_PREV, y=COHORT), color="maroon") +
  scale_size_area(max_size = 20) +
  labs(title="Municipalities by previous and updated cohort", x="Previous cohort", y="Updated cohort")

ggplot(COMPARE_CCA) +
  geom_count(aes(x=COHORT_PREV, y=COHORT), color="maroon") +
  scale_size_area(max_size = 20) +
  labs(title="CCAs by previous and updated cohort", x="Previous cohort", y="Updated cohort")

ggplot(COMPARE_MUNI) +
  geom_histogram(aes(x=COHORT_PREV, fill="Previous"), stat="count", width=0.4, position=position_nudge(x=-0.2)) +
  geom_histogram(aes(x=COHORT, fill="Updated"), stat="count", width=0.4, position=position_nudge(x=0.2)) +
  labs(title="Comparison of updated cohorts vs. previous cohorts (municipalities)",
       x="Cohort", y="Number of municipalities") +
  theme_classic() +
  theme(legend.title=element_blank())

ggplot(COMPARE_CCA) +
  geom_histogram(aes(x=COHORT_PREV, fill="Previous"), stat="count", width=0.4, position=position_nudge(x=-0.2)) +
  geom_histogram(aes(x=COHORT, fill="Updated"), stat="count", width=0.4, position=position_nudge(x=0.2)) +
  labs(title="Comparison of updated cohorts vs. previous cohorts (CCAs)",
       x="Cohort", y="Number of CCAs") +
  theme_classic() +
  theme(legend.title=element_blank())

COHORT_POP <- COMPARE_MUNI %>%
  select(COHORT, POP) %>%
  group_by(COHORT) %>%
  summarize(POP = sum(POP))

COHORT_POP_PREV <- COMPARE_MUNI %>%
  select(COHORT_PREV, POP) %>%
  group_by(COHORT_PREV) %>%
  summarize(POP = sum(POP))

COHORT_POP_CHG <- COHORT_POP %>%
  left_join(COHORT_POP_PREV, by=c("COHORT" = "COHORT_PREV"), suffix=c("", "_PREV"))

ggplot(COHORT_POP_CHG) +
  geom_col(aes(x=COHORT, y=POP_PREV, fill="Previous"), width=0.4, position=position_nudge(x=-0.2)) +
  geom_col(aes(x=COHORT, y=POP, fill="Updated"), width=0.4, position=position_nudge(x=0.2)) +
  labs(title="Comparison of population in updated cohorts vs. previous cohorts (municipalities)",
       x="Cohort", y="Total population") +
  theme_classic() +
  theme(legend.title=element_blank())

# Maps
muni_geo <- st_read("input/cmap_munis.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  left_join(COMPARE_MUNI, by=c("GEOID_n"="GEOID"))

cca_geo <- st_read("input/chicago_ccas.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  left_join(COMPARE_CCA, by=c("CCA_NUM"="CCA_ID"))

tm_shape(muni_geo, bbox=bb(cnty_geo, ext=1.2)) +
  tm_polygons("COHORT_CHG", title="", palette="-PuOr", contrast=c(0,1), n=7, border.col="#ffffff",
              midpoint=NA, style="fixed", breaks=c(-3,-2,-1,0,1,2,3,4),
              labels=c("-3 (lower need)", "-2", "-1", "+0 (no change)", "+1", "+2", "+3 (higher need)")) +
tm_shape(cnty_geo) +
  tm_lines(col="#888888", lwd=2) +
tm_shape(muni_labels) +
  tm_text("MUNI.x", size=0.7, col="#000000", fontface="bold") +
tm_legend(legend.position=c("left", "bottom")) +
tm_layout(title="Change in municipality cohort (previous to updated)", frame=FALSE)

tm_shape(cca_geo, bbox=bb(cca_geo, ext=1.2)) +
  tm_polygons("COHORT_CHG", title="", palette="-PuOr", contrast=c(0,1), n=7, border.col="#ffffff",
              midpoint=NA, style="fixed", breaks=c(-3,-2,-1,0,1,2,3,4),
              labels=c("-3 (lower need)", "-2", "-1", "+0 (no change)", "+1", "+2", "+3 (higher need)")) +
tm_legend(legend.position=c("left", "bottom")) +
tm_layout(title="Change in CCA cohort (previous to updated)", frame=FALSE)


# Tables for memo ---------------------------------------------------------

MEMO_MUNI <- FACTORS_MUNI %>%
  mutate(
    POP = exp(ln_POP),
    TAX_BASE_PER_CAP = exp(ln_TAX_BASE_PER_CAP),
    MED_HH_INC = exp(ln_MED_HH_INC),
    COHORT_NAME = paste("Cohort", COHORT)
  ) %>%
  select(MUNI, COHORT_NAME, SCORE_OVERALL_SCALED, MED_HH_INC, POP, TAX_BASE_PER_CAP, PCT_EDA_POP) %>%
  rename(
    `Community Name` = MUNI,
    `Cohort` = COHORT_NAME,
    `Overall Score` = SCORE_OVERALL_SCALED,
    `Median Income` = MED_HH_INC,
    `Population` = POP,
    `Tax Base Per Capita` = TAX_BASE_PER_CAP,
    `Population in EDAs` = PCT_EDA_POP
  )

for (cohort_name in c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4")) {
  MEMO_MUNI %>%
    filter(`Cohort` == cohort_name) %>%
    select(-`Cohort`, -`Overall Score`) %>%
    write_csv(paste0("output/Memo - Municipalities - ", cohort_name, ".csv"))
}

MEMO_MUNI %>%
  select(`Community Name`, `Cohort`, `Overall Score`) %>%
  write_csv(paste0("output/Memo - Municipalities - All Cohorts - Scores.csv"))

MEMO_CCA <- FACTORS_CCA %>%
  mutate(
    POP = exp(ln_POP),
    TAX_BASE_PER_CAP = exp(ln_TAX_BASE_PER_CAP),
    MED_HH_INC = exp(ln_MED_HH_INC),
    COHORT_NAME = paste("Cohort", COHORT)
  ) %>%
  select(CCA_NAME, COHORT_NAME, SCORE_OVERALL_SCALED, MED_HH_INC, POP, TAX_BASE_PER_CAP, PCT_EDA_POP) %>%
  rename(
    `Community Name` = CCA_NAME,
    `Cohort` = COHORT_NAME,
    `Overall Score` = SCORE_OVERALL_SCALED,
    `Median Income` = MED_HH_INC,
    `Population` = POP,  # Still using entire city's population for each CCA
    `Tax Base Per Capita` = TAX_BASE_PER_CAP,  # Still using entire city's TB/C for each CCA
    `Population in EDAs` = PCT_EDA_POP
  )

for (cohort_name in c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4")) {
  MEMO_CCA %>%
    filter(`Cohort` == cohort_name) %>%
    select(-`Cohort`, -`Overall Score`, -`Population`, -`Tax Base Per Capita`) %>%
    write_csv(paste0("output/Memo - CCAs - ", cohort_name, ".csv"))
}

MEMO_CCA %>%
  select(`Community Name`, `Cohort`, `Overall Score`) %>%
  write_csv(paste0("output/Memo - CCAs - All Cohorts - Scores.csv"))

