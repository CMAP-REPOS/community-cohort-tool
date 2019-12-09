#install.packages("tidyverse", "readxl")
library(tidyverse)
library(readxl)
library(ggplot2)


# Load factors and weights from Excel file --------------------------------

FACTORS <- read_xlsx("LTA_Community_Cohort_Factors.xlsx", sheet="FACTORS")
WEIGHTS <- read_xlsx("LTA_Community_Cohort_Factors.xlsx", sheet="WEIGHTS")


# Calculate means and standard deviations for weighted factors ------------

WEIGHTS$MED <- unlist(summarize_all(FACTORS[, WEIGHTS$FACTOR_NAME], median)[1,])  # Or use median instead?
WEIGHTS$SD <- unlist(summarize_all(FACTORS[, WEIGHTS$FACTOR_NAME], sd)[1,])


# Use median & s.d. to create approx. deciles -----------------------------

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


# Calculate scores for each factor ----------------------------------------

keep_cols <- append(c("GEOID", "MUNI"), WEIGHTS$FACTOR_NAME)
score_cols <- c()
wt_score_cols <- c()
FACTORS <- FACTORS[, keep_cols]

for (factor in WEIGHTS$FACTOR_NAME) {
  weight <- WEIGHTS[WEIGHTS$FACTOR_NAME==factor, "WEIGHT"][[1]]

  score_col <- paste0("SCORE_", factor)
  score_cols <- append(score_cols, score_col)

  wt_score_col <- paste0("WT_SCORE_", factor)
  wt_score_cols <- append(wt_score_cols, wt_score_col)

  cuts <- WEIGHTS[WEIGHTS$FACTOR_NAME==factor,] %>%
    select(starts_with("CUT"))
  groups <- c(1:(length(cuts)-1))
  FACTORS[, score_col] <- cut(as.matrix(FACTORS[, factor]), cuts, groups, labels=FALSE)
  FACTORS[, wt_score_col] <- FACTORS[, score_col] * weight

  # Inspect score distribution
  print(ggplot(FACTORS) +
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
    labs(x=factor) +
    theme_classic())

  print(ggplot(FACTORS) +
    geom_histogram(aes(x=get(score_col)), color="white", fill="skyblue", binwidth=1) +
    scale_x_continuous(limits=c(min(groups)-1, max(groups)+1), breaks=groups) +
    labs(x=score_col) +
    theme_classic())
}


# Calculate overall score -------------------------------------------------

FACTORS$SCORE_OVERALL <- rowSums(FACTORS[, wt_score_cols])

# Rescale from 0-100
min_score <- min(FACTORS$SCORE_OVERALL)
max_score <- max(FACTORS$SCORE_OVERALL)
FACTORS <- FACTORS %>%
  mutate(
    SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_score) / (max_score - min_score) * 100
  )

ggplot(FACTORS) +
  geom_histogram(aes(x=SCORE_OVERALL_SCALED), color="white", fill="skyblue", binwidth=4, center=2) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
  theme_classic()


# Compare scores against the previous scores ------------------------------

PREV_SCORES_CSV <- "S:/AdminGroups/ResearchAnalysis/nmp/LTA/Local_Contribution_Cohort_Tool/data/original_scores_fy20.csv"
PREV_SCORES <- read_csv(PREV_SCORES_CSV, col_types=cols(COHORT=col_character()))

# Rescale from 0-100
min_score_prev <- min(PREV_SCORES$FINAL_SCORE)
max_score_prev <- max(PREV_SCORES$FINAL_SCORE)
PREV_SCORES <- PREV_SCORES %>%
  rename(
    SCORE_PREV = FINAL_SCORE,
    COHORT_PREV = COHORT
  ) %>%
  mutate(
    SCORE_PREV_SCALED = (SCORE_PREV - min_score_prev) / (max_score_prev - min_score_prev) * 100
  )

COMPARE <- FACTORS %>%
  left_join(PREV_SCORES, by="MUNI")

cor(COMPARE$SCORE_PREV_SCALED, COMPARE$SCORE_OVERALL_SCALED)

library(modelr)
lm_old_new <- lm(SCORE_OVERALL_SCALED ~ SCORE_PREV_SCALED, data=COMPARE)
lm_old_new
rmse(lm_old_new, COMPARE)

ggplot(COMPARE) +
  geom_point(aes(x=SCORE_PREV_SCALED, y=SCORE_OVERALL_SCALED, color=COHORT_PREV), alpha=0.6) +
  geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
  geom_abline(intercept=lm_old_new$coefficients[1], slope=lm_old_new$coefficients[2]) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_color_discrete(name="Previous Cohort") +
  labs(x="Previous Score", y="Updated Score") +
  theme_classic()
