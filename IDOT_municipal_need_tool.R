#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools", "devtools")
#devtools::install_github("CMAP-REPOS/cmapplot", build_vignettes = TRUE)
library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(cmapplot)
apply_cmap_default_aes()

ANALYSIS_YEAR <- 2021  # Update this each year!
IN_XLSX <- "input/municipality_need_inputs.xlsx"  # Spreadsheet containing latest data


# Load input factors, weights and cohort thresholds -----------------------

FACTORS_MUNI <- read_xlsx(IN_XLSX, sheet="FACTORS_MUNI")
WEIGHTS <- read_xlsx(IN_XLSX, sheet="WEIGHTS")


# Calculate factor-specific scoring thresholds ----------------------------

WEIGHTS$MED <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], median, na.rm = TRUE)[1,])
WEIGHTS$SD <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], sd, na.rm = TRUE)[1,])

# Exclude 0% vacancy rate from median/sd calculations
VAC_NONZERO <- FACTORS_MUNI %>%
  filter(ln_VAC_RATE > log(0.0001)) %>%
  .$ln_VAC_RATE
WEIGHTS[WEIGHTS$FACTOR_NAME=="ln_VAC_RATE", "MED"] <- median(VAC_NONZERO)
WEIGHTS[WEIGHTS$FACTOR_NAME=="ln_VAC_RATE", "SD"] <- sd(VAC_NONZERO)

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
WEIGHTS[WEIGHTS$FACTOR_NAME=="PCT_EJ_POP", paste0("CUT", 1:9)] <- as.list(seq(0.1, 0.9, 0.1))

# Give 0% vacancy rate its own group
WEIGHTS[WEIGHTS$FACTOR_NAME=="ln_VAC_RATE", "CUT1"] <- log(0.0001)


# Calculate factor-specific scores ----------------------------------------

keep_cols_muni <- append(c("GEOID", "MUNI"), WEIGHTS$FACTOR_NAME)
FACTORS_MUNI <- FACTORS_MUNI[, keep_cols_muni]

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
  if (weight < 0) {
    # Reverse score order for factors with negative weights
    FACTORS_MUNI[, score_col] <-  max(groups) + 1 - FACTORS_MUNI[, score_col]
  }
  FACTORS_MUNI[, wt_score_col] <- FACTORS_MUNI[, score_col] * abs(weight)

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
      geom_hline(yintercept=129.8, color="#222222", size=0.5, linetype="dashed") +
      scale_x_continuous(limits=c(min(groups)-0.5, max(groups)+0.5), breaks=groups) +
      labs(title = paste("Distribution of municipality need scores", score_col, sep="\n"),
           caption="Note: Dashed line represents a perfect decile distribution of 129.8 municipalities per group.") +
      theme_cmap(hline=0, ylab="Number of municipalities")
  )
}


# Calculate overall score & cohorts ---------------------------------------

FACTORS_MUNI$WEIGHTED_SCORE <- rowSums(FACTORS_MUNI[, wt_score_cols])

# Rescale from 0-100, and invert relationship (0=low need, 100=high need)
min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(MUNI_NEED_SCORE = 100 - ((WEIGHTED_SCORE - min_wt_score) / (max_wt_score - min_wt_score) * 100))

bin_width = 100 / (max_wt_score - min_wt_score)
bin_center = bin_width / 2

ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=MUNI_NEED_SCORE), color="#222222", fill="#73c9e3", size=0.3,
                 binwidth=bin_width, center=bin_center) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title="Distribution of municipality need scores") +
  theme_cmap(hline=0, ylab="Number of municipalities")


# Map the results ---------------------------------------------------------

IL_E_NAD83 <- 26971  # EPSG projection ID: IL State Plane East (NAD83)
tmap_mode("view")  # "plot" (static) or "view" (interactive)

il_geo <- st_read("input/illinois.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83)

muni_geo <- st_read("input/municipalities.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  mutate(GEOID_n = as.numeric(GEOID)) %>%
  left_join(FACTORS_MUNI, by=c("GEOID_n"="GEOID"))

tm_shape(muni_geo, bbox=bb(il_geo, ext=1.2)) +
  tm_polygons(col="MUNI_NEED_SCORE", title="Municipality Need Score", id="NAME",
              palette="-magma", n=10, border.col="#cccccc", lwd=0.5,
              popup.vars=append("MUNI_NEED_SCORE", score_cols), legend.reverse=TRUE) +
tm_shape(il_geo) +
  tm_borders(col="#888888", lwd=2)


# # Write output files ------------------------------------------------------
#
# OUT_DATA_MUNI <- FACTORS_MUNI %>%
#   select(GEOID, MUNI, MUNI_NEED_SCORE, starts_with("SCORE_"))
#
# write_csv(OUT_DATA_MUNI, paste0("output/municipality_need_scores_", ANALYSIS_YEAR, ".csv"))
#
# # Export shapefile
# OUT_SHP <- paste0("output/IDOT_muni_need_scores_", ANALYSIS_YEAR, "_DRAFT.shp")
# muni_geo %>%
#   select(append("MUNI_NEED_SCORE", score_cols)) %>%
#   rename(MUNI_NEED = MUNI_NEED_SCORE,
#          #D_MED_INC = SCORE_ln_MED_HH_INC,
#          D_EAV_PC = SCORE_ln_NF_TIF_EAV_PER_CAP,
#          D_SVI_SES = SCORE_SVI_SES,
#          D_SVI_MSL = SCORE_ln_SVI_MSL) %>%
#   st_write(OUT_SHP)


# # Compare scores/cohorts against previous year ----------------------------
#
# prev_year <- ANALYSIS_YEAR - 1
# prev_muni_csv <- paste0("output/municipality_need_scores_", prev_year, ".csv")
#
# PREV_SCORES_MUNI <- read_csv(prev_muni_csv) %>%
#   rename(SCORE_PREV = MUNI_NEED_SCORE) %>%
#   select(GEOID, MUNI, SCORE_PREV)
#
# COMPARE_MUNI <- FACTORS_MUNI %>%
#   select(GEOID, MUNI, MUNI_NEED_SCORE) %>%
#   left_join(PREV_SCORES_MUNI, by="GEOID") %>%
#   mutate(SCORE_CHANGE = MUNI_NEED_SCORE - SCORE_PREV)
#
# # Descriptive statistics
# library(modelr)
#
# cat("MUNICIPALITIES")
# lm_muni <- lm(MUNI_NEED_SCORE ~ SCORE_PREV, data=COMPARE_MUNI)
# cat("Correlation (R-squared):", cor(COMPARE_MUNI$SCORE_PREV, COMPARE_MUNI$MUNI_NEED_SCORE, use="pairwise.complete.obs"))
# cat("Linear trend: SCORE_new = ", lm_muni$coefficients[1], " + ", lm_muni$coefficients[2], "*SCORE_old", sep="")
# cat("Root-mean-square error (RMSE):", rmse(lm_muni, COMPARE_MUNI))
#
# # Plots
# ggplot(COMPARE_MUNI) +
#   geom_point(aes(x=SCORE_PREV, y=MUNI_NEED_SCORE), alpha=0.6, size=3) +
#   geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
#   geom_abline(intercept=lm_muni$coefficients[1], slope=lm_muni$coefficients[2]) +
#   scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
#   scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
#   labs(title="Updated vs. previous municipality need scores") +
#   theme_cmap(gridlines="hv", xlab="Previous score", ylab="Updated score",
#              legend.position="right", legend.direction="vertical",
#              legend.title=element_text())
#
# # Maps
# muni_chg_geo <- st_read("input/municipalities.geojson", quiet=TRUE) %>%
#   st_transform(IL_E_NAD83) %>%
#   mutate(GEOID_n = as.numeric(GEOID)) %>%
#   left_join(COMPARE_MUNI, by=c("GEOID_n"="GEOID"))
#
# tm_shape(muni_chg_geo) +
#   tm_polygons("SCORE_CHANGE", id="NAME", palette="-PuOr", contrast=c(0,1), midpoint=0,
#               popup.vars=c("MUNI_NEED_SCORE", "SCORE_PREV", "SCORE_CHANGE"),
#               border.col="#cccccc", lwd=0.5) +
#   tm_shape(il_geo) +
#     tm_borders(col="#888888", lwd=2)









# # Compare scores/cohorts against CMAP cohorts ----------------------------
#
# cmap_muni_csv <- paste0("output/cohort_assignments_muni_2021.csv")
#
# CMAP_SCORES_MUNI <- read_csv(cmap_muni_csv) %>%
#   mutate(SCORE_CMAP = 100 - WEIGHTED_SCORE) %>%
#   select(GEOID, SCORE_CMAP)
#
# COMPARE_MUNI <- FACTORS_MUNI %>%
#   select(GEOID, MUNI, MUNI_NEED_SCORE) %>%
#   rename(SCORE_IDOT = MUNI_NEED_SCORE) %>%
#   right_join(CMAP_SCORES_MUNI, by="GEOID") %>%
#   mutate(SCORE_CHANGE = SCORE_IDOT - SCORE_CMAP)
#
# # Descriptive statistics
# library(modelr)
#
# cat("MUNICIPALITIES")
# lm_muni <- lm(SCORE_IDOT ~ SCORE_CMAP, data=COMPARE_MUNI)
# cat("Correlation (R-squared):", cor(COMPARE_MUNI$SCORE_CMAP, COMPARE_MUNI$SCORE_IDOT, use="pairwise.complete.obs"))
# cat("Linear trend: SCORE_IDOT = ", lm_muni$coefficients[1], " + ", lm_muni$coefficients[2], "*SCORE_CMAP", sep="")
# cat("Root-mean-square error (RMSE):", rmse(lm_muni, COMPARE_MUNI))
#
# # Plots
# ggplot(COMPARE_MUNI) +
#   geom_point(aes(x=SCORE_CMAP, y=SCORE_IDOT), alpha=0.6, size=3) +
#   geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
#   geom_abline(intercept=lm_muni$coefficients[1], slope=lm_muni$coefficients[2]) +
#   scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
#   scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
#   labs(title="IDOT vs. CMAP municipality need scores") +
#   theme_cmap(gridlines="hv", xlab="CMAP score", ylab="IDOT score",
#              legend.position="right", legend.direction="vertical",
#              legend.title=element_text())
#
# # Maps
# muni_chg_geo <- st_read("input/municipalities.geojson", quiet=TRUE) %>%
#   st_transform(IL_E_NAD83) %>%
#   mutate(GEOID_n = as.numeric(GEOID)) %>%
#   right_join(COMPARE_MUNI, by=c("GEOID_n"="GEOID"))
#
# tm_shape(muni_chg_geo) +
#   tm_polygons("SCORE_CHANGE", id="NAME", palette="-PuOr", contrast=c(0,1), midpoint=0,
#               popup.vars=c("SCORE_IDOT", "SCORE_CMAP", "SCORE_CHANGE"),
#               border.col="#cccccc", lwd=0.5)
